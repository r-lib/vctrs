#include <rlang.h>
#include "vctrs.h"
#include "utils.h"
#include "complete.h"
#include "poly-op.h"
#include "compare.h"
#include "ptype2.h"
#include "order.h"
#include "match-joint.h"

// -----------------------------------------------------------------------------

enum vctrs_multiple {
  VCTRS_MULTIPLE_all = 0,
  VCTRS_MULTIPLE_warning = 1,
  VCTRS_MULTIPLE_error = 2,
  VCTRS_MULTIPLE_first = 3,
  VCTRS_MULTIPLE_last = 4
};

enum vctrs_filter {
  VCTRS_FILTER_none = 0,
  VCTRS_FILTER_min = 1,
  VCTRS_FILTER_max = 2
};

enum vctrs_ops {
  VCTRS_OPS_eq = 0,
  VCTRS_OPS_gt = 1,
  VCTRS_OPS_gte = 2,
  VCTRS_OPS_lt = 3,
  VCTRS_OPS_lte = 4
};

enum vctrs_incomplete_action {
  VCTRS_INCOMPLETE_ACTION_match = 0,
  VCTRS_INCOMPLETE_ACTION_value = 1,
  VCTRS_INCOMPLETE_ACTION_drop = 2,
  VCTRS_INCOMPLETE_ACTION_error = 3
};
struct vctrs_incomplete {
  enum vctrs_incomplete_action action;
  int value;
};

enum vctrs_no_match_action {
  VCTRS_NO_MATCH_ACTION_drop = 0,
  VCTRS_NO_MATCH_ACTION_error = 1,
  VCTRS_NO_MATCH_ACTION_value = 2
};
struct vctrs_no_match {
  enum vctrs_no_match_action action;
  int value;
};

enum vctrs_remaining_action {
  VCTRS_REMAINING_ACTION_drop = 0,
  VCTRS_REMAINING_ACTION_error = 1,
  VCTRS_REMAINING_ACTION_value = 2
};
struct vctrs_remaining {
  enum vctrs_remaining_action action;
  int value;
};

#define SIGNAL_NO_MATCH r_globals.na_int
#define SIGNAL_INCOMPLETE -1

// -----------------------------------------------------------------------------

#include "decl/match-decl.h"

// -----------------------------------------------------------------------------

// [[ register() ]]
r_obj* vctrs_matches(r_obj* needles,
                     r_obj* haystack,
                     r_obj* condition,
                     r_obj* filter,
                     r_obj* incomplete,
                     r_obj* no_match,
                     r_obj* remaining,
                     r_obj* multiple,
                     r_obj* nan_distinct,
                     r_obj* chr_transform,
                     r_obj* needles_arg,
                     r_obj* haystack_arg) {
  const struct vctrs_incomplete c_incomplete = parse_incomplete(incomplete);
  const struct vctrs_no_match c_no_match = parse_no_match(no_match);
  const struct vctrs_remaining c_remaining = parse_remaining(remaining);
  enum vctrs_multiple c_multiple = parse_multiple(multiple);

  // TODO: Use `r_arg_as_bool()`
  if (!r_is_bool(nan_distinct)) {
    r_abort("`nan_distinct` must be a single `TRUE` or `FALSE`.");
  }
  bool c_nan_distinct = r_lgl_get(nan_distinct, 0);

  struct vctrs_arg c_needles_arg = vec_as_arg(needles_arg);
  struct vctrs_arg c_haystack_arg = vec_as_arg(haystack_arg);

  return vec_matches(
    needles,
    haystack,
    condition,
    filter,
    &c_incomplete,
    &c_no_match,
    &c_remaining,
    c_multiple,
    c_nan_distinct,
    chr_transform,
    &c_needles_arg,
    &c_haystack_arg
  );
}

static
r_obj* vec_matches(r_obj* needles,
                   r_obj* haystack,
                   r_obj* condition,
                   r_obj* filter,
                   const struct vctrs_incomplete* incomplete,
                   const struct vctrs_no_match* no_match,
                   const struct vctrs_remaining* remaining,
                   enum vctrs_multiple multiple,
                   bool nan_distinct,
                   r_obj* chr_transform,
                   struct vctrs_arg* needles_arg,
                   struct vctrs_arg* haystack_arg) {
  int n_prot = 0;

  int _;
  r_obj* ptype = KEEP_N(vec_ptype2_params(
    needles,
    haystack,
    needles_arg,
    haystack_arg,
    DF_FALLBACK_quiet,
    &_
  ), &n_prot);

  needles = KEEP_N(vec_cast_params(
    needles,
    ptype,
    needles_arg,
    args_empty,
    DF_FALLBACK_quiet,
    S3_FALLBACK_false
  ), &n_prot);

  haystack = KEEP_N(vec_cast_params(
    haystack,
    ptype,
    haystack_arg,
    args_empty,
    DF_FALLBACK_quiet,
    S3_FALLBACK_false
  ), &n_prot);

  r_ssize size_needles = vec_size(needles);
  r_ssize size_haystack = vec_size(haystack);

  // Support non-data frame types by wrapping them in a 1-col data frame
  if (!is_data_frame(needles)) {
    needles = KEEP_N(r_list(needles), &n_prot);
    haystack = KEEP_N(r_list(haystack), &n_prot);

    r_obj* names = KEEP_N(r_chr("x"), &n_prot);
    r_poke_names(needles, names);
    r_poke_names(haystack, names);

    r_init_data_frame(needles, size_needles);
    r_init_data_frame(haystack, size_haystack);
  }

  if (condition == r_null) {
    // Special case of "match on nothing" to mimic `LEFT JOIN needles, haystack`
    // with no ON condition
    r_obj* out = expand_match_on_nothing(
      size_needles,
      size_haystack,
      multiple,
      no_match,
      remaining,
      needles_arg,
      haystack_arg
    );
    FREE(n_prot);
    return out;
  }

  r_ssize n_cols = r_length(needles);

  enum vctrs_ops* v_ops = (enum vctrs_ops*) R_alloc(n_cols, sizeof(enum vctrs_ops));
  parse_condition(condition, n_cols, v_ops);

  enum vctrs_filter* v_filters = (enum vctrs_filter*) R_alloc(n_cols, sizeof(enum vctrs_filter));
  parse_filter(filter, n_cols, v_filters);

  bool any_filters = false;
  for (r_ssize i = 0; i < n_cols; ++i) {
    if (v_filters[i] != VCTRS_FILTER_none) {
      any_filters = true;
      break;
    }
  }

  if (n_cols == 0) {
    // If we have a match `condition`, but there are no columns, this operation
    // isn't well defined
    r_abort("Must have at least 1 column to match on unless `condition = NULL`.");
  }

  // Compute the locations of incomplete values per column since computing
  // joint ranks per column is going to replace the incomplete values with
  // integer ranks
  r_obj* needles_complete = df_detect_complete_by_col(needles, size_needles, n_cols);
  KEEP_N(needles_complete, &n_prot);

  r_obj* haystack_complete = df_detect_complete_by_col(haystack, size_haystack, n_cols);
  KEEP_N(haystack_complete, &n_prot);

  // Compute joint xtfrm to simplify each column down to an integer vector
  r_obj* args = KEEP_N(df_joint_xtfrm_by_col(
    needles,
    haystack,
    size_needles,
    size_haystack,
    n_cols,
    nan_distinct,
    chr_transform
  ), &n_prot);
  needles = r_list_get(args, 0);
  haystack = r_list_get(args, 1);

  r_obj* out = df_matches(
    needles,
    haystack,
    needles_complete,
    haystack_complete,
    size_needles,
    size_haystack,
    incomplete,
    no_match,
    remaining,
    multiple,
    any_filters,
    v_filters,
    v_ops,
    needles_arg,
    haystack_arg
  );

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

static
r_obj* df_matches(r_obj* needles,
                  r_obj* haystack,
                  r_obj* needles_complete,
                  r_obj* haystack_complete,
                  r_ssize size_needles,
                  r_ssize size_haystack,
                  const struct vctrs_incomplete* incomplete,
                  const struct vctrs_no_match* no_match,
                  const struct vctrs_remaining* remaining,
                  enum vctrs_multiple multiple,
                  bool any_filters,
                  const enum vctrs_filter* v_filters,
                  const enum vctrs_ops* v_ops,
                  struct vctrs_arg* needles_arg,
                  struct vctrs_arg* haystack_arg) {
  int n_prot = 0;

  r_obj* o_needles = KEEP_N(vec_order(needles, chrs_asc, chrs_smallest, true, r_null), &n_prot);
  const int* v_o_needles = r_int_cbegin(o_needles);

  r_obj* info = KEEP_N(compute_nested_containment_info(
    haystack,
    multiple,
    v_ops,
    haystack_arg
  ), &n_prot);

  r_obj* o_haystack = r_list_get(info, 0);
  const int* v_o_haystack = r_int_cbegin(o_haystack);

  // Will be `integer()` if no nesting info is required.
  // In that case, `n_nested_groups == 1`.
  r_obj* nested_groups = r_list_get(info, 1);
  const int* v_nested_groups = r_int_cbegin(nested_groups);

  int n_nested_groups = r_as_int(r_list_get(info, 2));
  bool any_non_equi = r_as_bool(r_list_get(info, 3));

  // In the case of possible multiple matches that fall in separate
  // nested containers, allocate ~20% extra room
  r_ssize initial_capacity =
    (n_nested_groups == 1) ?
    size_needles :
    r_double_as_ssize(r_ssize_as_double(size_needles) * 1.2);

  struct r_dyn_array* p_loc_first_match_o_haystack = r_new_dyn_vector(R_TYPE_integer, initial_capacity);
  KEEP_N(p_loc_first_match_o_haystack->shelter, &n_prot);

  {
    // Temporary unstable pointer
    int* v_loc_first_match_o_haystack = (int*) r_arr_begin(p_loc_first_match_o_haystack);
    for (r_ssize i = 0; i < size_needles; ++i) {
      // Initialize to no match everywhere, no need to initialize extra buffer
      v_loc_first_match_o_haystack[i] = SIGNAL_NO_MATCH;
    }
    p_loc_first_match_o_haystack->count = size_needles;
  }

  // If we can skip, `size_match` will always be `1`
  const bool skip_size_match = (multiple == VCTRS_MULTIPLE_first || multiple == VCTRS_MULTIPLE_last);

  struct r_dyn_array* p_size_match = NULL;
  if (!skip_size_match) {
    p_size_match = r_new_dyn_vector(R_TYPE_integer, initial_capacity);
    KEEP_N(p_size_match->shelter, &n_prot);

    int* v_size_match = (int*) r_arr_begin(p_size_match);
    for (r_ssize i = 0; i < size_needles; ++i) {
      // No need to initialize extra buffer
      v_size_match[i] = 1;
    }
    p_size_match->count = size_needles;
  }

  // If we can skip, `loc_needles` will always be an increasing sequence of values
  const bool skip_loc_needles = (multiple == VCTRS_MULTIPLE_first || multiple == VCTRS_MULTIPLE_last);

  struct r_dyn_array* p_loc_needles = NULL;
  if (!skip_loc_needles) {
    p_loc_needles = r_new_dyn_vector(R_TYPE_integer, initial_capacity);
    KEEP_N(p_loc_needles->shelter, &n_prot);

    int* v_loc_needles = (int*) r_arr_begin(p_loc_needles);
    for (r_ssize i = 0; i < size_needles; ++i) {
      // No need to initialize extra buffer
      v_loc_needles[i] = i;
    }
    p_loc_needles->count = size_needles;
  }

  // When filtering, we find the filtered match for a particular needle in each
  // nested containment group of the haystack. `locs_filter_match_haystack`
  // keeps track of the overall filtered match loc for a needle across all
  // nested groups in the haystack.
  bool has_locs_filter_match_haystack =
    any_filters &&
    (multiple == VCTRS_MULTIPLE_all ||
     multiple == VCTRS_MULTIPLE_warning ||
     multiple == VCTRS_MULTIPLE_error);

  int* v_locs_filter_match_haystack = NULL;
  if (has_locs_filter_match_haystack) {
    r_obj* locs_filter_match_haystack = KEEP_N(r_alloc_integer(size_needles), &n_prot);
    v_locs_filter_match_haystack = r_int_begin(locs_filter_match_haystack);
  }

  struct poly_vec* p_poly_needles = new_poly_vec(needles, vctrs_type_dataframe);
  PROTECT_POLY_VEC(p_poly_needles, &n_prot);
  const struct poly_df_data* p_needles = (const struct poly_df_data*) p_poly_needles->p_vec;

  struct poly_vec* p_poly_haystack = new_poly_vec(haystack, vctrs_type_dataframe);
  PROTECT_POLY_VEC(p_poly_haystack, &n_prot);
  const struct poly_df_data* p_haystack = (const struct poly_df_data*) p_poly_haystack->p_vec;

  const struct poly_vec* p_poly_needles_complete = new_poly_vec(needles_complete, vctrs_type_dataframe);
  PROTECT_POLY_VEC(p_poly_needles_complete, &n_prot);
  const struct poly_df_data* p_needles_complete = (const struct poly_df_data*) p_poly_needles_complete->p_vec;

  struct poly_vec* p_poly_haystack_complete = new_poly_vec(haystack_complete, vctrs_type_dataframe);
  PROTECT_POLY_VEC(p_poly_haystack_complete, &n_prot);
  const struct poly_df_data* p_haystack_complete = (const struct poly_df_data*) p_poly_haystack_complete->p_vec;

  r_ssize n_extra = 0;

  if (size_needles > 0) {
    // Recursion requires at least 1 row in needles.
    // In the case of size 0 needles, there is nothing to do, but this avoids
    // a segfault.

    const r_ssize col = 0;
    const r_ssize loc_lower_bound_o_needles = 0;
    const r_ssize loc_upper_bound_o_needles = size_needles - 1;

    if (n_nested_groups == 1) {
      const r_ssize loc_lower_bound_o_haystack = 0;
      const r_ssize loc_upper_bound_o_haystack = size_haystack - 1;

      df_matches_recurse(
        col,
        loc_lower_bound_o_needles,
        loc_upper_bound_o_needles,
        loc_lower_bound_o_haystack,
        loc_upper_bound_o_haystack,
        p_needles,
        p_haystack,
        p_needles_complete,
        p_haystack_complete,
        v_o_needles,
        v_o_haystack,
        incomplete,
        multiple,
        any_filters,
        v_filters,
        v_ops,
        p_loc_first_match_o_haystack,
        p_size_match,
        p_loc_needles,
        v_locs_filter_match_haystack,
        &n_extra
      );
    } else {
      df_matches_with_nested_groups(
        size_haystack,
        n_nested_groups,
        v_nested_groups,
        col,
        loc_lower_bound_o_needles,
        loc_upper_bound_o_needles,
        p_needles,
        p_haystack,
        p_needles_complete,
        p_haystack_complete,
        v_o_needles,
        v_o_haystack,
        incomplete,
        multiple,
        any_filters,
        v_filters,
        v_ops,
        p_loc_first_match_o_haystack,
        p_size_match,
        p_loc_needles,
        v_locs_filter_match_haystack,
        &n_extra
      );
    }
  }

  r_obj* out = KEEP_N(expand_compact_indices(
    v_o_haystack,
    p_loc_first_match_o_haystack,
    p_size_match,
    p_loc_needles,
    skip_size_match,
    skip_loc_needles,
    incomplete,
    no_match,
    remaining,
    multiple,
    size_needles,
    size_haystack,
    any_non_equi,
    has_locs_filter_match_haystack,
    v_filters,
    v_locs_filter_match_haystack,
    p_haystack,
    needles_arg,
    haystack_arg
  ), &n_prot);

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

static
void df_matches_recurse(r_ssize col,
                        r_ssize loc_lower_bound_o_needles,
                        r_ssize loc_upper_bound_o_needles,
                        r_ssize loc_lower_bound_o_haystack,
                        r_ssize loc_upper_bound_o_haystack,
                        const struct poly_df_data* p_needles,
                        const struct poly_df_data* p_haystack,
                        const struct poly_df_data* p_needles_complete,
                        const struct poly_df_data* p_haystack_complete,
                        const int* v_o_needles,
                        const int* v_o_haystack,
                        const struct vctrs_incomplete* incomplete,
                        enum vctrs_multiple multiple,
                        bool any_filters,
                        const enum vctrs_filter* v_filters,
                        const enum vctrs_ops* v_ops,
                        struct r_dyn_array* p_loc_first_match_o_haystack,
                        struct r_dyn_array* p_size_match,
                        struct r_dyn_array* p_loc_needles,
                        int* v_locs_filter_match_haystack,
                        r_ssize* p_n_extra) {
  const enum vctrs_ops op = v_ops[col];
  const enum vctrs_filter filter = v_filters[col];
  const r_ssize n_col = p_needles->n_col;

  const int* v_needles = (const int*) p_needles->col_ptrs[col];
  const int* v_needles_complete = (const int*) p_needles_complete->col_ptrs[col];

  const int* v_haystack = (const int*) p_haystack->col_ptrs[col];
  const int* v_haystack_complete = (const int*) p_haystack_complete->col_ptrs[col];

  const r_ssize loc_mid_bound_o_needles = midpoint(loc_lower_bound_o_needles, loc_upper_bound_o_needles);
  const r_ssize loc_mid_bound_needles = v_o_needles[loc_mid_bound_o_needles] - 1;

  const int val_needle = v_needles[loc_mid_bound_needles];
  const bool needle_is_complete = v_needles_complete[loc_mid_bound_needles];

  // Find lower and upper duplicate location for this needle
  r_ssize loc_lower_duplicate_o_needles = int_lower_duplicate(
    val_needle,
    v_needles,
    v_o_needles,
    loc_lower_bound_o_needles,
    loc_mid_bound_o_needles
  );
  r_ssize loc_upper_duplicate_o_needles = int_upper_duplicate(
    val_needle,
    v_needles,
    v_o_needles,
    loc_mid_bound_o_needles,
    loc_upper_bound_o_needles
  );

  if (incomplete->action != VCTRS_INCOMPLETE_ACTION_match && !needle_is_complete) {
    // Signal incomplete needle, don't recursive into further columns.
    for (r_ssize i = loc_lower_duplicate_o_needles; i <= loc_upper_duplicate_o_needles; ++i) {
      // Will always be the first and only time the output is touched for this
      // needle, so we can poke directly into it
      const int loc_needles = v_o_needles[i] - 1;
      R_ARR_POKE(int, p_loc_first_match_o_haystack, loc_needles, SIGNAL_INCOMPLETE);
    }

    // Learned nothing about haystack!
    bool do_lhs = loc_lower_duplicate_o_needles > loc_lower_bound_o_needles;
    bool do_rhs = loc_upper_duplicate_o_needles < loc_upper_bound_o_needles;

    if (do_lhs) {
      const r_ssize lhs_loc_lower_bound_o_needles = loc_lower_bound_o_needles;
      const r_ssize lhs_loc_upper_bound_o_needles = loc_lower_duplicate_o_needles - 1;

      df_matches_recurse(
        col,
        lhs_loc_lower_bound_o_needles,
        lhs_loc_upper_bound_o_needles,
        loc_lower_bound_o_haystack,
        loc_upper_bound_o_haystack,
        p_needles,
        p_haystack,
        p_needles_complete,
        p_haystack_complete,
        v_o_needles,
        v_o_haystack,
        incomplete,
        multiple,
        any_filters,
        v_filters,
        v_ops,
        p_loc_first_match_o_haystack,
        p_size_match,
        p_loc_needles,
        v_locs_filter_match_haystack,
        p_n_extra
      );
    }
    if (do_rhs) {
      const r_ssize rhs_loc_lower_bound_o_needles = loc_upper_duplicate_o_needles + 1;
      const r_ssize rhs_loc_upper_bound_o_needles = loc_upper_bound_o_needles;

      df_matches_recurse(
        col,
        rhs_loc_lower_bound_o_needles,
        rhs_loc_upper_bound_o_needles,
        loc_lower_bound_o_haystack,
        loc_upper_bound_o_haystack,
        p_needles,
        p_haystack,
        p_needles_complete,
        p_haystack_complete,
        v_o_needles,
        v_o_haystack,
        incomplete,
        multiple,
        any_filters,
        v_filters,
        v_ops,
        p_loc_first_match_o_haystack,
        p_size_match,
        p_loc_needles,
        v_locs_filter_match_haystack,
        p_n_extra
      );
    }

    return;
  }

  r_ssize loc_lower_match_o_haystack = loc_lower_bound_o_haystack;
  r_ssize loc_upper_match_o_haystack = loc_upper_bound_o_haystack;

  while (loc_lower_match_o_haystack <= loc_upper_match_o_haystack) {
    const r_ssize loc_mid_match_o_haystack = midpoint(loc_lower_match_o_haystack, loc_upper_match_o_haystack);
    const r_ssize loc_mid_match_haystack = v_o_haystack[loc_mid_match_o_haystack] - 1;
    const int val_haystack = v_haystack[loc_mid_match_haystack];

    const int cmp = int_compare_na_equal(val_needle, val_haystack);

    if (cmp == 1) {
      loc_lower_match_o_haystack = loc_mid_match_o_haystack + 1;
    } else if (cmp == -1) {
      loc_upper_match_o_haystack = loc_mid_match_o_haystack - 1;
    } else {
      // Hit!
      // Find lower and upper match bounds for the haystack value
      loc_lower_match_o_haystack = int_lower_duplicate(
        val_haystack,
        v_haystack,
        v_o_haystack,
        loc_lower_match_o_haystack,
        loc_mid_match_o_haystack
      );
      loc_upper_match_o_haystack = int_upper_duplicate(
        val_haystack,
        v_haystack,
        v_o_haystack,
        loc_mid_match_o_haystack,
        loc_upper_match_o_haystack
      );
      break;
    }
  }

  // Adjust bounds based on non-equi condition.
  // If needle is NA, never extend the bounds to capture values past it.
  switch (op) {
  case VCTRS_OPS_lt: {
    // Exclude found needle
    loc_lower_match_o_haystack = loc_upper_match_o_haystack + 1;
    if (needle_is_complete) {
      loc_upper_match_o_haystack = loc_upper_bound_o_haystack;
    }
    break;
  }
  case VCTRS_OPS_lte: {
    if (needle_is_complete) {
      loc_upper_match_o_haystack = loc_upper_bound_o_haystack;
    }
    break;
  }
  case VCTRS_OPS_gt: {
    // Exclude found needle
    loc_upper_match_o_haystack = loc_lower_match_o_haystack - 1;
    if (needle_is_complete) {
      loc_lower_match_o_haystack = loc_lower_bound_o_haystack;
    }
    break;
  }
  case VCTRS_OPS_gte: {
    if (needle_is_complete) {
      loc_lower_match_o_haystack = loc_lower_bound_o_haystack;
    }
    break;
  }
  case VCTRS_OPS_eq: {
    break;
  }
  }

  if (needle_is_complete &&
      (op == VCTRS_OPS_gt || op == VCTRS_OPS_gte) &&
      (loc_lower_match_o_haystack <= loc_upper_match_o_haystack)) {
    // In this specific case, a non-NA needle may match an NA in the haystack
    // from the condition adjustments made above. If there was an NA in the
    // haystack, we avoid including it by shifting the lower bound to 1 past
    // the final NA.
    const r_ssize loc_lower_match_haystack = v_o_haystack[loc_lower_match_o_haystack] - 1;
    const bool lower_match_haystack_is_complete = v_haystack_complete[loc_lower_match_haystack];

    if (!lower_match_haystack_is_complete) {
      /* If there was an NA in the haystack, find the last NA */
      loc_lower_match_o_haystack = int_locate_upper_incomplete(
        v_haystack_complete,
        v_o_haystack,
        loc_lower_match_o_haystack,
        loc_upper_match_o_haystack
      );

      /* Exclude it and all before it */
      ++loc_lower_match_o_haystack;
    }
  }

  if (loc_lower_match_o_haystack <= loc_upper_match_o_haystack) {
    // Hit!

    switch (filter) {
    case VCTRS_FILTER_max: {
      if (!needle_is_complete || op == VCTRS_OPS_eq) {
        // Lower bound value will already equal upper bound value
        break;
      }
      if (multiple == VCTRS_MULTIPLE_last) {
        // "last" only requires the upper bound, which already points to "max"
        break;
      }

      // We want the max values of this group. That's the upper match of the
      // haystack and its corresponding lower duplicate.
      const int loc_lower_match_haystack = v_o_haystack[loc_lower_match_o_haystack] - 1;
      const int loc_upper_match_haystack = v_o_haystack[loc_upper_match_o_haystack] - 1;
      const int val_lower_match_haystack = v_haystack[loc_lower_match_haystack];
      const int val_upper_match_haystack = v_haystack[loc_upper_match_haystack];

      if (val_lower_match_haystack != val_upper_match_haystack) {
        loc_lower_match_o_haystack = int_lower_duplicate(
          val_upper_match_haystack,
          v_haystack,
          v_o_haystack,
          loc_lower_match_o_haystack,
          loc_upper_match_o_haystack
        );
      }

      break;
    }
    case VCTRS_FILTER_min: {
      if (!needle_is_complete || op == VCTRS_OPS_eq) {
        // Lower bound value will already equal upper bound value
        break;
      }
      if (multiple == VCTRS_MULTIPLE_first) {
        // "first" only requires the lower bound, which already points to "min"
        break;
      }

      // We want the min values of this group. That's the lower match of the
      // haystack and its corresponding upper duplicate.
      const int loc_lower_match_haystack = v_o_haystack[loc_lower_match_o_haystack] - 1;
      const int loc_upper_match_haystack = v_o_haystack[loc_upper_match_o_haystack] - 1;
      const int val_lower_match_haystack = v_haystack[loc_lower_match_haystack];
      const int val_upper_match_haystack = v_haystack[loc_upper_match_haystack];

      if (val_lower_match_haystack != val_upper_match_haystack) {
        loc_upper_match_o_haystack = int_upper_duplicate(
          val_lower_match_haystack,
          v_haystack,
          v_o_haystack,
          loc_lower_match_o_haystack,
          loc_upper_match_o_haystack
        );
      }

      break;
    }
    case VCTRS_FILTER_none: {
      break;
    }
    }

    if (col < n_col - 1) {
      // Recurse into next column on this subgroup
      df_matches_recurse(
        col + 1,
        loc_lower_duplicate_o_needles,
        loc_upper_duplicate_o_needles,
        loc_lower_match_o_haystack,
        loc_upper_match_o_haystack,
        p_needles,
        p_haystack,
        p_needles_complete,
        p_haystack_complete,
        v_o_needles,
        v_o_haystack,
        incomplete,
        multiple,
        any_filters,
        v_filters,
        v_ops,
        p_loc_first_match_o_haystack,
        p_size_match,
        p_loc_needles,
        v_locs_filter_match_haystack,
        p_n_extra
      );
    } else {
      for (r_ssize i = loc_lower_duplicate_o_needles; i <= loc_upper_duplicate_o_needles; ++i) {
        const int loc_needles = v_o_needles[i] - 1;
        const int loc_first_match_o_haystack = R_ARR_GET(int, p_loc_first_match_o_haystack, loc_needles);
        const bool first_touch = loc_first_match_o_haystack == r_globals.na_int;

        switch (multiple) {
        case VCTRS_MULTIPLE_first: {
          if (first_touch) {
            R_ARR_POKE(int, p_loc_first_match_o_haystack, loc_needles, loc_lower_match_o_haystack);
            break;
          }

          const int loc_first_match_haystack = v_o_haystack[loc_first_match_o_haystack] - 1;
          const int loc_lower_match_haystack = v_o_haystack[loc_lower_match_o_haystack] - 1;

          if (any_filters) {
            int cmp = p_matches_df_compare_na_equal(
              p_haystack,
              loc_lower_match_haystack,
              p_haystack,
              loc_first_match_haystack,
              v_filters
            );

            if (cmp == -1) {
              // New haystack value loses vs previous one, do nothing
              break;
            } else if (cmp == 1) {
              // New haystack value wins vs previous one, automatically update like first_touch
              R_ARR_POKE(int, p_loc_first_match_o_haystack, loc_needles, loc_lower_match_o_haystack);
              break;
            }
          }

          if (loc_lower_match_haystack < loc_first_match_haystack) {
            // New start is before current one
            R_ARR_POKE(int, p_loc_first_match_o_haystack, loc_needles, loc_lower_match_o_haystack);
          }

          break;
        }
        case VCTRS_MULTIPLE_last: {
          if (first_touch) {
            R_ARR_POKE(int, p_loc_first_match_o_haystack, loc_needles, loc_upper_match_o_haystack);
            break;
          }

          const int loc_first_match_haystack = v_o_haystack[loc_first_match_o_haystack] - 1;
          const int loc_upper_match_haystack = v_o_haystack[loc_upper_match_o_haystack] - 1;

          if (any_filters) {
            int cmp = p_matches_df_compare_na_equal(
              p_haystack,
              loc_upper_match_haystack,
              p_haystack,
              loc_first_match_haystack,
              v_filters
            );

            if (cmp == -1) {
              // New haystack value loses vs previous one, do nothing
              break;
            } else if (cmp == 1) {
              // New haystack value wins vs previous one, automatically update like first_touch
              R_ARR_POKE(int, p_loc_first_match_o_haystack, loc_needles, loc_upper_match_o_haystack);
              break;
            }
          }

          if (loc_upper_match_haystack > loc_first_match_haystack) {
            // New start is after current one
            R_ARR_POKE(int, p_loc_first_match_o_haystack, loc_needles, loc_upper_match_o_haystack);
          }

          break;
        }
        case VCTRS_MULTIPLE_all:
        case VCTRS_MULTIPLE_error:
        case VCTRS_MULTIPLE_warning: {
          const int size_match = loc_upper_match_o_haystack - loc_lower_match_o_haystack + 1;

          if (first_touch) {
            R_ARR_POKE(int, p_loc_first_match_o_haystack, loc_needles, loc_lower_match_o_haystack);
            R_ARR_POKE(int, p_size_match, loc_needles, size_match);

            if (any_filters) {
              const int loc_upper_match_haystack = v_o_haystack[loc_upper_match_o_haystack] - 1;
              v_locs_filter_match_haystack[loc_needles] = loc_upper_match_haystack;
            }

            break;
          }

          if (any_filters) {
            const int loc_filter_match_haystack = v_locs_filter_match_haystack[loc_needles];
            const int loc_upper_match_haystack = v_o_haystack[loc_upper_match_o_haystack] - 1;

            int cmp = p_matches_df_compare_na_equal(
              p_haystack,
              loc_upper_match_haystack,
              p_haystack,
              loc_filter_match_haystack,
              v_filters
            );

            if (cmp == 1) {
              // Update filter match location for later use
              v_locs_filter_match_haystack[loc_needles] = loc_upper_match_haystack;
            } else if (cmp == -1) {
              // Not a valid match, as another previous match wins over this one
              break;
            }
          }

          r_arr_push_back(p_loc_first_match_o_haystack, &loc_lower_match_o_haystack);
          r_arr_push_back(p_size_match, &size_match);
          r_arr_push_back(p_loc_needles, &loc_needles);
          ++(*p_n_extra);
          break;
        }
        }
      }
    }
  } else if (incomplete->action != VCTRS_INCOMPLETE_ACTION_match && col < n_col - 1) {
    // Miss! This `needles` column group has no matches in the corresponding
    // `haystack` column. However, we still need to propagate any potential
    // NAs that might occur in future columns of this `needles` group. If
    // `val_needles` was an NA, it would have been caught above, so we only
    // need to look at future columns.
    for (r_ssize i = loc_lower_duplicate_o_needles; i <= loc_upper_duplicate_o_needles; ++i) {
      const r_ssize loc_needles = v_o_needles[i] - 1;

      for (r_ssize j = col + 1; j < n_col; ++j) {
        const int* v_future_needles_complete = (const int*) p_needles_complete->col_ptrs[j];
        const bool future_needle_is_complete = v_future_needles_complete[loc_needles];

        if (!future_needle_is_complete) {
          R_ARR_POKE(int, p_loc_first_match_o_haystack, loc_needles, SIGNAL_INCOMPLETE);
          break;
        }
      }
    }
  }

  bool do_lhs;
  bool do_rhs;

  // Default to current bounds
  r_ssize lhs_loc_lower_bound_o_needles = loc_lower_bound_o_needles;
  r_ssize lhs_loc_upper_bound_o_needles = loc_upper_bound_o_needles;
  r_ssize lhs_loc_lower_bound_o_haystack = loc_lower_bound_o_haystack;
  r_ssize lhs_loc_upper_bound_o_haystack = loc_upper_bound_o_haystack;

  r_ssize rhs_loc_lower_bound_o_needles = loc_lower_bound_o_needles;
  r_ssize rhs_loc_upper_bound_o_needles = loc_upper_bound_o_needles;
  r_ssize rhs_loc_lower_bound_o_haystack = loc_lower_bound_o_haystack;
  r_ssize rhs_loc_upper_bound_o_haystack = loc_upper_bound_o_haystack;

  switch (op) {
  case VCTRS_OPS_eq: {
    do_lhs =
      (loc_lower_duplicate_o_needles > loc_lower_bound_o_needles) &&
      (loc_lower_match_o_haystack > loc_lower_bound_o_haystack);
    do_rhs =
      (loc_upper_duplicate_o_needles < loc_upper_bound_o_needles) &&
      (loc_upper_match_o_haystack < loc_upper_bound_o_haystack);

    // Limit bounds of both needle and haystack using existing info
    if (do_lhs) {
      lhs_loc_upper_bound_o_needles = loc_lower_duplicate_o_needles - 1;
      lhs_loc_upper_bound_o_haystack = loc_lower_match_o_haystack - 1;
    }
    if (do_rhs) {
      rhs_loc_lower_bound_o_needles = loc_upper_duplicate_o_needles + 1;
      rhs_loc_lower_bound_o_haystack = loc_upper_match_o_haystack + 1;
    }

    break;
  }
  case VCTRS_OPS_lt:
  case VCTRS_OPS_lte:
  case VCTRS_OPS_gt:
  case VCTRS_OPS_gte: {
    // Can't update haystack here, as nested containment groups make this impossible
    do_lhs = loc_lower_duplicate_o_needles > loc_lower_bound_o_needles;
    do_rhs = loc_upper_duplicate_o_needles < loc_upper_bound_o_needles;

    if (do_lhs) {
      lhs_loc_upper_bound_o_needles = loc_lower_duplicate_o_needles - 1;
    }
    if (do_rhs) {
      rhs_loc_lower_bound_o_needles = loc_upper_duplicate_o_needles + 1;
    }

    break;
  }
  }

  if (do_lhs) {
    df_matches_recurse(
      col,
      lhs_loc_lower_bound_o_needles,
      lhs_loc_upper_bound_o_needles,
      lhs_loc_lower_bound_o_haystack,
      lhs_loc_upper_bound_o_haystack,
      p_needles,
      p_haystack,
      p_needles_complete,
      p_haystack_complete,
      v_o_needles,
      v_o_haystack,
      incomplete,
      multiple,
      any_filters,
      v_filters,
      v_ops,
      p_loc_first_match_o_haystack,
      p_size_match,
      p_loc_needles,
      v_locs_filter_match_haystack,
      p_n_extra
    );
  }
  if (do_rhs) {
    df_matches_recurse(
      col,
      rhs_loc_lower_bound_o_needles,
      rhs_loc_upper_bound_o_needles,
      rhs_loc_lower_bound_o_haystack,
      rhs_loc_upper_bound_o_haystack,
      p_needles,
      p_haystack,
      p_needles_complete,
      p_haystack_complete,
      v_o_needles,
      v_o_haystack,
      incomplete,
      multiple,
      any_filters,
      v_filters,
      v_ops,
      p_loc_first_match_o_haystack,
      p_size_match,
      p_loc_needles,
      v_locs_filter_match_haystack,
      p_n_extra
    );
  }
}

// -----------------------------------------------------------------------------

static
void df_matches_with_nested_groups(r_ssize size_haystack,
                                   int n_nested_groups,
                                   const int* v_nested_groups,
                                   r_ssize col,
                                   r_ssize loc_lower_bound_o_needles,
                                   r_ssize loc_upper_bound_o_needles,
                                   const struct poly_df_data* p_needles,
                                   const struct poly_df_data* p_haystack,
                                   const struct poly_df_data* p_needles_complete,
                                   const struct poly_df_data* p_haystack_complete,
                                   const int* v_o_needles,
                                   const int* v_o_haystack,
                                   const struct vctrs_incomplete* incomplete,
                                   enum vctrs_multiple multiple,
                                   bool any_filters,
                                   const enum vctrs_filter* v_filters,
                                   const enum vctrs_ops* v_ops,
                                   struct r_dyn_array* p_loc_first_match_o_haystack,
                                   struct r_dyn_array* p_size_match,
                                   struct r_dyn_array* p_loc_needles,
                                   int* v_locs_filter_match_haystack,
                                   r_ssize* p_n_extra) {
  const int* v_haystack = v_nested_groups;

  r_ssize loc_lower_match_o_haystack = 0;
  r_ssize loc_upper_match_o_haystack = size_haystack - 1;

  for (int i = 0; i < n_nested_groups; ++i) {
    const int val_needle = i;

    while (loc_lower_match_o_haystack <= loc_upper_match_o_haystack) {
      const r_ssize loc_mid_match_o_haystack = midpoint(loc_lower_match_o_haystack, loc_upper_match_o_haystack);
      const r_ssize loc_mid_match_haystack = v_o_haystack[loc_mid_match_o_haystack] - 1;
      const int val_haystack = v_haystack[loc_mid_match_haystack];

      const int cmp = int_compare_na_equal(val_needle, val_haystack);

      if (cmp == 1) {
        loc_lower_match_o_haystack = loc_mid_match_o_haystack + 1;
      } else if (cmp == -1) {
        loc_upper_match_o_haystack = loc_mid_match_o_haystack - 1;
      } else {
        // Hit!
        // Find lower and upper group bounds
        loc_lower_match_o_haystack = int_lower_duplicate(
          val_haystack,
          v_haystack,
          v_o_haystack,
          loc_lower_match_o_haystack,
          loc_mid_match_o_haystack
        );
        loc_upper_match_o_haystack = int_upper_duplicate(
          val_haystack,
          v_haystack,
          v_o_haystack,
          loc_mid_match_o_haystack,
          loc_upper_match_o_haystack
        );
        break;
      }
     }

    df_matches_recurse(
      col,
      loc_lower_bound_o_needles,
      loc_upper_bound_o_needles,
      loc_lower_match_o_haystack,
      loc_upper_match_o_haystack,
      p_needles,
      p_haystack,
      p_needles_complete,
      p_haystack_complete,
      v_o_needles,
      v_o_haystack,
      incomplete,
      multiple,
      any_filters,
      v_filters,
      v_ops,
      p_loc_first_match_o_haystack,
      p_size_match,
      p_loc_needles,
      v_locs_filter_match_haystack,
      p_n_extra
    );

    // Update bounds for next group
    loc_lower_match_o_haystack = loc_upper_match_o_haystack + 1;
    loc_upper_match_o_haystack = size_haystack - 1;
  }
}

// -----------------------------------------------------------------------------

// Find the largest contiguous location containing an incomplete value
static inline
r_ssize int_locate_upper_incomplete(const int* v_haystack_complete,
                                    const int* v_o_haystack,
                                    r_ssize loc_lower_bound_o_haystack,
                                    r_ssize loc_upper_bound_o_haystack) {
  while (loc_lower_bound_o_haystack <= loc_upper_bound_o_haystack) {
    const r_ssize loc_mid_bound_o_haystack = midpoint(loc_lower_bound_o_haystack, loc_upper_bound_o_haystack);
    const r_ssize loc_mid_bound_haystack = v_o_haystack[loc_mid_bound_o_haystack] - 1;
    const int haystack_is_complete = v_haystack_complete[loc_mid_bound_haystack];

    if (haystack_is_complete) {
      loc_upper_bound_o_haystack = loc_mid_bound_o_haystack - 1;
    } else {
      loc_lower_bound_o_haystack = loc_mid_bound_o_haystack + 1;
    }
  }

  return loc_upper_bound_o_haystack;
}

// -----------------------------------------------------------------------------

// Find the smallest contiguous location containing `needle`
static inline
r_ssize int_lower_duplicate(int needle,
                            const int* v_haystack,
                            const int* v_o_haystack,
                            r_ssize loc_lower_bound_o_haystack,
                            r_ssize loc_upper_bound_o_haystack) {
  while (loc_lower_bound_o_haystack <= loc_upper_bound_o_haystack) {
    const r_ssize loc_mid_bound_o_haystack = midpoint(loc_lower_bound_o_haystack, loc_upper_bound_o_haystack);
    const r_ssize loc_mid_bound_haystack = v_o_haystack[loc_mid_bound_o_haystack] - 1;
    const int val_haystack = v_haystack[loc_mid_bound_haystack];

    if (int_equal_na_equal(needle, val_haystack)) {
      loc_upper_bound_o_haystack = loc_mid_bound_o_haystack - 1;
    } else {
      loc_lower_bound_o_haystack = loc_mid_bound_o_haystack + 1;
    }
  }

  return loc_lower_bound_o_haystack;
}

// -----------------------------------------------------------------------------

// Find the largest contiguous location containing `needle`
static inline
r_ssize int_upper_duplicate(int needle,
                            const int* v_haystack,
                            const int* v_o_haystack,
                            r_ssize loc_lower_bound_o_haystack,
                            r_ssize loc_upper_bound_o_haystack) {
  while (loc_lower_bound_o_haystack <= loc_upper_bound_o_haystack) {
    const r_ssize loc_mid_bound_o_haystack = midpoint(loc_lower_bound_o_haystack, loc_upper_bound_o_haystack);
    const r_ssize loc_mid_bound_haystack = v_o_haystack[loc_mid_bound_o_haystack] - 1;
    const int elt_haystack = v_haystack[loc_mid_bound_haystack];

    if (int_equal_na_equal(needle, elt_haystack)) {
      loc_lower_bound_o_haystack = loc_mid_bound_o_haystack + 1;
    } else {
      loc_upper_bound_o_haystack = loc_mid_bound_o_haystack - 1;
    }
  }

  return loc_upper_bound_o_haystack;
}

// -----------------------------------------------------------------------------

static
r_obj* df_joint_xtfrm_by_col(r_obj* x,
                             r_obj* y,
                             r_ssize x_size,
                             r_ssize y_size,
                             r_ssize n_cols,
                             bool nan_distinct,
                             r_obj* chr_transform) {
  r_obj* out = KEEP(r_alloc_list(2));

  x = r_clone(x);
  r_list_poke(out, 0, x);

  y = r_clone(y);
  r_list_poke(out, 1, y);

  r_obj* const* v_x = r_list_cbegin(x);
  r_obj* const* v_y = r_list_cbegin(y);

  for (r_ssize col = 0; col < n_cols; ++col) {
    r_obj* x_col = v_x[col];
    r_obj* y_col = v_y[col];
    r_obj* xtfrms = vec_joint_xtfrm(x_col, y_col, x_size, y_size, nan_distinct, chr_transform);
    r_list_poke(x, col, r_list_get(xtfrms, 0));
    r_list_poke(y, col, r_list_get(xtfrms, 1));
  }

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------

static
r_obj* df_detect_complete_by_col(r_obj* x, r_ssize x_size, r_ssize n_cols) {
  r_obj* out = KEEP(r_alloc_list(n_cols));
  r_poke_names(out, r_names(x));
  r_init_data_frame(out, x_size);

  r_obj* const* v_x = r_list_cbegin(x);

  for (r_ssize i = 0; i < n_cols; ++i) {
    r_obj* col = v_x[i];
    // Use completeness to match `vec_rank()` and `vec_match()`
    r_obj* complete = vec_detect_complete(col);
    r_list_poke(out, i, complete);
  }

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------

static inline
enum vctrs_ops parse_condition_one(const char* condition) {
  if (!strcmp(condition, "==")) { return VCTRS_OPS_eq; }
  if (!strcmp(condition, ">"))  { return VCTRS_OPS_gt; }
  if (!strcmp(condition, ">=")) { return VCTRS_OPS_gte; }
  if (!strcmp(condition, "<"))  { return VCTRS_OPS_lt; }
  if (!strcmp(condition, "<=")) { return VCTRS_OPS_lte; }

  r_abort("`condition` must only contain \"==\", \">\", \">=\", \"<\", or \"<=\".");
}

static inline
void parse_condition(r_obj* condition, r_ssize n_cols, enum vctrs_ops* v_ops) {
  if (r_typeof(condition) != R_TYPE_character) {
    r_abort("`condition` must be a character vector, or `NULL`.");
  }

  r_obj* const* v_condition = r_chr_cbegin(condition);
  r_ssize size_condition = vec_size(condition);

  if (size_condition == 1) {
    const char* elt = r_str_c_string(v_condition[0]);
    enum vctrs_ops op = parse_condition_one(elt);

    for (r_ssize i = 0; i < n_cols; ++i) {
      v_ops[i] = op;
    }

    return;
  }

  if (size_condition == n_cols) {
    for (r_ssize i = 0; i < n_cols; ++i) {
      const char* elt = r_str_c_string(v_condition[i]);
      v_ops[i] = parse_condition_one(elt);
    }

    return;
  }

  r_abort(
    "If `condition` is a character vector, it must be length 1, or the same "
    "length as the number of columns of the input."
  );
}

// -----------------------------------------------------------------------------

static inline
struct vctrs_incomplete parse_incomplete(r_obj* incomplete) {
  if (r_length(incomplete) != 1) {
    r_abort("`incomplete` must be length 1, not length %i.", r_length(incomplete));
  }

  if (r_is_string(incomplete)) {
    const char* c_incomplete = r_chr_get_c_string(incomplete, 0);

    if (!strcmp(c_incomplete, "match")) {
      return (struct vctrs_incomplete) {
        .action = VCTRS_INCOMPLETE_ACTION_match,
        .value = -1
      };
    }

    if (!strcmp(c_incomplete, "drop")) {
      return (struct vctrs_incomplete) {
        .action = VCTRS_INCOMPLETE_ACTION_drop,
        .value = -1
      };
    }

    if (!strcmp(c_incomplete, "error")) {
      return (struct vctrs_incomplete) {
        .action = VCTRS_INCOMPLETE_ACTION_error,
        .value = -1
      };
    }

    r_abort("`incomplete` must be one of: \"match\", \"drop\", or \"error\".");
  }

  incomplete = vec_cast(incomplete, vctrs_shared_empty_int, args_incomplete, args_empty);
  int c_incomplete = r_int_get(incomplete, 0);

  return (struct vctrs_incomplete) {
    .action = VCTRS_INCOMPLETE_ACTION_value,
    .value = c_incomplete
  };
}

// -----------------------------------------------------------------------------

static inline
enum vctrs_multiple parse_multiple(r_obj* multiple) {
  if (!r_is_string(multiple)) {
    r_abort("`multiple` must be a string.");
  }

  const char* c_multiple = r_chr_get_c_string(multiple, 0);

  if (!strcmp(c_multiple, "all")) return VCTRS_MULTIPLE_all;
  if (!strcmp(c_multiple, "warning")) return VCTRS_MULTIPLE_warning;
  if (!strcmp(c_multiple, "first")) return VCTRS_MULTIPLE_first;
  if (!strcmp(c_multiple, "last")) return VCTRS_MULTIPLE_last;
  if (!strcmp(c_multiple, "error")) return VCTRS_MULTIPLE_error;

  r_abort("`multiple` must be one of \"all\", \"first\", \"last\", \"warning\", or \"error\".");
}

// -----------------------------------------------------------------------------

static inline
enum vctrs_filter parse_filter_one(const char* filter) {
  if (!strcmp(filter, "none")) return VCTRS_FILTER_none;
  if (!strcmp(filter, "min")) return VCTRS_FILTER_min;
  if (!strcmp(filter, "max")) return VCTRS_FILTER_max;

  r_abort("`filter` must be one of \"none\", \"min\", or \"max\".");
}

static inline
void parse_filter(r_obj* filter, r_ssize n_cols, enum vctrs_filter* v_filters) {
  if (r_typeof(filter) != R_TYPE_character) {
    r_abort("`filter` must be a character vector.");
  }

  r_obj* const* v_filter = r_chr_cbegin(filter);
  r_ssize size_filter = vec_size(filter);

  if (size_filter == 1) {
    const char* elt = r_str_c_string(v_filter[0]);
    enum vctrs_filter elt_filter = parse_filter_one(elt);

    for (r_ssize i = 0; i < n_cols; ++i) {
      v_filters[i] = elt_filter;
    }

    return;
  }

  if (size_filter == n_cols) {
    for (r_ssize i = 0; i < n_cols; ++i) {
      const char* elt = r_str_c_string(v_filter[i]);
      v_filters[i] = parse_filter_one(elt);
    }

    return;
  }

  r_abort(
    "`filter` must be length 1, or the same "
    "length as the number of columns of the input."
  );
}

// -----------------------------------------------------------------------------

static inline
struct vctrs_no_match parse_no_match(r_obj* no_match) {
  if (r_length(no_match) != 1) {
    r_abort("`no_match` must be length 1, not length %i.", r_length(no_match));
  }

  if (r_is_string(no_match)) {
    const char* c_no_match = r_chr_get_c_string(no_match, 0);

    if (!strcmp(c_no_match, "error")) {
      return (struct vctrs_no_match) {
        .action = VCTRS_NO_MATCH_ACTION_error,
        .value = -1
      };
    }

    if (!strcmp(c_no_match, "drop")) {
      return (struct vctrs_no_match) {
        .action = VCTRS_NO_MATCH_ACTION_drop,
        .value = -1
      };
    }

    r_abort("`no_match` must be either \"drop\" or \"error\".");
  }

  no_match = vec_cast(no_match, vctrs_shared_empty_int, args_no_match, args_empty);
  int c_no_match = r_int_get(no_match, 0);

  return (struct vctrs_no_match) {
    .action = VCTRS_NO_MATCH_ACTION_value,
    .value = c_no_match
  };
}

// -----------------------------------------------------------------------------

static inline
struct vctrs_remaining parse_remaining(r_obj* remaining) {
  if (r_length(remaining) != 1) {
    r_abort("`remaining` must be length 1, not length %i.", r_length(remaining));
  }

  if (r_is_string(remaining)) {
    const char* c_remaining = r_chr_get_c_string(remaining, 0);

    if (!strcmp(c_remaining, "error")) {
      return (struct vctrs_remaining) {
        .action = VCTRS_REMAINING_ACTION_error,
        .value = -1
      };
    }

    if (!strcmp(c_remaining, "drop")) {
      return (struct vctrs_remaining) {
        .action = VCTRS_REMAINING_ACTION_drop,
        .value = -1
      };
    }

    r_abort("`remaining` must be either \"drop\" or \"error\".");
  }

  remaining = vec_cast(remaining, vctrs_shared_empty_int, args_remaining, args_empty);
  int c_remaining = r_int_get(remaining, 0);

  return (struct vctrs_remaining) {
    .action = VCTRS_REMAINING_ACTION_value,
    .value = c_remaining
  };
}

// -----------------------------------------------------------------------------

static
const char* v_matches_df_names_c_strings[] = {
  "needles",
  "haystack"
};
static
const enum r_type v_matches_df_types[] = {
  R_TYPE_integer,
  R_TYPE_integer
};
enum matches_df_locs {
  MATCHES_DF_LOCS_needles,
  MATCHES_DF_LOCS_haystack
};
#define MATCHES_DF_SIZE R_ARR_SIZEOF(v_matches_df_types)

static inline
r_obj* new_vec_matches_result(r_ssize size) {
  r_obj* names = KEEP(r_chr_n(v_matches_df_names_c_strings, MATCHES_DF_SIZE));

  r_obj* out = KEEP(r_alloc_df_list(
    size,
    names,
    v_matches_df_types,
    MATCHES_DF_SIZE
  ));
  r_init_data_frame(out, size);

  FREE(2);
  return out;
}

static inline
r_obj* new_vec_matches_result_from_columns(r_obj* needles, r_obj* haystack) {
  r_obj* out = KEEP(r_alloc_list(MATCHES_DF_SIZE));

  r_list_poke(out, MATCHES_DF_LOCS_needles, needles);
  r_list_poke(out, MATCHES_DF_LOCS_haystack, haystack);

  r_poke_names(out, r_chr_n(v_matches_df_names_c_strings, MATCHES_DF_SIZE));

  r_init_data_frame(out, r_length(needles));

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------

static
r_obj* expand_match_on_nothing(r_ssize size_needles,
                               r_ssize size_haystack,
                               enum vctrs_multiple multiple,
                               const struct vctrs_no_match* no_match,
                               const struct vctrs_remaining* remaining,
                               struct vctrs_arg* needles_arg,
                               struct vctrs_arg* haystack_arg) {
  if (size_haystack == 0) {
    // Handle empty `haystack` up front
    // `no_match` everywhere, retaining size of `needles`

    if (no_match->action == VCTRS_NO_MATCH_ACTION_error && size_needles > 0) {
      stop_matches_nothing(0, needles_arg, haystack_arg);
    }

    // If `no_match = "drop"`, since everything is a no-match there are
    // no results
    const r_ssize size_out = (no_match->action == VCTRS_NO_MATCH_ACTION_drop) ? 0 : size_needles;

    r_obj* out = KEEP(new_vec_matches_result(size_out));
    int* v_out_needles = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_needles));
    int* v_out_haystack = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_haystack));
    r_ssize loc_out = 0;

    const int loc_haystack = no_match->value;

    for (r_ssize i = 0; i < size_out; ++i) {
      v_out_needles[loc_out] = i + 1;
      v_out_haystack[loc_out] = loc_haystack;
      ++loc_out;
    }

    FREE(1);
    return out;
  }

  if (size_needles == 0 && remaining->action != VCTRS_REMAINING_ACTION_drop) {
    // Handle empty `needles` up front
    // All elements of `haystack` are "remaining"

    if (remaining->action == VCTRS_REMAINING_ACTION_error) {
      stop_matches_remaining(0, needles_arg, haystack_arg);
    }
    if (remaining->action != VCTRS_REMAINING_ACTION_value) {
      r_stop_internal("expand_match_on_nothing", "`remaining->action` must be `value`.");
    }

    const r_ssize size_out = size_haystack;

    r_obj* out = KEEP(new_vec_matches_result(size_out));
    int* v_out_needles = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_needles));
    int* v_out_haystack = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_haystack));
    r_ssize loc_out = 0;

    const int loc_needles = remaining->value;

    for (r_ssize i = 0; i < size_out; ++i) {
      v_out_needles[loc_out] = loc_needles;
      v_out_haystack[loc_out] = i + 1;
      ++loc_out;
    }

    FREE(1);
    return out;
  }

  if (multiple == VCTRS_MULTIPLE_first || multiple == VCTRS_MULTIPLE_last) {
    // Handle first/last cases next
    r_obj* out = KEEP(new_vec_matches_result(size_needles));
    int* v_out_needles = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_needles));
    int* v_out_haystack = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_haystack));
    r_ssize loc_out = 0;

    const int loc_haystack = (multiple == VCTRS_MULTIPLE_first) ? 1 : size_haystack;

    for (r_ssize i = 0; i < size_needles; ++i) {
      v_out_needles[loc_out] = i + 1;
      v_out_haystack[loc_out] = loc_haystack;
      ++loc_out;
    }

    FREE(1);
    return out;
  }

  if (size_haystack > 1 && size_needles > 0) {
    if (multiple == VCTRS_MULTIPLE_error) {
      stop_matches_multiple(0, needles_arg, haystack_arg);
    } else if (multiple == VCTRS_MULTIPLE_warning) {
      warn_matches_multiple(0, needles_arg, haystack_arg);
    }
  }

  r_ssize size_out = r_ssize_mult(size_needles, size_haystack);

  if (size_out > R_LEN_T_MAX) {
    stop_matches_overflow((double) size_out);
  }

  r_obj* out = KEEP(new_vec_matches_result(size_out));
  int* v_out_needles = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_needles));
  int* v_out_haystack = r_int_begin(r_list_get(out, MATCHES_DF_LOCS_haystack));
  r_ssize loc_out = 0;

  for (r_ssize i = 0; i < size_needles; ++i) {
    for (r_ssize j = 0; j < size_haystack; ++j) {
      v_out_needles[loc_out] = i + 1;
      v_out_haystack[loc_out] = j + 1;
      ++loc_out;
    }
  }

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------

static
r_obj* expand_compact_indices(const int* v_o_haystack,
                              struct r_dyn_array* p_loc_first_match_o_haystack,
                              struct r_dyn_array* p_size_match,
                              struct r_dyn_array* p_loc_needles,
                              bool skip_size_match,
                              bool skip_loc_needles,
                              const struct vctrs_incomplete* incomplete,
                              const struct vctrs_no_match* no_match,
                              const struct vctrs_remaining* remaining,
                              enum vctrs_multiple multiple,
                              r_ssize size_needles,
                              r_ssize size_haystack,
                              bool any_non_equi,
                              bool has_locs_filter_match_haystack,
                              const enum vctrs_filter* v_filters,
                              const int* v_locs_filter_match_haystack,
                              const struct poly_df_data* p_haystack,
                              struct vctrs_arg* needles_arg,
                              struct vctrs_arg* haystack_arg) {
  int n_prot = 0;

  const r_ssize n_used = p_loc_first_match_o_haystack->count;

  const int* v_loc_first_match_o_haystack = (const int*) r_arr_cbegin(p_loc_first_match_o_haystack);
  const int* v_size_match = skip_size_match ? NULL : (const int*) r_arr_cbegin(p_size_match);
  const int* v_loc_needles = skip_loc_needles ? NULL : (const int*) r_arr_cbegin(p_loc_needles);

  r_ssize size_out = 0;
  if (skip_size_match) {
    size_out = n_used;
  } else {
    double dbl_size_out = 0;

    for (r_ssize i = 0; i < n_used; ++i) {
      // This could get extremely large with improperly specified non-equi joins.
      // May over-allocate in the case of `filters` with `multiple = "all"`,
      // or when `no_match = "drop"` or `incomplete = "drop"`.
      dbl_size_out += (double) v_size_match[i];
    }

    if (dbl_size_out > R_LEN_T_MAX) {
      // TODO: Update this after a switch to long vector support
      r_abort(
        "Match procedure results in an allocation larger than 2^31-1 elements. "
        "Attempted allocation size was %.0lf. "
        "Please report this to the vctrs maintainers at "
        "<https://github.com/r-lib/vctrs/issues>.",
        dbl_size_out
      );
    }

    size_out = (r_ssize) dbl_size_out;
  }

  r_keep_t out_needles_pi;
  r_obj* out_needles = r_alloc_integer(size_out);
  KEEP_HERE(out_needles, &out_needles_pi);
  ++n_prot;

  r_keep_t out_haystack_pi;
  r_obj* out_haystack = r_alloc_integer(size_out);
  KEEP_HERE(out_haystack, &out_haystack_pi);
  ++n_prot;

  int* v_out_needles = r_int_begin(out_needles);
  int* v_out_haystack = r_int_begin(out_haystack);

  const int* v_o_loc_needles = NULL;
  if (!skip_loc_needles) {
    r_obj* loc_needles = KEEP_N(r_arr_unwrap(p_loc_needles), &n_prot);
    r_obj* o_loc_needles = KEEP_N(vec_order(loc_needles, chrs_asc, chrs_smallest, true, r_null), &n_prot);
    v_o_loc_needles = r_int_cbegin(o_loc_needles);
  }

  const bool retain_remaining_haystack = (remaining->action != VCTRS_REMAINING_ACTION_drop);
  int* v_detect_remaining_haystack = NULL;
  if (retain_remaining_haystack) {
    r_obj* detect_remaining_haystack = KEEP_N(r_alloc_integer(size_haystack), &n_prot);
    v_detect_remaining_haystack = r_int_begin(detect_remaining_haystack);

    for (r_ssize i = 0; i < size_haystack; ++i) {
      // Initialize to remaining (i.e. unmatched)
      v_detect_remaining_haystack[i] = 1;
    }
  }

  r_ssize loc_out = 0;

  bool any_multiple = false;
  bool maybe_multiple =
    multiple == VCTRS_MULTIPLE_all ||
    multiple == VCTRS_MULTIPLE_error ||
    multiple == VCTRS_MULTIPLE_warning;

  const bool match_incomplete = incomplete->action == VCTRS_INCOMPLETE_ACTION_match;

  for (r_ssize i = 0; i < n_used; ++i) {
    const int loc = skip_loc_needles ? i : v_o_loc_needles[i] - 1;

    int loc_first_match_o_haystack = v_loc_first_match_o_haystack[loc];
    const int size_match = skip_size_match ? 1 : v_size_match[loc];
    const int loc_needles = skip_loc_needles ? loc : v_loc_needles[loc];

    if (!match_incomplete && loc_first_match_o_haystack == SIGNAL_INCOMPLETE) {
      if (size_match != 1) {
        r_stop_internal(
          "expand_compact_indices",
          "`size_match` should always be 1 in the case of incomplete values."
        );
      }

      switch (incomplete->action) {
      case VCTRS_INCOMPLETE_ACTION_value: {
        v_out_needles[loc_out] = loc_needles + 1;
        v_out_haystack[loc_out] = incomplete->value;
        ++loc_out;
        break;
      }
      case VCTRS_INCOMPLETE_ACTION_drop: {
        // Do not increment `loc_out`, do not store locations
        break;
      }
      case VCTRS_INCOMPLETE_ACTION_error: {
        stop_matches_incomplete(loc_needles, needles_arg);
      }
      case VCTRS_INCOMPLETE_ACTION_match: {
        r_stop_internal(
          "expand_compact_indices",
          "Needles should never be marked as `SIGNAL_INCOMPLETE` when `incomplete = 'match'`."
        );
      }
      }

      continue;
    }

    if (loc_first_match_o_haystack == SIGNAL_NO_MATCH) {
      if (size_match != 1) {
        r_stop_internal(
          "expand_compact_indices",
          "`size_match` should always be 1 in the case of no matches."
        );
      }

      switch (no_match->action) {
      case VCTRS_NO_MATCH_ACTION_value: {
        v_out_needles[loc_out] = loc_needles + 1;
        v_out_haystack[loc_out] = no_match->value;
        ++loc_out;
        break;
      }
      case VCTRS_NO_MATCH_ACTION_drop: {
        break;
      }
      case VCTRS_NO_MATCH_ACTION_error: {
        stop_matches_nothing(i, needles_arg, haystack_arg);
      }
      default: {
        r_stop_internal("expand_compact_indices", "Unknown `no_match->action`.");
      }
      }

      continue;
    }

    if (has_locs_filter_match_haystack) {
      const int loc_first_match_haystack = v_o_haystack[loc_first_match_o_haystack] - 1;
      const int loc_filter_match_haystack = v_locs_filter_match_haystack[loc_needles];

      const bool equal = p_matches_df_equal_na_equal(
        p_haystack,
        loc_first_match_haystack,
        p_haystack,
        loc_filter_match_haystack,
        v_filters
      );

      if (!equal) {
        // Potential haystack match doesn't aligned with known filtered match.
        // This can happen if the potential match came from a different nested group.
        continue;
      }
    }

    if (maybe_multiple && !any_multiple) {
      if (i < size_needles) {
        any_multiple = size_match > 1;
      } else {
        // Guaranteed second match if in the "extra" matches section
        any_multiple = true;
      }

      if (any_multiple) {
        if (multiple == VCTRS_MULTIPLE_error) {
          stop_matches_multiple(loc_needles, needles_arg, haystack_arg);
        } else if (multiple == VCTRS_MULTIPLE_warning) {
          warn_matches_multiple(loc_needles, needles_arg, haystack_arg);
        }
      }
    }

    int loc_o_haystack = loc_first_match_o_haystack;

    for (r_ssize j = 0; j < size_match; ++j) {
      const int loc_haystack = v_o_haystack[loc_o_haystack] - 1;

      v_out_needles[loc_out] = loc_needles + 1;
      v_out_haystack[loc_out] = loc_haystack + 1;

      if (retain_remaining_haystack) {
        v_detect_remaining_haystack[loc_haystack] = 0;
      }

      ++loc_out;
      ++loc_o_haystack;
    }
  }

  if (loc_out < size_out) {
    // Can happen with a `filter` and `multiple = "all"`, where it is possible
    // for potential matches coming from a different nested containment group
    // to be filtered out in the above loop.
    // Can also happen with `no_match = "drop"` or `incomplete = "drop"`.
    // Resize should be free by setting truelength and growable bit.
    size_out = loc_out;

    out_needles = r_int_resize(out_needles, size_out);
    KEEP_AT(out_needles, out_needles_pi);
    v_out_needles = r_int_begin(out_needles);

    out_haystack = r_int_resize(out_haystack, size_out);
    KEEP_AT(out_haystack, out_haystack_pi);
    v_out_haystack = r_int_begin(out_haystack);
  }

  if (any_multiple && any_non_equi) {
    // If we had multiple matches and we were doing a non-equi join, then
    // the needles column will be correct, but any group of multiple matches in
    // the haystack column will be ordered incorrectly within the needle group.
    // They will be ordered using the order of the original haystack values,
    // rather than by first appearance. Reordering the entire output data frame
    // orders them correctly, as within each needle group it will put the
    // haystack locations in ascending order (i.e. by first appearance).
    // This is expensive! `out` could have a huge number of matches.
    r_obj* both = KEEP(new_vec_matches_result_from_columns(out_needles, out_haystack));

    r_obj* o_haystack_appearance = KEEP(vec_order(both, chrs_asc, chrs_smallest, true, r_null));
    const int* v_o_haystack_appearance = r_int_cbegin(o_haystack_appearance);

    r_obj* out_haystack2 = KEEP(r_alloc_integer(size_out));
    int* v_out_haystack2 = r_int_begin(out_haystack2);

    for (r_ssize i = 0; i < size_out; ++i) {
      v_out_haystack2[i] = v_out_haystack[v_o_haystack_appearance[i] - 1];
    }

    out_haystack = out_haystack2;
    v_out_haystack = v_out_haystack2;

    FREE(3);
    KEEP_AT(out_haystack, out_haystack_pi);
  }

  if (retain_remaining_haystack) {
    r_ssize n_remaining_haystack = 0;

    for (r_ssize i = 0; i < size_haystack; ++i) {
      if (!v_detect_remaining_haystack[i]) {
        continue;
      }

      if (remaining->action == VCTRS_REMAINING_ACTION_error) {
        stop_matches_remaining(i, needles_arg, haystack_arg);
      }

      // Overwrite with location, this moves all remaining locs up to the front
      v_detect_remaining_haystack[n_remaining_haystack] = i + 1;
      ++n_remaining_haystack;
    }

    if (n_remaining_haystack > 0) {
      // Resize to have enough room for haystack remainings at the end
      r_ssize new_size_out = r_ssize_add(size_out, n_remaining_haystack);

      out_needles = r_int_resize(out_needles, new_size_out);
      KEEP_AT(out_needles, out_needles_pi);
      v_out_needles = r_int_begin(out_needles);

      out_haystack = r_int_resize(out_haystack, new_size_out);
      KEEP_AT(out_haystack, out_haystack_pi);
      v_out_haystack = r_int_begin(out_haystack);

      for (r_ssize i = size_out, j = 0; i < new_size_out; ++i, ++j) {
        v_out_needles[i] = remaining->value;
        v_out_haystack[i] = v_detect_remaining_haystack[j];
      }

      size_out = new_size_out;
    }
  }

  r_obj* out = new_vec_matches_result_from_columns(out_needles, out_haystack);

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

// [[ register() ]]
r_obj* vctrs_test_compute_nested_containment_info(r_obj* haystack,
                                                  r_obj* condition,
                                                  r_obj* multiple) {
  r_ssize n_cols = r_length(haystack);
  enum vctrs_ops* v_ops = (enum vctrs_ops*) R_alloc(n_cols, sizeof(enum vctrs_ops));
  parse_condition(condition, n_cols, v_ops);
  enum vctrs_multiple c_multiple = parse_multiple(multiple);
  struct vctrs_arg haystack_arg = new_wrapper_arg(NULL, "haystack");
  return compute_nested_containment_info(haystack, c_multiple, v_ops, &haystack_arg);
}

static
r_obj* compute_nested_containment_info(r_obj* haystack,
                                       enum vctrs_multiple multiple,
                                       const enum vctrs_ops* v_ops,
                                       struct vctrs_arg* haystack_arg) {
  r_ssize n_prot = 0;

  r_ssize n_cols = r_length(haystack);
  r_ssize size_haystack = vec_size(haystack);

  // Haystack order, nested groups, number of nested groups, and any directional
  r_obj* out = KEEP_N(r_alloc_list(4), &n_prot);

  // Are there any directional ops (>, >=, <, <=)? And where is the first?
  bool any_non_equi = false;
  int first_directional = 0;
  for (r_ssize i = 0; i < n_cols; ++i) {
    enum vctrs_ops op = v_ops[i];
    if (op == VCTRS_OPS_eq) {
      continue;
    }
    any_non_equi = true;
    first_directional = i;
    break;
  }

  if (!any_non_equi) {
    // Nested group info isn't required for only `==`
    r_list_poke(out, 0, vec_order(haystack, chrs_asc, chrs_smallest, true, r_null));
    r_list_poke(out, 1, vctrs_shared_empty_int);
    r_list_poke(out, 2, r_int(1));
    r_list_poke(out, 3, r_lgl(any_non_equi));
    FREE(n_prot);
    return out;
  }

  r_obj* info = KEEP_N(vec_order_info(haystack, chrs_asc, chrs_smallest, true, r_null, true), &n_prot);

  r_obj* o_haystack = r_list_get(info, 0);
  r_obj* group_sizes_haystack = r_list_get(info, 1);

  r_keep_t haystack_inner_pi;
  r_obj* haystack_inner = haystack;
  KEEP_HERE(haystack_inner, &haystack_inner_pi);
  ++n_prot;

  r_keep_t outer_run_sizes_pi;
  r_obj* outer_run_sizes = vctrs_shared_empty_int;
  KEEP_HERE(outer_run_sizes, &outer_run_sizes_pi);
  ++n_prot;

  if (first_directional != 0) {
    // We have equality comparisons before the first directional comparison.
    // In this case, we can skip nested containment ordering for the equality
    // comparisons before the first directional comparison if we pass on the
    // group sizes of the ordered equality columns.
    r_obj* const* v_haystack = r_list_cbegin(haystack);
    r_obj* const* v_haystack_names = r_chr_cbegin(r_names(haystack));

    // "Outer" data frame columns
    r_obj* haystack_outer = KEEP_N(r_alloc_list(first_directional), &n_prot);
    r_obj* haystack_outer_names = r_alloc_character(first_directional);
    r_poke_names(haystack_outer, haystack_outer_names);
    r_init_data_frame(haystack_outer, size_haystack);
    for (r_ssize i = 0; i < first_directional; ++i) {
      r_list_poke(haystack_outer, i, v_haystack[i]);
      r_chr_poke(haystack_outer_names, i, v_haystack_names[i]);
    }

    // "Inner" data frame columns
    haystack_inner = r_alloc_list(n_cols - first_directional);
    KEEP_AT(haystack_inner, haystack_inner_pi);
    r_obj* haystack_inner_names = r_alloc_character(n_cols - first_directional);
    r_poke_names(haystack_inner, haystack_inner_names);
    r_init_data_frame(haystack_inner, size_haystack);
    for (r_ssize i = first_directional, j = 0; i < n_cols; ++i, ++j) {
      r_list_poke(haystack_inner, j, v_haystack[i]);
      r_chr_poke(haystack_inner_names, j, v_haystack_names[i]);
    }

    r_obj* info = vec_order_info(
      haystack_outer,
      chrs_asc,
      chrs_smallest,
      true,
      r_null,
      true
    );
    outer_run_sizes = r_list_get(info, 1);
    KEEP_AT(outer_run_sizes, outer_run_sizes_pi);
  }

  r_obj* nested_info = KEEP_N(nested_containment_order(
    haystack_inner,
    o_haystack,
    group_sizes_haystack,
    outer_run_sizes,
    multiple
  ), &n_prot);

  int n_nested_groups = r_as_int(r_list_get(nested_info, 1));

  if (n_nested_groups == 1) {
    // If only a single nested group exists, we hit the somewhat rare case of
    // having a >1 col data frame that is already in nested containment order.
    // In that case, original haystack ordering is sufficient.
    r_list_poke(out, 0, o_haystack);
    r_list_poke(out, 1, vctrs_shared_empty_int);
    r_list_poke(out, 2, r_int(1));
    r_list_poke(out, 3, r_lgl(any_non_equi));
    FREE(n_prot);
    return out;
  }

  // Otherwise, we have to recompute haystack ordering with `nested_groups` as
  // the first column.
  r_obj* nested_groups = r_list_get(nested_info, 0);

  r_obj* const* v_haystack = r_list_cbegin(haystack);
  r_obj* const* v_haystack_names = r_chr_cbegin(r_names(haystack));

  r_obj* haystack_with_nesting = KEEP_N(r_alloc_list(n_cols + 1), &n_prot);
  r_obj* haystack_with_nesting_names = r_alloc_character(n_cols + 1);
  r_poke_names(haystack_with_nesting, haystack_with_nesting_names);
  r_init_data_frame(haystack_with_nesting, size_haystack);

  r_list_poke(haystack_with_nesting, 0, nested_groups);
  r_chr_poke(haystack_with_nesting_names, 0, r_str("..nested_groups.."));

  for (r_ssize i = 0; i < n_cols; ++i) {
    r_list_poke(haystack_with_nesting, i + 1, v_haystack[i]);
    r_chr_poke(haystack_with_nesting_names, i + 1, v_haystack_names[i]);
  }

  o_haystack = KEEP_N(vec_order(
    haystack_with_nesting,
    chrs_asc,
    chrs_smallest,
    true,
    r_null
  ), &n_prot);

  r_list_poke(out, 0, o_haystack);
  r_list_poke(out, 1, nested_groups);
  r_list_poke(out, 2, r_int(n_nested_groups));
  r_list_poke(out, 3, r_lgl(any_non_equi));

  FREE(n_prot);
  return out;
}


// -----------------------------------------------------------------------------

static
r_obj* nested_containment_order(r_obj* x,
                                r_obj* order,
                                r_obj* group_sizes,
                                r_obj* outer_run_sizes,
                                enum vctrs_multiple multiple) {
  if (!is_data_frame(x)) {
    r_stop_internal("nested_containment_order", "`x` must be a data frame.");
  }

  int n_prot = 0;

  r_ssize n_cols = r_length(x);
  r_ssize size = r_length(order);

  // For first/last, we require not only increasing order for each
  // column, but also increasing row order as well. This can generate more
  // groups, but ensures that the assignment loop of `df_matches_recurse()`
  // works correctly for these cases.
  const bool enforce_row_order =
    multiple == VCTRS_MULTIPLE_first ||
    multiple == VCTRS_MULTIPLE_last;

  r_obj* out = KEEP_N(r_alloc_list(2), &n_prot);

  r_obj* ids = r_alloc_integer(size);
  r_list_poke(out, 0, ids);
  int* v_ids = r_int_begin(ids);

  r_obj* n_ids = r_alloc_integer(1);
  r_list_poke(out, 1, n_ids);
  int* p_n_ids = r_int_begin(n_ids);
  *p_n_ids = 1;

  for (r_ssize i = 0; i < size; ++i) {
    v_ids[i] = 0;
  }

  if (size == 0) {
    // Algorithm requires at least 1 row
    FREE(n_prot);
    return out;
  }
  if (n_cols == 1 && !enforce_row_order) {
    // If there is only 1 column, `x` is in increasing order already when
    // ordered by `order`. If we don't require that the actual row numbers also
    // be in order, then we are done.
    // If `outer_run_sizes` were supplied, each individual run group will
    // be in increasing order (since the single `x` column is the one that
    // broke any ties), and that is all that is required.
    FREE(n_prot);
    return out;
  }

  const int* v_order = r_int_cbegin(order);
  const int* v_group_sizes = r_int_cbegin(group_sizes);
  const int* v_outer_run_sizes = r_int_cbegin(outer_run_sizes);

  r_ssize n_groups = r_length(group_sizes);

  struct r_dyn_array* p_prev_rows = r_new_dyn_vector(R_TYPE_integer, 10000);
  KEEP_N(p_prev_rows->shelter, &n_prot);

  struct poly_vec* p_poly_vec = new_poly_vec(x, vctrs_type_dataframe);
  PROTECT_POLY_VEC(p_poly_vec, &n_prot);
  const void* v_x = p_poly_vec->p_vec;

  int i_order_group_start = 0;
  int group_size = v_group_sizes[0];
  int i_order = i_order_group_start + ((multiple == VCTRS_MULTIPLE_last) ? group_size - 1 : 0);

  const int first_row = v_order[i_order] - 1;
  p_prev_rows->count = 1;
  R_ARR_POKE(int, p_prev_rows, 0, first_row);

  r_ssize next_outer_run_start = 0;
  r_ssize outer_run_sizes_loc = 0;
  if (r_length(outer_run_sizes) > 0) {
    // If no outer runs exist, `integer(0)` is used and `next_outer_run_start = 0`
    // will never match `i_order_group_start` since the first group size is at least 1.
    next_outer_run_start = v_outer_run_sizes[outer_run_sizes_loc];
    ++outer_run_sizes_loc;
  }

  for (r_ssize i_group = 1; i_group < n_groups; ++i_group) {
    // Catch group start index up to current group using previous group size
    i_order_group_start += group_size;
    // Update to size of current group
    group_size = v_group_sizes[i_group];
    // Update index into order to point to either start / end of this group
    i_order = i_order_group_start + ((multiple == VCTRS_MULTIPLE_last) ? group_size - 1 : 0);

    int cur_row = v_order[i_order] - 1;

    bool new_id = true;
    int prev_row_id = 0;
    int max_prev_row_id = p_prev_rows->count;

    for (; prev_row_id < max_prev_row_id; ++prev_row_id) {
      int prev_row = R_ARR_GET(int, p_prev_rows, prev_row_id);

      if (enforce_row_order && cur_row < prev_row) {
        // Can't add to current group, `multiple = "first"/"last"`
        // require increasing row indices per group
        continue;
      }

      if (p_df_nested_containment_compare_ge_na_equal(v_x, cur_row, v_x, prev_row)) {
        new_id = false;
        break;
      }
    }

    int id;
    if (next_outer_run_start == i_order_group_start) {
      // Start of a new outer run
      id = 0;
      next_outer_run_start += v_outer_run_sizes[outer_run_sizes_loc];
      ++outer_run_sizes_loc;
      p_prev_rows->count = 1;
      R_ARR_POKE(int, p_prev_rows, 0, cur_row);
    } else if (new_id) {
      // Completely new id for this outer run, which we add to the end
      id = max_prev_row_id;
      r_arr_push_back(p_prev_rows, &cur_row);

      if (p_prev_rows->count > *p_n_ids) {
        *p_n_ids = p_prev_rows->count;
      }
    } else {
      // Update existing row location to the current row, since it is larger
      id = prev_row_id;
      R_ARR_POKE(int, p_prev_rows, prev_row_id, cur_row);
    }

    for (int i = 0; i < group_size; ++i) {
      v_ids[v_order[i_order_group_start + i] - 1] = id;
    }
  }

  FREE(n_prot);
  return out;
}

static inline
int p_df_nested_containment_compare_ge_na_equal(const void* x,
                                                r_ssize i,
                                                const void* y,
                                                r_ssize j) {
  // Checks if each column of `x` is `>=` `y`.
  // Assumes inputs are ordered, so first column can be ignored.
  // Iterates backwards to ideally maximize chance of hitting the fastest
  // varying column.
  // All columns are integer vectors (ranks).

  struct poly_df_data* x_data = (struct poly_df_data*) x;
  struct poly_df_data* y_data = (struct poly_df_data*) y;

  r_ssize n_col = x_data->n_col;

  const void** x_ptrs = x_data->col_ptrs;
  const void** y_ptrs = y_data->col_ptrs;

  for (r_ssize col = n_col - 1; col > 0; --col) {
    if (p_int_compare_na_equal(x_ptrs[col], i, y_ptrs[col], j) < 0) {
      return false;
    }
  }

  return true;
}

// -----------------------------------------------------------------------------

static inline
int p_matches_df_compare_na_equal(const void* x,
                                  r_ssize i,
                                  const void* y,
                                  r_ssize j,
                                  const enum vctrs_filter* v_filters) {
  // First broken tie wins.
  // All columns are integer vectors (ranks).

  struct poly_df_data* x_data = (struct poly_df_data*) x;
  struct poly_df_data* y_data = (struct poly_df_data*) y;

  r_ssize n_col = x_data->n_col;

  const void** x_ptrs = x_data->col_ptrs;
  const void** y_ptrs = y_data->col_ptrs;

  for (r_ssize col = 0; col < n_col; ++col) {
    const enum vctrs_filter filter = v_filters[col];

    switch (filter) {
    case VCTRS_FILTER_none: {
      break;
    }
    case VCTRS_FILTER_max: {
      int cmp = p_int_compare_na_equal(x_ptrs[col], i, y_ptrs[col], j);
      if (cmp == 1) {
        // Signal replace
        return 1;
      } else if (cmp == -1) {
        return -1;
      }
      break;
    }
    case VCTRS_FILTER_min: {
      int cmp = p_int_compare_na_equal(x_ptrs[col], i, y_ptrs[col], j);
      if (cmp == -1) {
        // Signal replace
        return 1;
      } else if (cmp == 1) {
        return -1;
      }
      break;
    }
    }
  }

  // All columns are equal, or no columns.
  // In the all equal case, we don't need to update.
  return 0;
}

static inline
bool p_matches_df_equal_na_equal(const void* x,
                                 r_ssize i,
                                 const void* y,
                                 r_ssize j,
                                 const enum vctrs_filter* v_filters) {
  // All columns are integer vectors (ranks).

  struct poly_df_data* x_data = (struct poly_df_data*) x;
  struct poly_df_data* y_data = (struct poly_df_data*) y;

  r_ssize n_col = x_data->n_col;

  const void** x_ptrs = x_data->col_ptrs;
  const void** y_ptrs = y_data->col_ptrs;

  for (r_ssize col = 0; col < n_col; ++col) {
    const enum vctrs_filter filter = v_filters[col];

    if (filter == VCTRS_FILTER_none) {
      continue;
    }

    if (!p_int_equal_na_equal(x_ptrs[col], i, y_ptrs[col], j)) {
      return false;
    }
  }

  // All columns are equal, or no columns.
  return true;
}

// -----------------------------------------------------------------------------

static inline
r_ssize midpoint(r_ssize lhs, r_ssize rhs) {
  return lhs + (rhs - lhs) / 2;
}

// -----------------------------------------------------------------------------

static inline
void stop_matches_overflow(double size) {
  r_abort(
    "Match procedure results in an allocation larger than 2^31-1 elements. "
    "Attempted allocation size was %.0lf. "
    "Please report this to the vctrs maintainers at "
    "<https://github.com/r-lib/vctrs/issues>.",
    size
  );
}

static inline
void stop_matches_nothing(r_ssize i,
                          struct vctrs_arg* needles_arg,
                          struct vctrs_arg* haystack_arg) {
  r_obj* syms[4] = {
    syms_i,
    syms_needles_arg,
    syms_haystack_arg,
    NULL
  };
  r_obj* args[4] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    NULL
  };

  r_obj* call = KEEP(r_call_n(syms_stop_matches_nothing, syms, args));
  Rf_eval(call, vctrs_ns_env);

  never_reached("stop_matches_nothing");
}

static inline
void stop_matches_remaining(r_ssize i,
                            struct vctrs_arg* needles_arg,
                            struct vctrs_arg* haystack_arg) {
  r_obj* syms[4] = {
    syms_i,
    syms_needles_arg,
    syms_haystack_arg,
    NULL
  };
  r_obj* args[4] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    NULL
  };

  r_obj* call = KEEP(r_call_n(syms_stop_matches_remaining, syms, args));
  Rf_eval(call, vctrs_ns_env);

  never_reached("stop_matches_remaining");
}

static inline
void stop_matches_incomplete(r_ssize i, struct vctrs_arg* needles_arg) {
  r_obj* syms[3] = {
    syms_i,
    syms_needles_arg,
    NULL
  };
  r_obj* args[3] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    NULL
  };

  r_obj* call = KEEP(r_call_n(syms_stop_matches_incomplete, syms, args));
  Rf_eval(call, vctrs_ns_env);

  never_reached("stop_matches_incomplete");
}

static inline
void stop_matches_multiple(r_ssize i,
                           struct vctrs_arg* needles_arg,
                           struct vctrs_arg* haystack_arg) {
  r_obj* syms[4] = {
    syms_i,
    syms_needles_arg,
    syms_haystack_arg,
    NULL
  };
  r_obj* args[4] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    NULL
  };

  r_obj* call = KEEP(r_call_n(syms_stop_matches_multiple, syms, args));
  Rf_eval(call, vctrs_ns_env);

  never_reached("stop_matches_multiple");
}

static inline
void warn_matches_multiple(r_ssize i,
                           struct vctrs_arg* needles_arg,
                           struct vctrs_arg* haystack_arg) {
  r_obj* syms[4] = {
    syms_i,
    syms_needles_arg,
    syms_haystack_arg,
    NULL
  };
  r_obj* args[4] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    NULL
  };

  r_obj* call = KEEP(r_call_n(syms_warn_matches_multiple, syms, args));
  Rf_eval(call, vctrs_ns_env);
  FREE(4);
}

// -----------------------------------------------------------------------------

void vctrs_init_match(r_obj* ns) {
  args_incomplete_ = new_wrapper_arg(NULL, "incomplete");
  args_no_match_ = new_wrapper_arg(NULL, "no_match");
  args_remaining_ = new_wrapper_arg(NULL, "remaining");
}

// -----------------------------------------------------------------------------

#undef SIGNAL_NO_MATCH
#undef SIGNAL_INCOMPLETE
