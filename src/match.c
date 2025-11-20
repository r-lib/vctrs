#include "unspecified.h"
#include "vctrs.h"

// -----------------------------------------------------------------------------

enum vctrs_multiple {
  VCTRS_MULTIPLE_all = 0,
  VCTRS_MULTIPLE_any = 1,
  VCTRS_MULTIPLE_first = 2,
  VCTRS_MULTIPLE_last = 3,

  // Deprecated in favor of `relationship`
  VCTRS_MULTIPLE_warning = 4,
  VCTRS_MULTIPLE_error = 5
};

enum vctrs_relationship {
  VCTRS_RELATIONSHIP_none = 0,
  VCTRS_RELATIONSHIP_one_to_one = 1,
  VCTRS_RELATIONSHIP_one_to_many = 2,
  VCTRS_RELATIONSHIP_many_to_one = 3,
  VCTRS_RELATIONSHIP_many_to_many = 4,
  VCTRS_RELATIONSHIP_warn_many_to_many = 5
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
  VCTRS_INCOMPLETE_ACTION_compare = 0,
  VCTRS_INCOMPLETE_ACTION_match = 1,
  VCTRS_INCOMPLETE_ACTION_value = 2,
  VCTRS_INCOMPLETE_ACTION_drop = 3,
  VCTRS_INCOMPLETE_ACTION_error = 4
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

struct vctrs_match_bounds {
  r_ssize lower;
  r_ssize upper;
};

#define SIGNAL_NO_MATCH r_globals.na_int
#define SIGNAL_INCOMPLETE -1

// -----------------------------------------------------------------------------

#include "decl/match-decl.h"

// -----------------------------------------------------------------------------

// [[ register() ]]
r_obj* ffi_locate_matches(r_obj* needles,
                          r_obj* haystack,
                          r_obj* condition,
                          r_obj* filter,
                          r_obj* incomplete,
                          r_obj* no_match,
                          r_obj* remaining,
                          r_obj* multiple,
                          r_obj* relationship,
                          r_obj* nan_distinct,
                          r_obj* chr_proxy_collate,
                          r_obj* needles_arg,
                          r_obj* haystack_arg,
                          r_obj* frame) {
  struct r_lazy error_call = { .x = r_syms.error_call, .env = frame };
  struct r_lazy internal_call = { .x = frame, .env = r_null };

  const struct vctrs_incomplete c_incomplete = parse_incomplete(incomplete, internal_call);
  const struct vctrs_no_match c_no_match = parse_no_match(no_match, internal_call);
  const struct vctrs_remaining c_remaining = parse_remaining(remaining, internal_call);
  const enum vctrs_multiple c_multiple = parse_multiple(multiple, internal_call);
  const enum vctrs_relationship c_relationship = parse_relationship(relationship, internal_call);
  const bool c_nan_distinct = r_arg_as_bool(nan_distinct, "nan_distinct");

  struct vctrs_arg c_needles_arg = vec_as_arg(needles_arg);
  struct vctrs_arg c_haystack_arg = vec_as_arg(haystack_arg);

  return vec_locate_matches(
    needles,
    haystack,
    condition,
    filter,
    &c_incomplete,
    &c_no_match,
    &c_remaining,
    c_multiple,
    c_relationship,
    c_nan_distinct,
    chr_proxy_collate,
    &c_needles_arg,
    &c_haystack_arg,
    error_call
  );
}

static
r_obj* vec_locate_matches(r_obj* needles,
                          r_obj* haystack,
                          r_obj* condition,
                          r_obj* filter,
                          const struct vctrs_incomplete* incomplete,
                          const struct vctrs_no_match* no_match,
                          const struct vctrs_remaining* remaining,
                          enum vctrs_multiple multiple,
                          enum vctrs_relationship relationship,
                          bool nan_distinct,
                          r_obj* chr_proxy_collate,
                          struct vctrs_arg* needles_arg,
                          struct vctrs_arg* haystack_arg,
                          struct r_lazy error_call) {
  int n_prot = 0;

  int _;
  r_obj* ptype = KEEP_N(vec_ptype2_params(
    needles,
    haystack,
    needles_arg,
    haystack_arg,
    error_call,
    &_
  ), &n_prot);

  ptype = vec_ptype_finalise(ptype);

  needles = KEEP_N(vec_cast_params(
    needles,
    ptype,
    needles_arg,
    vec_args.empty,
    error_call,
    S3_FALLBACK_false
  ), &n_prot);

  haystack = KEEP_N(vec_cast_params(
    haystack,
    ptype,
    haystack_arg,
    vec_args.empty,
    error_call,
    S3_FALLBACK_false
  ), &n_prot);

  r_ssize size_needles = vec_size(needles);
  r_ssize size_haystack = vec_size(haystack);

  // Support non-data frame types by wrapping them in a 1-col data frame
  if (!is_data_frame(needles)) {
    needles = KEEP_N(r_list(needles), &n_prot);
    haystack = KEEP_N(r_list(haystack), &n_prot);

    r_obj* names = KEEP_N(r_chr("x"), &n_prot);
    r_attrib_poke_names(needles, names);
    r_attrib_poke_names(haystack, names);

    r_init_data_frame(needles, size_needles);
    r_init_data_frame(haystack, size_haystack);
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
    // If there are no columns, this operation isn't well defined.
    r_abort_lazy_call(error_call, "Must have at least 1 column to match on.");
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
    chr_proxy_collate
  ), &n_prot);
  needles = r_list_get(args, 0);
  haystack = r_list_get(args, 1);

  r_obj* out = df_locate_matches(
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
    relationship,
    any_filters,
    v_filters,
    v_ops,
    needles_arg,
    haystack_arg,
    error_call
  );

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

static
r_obj* df_locate_matches(r_obj* needles,
                         r_obj* haystack,
                         r_obj* needles_complete,
                         r_obj* haystack_complete,
                         r_ssize size_needles,
                         r_ssize size_haystack,
                         const struct vctrs_incomplete* incomplete,
                         const struct vctrs_no_match* no_match,
                         const struct vctrs_remaining* remaining,
                         enum vctrs_multiple multiple,
                         enum vctrs_relationship relationship,
                         bool any_filters,
                         const enum vctrs_filter* v_filters,
                         const enum vctrs_ops* v_ops,
                         struct vctrs_arg* needles_arg,
                         struct vctrs_arg* haystack_arg,
                         struct r_lazy error_call) {
  int n_prot = 0;

  r_obj* o_needles = KEEP_N(vec_order(
    needles,
    chrs_asc,
    chrs_smallest,
    true,
    r_null
  ), &n_prot);
  const int* v_o_needles = r_int_cbegin(o_needles);

  r_obj* container_info = KEEP_N(compute_nesting_container_info(
    haystack,
    size_haystack,
    v_ops
  ), &n_prot);

  r_obj* o_haystack = r_list_get(container_info, 0);
  const int* v_o_haystack = r_int_cbegin(o_haystack);

  // Will be `integer()` if no container ids are required.
  // In that case, `n_containers == 1`.
  r_obj* container_ids = r_list_get(container_info, 1);
  const int* v_container_ids = r_int_cbegin(container_ids);

  const int n_containers = r_as_int(r_list_get(container_info, 2));
  const bool any_non_equi = r_as_bool(r_list_get(container_info, 3));

  // In the case of possible multiple matches that fall in separate
  // nesting containers, allocate ~20% extra room
  const r_ssize initial_capacity =
    (n_containers == 1) ?
    size_needles :
    r_double_as_ssize(r_ssize_as_double(size_needles) * 1.2);

  struct r_dyn_array* p_loc_first_match_o_haystack = r_new_dyn_vector(R_TYPE_integer, initial_capacity);
  KEEP_N(p_loc_first_match_o_haystack->shelter, &n_prot);

  {
    // Temporary unstable pointer
    int* v_loc_first_match_o_haystack = (int*) r_dyn_begin(p_loc_first_match_o_haystack);
    for (r_ssize i = 0; i < size_needles; ++i) {
      // Initialize to no match everywhere, no need to initialize extra buffer
      v_loc_first_match_o_haystack[i] = SIGNAL_NO_MATCH;
    }
    p_loc_first_match_o_haystack->count = size_needles;
  }

  // If we can skip, `size_match` will always be `1`
  const bool skip_size_match = (multiple == VCTRS_MULTIPLE_any);

  struct r_dyn_array* p_size_match = NULL;
  if (!skip_size_match) {
    p_size_match = r_new_dyn_vector(R_TYPE_integer, initial_capacity);
    KEEP_N(p_size_match->shelter, &n_prot);

    int* v_size_match = (int*) r_dyn_begin(p_size_match);
    for (r_ssize i = 0; i < size_needles; ++i) {
      // No need to initialize extra buffer
      v_size_match[i] = 1;
    }

    p_size_match->count = size_needles;
  }

  // If we can skip, `loc_needles` will always be an increasing sequence of values
  const bool skip_loc_needles = (multiple == VCTRS_MULTIPLE_any);

  struct r_dyn_array* p_loc_needles = NULL;
  if (!skip_loc_needles) {
    p_loc_needles = r_new_dyn_vector(R_TYPE_integer, initial_capacity);
    KEEP_N(p_loc_needles->shelter, &n_prot);

    int* v_loc_needles = (int*) r_dyn_begin(p_loc_needles);
    for (r_ssize i = 0; i < size_needles; ++i) {
      // No need to initialize extra buffer
      v_loc_needles[i] = i;
    }

    p_loc_needles->count = size_needles;
  }

  // When filtering, we find the filtered match for a particular needle in each
  // nesting container of the haystack. `v_loc_filter_match_o_haystack`
  // keeps track of the overall filtered match loc for a needle across all
  // nesting containers in the haystack.
  const bool has_loc_filter_match_o_haystack =
    any_filters &&
    (multiple == VCTRS_MULTIPLE_all ||
     multiple == VCTRS_MULTIPLE_warning ||
     multiple == VCTRS_MULTIPLE_error ||
     multiple == VCTRS_MULTIPLE_first ||
     multiple == VCTRS_MULTIPLE_last);

  int* v_loc_filter_match_o_haystack = NULL;
  if (has_loc_filter_match_o_haystack) {
    r_obj* loc_filter_match_o_haystack = KEEP_N(r_alloc_integer(size_needles), &n_prot);
    v_loc_filter_match_o_haystack = r_int_begin(loc_filter_match_o_haystack);
  }

  struct poly_vec* p_poly_needles = new_poly_vec(needles, VCTRS_TYPE_dataframe);
  KEEP_N(p_poly_needles->shelter, &n_prot);
  const struct poly_df_data* p_needles = (const struct poly_df_data*) p_poly_needles->p_vec;

  struct poly_vec* p_poly_haystack = new_poly_vec(haystack, VCTRS_TYPE_dataframe);
  KEEP_N(p_poly_haystack->shelter, &n_prot);
  const struct poly_df_data* p_haystack = (const struct poly_df_data*) p_poly_haystack->p_vec;

  const struct poly_vec* p_poly_needles_complete = new_poly_vec(needles_complete, VCTRS_TYPE_dataframe);
  KEEP_N(p_poly_needles_complete->shelter, &n_prot);
  const struct poly_df_data* p_needles_complete = (const struct poly_df_data*) p_poly_needles_complete->p_vec;

  struct poly_vec* p_poly_haystack_complete = new_poly_vec(haystack_complete, VCTRS_TYPE_dataframe);
  KEEP_N(p_poly_haystack_complete->shelter, &n_prot);
  const struct poly_df_data* p_haystack_complete = (const struct poly_df_data*) p_poly_haystack_complete->p_vec;

  if (size_needles > 0) {
    // Recursion requires at least 1 row in needles.
    // In the case of size 0 needles, there is nothing to do, but this avoids
    // a segfault.

    const r_ssize col = 0;
    const r_ssize loc_lower_bound_o_needles = 0;
    const r_ssize loc_upper_bound_o_needles = size_needles - 1;
    const r_ssize loc_lower_bound_o_haystack = 0;
    const r_ssize loc_upper_bound_o_haystack = size_haystack - 1;

    if (n_containers == 1) {
      df_locate_matches_recurse(
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
        v_loc_filter_match_o_haystack
      );
    } else {
      df_locate_matches_with_containers(
        n_containers,
        v_container_ids,
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
        v_loc_filter_match_o_haystack
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
    relationship,
    size_needles,
    size_haystack,
    any_non_equi,
    has_loc_filter_match_o_haystack,
    v_filters,
    v_loc_filter_match_o_haystack,
    p_haystack,
    needles_arg,
    haystack_arg,
    error_call
  ), &n_prot);

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

static
void df_locate_matches_recurse(r_ssize col,
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
                               int* v_loc_filter_match_o_haystack) {
  const enum vctrs_ops op = v_ops[col];
  const enum vctrs_filter filter = v_filters[col];
  const r_ssize n_col = p_needles->n_col;

  const int* v_needles = (const int*) p_needles->v_col_ptr[col];
  const int* v_needles_complete = (const int*) p_needles_complete->v_col_ptr[col];

  const int* v_haystack = (const int*) p_haystack->v_col_ptr[col];
  const int* v_haystack_complete = (const int*) p_haystack_complete->v_col_ptr[col];

  const r_ssize loc_mid_bound_o_needles = midpoint(loc_lower_bound_o_needles, loc_upper_bound_o_needles);
  const r_ssize loc_mid_bound_needles = v_o_needles[loc_mid_bound_o_needles] - 1;

  const int val_needle = v_needles[loc_mid_bound_needles];
  const bool needle_is_complete = v_needles_complete[loc_mid_bound_needles];

  // Find lower and upper duplicate location for this needle
  const r_ssize loc_lower_duplicate_o_needles = int_locate_lower_duplicate(
    val_needle,
    v_needles,
    v_o_needles,
    loc_lower_bound_o_needles,
    loc_mid_bound_o_needles
  );
  const r_ssize loc_upper_duplicate_o_needles = int_locate_upper_duplicate(
    val_needle,
    v_needles,
    v_o_needles,
    loc_mid_bound_o_needles,
    loc_upper_bound_o_needles
  );

  if (!needle_is_complete &&
      (incomplete->action == VCTRS_INCOMPLETE_ACTION_value ||
       incomplete->action == VCTRS_INCOMPLETE_ACTION_drop ||
       incomplete->action == VCTRS_INCOMPLETE_ACTION_error)) {
    // Signal incomplete needle, don't recursive into further columns.
    // Early return at the end of this branch.

    for (r_ssize i = loc_lower_duplicate_o_needles; i <= loc_upper_duplicate_o_needles; ++i) {
      // Will always be the first and only time the output is touched for this
      // needle, so we can poke directly into it
      const int loc_needles = v_o_needles[i] - 1;
      r_dyn_int_poke(p_loc_first_match_o_haystack, loc_needles, SIGNAL_INCOMPLETE);
    }

    // Learned nothing about haystack, so just update lhs/rhs bounds for
    // `o_needles` as needed and continue on
    bool do_lhs = loc_lower_duplicate_o_needles > loc_lower_bound_o_needles;
    bool do_rhs = loc_upper_duplicate_o_needles < loc_upper_bound_o_needles;

    if (do_lhs) {
      const r_ssize lhs_loc_lower_bound_o_needles = loc_lower_bound_o_needles;
      const r_ssize lhs_loc_upper_bound_o_needles = loc_lower_duplicate_o_needles - 1;

      df_locate_matches_recurse(
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
        v_loc_filter_match_o_haystack
      );
    }
    if (do_rhs) {
      const r_ssize rhs_loc_lower_bound_o_needles = loc_upper_duplicate_o_needles + 1;
      const r_ssize rhs_loc_upper_bound_o_needles = loc_upper_bound_o_needles;

      df_locate_matches_recurse(
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
        v_loc_filter_match_o_haystack
      );
    }

    return;
  }

  const struct vctrs_match_bounds bounds = int_locate_match(
    val_needle,
    v_haystack,
    v_o_haystack,
    loc_lower_bound_o_haystack,
    loc_upper_bound_o_haystack
  );

  r_ssize loc_lower_match_o_haystack = bounds.lower;
  r_ssize loc_upper_match_o_haystack = bounds.upper;

  // Adjust bounds based on non-equi condition.
  // If needle is NA and we are doing an exact match, then we treat it like an
  // equi condition here. Otherwise if needle is NA, then we are careful to
  // never extend the bounds to capture values past it.
  const enum vctrs_ops bounds_op =
    (!needle_is_complete && incomplete->action == VCTRS_INCOMPLETE_ACTION_match) ?
    VCTRS_OPS_eq :
    op;

  switch (bounds_op) {
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
    // after applying the non-equi adjustments above because NA values are
    // always ordered as the "smallest" values, and we set
    // `loc_lower_match_o_haystack` to be `loc_lower_bound_o_haystack`, which
    // may capture NAs at the lower bound. If there is an NA on the lower bound,
    // we avoid it by finding the last NA and then going 1 location beyond it.
    const r_ssize loc_lower_match_haystack = v_o_haystack[loc_lower_match_o_haystack] - 1;
    const bool lower_match_haystack_is_complete = v_haystack_complete[loc_lower_match_haystack];

    if (!lower_match_haystack_is_complete) {
      // Find the last incomplete value
      loc_lower_match_o_haystack = int_locate_upper_incomplete(
        v_haystack_complete,
        v_o_haystack,
        loc_lower_match_o_haystack,
        loc_upper_match_o_haystack
      );

      // Exclude it and all before it
      ++loc_lower_match_o_haystack;
    }
  }

  if (loc_lower_match_o_haystack <= loc_upper_match_o_haystack) {
    // Hit!

    switch (filter) {
    case VCTRS_FILTER_max: {
      if (!needle_is_complete || op == VCTRS_OPS_eq) {
        // Lower match value will already equal upper match value
        break;
      }

      // We want the max values of this group. That's the upper match of the
      // haystack and its corresponding lower duplicate.
      const int loc_lower_match_haystack = v_o_haystack[loc_lower_match_o_haystack] - 1;
      const int loc_upper_match_haystack = v_o_haystack[loc_upper_match_o_haystack] - 1;
      const int val_lower_match_haystack = v_haystack[loc_lower_match_haystack];
      const int val_upper_match_haystack = v_haystack[loc_upper_match_haystack];

      if (val_lower_match_haystack != val_upper_match_haystack) {
        loc_lower_match_o_haystack = int_locate_lower_duplicate(
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
        // Lower match value will already equal upper match value
        break;
      }

      // We want the min values of this group. That's the lower match of the
      // haystack and its corresponding upper duplicate.
      const int loc_lower_match_haystack = v_o_haystack[loc_lower_match_o_haystack] - 1;
      const int loc_upper_match_haystack = v_o_haystack[loc_upper_match_o_haystack] - 1;
      const int val_lower_match_haystack = v_haystack[loc_lower_match_haystack];
      const int val_upper_match_haystack = v_haystack[loc_upper_match_haystack];

      if (val_lower_match_haystack != val_upper_match_haystack) {
        loc_upper_match_o_haystack = int_locate_upper_duplicate(
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
      // For this column, we've bounded the needles locations to the upper/lower
      // duplicates of the current needle, and the haystack locations to the
      // upper/lower matches of that needle. Now recurse into the next column
      // to further refine the boundaries.
      df_locate_matches_recurse(
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
        v_loc_filter_match_o_haystack
      );
    } else {
      // We just finished locating matches for the last column,
      // and we still have at least 1 hit, so record it

      for (r_ssize i = loc_lower_duplicate_o_needles; i <= loc_upper_duplicate_o_needles; ++i) {
        const int loc_needles = v_o_needles[i] - 1;
        const int loc_first_match_o_haystack = r_dyn_int_get(p_loc_first_match_o_haystack, loc_needles);
        const bool first_touch = loc_first_match_o_haystack == r_globals.na_int;

        switch (multiple) {
        case VCTRS_MULTIPLE_any: {
          if (first_touch) {
            // Arbitrarily record the lower match
            r_dyn_int_poke(p_loc_first_match_o_haystack, loc_needles, loc_lower_match_o_haystack);
            break;
          }

          if (any_filters) {
            const int loc_first_match_haystack = v_o_haystack[loc_first_match_o_haystack] - 1;
            const int loc_lower_match_haystack = v_o_haystack[loc_lower_match_o_haystack] - 1;

            const int cmp = p_matches_df_compare_na_equal(
              p_haystack,
              loc_lower_match_haystack,
              p_haystack,
              loc_first_match_haystack,
              v_filters
            );

            // -1 = New haystack value "loses", nothing to update
            //  1 = New haystack value "wins", it becomes new match
            //  0 = Equal values, nothing to update
            if (cmp == 1) {
              r_dyn_int_poke(p_loc_first_match_o_haystack, loc_needles, loc_lower_match_o_haystack);
            }
          }

          break;
        }
        case VCTRS_MULTIPLE_all:
        case VCTRS_MULTIPLE_error:
        case VCTRS_MULTIPLE_warning:
        case VCTRS_MULTIPLE_first:
        case VCTRS_MULTIPLE_last: {
          const int size_match = loc_upper_match_o_haystack - loc_lower_match_o_haystack + 1;

          if (first_touch) {
            r_dyn_int_poke(p_loc_first_match_o_haystack, loc_needles, loc_lower_match_o_haystack);
            r_dyn_int_poke(p_size_match, loc_needles, size_match);

            if (any_filters) {
              v_loc_filter_match_o_haystack[loc_needles] = loc_lower_match_o_haystack;
            }

            break;
          }

          if (any_filters) {
            const int loc_filter_match_o_haystack = v_loc_filter_match_o_haystack[loc_needles];

            const int loc_filter_match_haystack = v_o_haystack[loc_filter_match_o_haystack] - 1;
            const int loc_lower_match_haystack = v_o_haystack[loc_lower_match_o_haystack] - 1;

            const int cmp = p_matches_df_compare_na_equal(
              p_haystack,
              loc_lower_match_haystack,
              p_haystack,
              loc_filter_match_haystack,
              v_filters
            );

            // -1 = New haystack value "loses", nothing to update
            //  1 = New haystack value "wins", it becomes new filter match
            //  0 = Equal values, fall through and append this set of matches
            // Note that in the 1 case, we have no way to invalidate the old
            // match at this point in time. Instead, we record all matches and
            // in `expand_compact_indices()` we skip the ones that aren't
            // equivalent to the filter match.
            if (cmp == -1) {
              break;
            } else if (cmp == 1) {
              v_loc_filter_match_o_haystack[loc_needles] = loc_lower_match_o_haystack;
            }
          }

          r_dyn_push_back(p_loc_first_match_o_haystack, &loc_lower_match_o_haystack);
          r_dyn_push_back(p_size_match, &size_match);
          r_dyn_push_back(p_loc_needles, &loc_needles);
          break;
        }
        }
      }
    }
  } else if (col < n_col - 1 &&
             (incomplete->action == VCTRS_INCOMPLETE_ACTION_value ||
              incomplete->action == VCTRS_INCOMPLETE_ACTION_drop ||
              incomplete->action == VCTRS_INCOMPLETE_ACTION_error)) {
    // This branch occurs if there is no match in `haystack` for this needle,
    // but we aren't on the last column and we are tracking incomplete needles.
    // Before we move on from this needle, we check its future columns for
    // incomplete values. If the current `val_needles` was incomplete, it would
    // have already been caught above, so we only look at future columns.
    for (r_ssize i = loc_lower_duplicate_o_needles; i <= loc_upper_duplicate_o_needles; ++i) {
      const r_ssize loc_needles = v_o_needles[i] - 1;

      for (r_ssize j = col + 1; j < n_col; ++j) {
        const int* v_future_needles_complete = (const int*) p_needles_complete->v_col_ptr[j];
        const bool future_needle_is_complete = v_future_needles_complete[loc_needles];

        if (!future_needle_is_complete) {
          r_dyn_int_poke(p_loc_first_match_o_haystack, loc_needles, SIGNAL_INCOMPLETE);
          break;
        }
      }
    }
  }

  // At this point we have finished recording matches for the current needle in
  // this column, and we need to move on to other needles on the LHS and RHS
  // of the current needle (remember the current needle is the midpoint). For
  // the `==` op case we can also limit the haystack bounds we search in for
  // needles on the LHS/RHS, since those needles won't ever match the current
  // haystack values.
  bool do_lhs = false;
  bool do_rhs = false;

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
    // Can't update haystack here, as nesting containers make this impossible
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
    df_locate_matches_recurse(
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
      v_loc_filter_match_o_haystack
    );
  }
  if (do_rhs) {
    df_locate_matches_recurse(
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
      v_loc_filter_match_o_haystack
    );
  }
}

// -----------------------------------------------------------------------------

static
void df_locate_matches_with_containers(int n_containers,
                                       const int* v_container_ids,
                                       r_ssize col,
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
                                       int* v_loc_filter_match_o_haystack) {
  const int* v_haystack = v_container_ids;

  for (int i = 0; i < n_containers; ++i) {
    const int val_needle = i;

    const struct vctrs_match_bounds bounds = int_locate_match(
      val_needle,
      v_haystack,
      v_o_haystack,
      loc_lower_bound_o_haystack,
      loc_upper_bound_o_haystack
    );

    const r_ssize loc_lower_match_o_haystack = bounds.lower;
    const r_ssize loc_upper_match_o_haystack = bounds.upper;

    df_locate_matches_recurse(
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
      v_loc_filter_match_o_haystack
    );

    // Advance lower bound for next container
    loc_lower_bound_o_haystack = loc_upper_match_o_haystack + 1;
  }
}

// -----------------------------------------------------------------------------

// In a sorted array, binary search between
// [loc_lower_bound_o_haystack, loc_upper_bound_o_haystack]
// to find the last incomplete value
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

// In a sorted array, binary search between
// [loc_lower_bound_o_haystack, loc_upper_bound_o_haystack]
// to find the first occurrence of `val_needle`
static inline
r_ssize int_locate_lower_duplicate(int val_needle,
                                   const int* v_haystack,
                                   const int* v_o_haystack,
                                   r_ssize loc_lower_bound_o_haystack,
                                   r_ssize loc_upper_bound_o_haystack) {
  while (loc_lower_bound_o_haystack <= loc_upper_bound_o_haystack) {
    const r_ssize loc_mid_bound_o_haystack = midpoint(loc_lower_bound_o_haystack, loc_upper_bound_o_haystack);
    const r_ssize loc_mid_bound_haystack = v_o_haystack[loc_mid_bound_o_haystack] - 1;
    const int val_haystack = v_haystack[loc_mid_bound_haystack];

    if (int_equal_na_equal(val_needle, val_haystack)) {
      loc_upper_bound_o_haystack = loc_mid_bound_o_haystack - 1;
    } else {
      loc_lower_bound_o_haystack = loc_mid_bound_o_haystack + 1;
    }
  }

  return loc_lower_bound_o_haystack;
}

// -----------------------------------------------------------------------------

// In a sorted array, binary search between
// [loc_lower_bound_o_haystack, loc_upper_bound_o_haystack]
// to find the last occurrence of `val_needle`
static inline
r_ssize int_locate_upper_duplicate(int val_needle,
                                   const int* v_haystack,
                                   const int* v_o_haystack,
                                   r_ssize loc_lower_bound_o_haystack,
                                   r_ssize loc_upper_bound_o_haystack) {
  while (loc_lower_bound_o_haystack <= loc_upper_bound_o_haystack) {
    const r_ssize loc_mid_bound_o_haystack = midpoint(loc_lower_bound_o_haystack, loc_upper_bound_o_haystack);
    const r_ssize loc_mid_bound_haystack = v_o_haystack[loc_mid_bound_o_haystack] - 1;
    const int val_haystack = v_haystack[loc_mid_bound_haystack];

    if (int_equal_na_equal(val_needle, val_haystack)) {
      loc_lower_bound_o_haystack = loc_mid_bound_o_haystack + 1;
    } else {
      loc_upper_bound_o_haystack = loc_mid_bound_o_haystack - 1;
    }
  }

  return loc_upper_bound_o_haystack;
}

// -----------------------------------------------------------------------------

// In a sorted array, binary search between
// [loc_lower_bound_o_haystack, loc_upper_bound_o_haystack]
// to find the first and last occurrence of `val_needle`
static inline
struct vctrs_match_bounds int_locate_match(int val_needle,
                                           const int* v_haystack,
                                           const int* v_o_haystack,
                                           r_ssize loc_lower_bound_o_haystack,
                                           r_ssize loc_upper_bound_o_haystack) {
  while (loc_lower_bound_o_haystack <= loc_upper_bound_o_haystack) {
    const r_ssize loc_mid_bound_o_haystack = midpoint(loc_lower_bound_o_haystack, loc_upper_bound_o_haystack);
    const r_ssize loc_mid_bound_haystack = v_o_haystack[loc_mid_bound_o_haystack] - 1;
    const int val_haystack = v_haystack[loc_mid_bound_haystack];

    const int cmp = int_compare_na_equal(val_needle, val_haystack);

    if (cmp == 1) {
      loc_lower_bound_o_haystack = loc_mid_bound_o_haystack + 1;
    } else if (cmp == -1) {
      loc_upper_bound_o_haystack = loc_mid_bound_o_haystack - 1;
    } else {
      // Hit!
      // Find lower and upper duplicate bounds for the haystack value
      loc_lower_bound_o_haystack = int_locate_lower_duplicate(
        val_haystack,
        v_haystack,
        v_o_haystack,
        loc_lower_bound_o_haystack,
        loc_mid_bound_o_haystack
      );
      loc_upper_bound_o_haystack = int_locate_upper_duplicate(
        val_haystack,
        v_haystack,
        v_o_haystack,
        loc_mid_bound_o_haystack,
        loc_upper_bound_o_haystack
      );
      break;
    }
  }

  return (struct vctrs_match_bounds) {
    loc_lower_bound_o_haystack,
    loc_upper_bound_o_haystack
  };
}

// -----------------------------------------------------------------------------

static
r_obj* df_joint_xtfrm_by_col(r_obj* x,
                             r_obj* y,
                             r_ssize x_size,
                             r_ssize y_size,
                             r_ssize n_cols,
                             bool nan_distinct,
                             r_obj* chr_proxy_collate) {
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
    r_obj* xtfrms = vec_joint_xtfrm(x_col, y_col, x_size, y_size, nan_distinct, chr_proxy_collate);
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
  r_attrib_poke_names(out, r_names(x));
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
    r_abort("`condition` must be a character vector.");
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
    "`condition` must be length 1, or the same "
    "length as the number of columns of the input."
  );
}

// -----------------------------------------------------------------------------

static inline
struct vctrs_incomplete parse_incomplete(r_obj* incomplete,
                                         struct r_lazy call) {
  if (r_length(incomplete) != 1) {
    r_abort_lazy_call(
      call,
      "`incomplete` must be length 1, not length %i.",
      r_length(incomplete)
    );
  }

  if (r_is_string(incomplete)) {
    const char* c_incomplete = r_chr_get_c_string(incomplete, 0);

    if (!strcmp(c_incomplete, "compare")) {
      return (struct vctrs_incomplete) {
        .action = VCTRS_INCOMPLETE_ACTION_compare,
        .value = -1
      };
    }

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

    r_abort_lazy_call(
      call,
      "`incomplete` must be one of: \"compare\", \"match\", \"drop\", or \"error\"."
    );
  }

  incomplete = vec_cast(
    incomplete,
    r_globals.empty_int,
    args_incomplete,
    vec_args.empty,
    call
  );
  int c_incomplete = r_int_get(incomplete, 0);

  return (struct vctrs_incomplete) {
    .action = VCTRS_INCOMPLETE_ACTION_value,
    .value = c_incomplete
  };
}

// -----------------------------------------------------------------------------

static inline
enum vctrs_multiple parse_multiple(r_obj* multiple, struct r_lazy call) {
  if (!r_is_string(multiple)) {
    r_abort_lazy_call(call, "`multiple` must be a string.");
  }

  const char* c_multiple = r_chr_get_c_string(multiple, 0);

  if (!strcmp(c_multiple, "all")) return VCTRS_MULTIPLE_all;
  if (!strcmp(c_multiple, "any")) return VCTRS_MULTIPLE_any;
  if (!strcmp(c_multiple, "first")) return VCTRS_MULTIPLE_first;
  if (!strcmp(c_multiple, "last")) return VCTRS_MULTIPLE_last;
  // TODO: Remove deprecated support for `multiple = "error"/"warning"`
  if (!strcmp(c_multiple, "warning")) return VCTRS_MULTIPLE_warning;
  if (!strcmp(c_multiple, "error")) return VCTRS_MULTIPLE_error;

  r_abort_lazy_call(
    call,
    "`multiple` must be one of \"all\", \"any\", \"first\", or \"last\"."
  );
}

// -----------------------------------------------------------------------------

static inline
enum vctrs_relationship parse_relationship(r_obj* relationship, struct r_lazy call) {
  if (!r_is_string(relationship)) {
    r_abort_lazy_call(call, "`relationship` must be a string.");
  }

  const char* c_relationship = r_chr_get_c_string(relationship, 0);

  if (!strcmp(c_relationship, "none")) return VCTRS_RELATIONSHIP_none;
  if (!strcmp(c_relationship, "one-to-one")) return VCTRS_RELATIONSHIP_one_to_one;
  if (!strcmp(c_relationship, "one-to-many")) return VCTRS_RELATIONSHIP_one_to_many;
  if (!strcmp(c_relationship, "many-to-one")) return VCTRS_RELATIONSHIP_many_to_one;
  if (!strcmp(c_relationship, "many-to-many")) return VCTRS_RELATIONSHIP_many_to_many;
  if (!strcmp(c_relationship, "warn-many-to-many")) return VCTRS_RELATIONSHIP_warn_many_to_many;

  r_abort_lazy_call(
    call,
    "`relationship` must be one of \"none\", \"one-to-one\", \"one-to-many\", \"many-to-one\", \"many-to-many\", or \"warn-many-to-many\"."
  );
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
struct vctrs_no_match parse_no_match(r_obj* no_match,
                                     struct r_lazy call) {
  if (r_length(no_match) != 1) {
    r_abort_lazy_call(
      call,
      "`no_match` must be length 1, not length %i.",
      r_length(no_match)
    );
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

    r_abort_lazy_call(
      call,
      "`no_match` must be either \"drop\" or \"error\"."
    );
  }

  no_match = vec_cast(
    no_match,
    r_globals.empty_int,
    args_no_match,
    vec_args.empty,
    call
  );
  int c_no_match = r_int_get(no_match, 0);

  return (struct vctrs_no_match) {
    .action = VCTRS_NO_MATCH_ACTION_value,
    .value = c_no_match
  };
}

// -----------------------------------------------------------------------------

static inline
struct vctrs_remaining parse_remaining(r_obj* remaining,
                                       struct r_lazy call) {
  if (r_length(remaining) != 1) {
    r_abort_lazy_call(
      call,
      "`remaining` must be length 1, not length %i.",
      r_length(remaining)
    );
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

    r_abort_lazy_call(
      call,
      "`remaining` must be either \"drop\" or \"error\"."
    );
  }

  remaining = vec_cast(
    remaining,
    r_globals.empty_int,
    args_remaining,
    vec_args.empty,
    call
  );
  int c_remaining = r_int_get(remaining, 0);

  return (struct vctrs_remaining) {
    .action = VCTRS_REMAINING_ACTION_value,
    .value = c_remaining
  };
}

// -----------------------------------------------------------------------------

static inline
r_obj* new_matches_result(r_obj* needles, r_obj* haystack) {
  r_obj* out = KEEP(r_alloc_list(2));

  r_list_poke(out, 0, needles);
  r_list_poke(out, 1, haystack);

  r_obj* names = r_alloc_character(2);
  r_attrib_poke_names(out, names);

  r_chr_poke(names, 0, strings_needles);
  r_chr_poke(names, 1, strings_haystack);

  r_init_data_frame(out, r_length(needles));

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
                              enum vctrs_relationship relationship,
                              r_ssize size_needles,
                              r_ssize size_haystack,
                              bool any_non_equi,
                              bool has_loc_filter_match_o_haystack,
                              const enum vctrs_filter* v_filters,
                              const int* v_loc_filter_match_o_haystack,
                              const struct poly_df_data* p_haystack,
                              struct vctrs_arg* needles_arg,
                              struct vctrs_arg* haystack_arg,
                              struct r_lazy error_call) {
  int n_prot = 0;

  const r_ssize n_used = p_loc_first_match_o_haystack->count;

  const int* v_loc_first_match_o_haystack = (const int*) r_dyn_cbegin(p_loc_first_match_o_haystack);
  const int* v_size_match = skip_size_match ? NULL : (const int*) r_dyn_cbegin(p_size_match);
  const int* v_loc_needles = skip_loc_needles ? NULL : (const int*) r_dyn_cbegin(p_loc_needles);

  const bool one_match_per_needle =
    multiple == VCTRS_MULTIPLE_any ||
    multiple == VCTRS_MULTIPLE_first ||
    multiple == VCTRS_MULTIPLE_last;

  r_ssize size_out = 0;
  if (one_match_per_needle) {
    size_out = size_needles;
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
      stop_matches_overflow(dbl_size_out, error_call);
    }

    size_out = r_double_as_ssize(dbl_size_out);
  }

  r_keep_loc out_needles_pi;
  r_obj* out_needles = r_alloc_integer(size_out);
  KEEP_HERE(out_needles, &out_needles_pi);
  ++n_prot;

  r_keep_loc out_haystack_pi;
  r_obj* out_haystack = r_alloc_integer(size_out);
  KEEP_HERE(out_haystack, &out_haystack_pi);
  ++n_prot;

  int* v_out_needles = r_int_begin(out_needles);
  int* v_out_haystack = r_int_begin(out_haystack);

  const int* v_o_loc_needles = NULL;
  if (!skip_loc_needles) {
    // `loc_needles` is used to record the location of the needle that the
    // matches correspond to. The first `size_needles` elements will be in
    // sequential order, but locations after that correspond to the "extra"
    // matches gathered from different nesting containers. We need the order of
    // this `loc_needles` vector so we can process all the matches for needle 1,
    // then 2, then 3, etc, in that order, across all nesting containers.
    r_obj* loc_needles = KEEP_N(r_dyn_unwrap(p_loc_needles), &n_prot);
    r_obj* o_loc_needles = KEEP_N(vec_order(loc_needles, chrs_asc, chrs_smallest, true, r_null), &n_prot);
    v_o_loc_needles = r_int_cbegin(o_loc_needles);
  }

  bool any_multiple_needles = false;
  bool any_multiple_haystack = false;

  r_ssize loc_first_multiple_needles = -1;
  r_ssize loc_first_multiple_haystack = -1;

  // Check is always needed for `multiple = "all"`.
  // This also handles `relationship` options too, since if `multiple` is
  // `"any"`, `"first"`, or `"last"`, we can't invalidate a `relationship`.
  bool check_multiple_needles =
    multiple == VCTRS_MULTIPLE_all ||
    // TODO: Remove deprecated support for `multiple = "error"/"warning"`
    multiple == VCTRS_MULTIPLE_error ||
    multiple == VCTRS_MULTIPLE_warning;

  // Used to enforce `check_multiple_needles`
  r_ssize loc_needles_previous = r_globals.na_int;

  bool check_multiple_haystack = false;
  switch (relationship) {
  // Expecting `haystack` can match any number of `needles`
  case VCTRS_RELATIONSHIP_none:
  case VCTRS_RELATIONSHIP_many_to_one:
  case VCTRS_RELATIONSHIP_many_to_many: {
    check_multiple_haystack = false;
    break;
  }
  // Expecting `haystack` to match at most 1 `needles`
  case VCTRS_RELATIONSHIP_one_to_one:
  case VCTRS_RELATIONSHIP_one_to_many: {
    check_multiple_haystack = true;
    break;
  }
  // Only check for multiple matches in `haystack` if we are also checking
  // for them in `needles`. Otherwise we can't possibly have a many-to-many
  // issue so there is no need to check for one.
  case VCTRS_RELATIONSHIP_warn_many_to_many: {
    check_multiple_haystack = check_multiple_needles;
    break;
  }
  }

  const bool retain_remaining_haystack =
    remaining->action == VCTRS_REMAINING_ACTION_value ||
    remaining->action == VCTRS_REMAINING_ACTION_error;

  bool track_matches_haystack = check_multiple_haystack || retain_remaining_haystack;
  bool* v_detect_matches_haystack = NULL;
  if (track_matches_haystack) {
    r_obj* detect_matches_haystack = KEEP_N(r_alloc_raw(size_haystack * sizeof(bool)), &n_prot);
    v_detect_matches_haystack = r_raw_begin(detect_matches_haystack);
    r_memset(v_detect_matches_haystack, 0, size_haystack * sizeof(bool));
  }

  // For `multiple = "first" / "last"`
  r_ssize loc_haystack_overall = r_globals.na_int;

  r_ssize loc_out = 0;

  for (r_ssize i = 0; i < n_used; ++i) {
    const int loc = skip_loc_needles ? i : v_o_loc_needles[i] - 1;

    const int loc_first_match_o_haystack = v_loc_first_match_o_haystack[loc];
    const int size_match = skip_size_match ? 1 : v_size_match[loc];
    const int loc_needles = skip_loc_needles ? loc : v_loc_needles[loc];

    if (loc_first_match_o_haystack == SIGNAL_INCOMPLETE) {
      if (size_match != 1) {
        r_stop_internal(
          "`size_match` should always be 1 in the case of incomplete values."
        );
      }

      switch (incomplete->action) {
      case VCTRS_INCOMPLETE_ACTION_value: {
        v_out_needles[loc_out] = loc_needles + 1;
        v_out_haystack[loc_out] = incomplete->value;
        ++loc_out;
        continue;
      }
      case VCTRS_INCOMPLETE_ACTION_drop: {
        // Do not increment `loc_out`, do not store locations
        continue;
      }
      case VCTRS_INCOMPLETE_ACTION_error: {
        stop_matches_incomplete(loc_needles, needles_arg, error_call);
      }
      case VCTRS_INCOMPLETE_ACTION_compare:
      case VCTRS_INCOMPLETE_ACTION_match: {
        r_stop_internal(
          "Needles should never be marked as `SIGNAL_INCOMPLETE`",
          "when `incomplete = 'compare'` or `incomplete = 'match'`."
        );
      }
      default: {
        r_stop_internal("Unknown `incomplete->action`.");
      }
      }
    }

    if (loc_first_match_o_haystack == SIGNAL_NO_MATCH) {
      if (size_match != 1) {
        r_stop_internal(
          "`size_match` should always be 1 in the case of no matches."
        );
      }

      switch (no_match->action) {
      case VCTRS_NO_MATCH_ACTION_value: {
        v_out_needles[loc_out] = loc_needles + 1;
        v_out_haystack[loc_out] = no_match->value;
        ++loc_out;
        continue;
      }
      case VCTRS_NO_MATCH_ACTION_drop: {
        continue;
      }
      case VCTRS_NO_MATCH_ACTION_error: {
        stop_matches_nothing(loc_needles, needles_arg, haystack_arg, error_call);
      }
      default: {
        r_stop_internal("Unknown `no_match->action`.");
      }
      }
    }

    if (has_loc_filter_match_o_haystack) {
      // When recording matches, if we updated the filter match value for a
      // particular needle, then we weren't able to remove the old match from
      // `p_loc_first_match_o_haystack`. So we need to check that the current
      // match value in the haystack is the same as the recorded filter match
      // value for this needle. If it is the same, we continue, otherwise we
      // move on to the next value.
      const int loc_filter_match_o_haystack = v_loc_filter_match_o_haystack[loc_needles];

      bool equal = false;

      if (loc_filter_match_o_haystack == loc_first_match_o_haystack) {
        equal = true;
      } else {
        const int loc_filter_match_haystack = v_o_haystack[loc_filter_match_o_haystack] - 1;
        const int loc_first_match_haystack = v_o_haystack[loc_first_match_o_haystack] - 1;

        equal = p_matches_df_equal_na_equal(
          p_haystack,
          loc_first_match_haystack,
          p_haystack,
          loc_filter_match_haystack,
          v_filters
        );
      }

      if (!equal) {
        continue;
      }
    }

    if (check_multiple_needles) {
      if (size_match > 1) {
        // Easy, obvious, case.
        // This containment group had >1 matches for this `needle` so we
        // immediately handle multiple `needles` matches.
        any_multiple_needles = true;
      } else if (loc_needles == loc_needles_previous) {
        // We've recorded a match for this `needle` before. Remember that
        // `needles` are processed in increasing order across all containment
        // groups due to `v_o_loc_needles` so this simple tracking of the
        // previous `needle` works.
        any_multiple_needles = true;
      } else {
        // There was exactly 1 match for the `needle` in this containment group,
        // and we've never recorded a match for this `needle` before.
        // In that case we record that we've seen it for the next iteration.
        loc_needles_previous = loc_needles;
      }

      if (any_multiple_needles) {
        loc_first_multiple_needles = loc_needles;

        // TODO: Remove deprecated support for `multiple = "error"/"warning"`
        switch (multiple) {
        case VCTRS_MULTIPLE_all:
          break;
        case VCTRS_MULTIPLE_error:
          stop_matches_multiple(
            loc_first_multiple_needles,
            needles_arg,
            haystack_arg,
            error_call
          );
        case VCTRS_MULTIPLE_warning: {
          warn_matches_multiple(
            loc_first_multiple_needles,
            needles_arg,
            haystack_arg,
            error_call
          );
          break;
        }
        default:
          r_stop_internal("`check_multiple_needles` should have been false.");
        }

        switch (relationship) {
        case VCTRS_RELATIONSHIP_one_to_one:
          stop_matches_relationship_one_to_one(
            loc_first_multiple_needles,
            "needles",
            needles_arg,
            haystack_arg,
            error_call
          );
        case VCTRS_RELATIONSHIP_many_to_one:
          stop_matches_relationship_many_to_one(
            loc_first_multiple_needles,
            needles_arg,
            haystack_arg,
            error_call
          );
        case VCTRS_RELATIONSHIP_warn_many_to_many: {
          if (any_multiple_haystack) {
            warn_matches_relationship_many_to_many(
              loc_first_multiple_needles,
              loc_first_multiple_haystack,
              needles_arg,
              haystack_arg,
              error_call
            );
          }
          break;
        }
        default: {
          switch (multiple) {
          case VCTRS_MULTIPLE_all:
            // We are tracking if there are multiple matches, but don't throw
            // any errors or warnings on them
            break;
          // TODO: Remove deprecated support for `multiple = "error"/"warning"`
          case VCTRS_MULTIPLE_error:
            r_stop_internal("`multiple = 'error'` should have thrown by now.");
          case VCTRS_MULTIPLE_warning:
            break;
          default:
            r_stop_internal("`check_multiple_needles` should have been false.");
          }
        }
        }

        // We know there are multiple and don't need to continue checking
        check_multiple_needles = false;
      }
    }

    int loc_o_haystack = loc_first_match_o_haystack;

    switch (multiple) {
    case VCTRS_MULTIPLE_first:
    case VCTRS_MULTIPLE_last: {
      if (skip_loc_needles) {
        // We use `v_loc_needles` unconditionally below because it should always
        // be available when finding the first/last match
        r_stop_internal(
          "`skip_loc_needles` should never be `true` with `multiple = 'first'/'last'`."
        );
      }

      if (loc_haystack_overall == r_globals.na_int) {
        // Start of a new needle
        loc_haystack_overall = v_o_haystack[loc_o_haystack] - 1;
      }

      // Branching here seems to help a good bit when there are many matches
      if (multiple == VCTRS_MULTIPLE_first) {
        for (r_ssize j = 0; j < size_match; ++j) {
          const int loc_haystack = v_o_haystack[loc_o_haystack] - 1;

          if (loc_haystack_overall > loc_haystack) {
            loc_haystack_overall = loc_haystack;
          }

          ++loc_o_haystack;
        }
      } else if (multiple == VCTRS_MULTIPLE_last) {
        for (r_ssize j = 0; j < size_match; ++j) {
          const int loc_haystack = v_o_haystack[loc_o_haystack] - 1;

          if (loc_haystack_overall < loc_haystack) {
            loc_haystack_overall = loc_haystack;
          }

          ++loc_o_haystack;
        }
      } else {
        r_stop_internal(
          "`multiple` should only be 'first' or 'last' here."
        );
      }

      const bool at_end_of_all_matches = (i == n_used - 1);

      // Check if we are at the end of the vector or if the next needle location
      // is different from this one, at which point we can record the match
      // corresponding to the first/last result
      bool at_end_of_needle_matches = true;

      if (!at_end_of_all_matches) {
        const int loc_next = v_o_loc_needles[i + 1] - 1;
        const int loc_needles_next = v_loc_needles[loc_next];
        at_end_of_needle_matches = (loc_needles != loc_needles_next);
      }

      if (at_end_of_needle_matches) {
        v_out_needles[loc_out] = loc_needles + 1;
        v_out_haystack[loc_out] = loc_haystack_overall + 1;

        if (track_matches_haystack) {
          if (check_multiple_haystack) {
            // `true` if a match already existed
            any_multiple_haystack = v_detect_matches_haystack[loc_haystack_overall];

            if (any_multiple_haystack) {
              loc_first_multiple_haystack = loc_haystack_overall;

              switch (relationship) {
              case VCTRS_RELATIONSHIP_one_to_one:
                stop_matches_relationship_one_to_one(
                  loc_first_multiple_haystack,
                  "haystack",
                  needles_arg,
                  haystack_arg,
                  error_call
                );
              case VCTRS_RELATIONSHIP_one_to_many:
                stop_matches_relationship_one_to_many(
                  loc_first_multiple_haystack,
                  needles_arg,
                  haystack_arg,
                  error_call
                );
              case VCTRS_RELATIONSHIP_warn_many_to_many:
                r_stop_internal(
                  "`relationship = 'warn-many-to-many'` with "
                  "`multiple = 'first'/'last' should have resulted in "
                  "`check_multiple_haystack = false`."
                );
              default:
                r_stop_internal("`check_multiple_haystack` should have been false.");
              }
            }
          }

          // This haystack value was a match, so it isn't "remaining".
          v_detect_matches_haystack[loc_haystack_overall] = true;
        }

        ++loc_out;
        loc_haystack_overall = r_globals.na_int;
      }

      break;
    }
    case VCTRS_MULTIPLE_all:
    case VCTRS_MULTIPLE_error:
    case VCTRS_MULTIPLE_warning:
    case VCTRS_MULTIPLE_any: {
      for (r_ssize j = 0; j < size_match; ++j) {
        const int loc_haystack = v_o_haystack[loc_o_haystack] - 1;

        v_out_needles[loc_out] = loc_needles + 1;
        v_out_haystack[loc_out] = loc_haystack + 1;

        if (track_matches_haystack) {
          if (check_multiple_haystack) {
            // `true` if a match already existed
            any_multiple_haystack = v_detect_matches_haystack[loc_haystack];

            if (any_multiple_haystack) {
              loc_first_multiple_haystack = loc_haystack;

              switch (relationship) {
              case VCTRS_RELATIONSHIP_one_to_one:
                stop_matches_relationship_one_to_one(
                  loc_first_multiple_haystack,
                  "haystack",
                  needles_arg,
                  haystack_arg,
                  error_call
                );
              case VCTRS_RELATIONSHIP_one_to_many:
                stop_matches_relationship_one_to_many(
                  loc_first_multiple_haystack,
                  needles_arg,
                  haystack_arg,
                  error_call
                );
              case VCTRS_RELATIONSHIP_warn_many_to_many: {
                if (any_multiple_needles) {
                  warn_matches_relationship_many_to_many(
                    loc_first_multiple_needles,
                    loc_first_multiple_haystack,
                    needles_arg,
                    haystack_arg,
                    error_call
                  );
                }

                // We know there are multiple and don't need to continue checking
                check_multiple_haystack = false;

                // Only continue tracking if needed for `remaining`
                track_matches_haystack = retain_remaining_haystack;

                break;
              }
              default:
                r_stop_internal("`check_multiple_haystack` should have been false.");
              }
            }
          }

          // This haystack value was a match, so it isn't "remaining".
          v_detect_matches_haystack[loc_haystack] = true;
        }

        ++loc_out;
        ++loc_o_haystack;
      }

      break;
    }
    }
  }

  if (loc_out < size_out) {
    // Can happen with a `filter` and `multiple = "all"`, where it is possible
    // for potential matches coming from a different nesting container
    // to be skipped over in the `has_loc_filter_match_o_haystack` section.
    // Can also happen with `no_match = "drop"` or `incomplete = "drop"`.
    // This resize should be essentially free by setting truelength/growable.
    size_out = loc_out;

    out_needles = r_int_resize(out_needles, size_out);
    KEEP_AT(out_needles, out_needles_pi);
    v_out_needles = r_int_begin(out_needles);

    out_haystack = r_int_resize(out_haystack, size_out);
    KEEP_AT(out_haystack, out_haystack_pi);
    v_out_haystack = r_int_begin(out_haystack);
  }

  if (any_multiple_needles && any_non_equi) {
    // If we had multiple matches and we were doing a non-equi join, then
    // the needles column will be correct, but any group of multiple matches in
    // the haystack column will be ordered incorrectly within the needle group.
    // They will be ordered using the order of the original haystack values,
    // rather than by first appearance. Reordering the entire output data frame
    // orders them correctly, as within each needle group it will put the
    // haystack locations in ascending order (i.e. by first appearance).
    // This is expensive! `out` could have a huge number of matches.
    r_obj* both = KEEP(new_matches_result(out_needles, out_haystack));

    r_obj* o_haystack_appearance = KEEP(vec_order(both, chrs_asc, chrs_smallest, true, r_null));
    int* v_o_haystack_appearance = r_int_begin(o_haystack_appearance);

    // Avoid a second allocation by reusing the appearance order vector,
    // which has the same size and type as the output and we won't overwrite it
    r_obj* out_haystack_reordered = o_haystack_appearance;
    int* v_out_haystack_reordered = v_o_haystack_appearance;

    for (r_ssize i = 0; i < size_out; ++i) {
      v_out_haystack_reordered[i] = v_out_haystack[v_o_haystack_appearance[i] - 1];
    }

    out_haystack = out_haystack_reordered;
    v_out_haystack = v_out_haystack_reordered;

    FREE(2);
    KEEP_AT(out_haystack, out_haystack_pi);
  }

  if (retain_remaining_haystack) {
    r_ssize n_remaining_haystack = 0;

    switch (remaining->action) {
    case VCTRS_REMAINING_ACTION_error: {
      for (r_ssize i = 0; i < size_haystack; ++i) {
        if (!v_detect_matches_haystack[i]) {
          stop_matches_remaining(i, needles_arg, haystack_arg, error_call);
        }
      }
      break;
    }
    case VCTRS_REMAINING_ACTION_value: {
      for (r_ssize i = 0; i < size_haystack; ++i) {
        n_remaining_haystack += !v_detect_matches_haystack[i];
      }
      break;
    }
    case VCTRS_REMAINING_ACTION_drop: {
      r_stop_internal("`remaining` should never be 'drop' here.");
    }
    }

    if (n_remaining_haystack > 0) {
      // Resize to have enough room for "remaining" haystack values at the end
      r_ssize new_size_out = r_ssize_add(size_out, n_remaining_haystack);

      out_needles = r_int_resize(out_needles, new_size_out);
      KEEP_AT(out_needles, out_needles_pi);
      v_out_needles = r_int_begin(out_needles);

      out_haystack = r_int_resize(out_haystack, new_size_out);
      KEEP_AT(out_haystack, out_haystack_pi);
      v_out_haystack = r_int_begin(out_haystack);

      // Add in "remaining" values at the end of the output
      for (r_ssize i = size_out; i < new_size_out; ++i) {
        v_out_needles[i] = remaining->value;
      }

      r_ssize j = size_out;

      for (r_ssize i = 0; i < size_haystack; ++i) {
        if (!v_detect_matches_haystack[i]) {
          v_out_haystack[j] = i + 1;
          ++j;
        }
      }

      size_out = new_size_out;
    }
  }

  r_obj* out = new_matches_result(out_needles, out_haystack);

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

// Registered for testing purposes
// [[ register() ]]
r_obj* ffi_compute_nesting_container_info(r_obj* haystack, r_obj* condition) {
  r_ssize n_cols = r_length(haystack);
  enum vctrs_ops* v_ops = (enum vctrs_ops*) R_alloc(n_cols, sizeof(enum vctrs_ops));
  parse_condition(condition, n_cols, v_ops);
  const r_ssize size_haystack = vec_size(haystack);
  return compute_nesting_container_info(haystack, size_haystack, v_ops);
}

static
r_obj* compute_nesting_container_info(r_obj* haystack,
                                      r_ssize size_haystack,
                                      const enum vctrs_ops* v_ops) {
  int n_prot = 0;

  const r_ssize n_cols = r_length(haystack);

  // Outputs:
  // - `haystack` order
  // - Container id vector
  // - Number of containers as a scalar
  // - Boolean for if there are any-non-equi conditions
  r_obj* out = KEEP_N(r_alloc_list(4), &n_prot);

  bool any_non_equi = false;
  int first_non_equi = 0;
  for (r_ssize i = 0; i < n_cols; ++i) {
    const enum vctrs_ops op = v_ops[i];

    if (op != VCTRS_OPS_eq) {
      any_non_equi = true;
      first_non_equi = i;
      break;
    }
  }

  if (!any_non_equi) {
    // Container info isn't required for only `==`
    r_list_poke(out, 0, vec_order(haystack, chrs_asc, chrs_smallest, true, r_null));
    r_list_poke(out, 1, r_globals.empty_int);
    r_list_poke(out, 2, r_int(1));
    r_list_poke(out, 3, r_lgl(any_non_equi));
    FREE(n_prot);
    return out;
  }

  r_obj* info = KEEP_N(vec_order_info(
    haystack,
    chrs_asc,
    chrs_smallest,
    true,
    r_null
  ), &n_prot);

  r_obj* o_haystack = r_list_get(info, 0);
  const int* v_o_haystack = r_int_cbegin(o_haystack);

  r_obj* group_sizes = r_list_get(info, 1);
  const int* v_group_sizes = r_int_cbegin(group_sizes);
  const r_ssize n_groups = r_length(group_sizes);

  // This is the haystack we compute container ids with.
  // This is initially the whole `haystack`, but will be adjusted to contain
  // fewer columns if there are `==` conditions before the first non-equi
  // condition.
  r_keep_loc haystack_container_pi;
  r_obj* haystack_container = haystack;
  KEEP_HERE(haystack_container, &haystack_container_pi);
  ++n_prot;

  // If there are `==` conditions before the first non-equi condition,
  // we separate those columns from the haystack and compute their group sizes,
  // which are used for computing the container ids.
  bool has_outer_group_sizes = false;
  const int* v_outer_group_sizes = NULL;

  if (first_non_equi != 0) {
    // We have equality comparisons before the first non-equi comparison.
    // In this case, we can skip nested containment ordering for the equality
    // comparisons before the first non-equi comparison if we pass on the
    // group sizes of the ordered equality columns as `v_outer_group_sizes`.
    r_obj* const* v_haystack = r_list_cbegin(haystack);
    r_obj* const* v_haystack_names = r_chr_cbegin(r_names(haystack));

    // "Outer" data frame columns before the first non-equi condition
    r_obj* haystack_outer = KEEP_N(r_alloc_list(first_non_equi), &n_prot);
    r_obj* haystack_outer_names = r_alloc_character(first_non_equi);
    r_attrib_poke_names(haystack_outer, haystack_outer_names);
    r_init_data_frame(haystack_outer, size_haystack);
    for (r_ssize i = 0; i < first_non_equi; ++i) {
      r_list_poke(haystack_outer, i, v_haystack[i]);
      r_chr_poke(haystack_outer_names, i, v_haystack_names[i]);
    }

    // "Inner" data frame columns at and after the first non-equi condition
    r_obj* haystack_inner = KEEP_N(r_alloc_list(n_cols - first_non_equi), &n_prot);
    r_obj* haystack_inner_names = r_alloc_character(n_cols - first_non_equi);
    r_attrib_poke_names(haystack_inner, haystack_inner_names);
    r_init_data_frame(haystack_inner, size_haystack);
    for (r_ssize i = first_non_equi, j = 0; i < n_cols; ++i, ++j) {
      r_list_poke(haystack_inner, j, v_haystack[i]);
      r_chr_poke(haystack_inner_names, j, v_haystack_names[i]);
    }

    // Compute the order info of the outer columns, just to pluck off the
    // group sizes. These automatically create a set of groups that
    // "surround" the non-equi columns.
    r_obj* info = vec_order_info(
      haystack_outer,
      chrs_asc,
      chrs_smallest,
      true,
      r_null
    );
    r_obj* outer_group_sizes = KEEP_N(r_list_get(info, 1), &n_prot);
    v_outer_group_sizes = r_int_cbegin(outer_group_sizes);
    has_outer_group_sizes = true;

    // Inner columns become the new container haystack
    haystack_container = haystack_inner;
    KEEP_AT(haystack_container, haystack_container_pi);
  }

  r_obj* container_ids_info = KEEP_N(compute_nesting_container_ids(
    haystack_container,
    v_o_haystack,
    v_group_sizes,
    v_outer_group_sizes,
    size_haystack,
    n_groups,
    has_outer_group_sizes
  ), &n_prot);

  const int n_containers = r_as_int(r_list_get(container_ids_info, 1));

  if (n_containers == 1) {
    // If only a single container exists at this point, either there was
    // only 1 non-equi column which must already be in order, or we hit the
    // somewhat rare case of having a >1 col `haystack_container` data frame
    // that is already in nested containment order. In that case, original
    // haystack ordering is sufficient and we don't need the ids.
    r_list_poke(out, 0, o_haystack);
    r_list_poke(out, 1, r_globals.empty_int);
    r_list_poke(out, 2, r_int(1));
    r_list_poke(out, 3, r_lgl(any_non_equi));
    FREE(n_prot);
    return out;
  }

  // Otherwise, we need to recompute the haystack ordering accounting for
  // `container_ids`. One way to do this is to append `container_ids` to the
  // front of the `haystack` data frame and recompute the order, but since
  // we already have `o_haystack` and `group_sizes`, we can build a simpler
  // proxy for `haystack` that orders the exact same, but faster. So we end
  // up with a two column data frame of `container_ids` and `haystack_proxy`
  // to compute the new order for.
  r_obj* container_ids = r_list_get(container_ids_info, 0);

  r_obj* haystack_proxy = KEEP_N(r_alloc_integer(size_haystack), &n_prot);
  int* v_haystack_proxy = r_int_begin(haystack_proxy);

  r_ssize loc_o_haystack = 0;

  // Insert group number as the proxy value
  for (r_ssize i = 0; i < n_groups; ++i) {
    const r_ssize group_size = v_group_sizes[i];
    for (r_ssize j = 0; j < group_size; ++j) {
      v_haystack_proxy[v_o_haystack[loc_o_haystack] - 1] = i;
      ++loc_o_haystack;
    }
  }

  r_obj* df = KEEP_N(r_alloc_list(2), &n_prot);
  r_list_poke(df, 0, container_ids);
  r_list_poke(df, 1, haystack_proxy);

  r_obj* df_names = r_alloc_character(2);
  r_attrib_poke_names(df, df_names);
  r_chr_poke(df_names, 0, r_str("container_ids"));
  r_chr_poke(df_names, 1, r_str("haystack_proxy"));

  r_init_data_frame(df, size_haystack);

  o_haystack = KEEP_N(vec_order(
    df,
    chrs_asc,
    chrs_smallest,
    true,
    r_null
  ), &n_prot);

  r_list_poke(out, 0, o_haystack);
  r_list_poke(out, 1, container_ids);
  r_list_poke(out, 2, r_int(n_containers));
  r_list_poke(out, 3, r_lgl(any_non_equi));

  FREE(n_prot);
  return out;
}


// -----------------------------------------------------------------------------

static
r_obj* compute_nesting_container_ids(r_obj* x,
                                     const int* v_order,
                                     const int* v_group_sizes,
                                     const int* v_outer_group_sizes,
                                     r_ssize size,
                                     r_ssize n_groups,
                                     bool has_outer_group_sizes) {
  if (!is_data_frame(x)) {
    r_stop_internal("`x` must be a data frame.");
  }

  int n_prot = 0;

  const r_ssize n_cols = r_length(x);

  r_obj* out = KEEP_N(r_alloc_list(2), &n_prot);

  r_obj* container_ids = r_alloc_integer(size);
  r_list_poke(out, 0, container_ids);
  int* v_container_ids = r_int_begin(container_ids);

  r_obj* n_container_ids = r_alloc_integer(1);
  r_list_poke(out, 1, n_container_ids);
  int* p_n_container_ids = r_int_begin(n_container_ids);

  // Initialize ids to 0, which is always our first container id value.
  // This means we start with 1 container.
  r_memset(v_container_ids, 0, size * sizeof(int));
  *p_n_container_ids = 1;

  if (size == 0) {
    // Algorithm requires at least 1 row
    FREE(n_prot);
    return out;
  }

  if (n_cols == 1) {
    // If there is only 1 column, `x` is in increasing order already when
    // ordered by `v_order`.
    // If `v_outer_group_sizes` were supplied, within each group `x` will
    // be in increasing order (since the single `x` column is the one that
    // broke any ties), and that is all that is required.
    FREE(n_prot);
    return out;
  }

  struct r_dyn_array* p_prev_rows = r_new_dyn_vector(R_TYPE_integer, 10000);
  KEEP_N(p_prev_rows->shelter, &n_prot);

  struct poly_vec* p_poly_x = new_poly_vec(x, VCTRS_TYPE_dataframe);
  KEEP_N(p_poly_x->shelter, &n_prot);
  const void* v_x = p_poly_x->p_vec;

  // Will be used if `has_outer_group_sizes` is `true`
  r_ssize loc_outer_group_sizes = 0;
  r_ssize loc_next_outer_group_start = 0;

  r_ssize loc_group_start = 0;

  for (r_ssize i = 0; i < n_groups; ++i) {
    if (has_outer_group_sizes && loc_next_outer_group_start == loc_group_start) {
      // Start of a new outer group. Clear all stored previous rows.
      p_prev_rows->count = 0;
      loc_next_outer_group_start += v_outer_group_sizes[loc_outer_group_sizes];
      ++loc_outer_group_sizes;
    }

    const r_ssize group_size = v_group_sizes[i];

    const int cur_row = v_order[loc_group_start] - 1;

    int container_id = 0;
    int n_container_ids_group = p_prev_rows->count;

    for (; container_id < n_container_ids_group; ++container_id) {
      const int prev_row = r_dyn_int_get(p_prev_rows, container_id);

      if (p_nesting_container_df_compare_fully_ge_na_equal(v_x, cur_row, v_x, prev_row)) {
        // Current row is fully greater than or equal to previous row.
        // Meaning it is not a new `container_id`, and it falls in the current container.
        break;
      }
    }

    if (container_id == n_container_ids_group) {
      // New `container_id` for this outer group, which we add to the end
      r_dyn_push_back(p_prev_rows, &cur_row);
      ++n_container_ids_group;

      if (n_container_ids_group > *p_n_container_ids) {
        // `p_prev_rows` is reset for each outer group,
        // so we have to keep a running overall count
        *p_n_container_ids = n_container_ids_group;
      }
    } else {
      // Update stored row location to the current row,
      // since the current row is greater than or equal to it
      r_dyn_int_poke(p_prev_rows, container_id, cur_row);
    }

    for (r_ssize j = 0; j < group_size; ++j) {
      v_container_ids[v_order[loc_group_start] - 1] = container_id;
      ++loc_group_start;
    }
  }

  FREE(n_prot);
  return out;
}

static inline
bool p_nesting_container_df_compare_fully_ge_na_equal(const void* x,
                                                      r_ssize i,
                                                      const void* y,
                                                      r_ssize j) {
  // Checks if EVERY column of `x` is `>=` `y`.
  // Assumes original input that `x` and `y` came from is ordered, and that
  // `x` comes after `y` in terms of row location in that original input. This
  // means that the first column of `x` is always `>=` the first column of `y`,
  // so we can ignore it in the comparison.
  // Iterates backwards to (ideally) maximize chance of hitting the fastest
  // varying column.
  // All columns are integer vectors (ranks).

  const struct poly_df_data* x_data = (const struct poly_df_data*) x;
  const struct poly_df_data* y_data = (const struct poly_df_data*) y;

  const r_ssize n_col = x_data->n_col;

  const void** v_x_col_ptr = x_data->v_col_ptr;
  const void** v_y_col_ptr = y_data->v_col_ptr;

  for (r_ssize col = n_col - 1; col > 0; --col) {
    if (p_int_compare_na_equal(v_x_col_ptr[col], i, v_y_col_ptr[col], j) < 0) {
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
  // All columns are integer vectors (approximate ranks).

  const struct poly_df_data* x_data = (const struct poly_df_data*) x;
  const struct poly_df_data* y_data = (const struct poly_df_data*) y;

  const r_ssize n_col = x_data->n_col;

  const void** v_x_col_ptr = x_data->v_col_ptr;
  const void** v_y_col_ptr = y_data->v_col_ptr;

  for (r_ssize col = 0; col < n_col; ++col) {
    const enum vctrs_filter filter = v_filters[col];

    switch (filter) {
    case VCTRS_FILTER_none: {
      break;
    }
    case VCTRS_FILTER_max: {
      const int cmp = p_int_compare_na_equal(v_x_col_ptr[col], i, v_y_col_ptr[col], j);
      if (cmp != 0) {
        // Want max, new value is greater (1), signal replace (1)
        // Want max, new value is smaller (-1), signal keep (-1)
        return cmp;
      }
      break;
    }
    case VCTRS_FILTER_min: {
      const int cmp = p_int_compare_na_equal(v_x_col_ptr[col], i, v_y_col_ptr[col], j);
      if (cmp != 0) {
        // Want min, new value is smaller (-1), signal replace (1)
        // Want min, new value is larger (1), signal keep (-1)
        return -cmp;
      }
      break;
    }
    default: {
      r_stop_internal("Unknown `filter`.");
    }
    }
  }

  // All columns are equal, or no columns. No need to update anything.
  return 0;
}

static inline
bool p_matches_df_equal_na_equal(const void* x,
                                 r_ssize i,
                                 const void* y,
                                 r_ssize j,
                                 const enum vctrs_filter* v_filters) {
  // All columns are integer vectors (approximate ranks).

  const struct poly_df_data* x_data = (const struct poly_df_data*) x;
  const struct poly_df_data* y_data = (const struct poly_df_data*) y;

  const r_ssize n_col = x_data->n_col;

  const void** v_x_col_ptr = x_data->v_col_ptr;
  const void** v_y_col_ptr = y_data->v_col_ptr;

  for (r_ssize col = 0; col < n_col; ++col) {
    const enum vctrs_filter filter = v_filters[col];

    if (filter == VCTRS_FILTER_none) {
      continue;
    }

    if (!p_int_equal_na_equal(v_x_col_ptr[col], i, v_y_col_ptr[col], j)) {
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
void stop_matches_overflow(double size, struct r_lazy call) {
  r_obj* syms[3] = {
    syms_size,
    syms_call,
    NULL
  };
  r_obj* args[3] = {
    KEEP(r_dbl(size)),
    KEEP(r_lazy_eval_protect(call)),
    NULL
  };

  r_obj* ffi_call = KEEP(r_call_n(syms_stop_matches_overflow, syms, args));
  Rf_eval(ffi_call, vctrs_ns_env);

  never_reached("stop_matches_overflow");
}

static inline
void stop_matches_nothing(r_ssize i,
                          struct vctrs_arg* needles_arg,
                          struct vctrs_arg* haystack_arg,
                          struct r_lazy call) {
  r_obj* syms[5] = {
    syms_i,
    syms_needles_arg,
    syms_haystack_arg,
    syms_call,
    NULL
  };
  r_obj* args[5] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    KEEP(r_lazy_eval_protect(call)),
    NULL
  };

  r_obj* ffi_call = KEEP(r_call_n(syms_stop_matches_nothing, syms, args));
  Rf_eval(ffi_call, vctrs_ns_env);

  never_reached("stop_matches_nothing");
}

static inline
void stop_matches_remaining(r_ssize i,
                            struct vctrs_arg* needles_arg,
                            struct vctrs_arg* haystack_arg,
                            struct r_lazy call) {
  r_obj* syms[5] = {
    syms_i,
    syms_needles_arg,
    syms_haystack_arg,
    syms_call,
    NULL
  };
  r_obj* args[5] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    KEEP(r_lazy_eval_protect(call)),
    NULL
  };

  r_obj* ffi_call = KEEP(r_call_n(syms_stop_matches_remaining, syms, args));
  Rf_eval(ffi_call, vctrs_ns_env);

  never_reached("stop_matches_remaining");
}

static inline
void stop_matches_incomplete(r_ssize i,
                             struct vctrs_arg* needles_arg,
                             struct r_lazy call) {
  r_obj* syms[4] = {
    syms_i,
    syms_needles_arg,
    syms_call,
    NULL
  };
  r_obj* args[4] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(r_lazy_eval_protect(call)),
    NULL
  };

  r_obj* ffi_call = KEEP(r_call_n(syms_stop_matches_incomplete, syms, args));
  Rf_eval(ffi_call, vctrs_ns_env);

  never_reached("stop_matches_incomplete");
}

static inline
void stop_matches_multiple(r_ssize i,
                           struct vctrs_arg* needles_arg,
                           struct vctrs_arg* haystack_arg,
                           struct r_lazy call) {
  r_obj* syms[5] = {
    syms_i,
    syms_needles_arg,
    syms_haystack_arg,
    syms_call,
    NULL
  };
  r_obj* args[5] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    KEEP(r_lazy_eval_protect(call)),
    NULL
  };

  r_obj* ffi_call = KEEP(r_call_n(syms_stop_matches_multiple, syms, args));
  Rf_eval(ffi_call, vctrs_ns_env);

  never_reached("stop_matches_multiple");
}

static inline
void warn_matches_multiple(r_ssize i,
                           struct vctrs_arg* needles_arg,
                           struct vctrs_arg* haystack_arg,
                           struct r_lazy call) {
  r_obj* syms[5] = {
    syms_i,
    syms_needles_arg,
    syms_haystack_arg,
    syms_call,
    NULL
  };
  r_obj* args[5] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    KEEP(r_lazy_eval_protect(call)),
    NULL
  };

  r_obj* ffi_call = KEEP(r_call_n(syms_warn_matches_multiple, syms, args));
  Rf_eval(ffi_call, vctrs_ns_env);
  FREE(5);
}

static inline
void stop_matches_relationship_one_to_one(r_ssize i,
                                          const char* which,
                                          struct vctrs_arg* needles_arg,
                                          struct vctrs_arg* haystack_arg,
                                          struct r_lazy call) {
  r_obj* syms[6] = {
    syms_i,
    syms_which,
    syms_needles_arg,
    syms_haystack_arg,
    syms_call,
    NULL
  };
  r_obj* args[6] = {
    KEEP(r_int((int)i + 1)),
    KEEP(r_chr(which)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    KEEP(r_lazy_eval_protect(call)),
    NULL
  };

  r_obj* ffi_call = KEEP(r_call_n(syms_stop_matches_relationship_one_to_one, syms, args));
  Rf_eval(ffi_call, vctrs_ns_env);

  never_reached("stop_matches_relationship_one_to_one");
}

static inline
void stop_matches_relationship_one_to_many(r_ssize i,
                                           struct vctrs_arg* needles_arg,
                                           struct vctrs_arg* haystack_arg,
                                           struct r_lazy call) {
  r_obj* syms[5] = {
   syms_i,
   syms_needles_arg,
   syms_haystack_arg,
   syms_call,
   NULL
  };
  r_obj* args[5] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    KEEP(r_lazy_eval_protect(call)),
    NULL
  };

  r_obj* ffi_call = KEEP(r_call_n(syms_stop_matches_relationship_one_to_many, syms, args));
  Rf_eval(ffi_call, vctrs_ns_env);

  never_reached("stop_matches_relationship_one_to_many");
}

static inline
void stop_matches_relationship_many_to_one(r_ssize i,
                                           struct vctrs_arg* needles_arg,
                                           struct vctrs_arg* haystack_arg,
                                           struct r_lazy call) {
  r_obj* syms[5] = {
    syms_i,
    syms_needles_arg,
    syms_haystack_arg,
    syms_call,
    NULL
  };
  r_obj* args[5] = {
    KEEP(r_int((int)i + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    KEEP(r_lazy_eval_protect(call)),
    NULL
  };

  r_obj* ffi_call = KEEP(r_call_n(syms_stop_matches_relationship_many_to_one, syms, args));
  Rf_eval(ffi_call, vctrs_ns_env);

  never_reached("stop_matches_relationship_many_to_one");
}

static inline
void warn_matches_relationship_many_to_many(r_ssize i,
                                            r_ssize j,
                                            struct vctrs_arg* needles_arg,
                                            struct vctrs_arg* haystack_arg,
                                            struct r_lazy call) {
  r_obj* syms[6] = {
   syms_i,
   syms_j,
   syms_needles_arg,
   syms_haystack_arg,
   syms_call,
   NULL
  };
  r_obj* args[6] = {
    KEEP(r_int((int)i + 1)),
    KEEP(r_int((int)j + 1)),
    KEEP(vctrs_arg(needles_arg)),
    KEEP(vctrs_arg(haystack_arg)),
    KEEP(r_lazy_eval_protect(call)),
    NULL
  };

  r_obj* ffi_call = KEEP(r_call_n(syms_warn_matches_relationship_many_to_many, syms, args));
  Rf_eval(ffi_call, vctrs_ns_env);
  FREE(6);
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
