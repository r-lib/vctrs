#include <rlang.h>
#include "vctrs.h"
#include "equal.h"
#include "order-radix.h"
#include "decl/rank-decl.h"

// [[ register() ]]
sexp* vctrs_rank(sexp* x,
                 sexp* ties,
                 sexp* na_propagate,
                 sexp* na_value,
                 sexp* nan_distinct,
                 sexp* chr_transform) {
  const enum ties c_ties = parse_ties(ties);
  const bool c_na_propagate = r_bool_as_int(na_propagate);
  const bool c_nan_distinct = r_bool_as_int(nan_distinct);

  return vec_rank(
    x,
    c_ties,
    c_na_propagate,
    na_value,
    c_nan_distinct,
    chr_transform
  );
}

static
sexp* vec_rank(sexp* x,
               enum ties ties_type,
               bool na_propagate,
               sexp* na_value,
               bool nan_distinct,
               sexp* chr_transform) {
  r_ssize size = vec_size(x);

  sexp* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_deref(out);

  r_keep_t pi_x;
  KEEP_HERE(x, &pi_x);

  sexp* locs = R_NilValue;
  r_keep_t pi_locs;
  KEEP_HERE(locs, &pi_locs);
  const int* v_locs = NULL;

  if (na_propagate) {
    // Locate non-missing values and slice them out of `x`,
    // retaining their locations in `locs` for later placement in `out`
    locs = vec_equal_na(x);
    KEEP_AT(locs, pi_locs);

    bool any_missing = r_lgl_any(locs);
    if (!any_missing) {
      // Skipping random access into `v_locs` in `vec_rank_*()` when not
      // required greatly improves performance
      na_propagate = false;
      goto skip_propagate;
    }

    locs = r_lgl_negate(locs, false);
    KEEP_AT(locs, pi_locs);
    locs = r_lgl_which(locs, false);
    KEEP_AT(locs, pi_locs);

    v_locs = r_int_deref_const(locs);

    x = vec_slice_impl(x, locs);
    KEEP_AT(x, pi_x);

    // Initialize to `NA` to "propagate" it
    r_int_fill(out, r_globals.na_int, size);
  } else {
    skip_propagate:;
  }

  sexp* direction = KEEP(r_chr("asc"));
  sexp* info = KEEP(vec_order_info(x, direction, na_value, nan_distinct, chr_transform));

  sexp* order = r_list_get(info, 0);
  const int* v_order = r_int_deref_const(order);

  sexp* group_sizes = r_list_get(info, 1);
  const int* v_group_sizes = r_int_deref_const(group_sizes);
  r_ssize n_groups = r_length(group_sizes);

  switch (ties_type) {
  case TIES_min: vec_rank_min(v_order, v_group_sizes, v_locs, size, n_groups, na_propagate, v_out); break;
  case TIES_max: vec_rank_max(v_order, v_group_sizes, v_locs, size, n_groups, na_propagate, v_out); break;
  case TIES_sequential: vec_rank_sequential(v_order, v_group_sizes, v_locs, size, n_groups, na_propagate, v_out); break;
  case TIES_dense: vec_rank_dense(v_order, v_group_sizes, v_locs, size, n_groups, na_propagate, v_out); break;
  }

  FREE(5);
  return out;
}

// -----------------------------------------------------------------------------

static
void vec_rank_min(const int* v_order,
                  const int* v_group_sizes,
                  const int* v_locs,
                  r_ssize size,
                  r_ssize n_groups,
                  bool na_propagate,
                  int* v_out) {
  r_ssize k = 0;
  r_ssize rank = 1;

  for (r_ssize i = 0; i < n_groups; ++i) {
    const r_ssize group_size = v_group_sizes[i];

    for (r_ssize j = 0; j < group_size; ++j) {
      r_ssize loc = v_order[k] - 1;
      if (na_propagate) {
        loc = v_locs[loc] - 1;
      }
      v_out[loc] = rank;
      ++k;
    }

    rank += group_size;
  }
}

static
void vec_rank_max(const int* v_order,
                  const int* v_group_sizes,
                  const int* v_locs,
                  r_ssize size,
                  r_ssize n_groups,
                  bool na_propagate,
                  int* v_out) {
  r_ssize k = 0;
  r_ssize rank = 0;

  for (r_ssize i = 0; i < n_groups; ++i) {
    const r_ssize group_size = v_group_sizes[i];
    rank += group_size;

    for (r_ssize j = 0; j < group_size; ++j) {
      r_ssize loc = v_order[k] - 1;
      if (na_propagate) {
        loc = v_locs[loc] - 1;
      }
      v_out[loc] = rank;
      ++k;
    }
  }
}

static
void vec_rank_sequential(const int* v_order,
                         const int* v_group_sizes,
                         const int* v_locs,
                         r_ssize size,
                         r_ssize n_groups,
                         bool na_propagate,
                         int* v_out) {
  r_ssize k = 0;
  r_ssize rank = 1;

  for (r_ssize i = 0; i < n_groups; ++i) {
    const r_ssize group_size = v_group_sizes[i];

    for (r_ssize j = 0; j < group_size; ++j) {
      r_ssize loc = v_order[k] - 1;
      if (na_propagate) {
        loc = v_locs[loc] - 1;
      }
      v_out[loc] = rank;
      ++k;
      ++rank;
    }
  }
}

static
void vec_rank_dense(const int* v_order,
                    const int* v_group_sizes,
                    const int* v_locs,
                    r_ssize size,
                    r_ssize n_groups,
                    bool na_propagate,
                    int* v_out) {
  r_ssize k = 0;
  r_ssize rank = 1;

  for (r_ssize i = 0; i < n_groups; ++i) {
    const r_ssize group_size = v_group_sizes[i];

    for (r_ssize j = 0; j < group_size; ++j) {
      r_ssize loc = v_order[k] - 1;
      if (na_propagate) {
        loc = v_locs[loc] - 1;
      }
      v_out[loc] = rank;
      ++k;
    }

    ++rank;
  }
}

// -----------------------------------------------------------------------------

static inline
enum ties parse_ties(sexp* ties) {
  if (r_typeof(ties) != R_TYPE_character) {
    r_abort("`ties` must be a character vector.");
  }
  if (r_length(ties) < 1) {
    r_abort("`ties` must be at least length 1.");
  }

  const char* c_ties = r_chr_get_c_string(ties, 0);

  if (!strcmp(c_ties, "min")) return TIES_min;
  if (!strcmp(c_ties, "max")) return TIES_max;
  if (!strcmp(c_ties, "sequential")) return TIES_sequential;
  if (!strcmp(c_ties, "dense")) return TIES_dense;

  r_abort("`ties` must be one of: 'min', 'max', 'sequential', or 'dense'.");
}

// -----------------------------------------------------------------------------

static
sexp* r_lgl_negate(sexp* x, bool na_propagate) {
  if (r_typeof(x) != R_TYPE_logical) {
    r_abort("Internal error: Expected logical vector in `r_lgl_negate()`.");
  }

  const int* v_x = r_lgl_deref_const(x);
  r_ssize size = r_length(x);

  sexp* out = KEEP(r_new_logical(size));
  int* v_out = r_lgl_deref(out);

  if (na_propagate) {
    for (r_ssize i = 0; i < size; ++i) {
      const int elt = v_x[i];
      v_out[i] = (elt == r_globals.na_lgl) ? r_globals.na_lgl : !elt;
    }
  } else {
    for (r_ssize i = 0; i < size; ++i) {
      v_out[i] = !v_x[i];
    }
  }

  FREE(1);
  return out;
}

// Treats missing values as `true`
static
bool r_lgl_any(sexp* x) {
  if (r_typeof(x) != R_TYPE_logical) {
    r_abort("Internal error: Expected logical vector in `r_lgl_any()`.");
  }

  const int* v_x = r_lgl_deref_const(x);
  r_ssize size = r_length(x);

  for (r_ssize i = 0; i < size; ++i) {
    if (v_x[i]) {
      return true;
    }
  }

  return false;
}
