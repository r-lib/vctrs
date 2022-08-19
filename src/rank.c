#include "vctrs.h"

enum ties {
  TIES_min,
  TIES_max,
  TIES_sequential,
  TIES_dense
};

enum incomplete {
  INCOMPLETE_rank,
  INCOMPLETE_na
};

#include "decl/rank-decl.h"

// [[ register() ]]
r_obj* vctrs_rank(r_obj* x,
                  r_obj* ties,
                  r_obj* incomplete,
                  r_obj* direction,
                  r_obj* na_value,
                  r_obj* nan_distinct,
                  r_obj* chr_proxy_collate) {
  const enum ties c_ties = parse_ties(ties);
  const enum incomplete c_incomplete = parse_incomplete(incomplete);
  const bool c_nan_distinct = r_as_bool(nan_distinct);

  return vec_rank(
    x,
    c_ties,
    c_incomplete,
    direction,
    na_value,
    c_nan_distinct,
    chr_proxy_collate
  );
}

static
r_obj* vec_rank(r_obj* x,
                enum ties ties_type,
                enum incomplete incomplete_type,
                r_obj* direction,
                r_obj* na_value,
                bool nan_distinct,
                r_obj* chr_proxy_collate) {
  r_ssize size = vec_size(x);

  r_keep_loc pi_x;
  KEEP_HERE(x, &pi_x);

  r_obj* complete = r_null;
  r_keep_loc pi_complete;
  KEEP_HERE(complete, &pi_complete);
  int* v_complete = NULL;

  r_ssize rank_size = size;
  bool rank_incomplete_with_na = (incomplete_type == INCOMPLETE_na);

  if (rank_incomplete_with_na) {
    // Slice out complete values of `x` to rank.
    // Retain the logical vector for constructing `out`.
    complete = vec_detect_complete(x);
    KEEP_AT(complete, pi_complete);
    v_complete = r_lgl_begin(complete);

    bool all_complete = r_lgl_all(complete);

    if (all_complete) {
      // No incomplete values to rank
      rank_incomplete_with_na = false;
    } else {
      x = vec_slice(x, complete);
      KEEP_AT(x, pi_x);

      rank_size = vec_size(x);
    }
  }

  r_obj* rank = KEEP(r_alloc_integer(rank_size));
  int* v_rank = r_int_begin(rank);

  const bool chr_ordered = true;

  r_obj* info = KEEP(vec_order_info(x, direction, na_value, nan_distinct, chr_proxy_collate, chr_ordered));

  r_obj* order = r_list_get(info, 0);
  const int* v_order = r_int_cbegin(order);

  r_obj* group_sizes = r_list_get(info, 1);
  const int* v_group_sizes = r_int_cbegin(group_sizes);
  r_ssize n_groups = r_length(group_sizes);

  switch (ties_type) {
  case TIES_min: vec_rank_min(v_order, v_group_sizes, n_groups, v_rank); break;
  case TIES_max: vec_rank_max(v_order, v_group_sizes, n_groups, v_rank); break;
  case TIES_sequential: vec_rank_sequential(v_order, v_group_sizes, n_groups, v_rank); break;
  case TIES_dense: vec_rank_dense(v_order, v_group_sizes, n_groups, v_rank); break;
  }

  r_obj* out = r_null;

  if (rank_incomplete_with_na) {
    out = KEEP(r_alloc_integer(size));
    int* v_out = r_int_begin(out);
    r_ssize j = 0;

    for (r_ssize i = 0; i < size; ++i) {
      v_out[i] = v_complete[i] ? v_rank[j++] : r_globals.na_int;
    }

    FREE(1);
  } else {
    out = rank;
  }

  FREE(4);
  return out;
}

// -----------------------------------------------------------------------------

static
void vec_rank_min(const int* v_order,
                  const int* v_group_sizes,
                  r_ssize n_groups,
                  int* v_rank) {
  r_ssize k = 0;
  r_ssize rank = 1;

  for (r_ssize i = 0; i < n_groups; ++i) {
    const r_ssize group_size = v_group_sizes[i];

    for (r_ssize j = 0; j < group_size; ++j) {
      r_ssize loc = v_order[k] - 1;
      v_rank[loc] = rank;
      ++k;
    }

    rank += group_size;
  }
}

static
void vec_rank_max(const int* v_order,
                  const int* v_group_sizes,
                  r_ssize n_groups,
                  int* v_rank) {
  r_ssize k = 0;
  r_ssize rank = 0;

  for (r_ssize i = 0; i < n_groups; ++i) {
    const r_ssize group_size = v_group_sizes[i];
    rank += group_size;

    for (r_ssize j = 0; j < group_size; ++j) {
      r_ssize loc = v_order[k] - 1;
      v_rank[loc] = rank;
      ++k;
    }
  }
}

static
void vec_rank_sequential(const int* v_order,
                         const int* v_group_sizes,
                         r_ssize n_groups,
                         int* v_rank) {
  r_ssize k = 0;
  r_ssize rank = 1;

  for (r_ssize i = 0; i < n_groups; ++i) {
    const r_ssize group_size = v_group_sizes[i];

    for (r_ssize j = 0; j < group_size; ++j) {
      r_ssize loc = v_order[k] - 1;
      v_rank[loc] = rank;
      ++k;
      ++rank;
    }
  }
}

static
void vec_rank_dense(const int* v_order,
                    const int* v_group_sizes,
                    r_ssize n_groups,
                    int* v_rank) {
  r_ssize k = 0;
  r_ssize rank = 1;

  for (r_ssize i = 0; i < n_groups; ++i) {
    const r_ssize group_size = v_group_sizes[i];

    for (r_ssize j = 0; j < group_size; ++j) {
      r_ssize loc = v_order[k] - 1;
      v_rank[loc] = rank;
      ++k;
    }

    ++rank;
  }
}

// -----------------------------------------------------------------------------

static inline
enum ties parse_ties(r_obj* ties) {
  if (!r_is_string(ties)) {
    r_stop_internal("`ties` must be a string.");
  }

  const char* c_ties = r_chr_get_c_string(ties, 0);

  if (!strcmp(c_ties, "min")) return TIES_min;
  if (!strcmp(c_ties, "max")) return TIES_max;
  if (!strcmp(c_ties, "sequential")) return TIES_sequential;
  if (!strcmp(c_ties, "dense")) return TIES_dense;

  r_stop_internal(
    "`ties` must be one of: \"min\", \"max\", \"sequential\", or \"dense\"."
  );
}

// -----------------------------------------------------------------------------

static inline
enum incomplete parse_incomplete(r_obj* incomplete) {
  if (!r_is_string(incomplete)) {
    r_stop_internal("`incomplete` must be a string.");
  }

  const char* c_incomplete = r_chr_get_c_string(incomplete, 0);

  if (!strcmp(c_incomplete, "rank")) return INCOMPLETE_rank;
  if (!strcmp(c_incomplete, "na")) return INCOMPLETE_na;

  r_stop_internal(
    "`incomplete` must be either \"rank\" or \"na\"."
  );
}

// -----------------------------------------------------------------------------

// Treats missing values as `true`
static inline
bool r_lgl_all(r_obj* x) {
  if (r_typeof(x) != R_TYPE_logical) {
    r_stop_internal("`x` must be a logical vector.");
  }

  const int* v_x = r_lgl_cbegin(x);
  r_ssize size = r_length(x);

  for (r_ssize i = 0; i < size; ++i) {
    if (!v_x[i]) {
      return false;
    }
  }

  return true;
}
