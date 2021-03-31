#ifndef VCTRS_RANK_DECL_H
#define VCTRS_RANK_DECL_H

enum ties {
  TIES_min,
  TIES_max,
  TIES_sequential,
  TIES_dense
};
static inline enum ties parse_ties(sexp* ties);

static sexp* r_lgl_negate(sexp* x, bool na_propagate);
static bool r_lgl_any(sexp* x);

static sexp* vec_rank(sexp* x,
                      enum ties ties_type,
                      bool na_propagate,
                      sexp* na_value,
                      bool nan_distinct,
                      sexp* chr_transform);

static void vec_rank_min(const int* v_order,
                         const int* v_group_sizes,
                         const int* v_locs,
                         r_ssize size,
                         r_ssize n_groups,
                         bool na_propagate,
                         int* v_out);

static void vec_rank_max(const int* v_order,
                         const int* v_group_sizes,
                         const int* v_locs,
                         r_ssize size,
                         r_ssize n_groups,
                         bool na_propagate,
                         int* v_out);

static void vec_rank_sequential(const int* v_order,
                                const int* v_group_sizes,
                                const int* v_locs,
                                r_ssize size,
                                r_ssize n_groups,
                                bool na_propagate,
                                int* v_out);

static void vec_rank_dense(const int* v_order,
                           const int* v_group_sizes,
                           const int* v_locs,
                           r_ssize size,
                           r_ssize n_groups,
                           bool na_propagate,
                           int* v_out);

#endif
