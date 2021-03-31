#ifndef VCTRS_RANK_DECL_H
#define VCTRS_RANK_DECL_H

enum ties {
  TIES_min,
  TIES_max,
  TIES_sequential,
  TIES_dense
};
static inline enum ties parse_ties(sexp* ties);

static inline bool r_lgl_any(sexp* x);

static sexp* vec_rank(sexp* x,
                      enum ties ties_type,
                      bool na_propagate,
                      sexp* na_value,
                      bool nan_distinct,
                      sexp* chr_transform);

static void vec_rank_min(const int* v_order,
                         const int* v_group_sizes,
                         r_ssize n_groups,
                         int* v_rank);

static void vec_rank_max(const int* v_order,
                         const int* v_group_sizes,
                         r_ssize n_groups,
                         int* v_rank);

static void vec_rank_sequential(const int* v_order,
                                const int* v_group_sizes,
                                r_ssize n_groups,
                                int* v_rank);

static void vec_rank_dense(const int* v_order,
                           const int* v_group_sizes,
                           r_ssize n_groups,
                           int* v_rank);

#endif
