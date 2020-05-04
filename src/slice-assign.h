#ifndef VCTRS_SLICE_ASSIGN_H
#define VCTRS_SLICE_ASSIGN_H

// Ownership is recursive
enum vctrs_ownership {
  vctrs_ownership_shared,
  vctrs_ownership_total
};

struct vec_assign_opts {
  bool assign_names;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* value_arg;
};

SEXP vec_assign_opts(SEXP x, SEXP index, SEXP value,
                     const struct vec_assign_opts* opts);

SEXP vec_proxy_assign_opts(SEXP proxy, SEXP index, SEXP value,
                           const enum vctrs_ownership ownership,
                           const struct vec_assign_opts* opts);

SEXP chr_assign(SEXP out, SEXP index, SEXP value, const enum vctrs_ownership ownership);
SEXP list_assign(SEXP out, SEXP index, SEXP value, const enum vctrs_ownership ownership);
SEXP df_assign(SEXP x, SEXP index, SEXP value,
               const enum vctrs_ownership ownership,
               const struct vec_assign_opts* opts);

SEXP vec_assign_shaped(SEXP proxy, SEXP index, SEXP value,
                       const enum vctrs_ownership ownership,
                       const struct vec_assign_opts* opts);

#endif
