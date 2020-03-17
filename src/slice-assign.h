#ifndef VCTRS_SLICE_ASSIGN_H
#define VCTRS_SLICE_ASSIGN_H

struct vec_assign_opts {
  bool assign_names;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* value_arg;
};

static inline struct vec_assign_opts new_vec_assign_opts(bool assign_names,
                                                         struct vctrs_arg* x_arg,
                                                         struct vctrs_arg* value_arg) {
  return (struct vec_assign_opts) {
    .assign_names = assign_names,
    .x_arg = x_arg,
    .value_arg = value_arg
  };
}

SEXP vec_assign_opts(SEXP x, SEXP index, SEXP value,
                     const struct vec_assign_opts* opts);

SEXP vec_proxy_assign_opts(SEXP proxy, SEXP index, SEXP value,
                           const struct vec_assign_opts* opts);

SEXP chr_assign(SEXP out, SEXP index, SEXP value);
SEXP list_assign(SEXP out, SEXP index, SEXP value);
SEXP df_assign(SEXP x, SEXP index, SEXP value,
               const struct vec_assign_opts* opts);

#endif
