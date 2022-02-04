#ifndef VCTRS_SLICE_H
#define VCTRS_SLICE_H

struct vec_slice_opts {
  struct vctrs_arg* x_arg;
  struct vctrs_arg* i_arg;
  struct r_lazy call;
};

extern SEXP syms_vec_slice_dispatch_integer64;
extern SEXP fns_vec_slice_dispatch_integer64;

enum vctrs_materialize {
  VCTRS_MATERIALIZE_false = 0,
  VCTRS_MATERIALIZE_true
};

SEXP vec_slice_base(enum vctrs_type type,
                    SEXP x,
                    SEXP subscript,
                    enum vctrs_materialize materialize);

SEXP slice_names(SEXP names, SEXP subscript);
SEXP slice_rownames(SEXP names, SEXP subscript);
SEXP vec_slice_fallback(SEXP x, SEXP subscript);

bool vec_is_restored(SEXP x, SEXP to);


#endif
