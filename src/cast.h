#ifndef VCTRS_CAST_H
#define VCTRS_CAST_H


SEXP vec_cast_params(SEXP x,
                     SEXP to,
                     struct vctrs_arg* x_arg,
                     struct vctrs_arg* to_arg,
                     bool df_fallback);

SEXP vec_cast_dispatch(SEXP x,
                       SEXP to,
                       enum vctrs_type x_type,
                       enum vctrs_type to_type,
                       struct vctrs_arg* x_arg,
                       struct vctrs_arg* to_arg,
                       bool* lossy);

// Defined in type-data-frame.c
SEXP df_cast(SEXP x, SEXP to, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg);

// Defined in cast-bare.c
SEXP int_as_double(SEXP x, bool* lossy);
SEXP lgl_as_double(SEXP x, bool* lossy);
SEXP dbl_as_integer(SEXP x, bool* lossy);
SEXP lgl_as_integer(SEXP x, bool* lossy);
SEXP chr_as_logical(SEXP x, bool* lossy);
SEXP dbl_as_logical(SEXP x, bool* lossy);
SEXP int_as_logical(SEXP x, bool* lossy);


#endif
