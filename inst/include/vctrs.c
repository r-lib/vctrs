#include "vctrs.h"

R_len_t (*vec_size)(SEXP) = NULL;
SEXP (*vec_proxy)(SEXP) = NULL;
SEXP (*vec_restore)(SEXP, SEXP, SEXP) = NULL;
SEXP (*vec_init)(SEXP, R_len_t) = NULL;
SEXP (*vec_assign_impl)(SEXP, SEXP, SEXP, bool) = NULL;
SEXP (*vec_slice_impl)(SEXP, SEXP) = NULL;
SEXP (*vec_names)(SEXP) = NULL;
SEXP (*vec_set_names)(SEXP, SEXP) = NULL;

SEXP (*vctrs_cast)(SEXP, SEXP, SEXP, SEXP) = NULL;
SEXP (*compact_seq)(R_len_t, R_len_t, bool) = NULL;
SEXP (*init_compact_seq)(int*, R_len_t, R_len_t, bool) = NULL;

void vctrs_init_api() {
  vec_size = (R_len_t (*)(SEXP)) R_GetCCallable("vctrs", "vec_size");
  vec_proxy = (SEXP (*)(SEXP)) R_GetCCallable("vctrs", "vec_proxy");
  vec_restore = (SEXP (*)(SEXP, SEXP, SEXP)) R_GetCCallable("vctrs", "vec_restore");
  vec_init = (SEXP (*)(SEXP, R_len_t)) R_GetCCallable("vctrs", "vec_init");
  vec_assign_impl = (SEXP (*)(SEXP, SEXP, SEXP, bool)) R_GetCCallable("vctrs", "vec_assign_impl");
  vec_slice_impl = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("vctrs", "vec_slice_impl");
  vec_names = (SEXP (*)(SEXP)) R_GetCCallable("vctrs", "vec_names");
  vec_set_names = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("vctrs", "vec_set_names");

  vctrs_cast = (SEXP (*)(SEXP, SEXP, SEXP, SEXP)) R_GetCCallable("vctrs", "vctrs_cast");
  compact_seq = (SEXP (*)(R_len_t, R_len_t, bool)) R_GetCCallable("vctrs", "compact_seq");
  init_compact_seq = (SEXP (*)(int*, R_len_t, R_len_t, bool)) R_GetCCallable("vctrs", "init_compact_seq");
}
