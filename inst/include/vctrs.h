#ifndef VCTRS_H
#define VCTRS_H

#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdbool.h>

// Now named `obj_is_vector()` in vctrs, but we retain this for backwards
// compatibility for dplyr in particular
extern bool (*vec_is_vector)(SEXP);
extern R_len_t (*short_vec_size)(SEXP);
extern SEXP (*short_vec_recycle)(SEXP, R_len_t);

void vctrs_init_api(void);

#endif
