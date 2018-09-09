#ifndef VCTRS_DIM_H_
#define VCTRS_DIM_H_

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <stdbool.h>

R_len_t vec_length(SEXP x);

bool is_data_frame(SEXP x);

#endif

