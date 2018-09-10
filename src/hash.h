#ifndef VCTRS_HASH_H_
#define VCTRS_HASH_H_

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <stdint.h>
#include <stdbool.h>

int32_t hash_object(SEXP x);
int32_t hash_scalar(SEXP x, R_len_t i);

bool equal_object(SEXP x, SEXP y);
bool equal_scalar(SEXP x, int i, SEXP y, int j);

#endif
