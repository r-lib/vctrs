#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdint.h>

int32_t hash_object(SEXP x);
int32_t hash_scalar(SEXP x, R_len_t i);

bool equal_scalar(SEXP x, int i, SEXP y, int j);

R_len_t vec_length(SEXP x);
