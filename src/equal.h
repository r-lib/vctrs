#ifndef VCTRS_EQUAL_H_
#define VCTRS_EQUAL_H_

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <stdbool.h>

bool equal_object(SEXP x, SEXP y);
bool equal_scalar(SEXP x, int i, SEXP y, int j);

#endif
