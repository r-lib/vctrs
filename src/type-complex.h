#ifndef VCTRS_TYPE_COMPLEX_H
#define VCTRS_TYPE_COMPLEX_H

#include "vctrs.h"

/*
 * Normalises a complex value so that if one side is missing, both are. This
 * ensures that all missing complex values are grouped together, no matter
 * what type of missingness it is. NA and NaN can still be separated by
 * `nan_distinct`, resulting in 4 different combinations of missingness. These
 * 4 groups of missingness will still all be grouped together, either before
 * or after any non-missing values have appeared.
 * See issue #1403 for more information.
 */
static inline
r_complex cpl_normalise_missing(r_complex x) {
  const double na = r_globals.na_dbl;
  const double nan = R_NaN;

  const enum vctrs_dbl r_type = dbl_classify(x.r);
  const enum vctrs_dbl i_type = dbl_classify(x.i);

  switch (r_type) {
  case VCTRS_DBL_number:
    switch (i_type) {
    case VCTRS_DBL_number: return x;
    case VCTRS_DBL_missing: return (r_complex) { .r = na, .i = na};
    case VCTRS_DBL_nan: return (r_complex) { .r = nan, .i = nan};
    }
  case VCTRS_DBL_missing:
    switch (i_type) {
    case VCTRS_DBL_number: return (r_complex) { .r = na, .i = na};
    case VCTRS_DBL_missing: return x;
    case VCTRS_DBL_nan: return x;
    }
  case VCTRS_DBL_nan:
    switch (i_type) {
    case VCTRS_DBL_number: return (r_complex) { .r = nan, .i = nan};
    case VCTRS_DBL_missing: return x;
    case VCTRS_DBL_nan: return x;
    }
  }

  never_reached("cpl_normalise_missing");
}


#endif
