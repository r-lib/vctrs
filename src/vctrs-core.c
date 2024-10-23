#include "vctrs.h"

enum vctrs_dbl dbl_classify(double x) {
  if (!isnan(x)) {
    return VCTRS_DBL_number;
  }

  union vctrs_dbl_indicator indicator;
  indicator.value = x;

  if (indicator.key[vctrs_indicator_pos] == 1954) {
    return VCTRS_DBL_missing;
  } else {
    return VCTRS_DBL_nan;
  }
}
