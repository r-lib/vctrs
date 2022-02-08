#include "vctrs.h"

enum vctrs_dbl_class dbl_classify(double x) {
  if (!isnan(x)) {
    return vctrs_dbl_number;
  }

  union vctrs_dbl_indicator indicator;
  indicator.value = x;

  if (indicator.key[vctrs_indicator_pos] == 1954) {
    return vctrs_dbl_missing;
  } else {
    return vctrs_dbl_nan;
  }
}
