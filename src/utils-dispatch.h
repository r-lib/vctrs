#ifndef VCTRS_UTILS_DISPATCH_H
#define VCTRS_UTILS_DISPATCH_H

#include "vctrs-core.h"

enum vctrs_class_type {
  vctrs_class_list,
  vctrs_class_data_frame,
  vctrs_class_bare_data_frame,
  vctrs_class_bare_tibble,
  vctrs_class_bare_factor,
  vctrs_class_bare_ordered,
  vctrs_class_bare_date,
  vctrs_class_bare_posixct,
  vctrs_class_bare_posixlt,
  vctrs_class_unknown,
  vctrs_class_none
};

enum vctrs_class_type class_type(r_obj* x);

bool vec_is_partial(r_obj* x);


#endif
