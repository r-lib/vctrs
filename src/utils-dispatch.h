#ifndef VCTRS_UTILS_DISPATCH_H
#define VCTRS_UTILS_DISPATCH_H

#include "vctrs-core.h"

enum vctrs_class_type {
  VCTRS_CLASS_list,
  VCTRS_CLASS_data_frame,
  VCTRS_CLASS_bare_asis,
  VCTRS_CLASS_bare_data_frame,
  VCTRS_CLASS_bare_tibble,
  VCTRS_CLASS_bare_factor,
  VCTRS_CLASS_bare_ordered,
  VCTRS_CLASS_bare_date,
  VCTRS_CLASS_bare_posixct,
  VCTRS_CLASS_bare_posixlt,
  VCTRS_CLASS_unknown,
  VCTRS_CLASS_none
};

enum vctrs_class_type class_type(r_obj* x);

static inline
bool class_type_is_data_frame(enum vctrs_class_type type) {
  switch (type) {
  case VCTRS_CLASS_data_frame:
  case VCTRS_CLASS_bare_data_frame:
  case VCTRS_CLASS_bare_tibble:
    return true;
  default:
    return false;
  }
}

bool vec_is_partial(r_obj* x);


#endif
