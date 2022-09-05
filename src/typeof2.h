#ifndef VCTRS_TYPEOF2_H
#define VCTRS_TYPEOF2_H

#include "vctrs-core.h"


enum vctrs_type2 vec_typeof2_impl(enum vctrs_type type_x,
                                  enum vctrs_type type_y,
                                  int* left);

enum vctrs_type2_s3 vec_typeof2_s3_impl(r_obj* x, r_obj* y, enum vctrs_type type_x, enum vctrs_type type_y, int* left);

enum vctrs_type2 vec_typeof2(r_obj* x, r_obj* y);
const char* vctrs_type2_as_str(enum vctrs_type2 type);


#endif
