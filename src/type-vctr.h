#ifndef VCTRS_TYPE_VCTR_H
#define VCTRS_TYPE_VCTR_H

#include <rlang.h>

r_obj* new_vctr(r_obj* data,
                r_obj* cls,
                r_obj* inherit_base_type,
                r_obj* attributes);

#endif
