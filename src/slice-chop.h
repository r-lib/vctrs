#ifndef VCTRS_SLICE_CHOP_H
#define VCTRS_SLICE_CHOP_H

#include "vctrs-core.h"

r_obj* vec_chop(r_obj* x, r_obj* indices, r_obj* sizes);
r_obj* vec_chop_unsafe(r_obj*, r_obj* indices, r_obj* sizes);

r_obj* list_as_locations(r_obj* indices, r_ssize n, r_obj* names);


#endif
