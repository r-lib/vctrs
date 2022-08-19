#ifndef VCTRS_CAST_BARE_H
#define VCTRS_CAST_BARE_H

#include "vctrs-core.h"

r_obj* int_as_double(r_obj* x, bool* lossy);
r_obj* lgl_as_double(r_obj* x, bool* lossy);
r_obj* dbl_as_integer(r_obj* x, bool* lossy);
r_obj* lgl_as_integer(r_obj* x, bool* lossy);
r_obj* chr_as_logical(r_obj* x, bool* lossy);
r_obj* dbl_as_logical(r_obj* x, bool* lossy);
r_obj* int_as_logical(r_obj* x, bool* lossy);

#endif
