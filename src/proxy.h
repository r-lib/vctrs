#ifndef VCTRS_PROXY_H
#define VCTRS_PROXY_H

#include "vctrs-core.h"

r_obj* vec_proxy(r_obj* x);
r_obj* vec_proxy_equal(r_obj* x);
r_obj* vec_proxy_compare(r_obj* x);
r_obj* vec_proxy_order(r_obj* x);

r_obj* vec_proxy_method(r_obj* x);
r_obj* vec_proxy_invoke(r_obj* x, r_obj* method);
r_obj* vec_proxy_unwrap(r_obj* x);

#endif
