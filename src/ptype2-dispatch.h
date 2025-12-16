#ifndef VCTRS_PTYPE2_DISPATCH_H
#define VCTRS_PTYPE2_DISPATCH_H

#include "vctrs-core.h"
#include "ptype2.h"

r_obj* vec_ptype2_dispatch_native(
  r_obj* x,
  r_obj* y,
  enum vctrs_type x_type,
  enum vctrs_type y_type,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy call,
  enum s3_fallback s3_fallback,
  int* left
);

r_obj* vec_ptype2_dispatch_s3(
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy call,
  enum s3_fallback s3_fallback
);

r_obj* vec_invoke_coerce_method(r_obj* method_sym, r_obj* method,
                                r_obj* x_sym, r_obj* x,
                                r_obj* y_sym, r_obj* y,
                                r_obj* x_arg_sym, r_obj* x_arg,
                                r_obj* y_arg_sym, r_obj* y_arg,
                                struct r_lazy call,
                                enum s3_fallback s3_fallback);

r_obj* vec_ptype2_default(r_obj* x,
                          r_obj* y,
                          struct vctrs_arg* x_arg,
                          struct vctrs_arg* y_arg,
                          struct r_lazy call,
                          enum s3_fallback s3_fallback);


#endif
