#ifndef VCTRS_RLANG_DEV_H
#define VCTRS_RLANG_DEV_H

#include <rlang.h>

static inline
const char* r_c_str_format_error_arg(const char* x) {
  r_obj* ffi_x = KEEP(r_chr(x));
  const char* out = r_format_error_arg(ffi_x);
  FREE(1);
  return out;
}

// vmax-protected result
const char* r_obj_type_friendly_length(r_obj* x);


#endif
