#include "vctrs.h"

const char* r_friendly_type_of(r_obj* x) {
  r_obj* call = KEEP(r_parse("friendly_type_of(x)"));
  r_obj* ffi_out = KEEP(r_eval_with_x(call, x, vctrs_ns_env));

  const char* out_str = r_chr_get_c_string(ffi_out, 0);
  int n = strlen(out_str) + 1;

  // Uses the vmax protection stack.
  char* out = R_alloc(n, sizeof(char));
  memcpy(out, out_str, n);

  FREE(2);
  return out;
}
