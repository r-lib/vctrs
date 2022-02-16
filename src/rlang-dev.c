#include "vctrs.h"
#include "decl/rlang-dev-decl.h"

const char* r_friendly_type_of(r_obj* x) {
  return r_friendly_type_of_opts(x, true, false);
}

const char* r_friendly_type_of_length(r_obj* x) {
  return r_friendly_type_of_opts(x, true, true);
}

const char* r_friendly_type_of_opts(r_obj* x,
                                    bool value,
                                    bool length) {
  r_obj* call = KEEP(r_parse("friendly_type_of(x, value = y, length = z)"));
  r_obj* ffi_out = KEEP(r_eval_with_xyz(call, x, r_lgl(value), r_lgl(length), vctrs_ns_env));

  const char* out_str = r_chr_get_c_string(ffi_out, 0);
  int n = strlen(out_str) + 1;

  // Uses the vmax protection stack.
  char* out = R_alloc(n, sizeof(char));
  memcpy(out, out_str, n);

  FREE(2);
  return out;
}
