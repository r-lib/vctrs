#include "vctrs.h"
#include "type-data-frame.h"

r_obj* tib_ptype2(
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy call,
  enum s3_fallback s3_fallback
) {
  r_obj* out = KEEP(df_ptype2(
    x,
    y,
    p_x_arg,
    p_y_arg,
    call,
    s3_fallback
  ));
  r_attrib_poke_class(out, classes_tibble);
  FREE(1);
  return out;
}

// [[ register() ]]
r_obj* ffi_tib_ptype2(r_obj* x,
                      r_obj* y,
                      r_obj* ffi_x_arg_,
                      r_obj* ffi_y_arg_,
                      r_obj* frame) {
  struct vctrs_arg x_arg = vec_as_arg(ffi_x_arg_);
  struct vctrs_arg y_arg = vec_as_arg(ffi_y_arg_);

  struct r_lazy call = { .x = r_syms.call, .env = frame };

  return tib_ptype2(
    x,
    y,
    &x_arg,
    &y_arg,
    call,
    S3_FALLBACK_false
  );
}

// [[ include("type-tibble.h") ]]
SEXP tib_cast(const struct cast_opts* opts) {
  SEXP out = PROTECT(df_cast_opts(opts));

  Rf_setAttrib(out, R_ClassSymbol, classes_tibble);

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
r_obj* ffi_tib_cast(r_obj* x,
                    r_obj* to,
                    r_obj* ffi_x_arg,
                    r_obj* ffi_to_arg,
                    r_obj* frame) {
  struct vctrs_arg x_arg = vec_as_arg(ffi_x_arg);
  struct vctrs_arg to_arg = vec_as_arg(ffi_to_arg);

  const struct cast_opts opts = {
    .x = x,
    .to = to,
    .p_x_arg = &x_arg,
    .p_to_arg = &to_arg,
    .call = { .x = r_syms.call, .env = frame }
  };

  return tib_cast(&opts);
}
