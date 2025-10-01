#include "vctrs.h"
#include "decl/type-info-decl.h"

struct vctrs_proxy_info vec_proxy_info(r_obj* x) {
  struct vctrs_proxy_info info;

  // Avoid `KEEP(x_proxy_method)` if not required! This does help with
  // performance, since this is called in such a tight loop.
  //
  // `vec_proxy_method()` itself may also return `r_null`
  r_obj* x_proxy_method = r_is_object(x) ? vec_proxy_method(x) : r_null;

  if (x_proxy_method == r_null) {
    info.inner = x;
    info.type = vec_base_typeof(x, false);
    info.had_proxy_method = false;
  } else {
    KEEP(x_proxy_method);
    info.inner = KEEP(vec_proxy_invoke(x, x_proxy_method));
    info.type = vec_base_typeof(info.inner, true);
    info.had_proxy_method = true;
    FREE(2);
  }

  return info;
}

// Type info of `x`
//
// Does not take the proxy, so can return `VCTRS_TYPE_s3`, unlike `vec_proxy_info()`.
//
// [[ register() ]]
r_obj* ffi_type_info(r_obj* x) {
  r_obj* out = KEEP(Rf_mkNamed(R_TYPE_list, (const char*[]) { "type", "had_proxy_method", "" }));

  const enum vctrs_type type = vec_typeof(x);
  r_list_poke(out, 0, r_chr(vec_type_as_str(type)));
  r_list_poke(out, 1, r_lgl(vec_proxy_method(x) != r_null));

  FREE(1);
  return out;
}
// [[ register() ]]
r_obj* ffi_proxy_info(r_obj* x) {
  struct vctrs_proxy_info info = vec_proxy_info(x);
  KEEP(info.inner);

  r_obj* out = KEEP(Rf_mkNamed(R_TYPE_list, (const char*[]) { "type", "had_proxy_method", "proxy", "" }));
  r_list_poke(out, 0, r_chr(vec_type_as_str(info.type)));
  r_list_poke(out, 1, r_lgl(info.had_proxy_method));
  r_list_poke(out, 2, info.inner);

  FREE(2);
  return out;
}

static
enum vctrs_type vec_base_typeof(r_obj* x, bool proxied) {
  switch (r_typeof(x)) {
  // Atomic types are always vectors
  case R_TYPE_null: return VCTRS_TYPE_null;
  case R_TYPE_logical: return VCTRS_TYPE_logical;
  case R_TYPE_integer: return VCTRS_TYPE_integer;
  case R_TYPE_double: return VCTRS_TYPE_double;
  case R_TYPE_complex: return VCTRS_TYPE_complex;
  case R_TYPE_character: return VCTRS_TYPE_character;
  case R_TYPE_raw: return VCTRS_TYPE_raw;
  case R_TYPE_list:
    // Bare lists and data frames are vectors
    if (!r_is_object(x)) return VCTRS_TYPE_list;
    if (is_data_frame(x)) return VCTRS_TYPE_dataframe;
    // S3 lists are only vectors if they are proxied
    if (proxied || r_inherits(x, "list")) return VCTRS_TYPE_list;
    // fallthrough
  default: return VCTRS_TYPE_scalar;
  }
}

enum vctrs_type vec_proxy_typeof(r_obj* x) {
  return vec_base_typeof(x, true);
}

// [[ register() ]]
r_obj* vctrs_typeof(r_obj* x, r_obj* dispatch) {
  enum vctrs_type type;
  if (r_lgl_get(dispatch, 0)) {
    type = vec_proxy_info(x).type;
  } else {
    type = vec_typeof(x);
  }
  return r_chr(vec_type_as_str(type));
}

enum vctrs_type vec_typeof(r_obj* x) {
  // Check for unspecified vectors before `vec_base_typeof()` which
  // allows vectors of `NA` to pass through as `VCTRS_TYPE_logical`
  if (vec_is_unspecified(x)) {
    return VCTRS_TYPE_unspecified;
  }

  if (!r_is_object(x) || r_class(x) == r_null) {
    return vec_base_typeof(x, false);
  }

  // Bare data frames are treated as a base atomic type. Subclasses of
  // data frames are treated as S3 to give them a chance to be proxied
  // or implement their own methods for cast, type2, etc.
  if (is_bare_data_frame(x)) {
    return VCTRS_TYPE_dataframe;
  }

  return VCTRS_TYPE_s3;
}

r_no_return
void stop_unimplemented_vctrs_type(const char* fn, enum vctrs_type type) {
  r_stop_internal("Unsupported vctrs type `%s`.", vec_type_as_str(type));
}

const char* vec_type_as_str(enum vctrs_type type) {
  switch (type) {
  case VCTRS_TYPE_null:         return "null";
  case VCTRS_TYPE_unspecified:  return "unspecified";
  case VCTRS_TYPE_logical:      return "logical";
  case VCTRS_TYPE_integer:      return "integer";
  case VCTRS_TYPE_double:       return "double";
  case VCTRS_TYPE_complex:      return "complex";
  case VCTRS_TYPE_character:    return "character";
  case VCTRS_TYPE_raw:          return "raw";
  case VCTRS_TYPE_list:         return "list";
  case VCTRS_TYPE_dataframe:    return "dataframe";
  case VCTRS_TYPE_s3:           return "s3";
  case VCTRS_TYPE_scalar:       return "scalar";
  }
  never_reached("vec_type_as_str");
}


void vctrs_init_type_info(r_obj* ns) {

}
