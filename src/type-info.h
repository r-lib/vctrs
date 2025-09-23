#ifndef VCTRS_TYPE_INFO_H
#define VCTRS_TYPE_INFO_H

#include <rlang.h>

enum vctrs_type {
  VCTRS_TYPE_null = 0,
  VCTRS_TYPE_unspecified,
  VCTRS_TYPE_logical,
  VCTRS_TYPE_integer,
  VCTRS_TYPE_double,
  VCTRS_TYPE_complex,
  VCTRS_TYPE_character,
  VCTRS_TYPE_raw,
  VCTRS_TYPE_list,
  VCTRS_TYPE_dataframe,
  VCTRS_TYPE_scalar,
  VCTRS_TYPE_s3 = 255
};

/**
 * @member type If `proxy_method` was found, the vector type of the
 *   proxy data. Otherwise, the vector type of the original data.
 *   This is never `vctrs_type_s3`.
 * @member proxy_method The function of the `vec_proxy()` method, if
 *   any. This method is looked up with [vec_proxy_method()].
 * @member proxy If `proxy_method` was found, the result of invoking
 *   the method. Otherwise, the original data.
 */
struct vctrs_proxy_info {
  r_obj* shelter;
  enum vctrs_type type;
  r_obj* proxy_method;
  r_obj* proxy;
};

/**
 * Return type information of a vector's proxy
 *
 * `vec_proxy_info()` returns the vctrs type of `x` or its proxy if it has one.
 * This never returns `vctrs_type_s3`, as we invoke `vec_proxy()` on classed
 * objects and assume the result is a native type.
 */
struct vctrs_proxy_info vec_proxy_info(r_obj* x);

enum vctrs_type vec_typeof(r_obj* x);
enum vctrs_type vec_proxy_typeof(r_obj* x);
const char* vec_type_as_str(enum vctrs_type type);

bool obj_is_list(r_obj* x);
bool obj_is_vector(r_obj* x);
bool list_all_vectors(r_obj* x);

r_no_return
void stop_unimplemented_vctrs_type(const char* fn, enum vctrs_type type);

#endif
