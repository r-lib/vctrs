#ifndef VCTRS_TYPE_INFO_H
#define VCTRS_TYPE_INFO_H

enum vctrs_type {
  vctrs_type_null = 0,
  vctrs_type_unspecified,
  vctrs_type_logical,
  vctrs_type_integer,
  vctrs_type_double,
  vctrs_type_complex,
  vctrs_type_character,
  vctrs_type_raw,
  vctrs_type_list,
  vctrs_type_dataframe,
  vctrs_type_scalar,
  vctrs_type_s3 = 255
};

/**
 * @member type The vector type of the original data.
 * @member proxy_method The function of the `vec_proxy()` method, if
 *   any. This method is looked up with [vec_proxy_method()].
 */
struct vctrs_type_info {
  r_obj* shelter;
  enum vctrs_type type;
  r_obj* proxy_method;
};

/**
 * @inheritMembers vctrs_type_info
 * @member type If `proxy_method` was found, the vector type of the
 *   proxy data. Otherwise, the vector type of the original data.
 *   This is never `vctrs_type_s3`.
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
 * Return the type information of a vector or its proxy
 *
 * `vec_type_info()` returns the vctrs type of `x`. `vec_proxy_info()`
 * returns the vctrs type of `x` or its proxy if it has one. The
 * former returns `vctrs_type_s3` with S3 objects (expect for native
 * types like bare data frames). The latter returns the bare type of
 * the proxy, if any. It never returns `vctrs_type_s3`.
 *
 * `vec_proxy_info()` returns both the proxy method and the proxy
 * data. `vec_type_info()` only returns the proxy method, which it
 * needs to determine whether S3 lists and non-vector base types are
 * scalars or proxied vectors.
 */
struct vctrs_type_info vec_type_info(r_obj* x);
struct vctrs_proxy_info vec_proxy_info(r_obj* x);

enum vctrs_type vec_typeof(r_obj* x);
enum vctrs_type vec_proxy_typeof(r_obj* x);
const char* vec_type_as_str(enum vctrs_type type);

bool vec_is_list(r_obj* x);
bool vec_is_vector(r_obj* x);

r_no_return
void stop_unimplemented_vctrs_type(const char* fn, enum vctrs_type type);

#endif
