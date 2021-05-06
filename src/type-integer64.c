#include <rlang.h>
#include "vctrs.h"
#include "utils.h"
#include "decl/type-integer64-decl.h"

#define r_na_llong LLONG_MIN


static
const char* v_integer64_proxy_df_names_c_strings[] = {
  "left",
  "right"
};
static
const enum r_type v_integer64_proxy_df_types[] = {
  R_TYPE_double,
  R_TYPE_double
};
enum integer64_proxy_df_locs {
  INTEGER64_PROXY_DF_LOCS_left,
  INTEGER64_PROXY_DF_LOCS_right
};
#define INTEGER64_PROXY_DF_SIZE R_ARR_SIZEOF(v_integer64_proxy_df_types)


// [[ register() ]]
r_obj* vctrs_integer64_proxy(r_obj* x) {
  if (r_typeof(x) != R_TYPE_double) {
    r_stop_internal("vctrs_integer64_proxy", "`x` must be a double.");
  }
  if (r_attrib_get(x, R_DimSymbol) != r_null) {
    r_stop_internal("vctrs_integer64_proxy", "`x` should not have a `dim` attribute.");
  }

  r_ssize size = r_length(x);
  const long long* v_x = (const long long*) r_dbl_cbegin(x);

  r_obj* nms = KEEP(r_chr_n(
    v_integer64_proxy_df_names_c_strings,
    INTEGER64_PROXY_DF_SIZE
  ));

  r_obj* out = KEEP(r_alloc_df_list(
    size,
    nms,
    v_integer64_proxy_df_types,
    INTEGER64_PROXY_DF_SIZE
  ));

  r_init_data_frame(out, size);

  r_obj* left = r_list_get(out, INTEGER64_PROXY_DF_LOCS_left);
  r_obj* right = r_list_get(out, INTEGER64_PROXY_DF_LOCS_right);

  double* v_left = r_dbl_begin(left);
  double* v_right = r_dbl_begin(right);

  for (r_ssize i = 0; i < size; ++i) {
    const long long elt = v_x[i];

    if (elt == r_na_llong) {
      v_left[i] = r_globals.na_dbl;
      v_right[i] = r_globals.na_dbl;
      continue;
    }

    const int64_t elt_i64 = (int64_t) elt;

    int64_unpack(elt_i64, i, v_left, v_right);
  }

  FREE(2);
  return out;
}

// [[ register() ]]
r_obj* vctrs_integer64_restore(r_obj* x) {
  if (!is_data_frame(x)) {
    r_stop_internal("vctrs_integer64_restore", "`x` must be a data frame.");
  }
  if (r_length(x) != 2) {
    r_stop_internal("vctrs_integer64_restore", "`x` must have two columns.");
  }

  r_obj* left = r_list_get(x, INTEGER64_PROXY_DF_LOCS_left);
  r_obj* right = r_list_get(x, INTEGER64_PROXY_DF_LOCS_right);

  const double* v_left = r_dbl_cbegin(left);
  const double* v_right = r_dbl_cbegin(right);

  r_ssize size = r_length(left);

  r_obj* out = KEEP(r_alloc_double(size));
  long long* v_out = (long long*) r_dbl_begin(out);

  r_attrib_poke_class(out, r_chr("integer64"));

  for (r_ssize i = 0; i < size; ++i) {
    const double left = v_left[i];
    const double right = v_right[i];

    if (left == r_globals.na_dbl) {
      v_out[i] = r_na_llong;
      continue;
    }

    v_out[i] = (long long) int64_pack(left, right);
  }

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------

/*
 * This pair of functions facilitates:
 * - Splitting an `int64_t` into two `uint32_t` values, maintaining order
 * - Combining those two `uint32_t` values back into the original `int32_t`
 *
 * The two `uint32_t` values are stored in two doubles. This allows us to store
 * it in a two column data frame that vctrs knows how to work with, and we can
 * use the standard `NA_real_` as the missing value without fear of conflicting
 * with any other valid `int64_t` value.
 *
 * Unsigned 32-bit integers are used because bit shifting is undefined on signed
 * types.
 *
 * An arithmetic shift of `- INT64_MIN` is done to remap the int64_t value
 * into uint64_t space, while maintaining order. This relies on unsigned
 * arithmetic overflow behavior, which is well-defined.
 */

static inline
void int64_unpack(int64_t x, r_ssize i, double* v_left, double* v_right) {
  const uint64_t x_u64 = ((uint64_t) x) - INT64_MIN;

  const uint32_t left_u32 = (uint32_t) (x_u64 >> 32);
  const uint32_t right_u32 = (uint32_t) x_u64;

  v_left[i] = (double) left_u32;
  v_right[i] = (double) right_u32;
}

static inline
int64_t int64_pack(double left, double right) {
  const uint32_t left_u32 = (uint32_t) left;
  const uint32_t right_u32 = (uint32_t) right;

  const uint64_t out_u64 = ((uint64_t) left_u32) << 32 | right_u32;

  const int64_t out = (int64_t) (out_u64 + INT64_MIN);

  return out;
}
