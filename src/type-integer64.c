#include <rlang.h>
#include "vctrs.h"
#include "utils.h"
#include "decl/type-integer64-decl.h"

#define r_na_llong LLONG_MIN

// [[ register() ]]
r_obj* vctrs_integer64_to_complex(r_obj* x) {
  if (r_typeof(x) != R_TYPE_double) {
    r_stop_internal("vctrs_integer64_to_complex", "`x` must be a double.");
  }

  r_ssize size = r_length(x);
  const long long* v_x = (const long long*) r_dbl_cbegin(x);

  r_obj* out = KEEP(r_alloc_complex(size));
  r_complex_t* v_out = r_cpl_begin(out);

  r_attrib_poke_names(out, r_names(x));

  r_attrib_poke(out, R_DimSymbol, r_attrib_get(x, R_DimSymbol));
  r_attrib_poke(out, R_DimNamesSymbol, r_attrib_get(x, R_DimNamesSymbol));

  for (r_ssize i = 0; i < size; ++i) {
    const long long elt = v_x[i];

    if (elt == r_na_llong) {
      v_out[i].r = r_globals.na_dbl;
      v_out[i].i = r_globals.na_dbl;
      continue;
    }

    const int64_t elt_i64 = (int64_t) elt;

    v_out[i] = int64_to_r_complex(elt_i64);
  }

  FREE(1);
  return out;
}

// [[ register() ]]
r_obj* vctrs_complex_to_integer64(r_obj* x) {
  if (r_typeof(x) != R_TYPE_complex) {
    r_stop_internal("vctrs_complex_to_integer64", "`x` must be a complex.");
  }

  r_ssize size = r_length(x);
  const r_complex_t* v_x = r_cpl_cbegin(x);

  r_obj* out = KEEP(r_alloc_double(size));
  long long* v_out = (long long*) r_dbl_begin(out);

  r_attrib_poke_names(out, r_names(x));

  r_attrib_poke(out, R_DimSymbol, r_attrib_get(x, R_DimSymbol));
  r_attrib_poke(out, R_DimNamesSymbol, r_attrib_get(x, R_DimNamesSymbol));

  r_attrib_poke_class(out, r_chr("integer64"));

  for (r_ssize i = 0; i < size; ++i) {
    const r_complex_t elt = v_x[i];

    if (elt.r == r_globals.na_dbl) {
      v_out[i] = r_na_llong;
      continue;
    }

    v_out[i] = (long long) r_complex_to_int64(elt);
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
 * The two `uint32_t` values are stored in a single `r_complex_t` as two
 * doubles. At the cost of some memory, we gain R's native vectorization and an
 * easy way to represent an `NA` value in a way that vctrs understands (as
 * `NA_real_` in the real/imaginary slots).
 *
 * Unsigned 32-bit integers are used because bit shifting is undefined on signed
 * types.
 *
 * An arithmetic shift of `- INT64_MIN` is done to remap the int64_t value
 * into uint64_t space, while maintaining order. This relies on unsigned
 * arithmetic overflow behavior, which is well-defined.
 */

static inline
r_complex_t int64_to_r_complex(int64_t x) {
  const uint64_t x_u64 = ((uint64_t) x) - INT64_MIN;

  const uint32_t left_u32 = (uint32_t) (x_u64 >> 32);
  const uint32_t right_u32 = (uint32_t) x_u64;

  const double left = (double) left_u32;
  const double right = (double) right_u32;

  return (r_complex_t) {left, right};
}

static inline
int64_t r_complex_to_int64(r_complex_t x) {
  const double left = x.r;
  const double right = x.i;

  const uint32_t left_u32 = (uint32_t) left;
  const uint32_t right_u32 = (uint32_t) right;

  const uint64_t out_u64 = ((uint64_t) left_u32) << 32 | right_u32;

  const int64_t out = (int64_t) (out_u64 + INT64_MIN);

  return out;
}
