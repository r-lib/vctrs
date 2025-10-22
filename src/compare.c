#include "vctrs.h"
#include <strings.h>

#include "decl/compare-decl.h"

static
void stop_not_comparable(r_obj* x, r_obj* y, const char* message) {
  r_abort("`x` and `y` are not comparable: %s", message);
}

// -----------------------------------------------------------------------------

#define COMPARE(CTYPE, CBEGIN, SCALAR_COMPARE)          \
do {                                                    \
  r_obj* out = KEEP(r_alloc_integer(size));             \
  int* v_out = r_int_begin(out);                        \
                                                        \
  CTYPE const* v_x = CBEGIN(x);                         \
  CTYPE const* v_y = CBEGIN(y);                         \
                                                        \
  for (r_ssize i = 0; i < size; ++i) {                  \
    v_out[i] = SCALAR_COMPARE(v_x[i], v_y[i]);          \
  }                                                     \
                                                        \
  FREE(3);                                              \
  return out;                                           \
}                                                       \
while (0)

r_obj* vec_compare(r_obj* x, r_obj* y, bool na_equal) {
  r_ssize size = vec_size(x);

  enum vctrs_type type = vec_proxy_typeof(x);
  if (type != vec_proxy_typeof(y) || size != vec_size(y)) {
    stop_not_comparable(x, y, "must have the same types and lengths");
  }

  x = KEEP(vec_normalize_encoding(x));
  y = KEEP(vec_normalize_encoding(y));

  if (type == VCTRS_TYPE_dataframe) {
    r_obj* out = df_compare(x, y, na_equal, size);
    FREE(2);
    return out;
  }

  if (na_equal) {
    switch (type) {
    case VCTRS_TYPE_logical:   COMPARE(int, r_lgl_cbegin, lgl_compare_na_equal);
    case VCTRS_TYPE_integer:   COMPARE(int, r_int_cbegin, int_compare_na_equal);
    case VCTRS_TYPE_double:    COMPARE(double, r_dbl_cbegin, dbl_compare_na_equal);
    case VCTRS_TYPE_character: COMPARE(r_obj*, r_chr_cbegin, chr_compare_na_equal);
    case VCTRS_TYPE_complex:   r_abort("Can't compare complexes.");
    case VCTRS_TYPE_scalar:    r_abort("Can't compare scalars.");
    case VCTRS_TYPE_list:      r_abort("Can't compare lists.");
    default:                   stop_unimplemented_vctrs_type("vec_compare", type);
    }
  } else {
    switch (type) {
    case VCTRS_TYPE_logical:   COMPARE(int, r_lgl_cbegin, lgl_compare_na_propagate);
    case VCTRS_TYPE_integer:   COMPARE(int, r_int_cbegin, int_compare_na_propagate);
    case VCTRS_TYPE_double:    COMPARE(double, r_dbl_cbegin, dbl_compare_na_propagate);
    case VCTRS_TYPE_character: COMPARE(r_obj*, r_chr_cbegin, chr_compare_na_propagate);
    case VCTRS_TYPE_complex:   r_abort("Can't compare complexes.");
    case VCTRS_TYPE_scalar:    r_abort("Can't compare scalars.");
    case VCTRS_TYPE_list:      r_abort("Can't compare lists.");
    default:                   stop_unimplemented_vctrs_type("vec_compare", type);
    }
  }
}

#undef COMPARE

r_obj* ffi_vec_compare(r_obj* x, r_obj* y, r_obj* ffi_na_equal) {
  const bool na_equal = r_bool_as_int(ffi_na_equal);
  return vec_compare(x, y, na_equal);
}

// -----------------------------------------------------------------------------

static
r_obj* df_compare(r_obj* x, r_obj* y, bool na_equal, r_ssize size) {
  int nprot = 0;

  r_obj* out = KEEP_N(r_alloc_integer(size), &nprot);
  int* v_out = r_int_begin(out);

  // Initialize to "equality" value and only change if we learn that it differs.
  // This also determines the zero column result.
  r_memset(v_out, 0, size * sizeof(int));

  struct df_short_circuit_info info = new_df_short_circuit_info(size, false);
  struct df_short_circuit_info* p_info = &info;
  PROTECT_DF_SHORT_CIRCUIT_INFO(p_info, &nprot);

  df_compare_impl(v_out, p_info, x, y, na_equal);

  FREE(nprot);
  return out;
}

static
void df_compare_impl(int* v_out,
                     struct df_short_circuit_info* p_info,
                     r_obj* x,
                     r_obj* y,
                     bool na_equal) {
  r_ssize n_col = r_length(x);

  if (n_col != r_length(y)) {
    stop_not_comparable(x, y, "must have the same number of columns");
  }

  for (r_ssize i = 0; i < n_col; ++i) {
    r_obj* x_col = r_list_get(x, i);
    r_obj* y_col = r_list_get(y, i);

    vec_compare_col(v_out, p_info, x_col, y_col, na_equal);

    // If we know all comparison values, break
    if (p_info->remaining == 0) {
      break;
    }
  }
}

// -----------------------------------------------------------------------------

#define COMPARE_COL(CTYPE, CBEGIN, SCALAR_COMPARE)      \
do {                                                    \
  CTYPE const* v_x = CBEGIN(x);                         \
  CTYPE const* v_y = CBEGIN(y);                         \
                                                        \
  for (r_ssize i = 0; i < p_info->size; ++i) {          \
    if (p_info->p_row_known[i]) {                       \
      continue;                                         \
    }                                                   \
                                                        \
    int cmp = SCALAR_COMPARE(v_x[i], v_y[i]);           \
                                                        \
    if (cmp != 0) {                                     \
      v_out[i] = cmp;                                   \
      p_info->p_row_known[i] = true;                    \
      --p_info->remaining;                              \
                                                        \
      if (p_info->remaining == 0) {                     \
        break;                                          \
      }                                                 \
    }                                                   \
  }                                                     \
}                                                       \
while (0)

static
void vec_compare_col(int* v_out,
                     struct df_short_circuit_info* p_info,
                     r_obj* x,
                     r_obj* y,
                     bool na_equal) {
  enum vctrs_type type = vec_proxy_typeof(x);

  if (type == VCTRS_TYPE_dataframe) {
    df_compare_impl(v_out, p_info, x, y, na_equal);
    return;
  }

  if (na_equal) {
    switch (type) {
    case VCTRS_TYPE_logical:   COMPARE_COL(int, r_lgl_cbegin, lgl_compare_na_equal); break;
    case VCTRS_TYPE_integer:   COMPARE_COL(int, r_int_cbegin, int_compare_na_equal); break;
    case VCTRS_TYPE_double:    COMPARE_COL(double, r_dbl_cbegin, dbl_compare_na_equal); break;
    case VCTRS_TYPE_character: COMPARE_COL(r_obj*, r_chr_cbegin, chr_compare_na_equal); break;
    case VCTRS_TYPE_complex:   r_abort("Can't compare complexes.");
    case VCTRS_TYPE_scalar:    r_abort("Can't compare scalars.");
    case VCTRS_TYPE_list:      r_abort("Can't compare lists.");
    default:                   stop_unimplemented_vctrs_type("vec_compare_col", type);
    }
  } else {
    switch (type) {
    case VCTRS_TYPE_logical:   COMPARE_COL(int, r_lgl_cbegin, lgl_compare_na_propagate); break;
    case VCTRS_TYPE_integer:   COMPARE_COL(int, r_int_cbegin, int_compare_na_propagate); break;
    case VCTRS_TYPE_double:    COMPARE_COL(double, r_dbl_cbegin, dbl_compare_na_propagate); break;
    case VCTRS_TYPE_character: COMPARE_COL(r_obj*, r_chr_cbegin, chr_compare_na_propagate); break;
    case VCTRS_TYPE_complex:   r_abort("Can't compare complexes.");
    case VCTRS_TYPE_scalar:    r_abort("Can't compare scalars.");
    case VCTRS_TYPE_list:      r_abort("Can't compare lists.");
    default:                   stop_unimplemented_vctrs_type("vec_compare_col", type);
    }
  }
}

#undef COMPARE_COL
