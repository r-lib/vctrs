#include "vctrs.h"

#include "decl/missing-decl.h"

// [[ register() ]]
r_obj* ffi_vec_equal_na(r_obj* x) {
  return vec_equal_na(x);
}

// [[ include("missing.h") ]]
r_obj* vec_equal_na(r_obj* x) {
  r_obj* proxy = KEEP(vec_proxy_equal(x));
  r_obj* out = proxy_equal_na(proxy);
  FREE(1);
  return out;
}

static inline
r_obj* proxy_equal_na(r_obj* proxy) {
  const enum vctrs_type type = vec_proxy_typeof(proxy);

  switch (type) {
  case vctrs_type_logical: return lgl_equal_na(proxy);
  case vctrs_type_integer: return int_equal_na(proxy);
  case vctrs_type_double: return dbl_equal_na(proxy);
  case vctrs_type_complex: return cpl_equal_na(proxy);
  case vctrs_type_raw: return raw_equal_na(proxy);
  case vctrs_type_character: return chr_equal_na(proxy);
  case vctrs_type_list: return list_equal_na(proxy);
  case vctrs_type_dataframe: return df_equal_na(proxy);
  case vctrs_type_null: return vctrs_shared_empty_lgl;
  case vctrs_type_scalar: stop_scalar_type(proxy, vec_args.empty, r_lazy_null);
  default: stop_unimplemented_vctrs_type("vec_equal_na", type);
  }

  r_stop_unreachable();
}

// -----------------------------------------------------------------------------

#define EQUAL_NA(CTYPE, CBEGIN, IS_MISSING) do { \
  const r_ssize size = vec_size(x);              \
                                                 \
  r_obj* out = KEEP(r_new_logical(size));        \
  int* v_out = r_lgl_begin(out);                 \
                                                 \
  CTYPE const* v_x = CBEGIN(x);                  \
                                                 \
  for (r_ssize i = 0; i < size; ++i) {           \
    v_out[i] = IS_MISSING(v_x[i]);               \
  }                                              \
                                                 \
  FREE(1);                                       \
  return out;                                    \
} while (0)

static inline
r_obj* lgl_equal_na(r_obj* x) {
  EQUAL_NA(int, r_lgl_cbegin, lgl_is_missing);
}
static inline
r_obj* int_equal_na(r_obj* x) {
  EQUAL_NA(int, r_int_cbegin, int_is_missing);
}
static inline
r_obj* dbl_equal_na(r_obj* x) {
  EQUAL_NA(double, r_dbl_cbegin, dbl_is_missing);
}
static inline
r_obj* cpl_equal_na(r_obj* x) {
  EQUAL_NA(r_complex, r_cpl_cbegin, cpl_is_missing);
}
static inline
r_obj* raw_equal_na(r_obj* x) {
  EQUAL_NA(unsigned char, r_uchar_cbegin, raw_is_missing);
}
static inline
r_obj* chr_equal_na(r_obj* x) {
  EQUAL_NA(r_obj*, r_chr_cbegin, chr_is_missing);
}
static inline
r_obj* list_equal_na(r_obj* x) {
  EQUAL_NA(r_obj*, r_list_cbegin, list_is_missing);
}

#undef EQUAL_NA

// -----------------------------------------------------------------------------

static inline
r_obj* df_equal_na(r_obj* x) {
  int n_prot = 0;

  const r_ssize n_col = r_length(x);
  const r_ssize size = vec_size(x);
  r_obj* const* v_x = r_list_cbegin(x);

  r_obj* out = KEEP_N(r_new_logical(size), &n_prot);
  int* v_out = r_lgl_begin(out);

  // Initialize to "equality" value
  // and only change if we learn that it differs
  r_p_lgl_fill(v_out, 1, size);

  struct df_short_circuit_info info = new_df_short_circuit_info(size, false);
  struct df_short_circuit_info* p_info = &info;
  PROTECT_DF_SHORT_CIRCUIT_INFO(p_info, &n_prot);

  for (r_ssize i = 0; i < n_col; ++i) {
    r_obj* col = v_x[i];

    col_equal_na(col, v_out, p_info);

    // If all rows have at least one non-missing value, break
    if (p_info->remaining == 0) {
      break;
    }
  }

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

static inline
void col_equal_na(r_obj* x,
                  int* v_out,
                  struct df_short_circuit_info* p_info) {
  const enum vctrs_type type = vec_proxy_typeof(x);

  switch (type) {
  case vctrs_type_logical: lgl_col_equal_na(x, v_out, p_info); break;
  case vctrs_type_integer: int_col_equal_na(x, v_out, p_info); break;
  case vctrs_type_double: dbl_col_equal_na(x, v_out, p_info); break;
  case vctrs_type_complex: cpl_col_equal_na(x, v_out, p_info); break;
  case vctrs_type_raw: raw_col_equal_na(x, v_out, p_info); break;
  case vctrs_type_character: chr_col_equal_na(x, v_out, p_info); break;
  case vctrs_type_list: list_col_equal_na(x, v_out, p_info); break;
  case vctrs_type_dataframe: r_stop_internal("Data frame columns should have been flattened by now.");
  case vctrs_type_null: r_abort("Unexpected `NULL` column found in a data frame.");
  case vctrs_type_scalar: stop_scalar_type(x, vec_args.empty, r_lazy_null);
  default: stop_unimplemented_vctrs_type("vec_equal_na", type);
  }
}

// -----------------------------------------------------------------------------

#define COL_EQUAL_NA(CTYPE, CBEGIN, IS_MISSING) do { \
  CTYPE const* v_x = CBEGIN(x);                      \
                                                     \
  for (r_ssize i = 0; i < p_info->size; ++i) {       \
    if (p_info->p_row_known[i]) {                    \
      continue;                                      \
    }                                                \
                                                     \
    if (!IS_MISSING(v_x[i])) {                       \
      v_out[i] = 0;                                  \
      p_info->p_row_known[i] = true;                 \
      --p_info->remaining;                           \
                                                     \
      if (p_info->remaining == 0) {                  \
        break;                                       \
      }                                              \
    }                                                \
  }                                                  \
} while (0)

static inline
void lgl_col_equal_na(r_obj* x,
                      int* v_out,
                      struct df_short_circuit_info* p_info) {
  COL_EQUAL_NA(int, r_lgl_cbegin, lgl_is_missing);
}
static inline
void int_col_equal_na(r_obj* x,
                      int* v_out,
                      struct df_short_circuit_info* p_info) {
  COL_EQUAL_NA(int, r_int_cbegin, int_is_missing);
}
static inline
void dbl_col_equal_na(r_obj* x,
                      int* v_out,
                      struct df_short_circuit_info* p_info) {
  COL_EQUAL_NA(double, r_dbl_cbegin, dbl_is_missing);
}
static inline
void cpl_col_equal_na(r_obj* x,
                      int* v_out,
                      struct df_short_circuit_info* p_info) {
  COL_EQUAL_NA(r_complex, r_cpl_cbegin, cpl_is_missing);
}
static inline
void raw_col_equal_na(r_obj* x,
                      int* v_out,
                      struct df_short_circuit_info* p_info) {
  COL_EQUAL_NA(unsigned char, r_uchar_cbegin, raw_is_missing);
}
static inline
void chr_col_equal_na(r_obj* x,
                      int* v_out,
                      struct df_short_circuit_info* p_info) {
  COL_EQUAL_NA(r_obj*, r_chr_cbegin, chr_is_missing);
}
static inline
void list_col_equal_na(r_obj* x,
                       int* v_out,
                       struct df_short_circuit_info* p_info) {
  COL_EQUAL_NA(r_obj*, r_list_cbegin, list_is_missing);
}

#undef COL_EQUAL_NA

// -----------------------------------------------------------------------------

static inline
const unsigned char* r_uchar_cbegin(r_obj* x) {
  // TODO: Move to the rlang library
  return (const unsigned char*) r_raw_cbegin(x);
}
