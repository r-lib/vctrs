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

  // A location vector to track rows where we still need to check for missing
  // values. After we iterate through all columns, `v_loc` points to the missing
  // rows.
  r_ssize loc_size = size;
  r_obj* loc_shelter = KEEP_N(r_alloc_raw(loc_size * sizeof(r_ssize)), &n_prot);
  r_ssize* v_loc = (r_ssize*) r_raw_begin(loc_shelter);

  for (r_ssize i = 0; i < loc_size; ++i) {
    v_loc[i] = i;
  }

  for (r_ssize i = 0; i < n_col; ++i) {
    r_obj* col = v_x[i];

    loc_size = col_equal_na(col, v_loc, loc_size);

    // If all rows have at least one non-missing value, break
    if (loc_size == 0) {
      break;
    }
  }

  r_obj* out = KEEP_N(r_new_logical(size), &n_prot);
  int* v_out = r_lgl_begin(out);
  r_p_lgl_fill(v_out, 0, size);

  for (r_ssize i = 0; i < loc_size; ++i) {
    const r_ssize loc = v_loc[i];
    v_out[loc] = 1;
  }

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

static inline
r_ssize col_equal_na(r_obj* x,
                     r_ssize* v_loc,
                     r_ssize loc_size) {
  const enum vctrs_type type = vec_proxy_typeof(x);

  switch (type) {
  case vctrs_type_logical: return lgl_col_equal_na(x, v_loc, loc_size);
  case vctrs_type_integer: return int_col_equal_na(x, v_loc, loc_size);
  case vctrs_type_double: return dbl_col_equal_na(x, v_loc, loc_size);
  case vctrs_type_complex: return cpl_col_equal_na(x, v_loc, loc_size);
  case vctrs_type_raw: return raw_col_equal_na(x, v_loc, loc_size);
  case vctrs_type_character: return chr_col_equal_na(x, v_loc, loc_size);
  case vctrs_type_list: return list_col_equal_na(x, v_loc, loc_size);
  case vctrs_type_dataframe: r_stop_internal("Data frame columns should have been flattened by now.");
  case vctrs_type_null: r_abort("Unexpected `NULL` column found in a data frame.");
  case vctrs_type_scalar: stop_scalar_type(x, vec_args.empty, r_lazy_null);
  default: stop_unimplemented_vctrs_type("vec_equal_na", type);
  }
}

// -----------------------------------------------------------------------------

/*
 * The data frame algorithm for `vec_equal_na()` is fast because this inner
 * for loop doesn't have any `if` branches in it. We utilize the fact that
 * this is a no-op when the element isn't missing:
 * `new_loc_size += IS_MISSING(v_x[loc])`
 * This is faster than doing `if (IS_MISSING())` at each iteration, especially
 * when there is a moderate amount of missing values, which makes that branch
 * fairly unpredictable.
 *
 * `r_ssize* v_loc` is a location vector that tracks which rows we still need
 * to check for missingness. It is "narrowed" after each column is processed to
 * only point to the rows that might still be missing. After all columns are
 * processed, it points to exactly where the missing rows are. Here is some
 * pseudo R code that demonstrates how `v_loc` changes:
 *
 * ```
 * df <- data.frame(
 *  x = c(1,  NA, NA, 2, NA, 3),
 *  y = c(NA, NA, 1,  2, NA, 4)
 * )
 * df
 * #>    x  y
 * #> 1  1 NA
 * #> 2 NA NA
 * #> 3 NA  1
 * #> 4  2  2
 * #> 5 NA NA
 * #> 6  3  4
 *
 * # Initially any row could be missing
 * loc_size <- 6
 * loc <- 1:6
 *
 * # After processing the first column, only rows 2, 3, and 5 could be missing
 * loc_size <- 3
 * loc <- c(2, 3, 5)
 *
 * # After processing the second column, only 2 and 5 could be missing
 * # This is the last column, so these are the missing rows
 * loc_size <- 2
 * loc <- c(2, 5)
 * ```
 *
 * For more details, see: https://github.com/r-lib/vctrs/pull/1584
 */
#define COL_EQUAL_NA(CTYPE, CBEGIN, IS_MISSING) do { \
  CTYPE const* v_x = CBEGIN(x);                      \
  r_ssize new_loc_size = 0;                          \
                                                     \
  for (r_ssize i = 0; i < loc_size; ++i) {           \
    const r_ssize loc = v_loc[i];                    \
    v_loc[new_loc_size] = loc;                       \
    new_loc_size += IS_MISSING(v_x[loc]);            \
  }                                                  \
                                                     \
  return new_loc_size;                               \
} while (0)

static inline
r_ssize lgl_col_equal_na(r_obj* x,
                         r_ssize* v_loc,
                         r_ssize loc_size) {
  COL_EQUAL_NA(int, r_lgl_cbegin, lgl_is_missing);
}
static inline
r_ssize int_col_equal_na(r_obj* x,
                         r_ssize* v_loc,
                         r_ssize loc_size) {
  COL_EQUAL_NA(int, r_int_cbegin, int_is_missing);
}
static inline
r_ssize dbl_col_equal_na(r_obj* x,
                         r_ssize* v_loc,
                         r_ssize loc_size) {
  COL_EQUAL_NA(double, r_dbl_cbegin, dbl_is_missing);
}
static inline
r_ssize cpl_col_equal_na(r_obj* x,
                         r_ssize* v_loc,
                         r_ssize loc_size) {
  COL_EQUAL_NA(r_complex, r_cpl_cbegin, cpl_is_missing);
}
static inline
r_ssize raw_col_equal_na(r_obj* x,
                         r_ssize* v_loc,
                         r_ssize loc_size) {
  COL_EQUAL_NA(unsigned char, r_uchar_cbegin, raw_is_missing);
}
static inline
r_ssize chr_col_equal_na(r_obj* x,
                         r_ssize* v_loc,
                         r_ssize loc_size) {
  COL_EQUAL_NA(r_obj*, r_chr_cbegin, chr_is_missing);
}
static inline
r_ssize list_col_equal_na(r_obj* x,
                          r_ssize* v_loc,
                          r_ssize loc_size) {
  COL_EQUAL_NA(r_obj*, r_list_cbegin, list_is_missing);
}

#undef COL_EQUAL_NA

// -----------------------------------------------------------------------------

static inline
const unsigned char* r_uchar_cbegin(r_obj* x) {
  // TODO: Move to the rlang library
  return (const unsigned char*) r_raw_cbegin(x);
}
