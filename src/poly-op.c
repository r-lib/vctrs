#include "poly-op.h"
#include "vctrs.h"
#include "equal.h"
#include "utils.h"

// -----------------------------------------------------------------------------

static int p_df_equal_na_equal(const void* x, r_ssize i, const void* y, r_ssize j);

// [[ include("poly-op.h") ]]
poly_binary_int_fn_ptr new_poly_p_equal_na_equal(enum vctrs_type type) {
  switch (type) {
  case vctrs_type_null: return p_nil_equal_na_equal;
  case vctrs_type_logical: return p_lgl_equal_na_equal;
  case vctrs_type_integer: return p_int_equal_na_equal;
  case vctrs_type_double: return p_dbl_equal_na_equal;
  case vctrs_type_complex: return p_cpl_equal_na_equal;
  case vctrs_type_character: return p_chr_equal_na_equal;
  case vctrs_type_raw: return p_raw_equal_na_equal;
  case vctrs_type_list: return p_list_equal_na_equal;
  case vctrs_type_dataframe: return p_df_equal_na_equal;
  default: stop_unimplemented_vctrs_type("new_poly_p_equal_na_equal", type);
  }
}

static
int p_df_equal_na_equal(const void* x, r_ssize i, const void* y, r_ssize j) {
  struct poly_df_data* x_data = (struct poly_df_data*) x;
  struct poly_df_data* y_data = (struct poly_df_data*) y;

  r_ssize n_col = x_data->n_col;
  if (n_col != y_data->n_col) {
    r_stop_internal("`x` and `y` must have the same number of columns.");
  }

  enum vctrs_type* v_col_type = x_data->v_col_type;
  const void** v_x_col_ptr = x_data->v_col_ptr;
  const void** v_y_col_ptr = y_data->v_col_ptr;

  // df-cols should already be flattened
  for (r_ssize col = 0; col < n_col; ++col) {
    if (!p_equal_na_equal(v_x_col_ptr[col], i, v_y_col_ptr[col], j, v_col_type[col])) {
      return false;
    }
  }

  return true;
}

// -----------------------------------------------------------------------------

static bool p_df_is_incomplete(const void* x, r_ssize i);

// [[ include("poly-op.h") ]]
poly_unary_bool_fn_ptr new_poly_p_is_incomplete(enum vctrs_type type) {
  switch (type) {
  case vctrs_type_null: return p_nil_is_missing;
  case vctrs_type_logical: return p_lgl_is_missing;
  case vctrs_type_integer: return p_int_is_missing;
  case vctrs_type_double: return p_dbl_is_missing;
  case vctrs_type_complex: return p_cpl_is_missing;
  case vctrs_type_character: return p_chr_is_missing;
  case vctrs_type_raw: return p_raw_is_missing;
  case vctrs_type_list: return p_list_is_missing;
  case vctrs_type_dataframe: return p_df_is_incomplete;
  default: stop_unimplemented_vctrs_type("new_poly_p_is_incomplete", type);
  }
}

static
bool p_df_is_incomplete(const void* x, r_ssize i) {
  struct poly_df_data* x_data = (struct poly_df_data*) x;

  enum vctrs_type* v_col_type = x_data->v_col_type;
  const void** v_col_ptr = x_data->v_col_ptr;
  r_ssize n_col = x_data->n_col;

  // df-cols should already be flattened,
  // so we only need missingness of each column, not completeness
  for (r_ssize col = 0; col < n_col; ++col) {
    if (p_is_missing(v_col_ptr[col], i, v_col_type[col])) {
      return true;
    }
  }

  return false;
}

// -----------------------------------------------------------------------------

static void init_nil_poly_vec(struct poly_vec* p_poly_vec);
static void init_lgl_poly_vec(struct poly_vec* p_poly_vec);
static void init_int_poly_vec(struct poly_vec* p_poly_vec);
static void init_dbl_poly_vec(struct poly_vec* p_poly_vec);
static void init_cpl_poly_vec(struct poly_vec* p_poly_vec);
static void init_chr_poly_vec(struct poly_vec* p_poly_vec);
static void init_raw_poly_vec(struct poly_vec* p_poly_vec);
static void init_list_poly_vec(struct poly_vec* p_poly_vec);
static void init_df_poly_vec(struct poly_vec* p_poly_vec);

// [[ include("poly-op.h") ]]
struct poly_vec* new_poly_vec(SEXP proxy, enum vctrs_type type) {
  SEXP self = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct poly_vec)));
  struct poly_vec* p_poly_vec = (struct poly_vec*) RAW(self);

  p_poly_vec->self = self;
  p_poly_vec->vec = proxy;

  switch (type) {
  case vctrs_type_null: init_nil_poly_vec(p_poly_vec); break;
  case vctrs_type_logical: init_lgl_poly_vec(p_poly_vec); break;
  case vctrs_type_integer: init_int_poly_vec(p_poly_vec); break;
  case vctrs_type_double: init_dbl_poly_vec(p_poly_vec); break;
  case vctrs_type_complex: init_cpl_poly_vec(p_poly_vec); break;
  case vctrs_type_character: init_chr_poly_vec(p_poly_vec); break;
  case vctrs_type_raw: init_raw_poly_vec(p_poly_vec); break;
  case vctrs_type_list: init_list_poly_vec(p_poly_vec); break;
  case vctrs_type_dataframe: init_df_poly_vec(p_poly_vec); break;
  default: stop_unimplemented_vctrs_type("new_poly_vec", type);
  }

  // `init_*_poly_vec()` functions may allocate
  PROTECT(p_poly_vec->self);

  UNPROTECT(2);
  return p_poly_vec;
}

static
void init_nil_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = NULL;
}
static
void init_lgl_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) LOGICAL_RO(p_poly_vec->vec);
}
static
void init_int_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) INTEGER_RO(p_poly_vec->vec);
}
static
void init_dbl_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) REAL_RO(p_poly_vec->vec);
}
static
void init_cpl_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) COMPLEX_RO(p_poly_vec->vec);
}
static
void init_chr_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) STRING_PTR_RO(p_poly_vec->vec);
}
static
void init_raw_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) RAW_RO(p_poly_vec->vec);
}
static
void init_list_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) VECTOR_PTR_RO(p_poly_vec->vec);
}
static
void init_df_poly_vec(struct poly_vec* p_poly_vec) {
  SEXP df = p_poly_vec->vec;
  r_ssize n_col = Rf_xlength(df);

  SEXP self = PROTECT(Rf_allocVector(VECSXP, 4));

  SET_VECTOR_ELT(self, 0, p_poly_vec->self);
  p_poly_vec->self = self;

  SEXP data_handle = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct poly_df_data)));
  struct poly_df_data* data = (struct poly_df_data*) RAW(data_handle);
  SET_VECTOR_ELT(self, 1, data_handle);

  SEXP col_type_handle = PROTECT(Rf_allocVector(RAWSXP, n_col * sizeof(enum vctrs_type)));
  enum vctrs_type* v_col_type = (enum vctrs_type*) RAW(col_type_handle);
  SET_VECTOR_ELT(self, 2, col_type_handle);

  SEXP col_ptr_handle = PROTECT(Rf_allocVector(RAWSXP, n_col * sizeof(void*)));
  const void** v_col_ptr = (const void**) RAW(col_ptr_handle);
  SET_VECTOR_ELT(self, 3, col_ptr_handle);

  for (r_ssize i = 0; i < n_col; ++i) {
    SEXP col = VECTOR_ELT(df, i);
    v_col_type[i] = vec_proxy_typeof(col);
    v_col_ptr[i] = r_vec_cbegin(col);
  }

  data->v_col_type = v_col_type;
  data->v_col_ptr = v_col_ptr;
  data->n_col = n_col;

  p_poly_vec->p_vec = (void*) data;

  UNPROTECT(4);
}
