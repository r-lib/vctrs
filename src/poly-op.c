#include "poly-op.h"
#include "vctrs.h"
#include "equal.h"
#include "utils.h"

// -----------------------------------------------------------------------------

struct poly_df_data {
  enum vctrs_type* col_types;
  const void** col_ptrs;
  R_len_t n_col;
};

// -----------------------------------------------------------------------------

static int nil_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int lgl_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int int_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int dbl_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int cpl_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int chr_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int raw_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int list_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int df_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);

// [[ include("poly-op.h") ]]
poly_binary_op_fn_t new_poly_op_equal_scalar_na_equal_p(SEXP proxy) {
  enum vctrs_type type = vec_proxy_typeof(proxy);

  switch (type) {
  case vctrs_type_null: return nil_equal_scalar_na_equal_p;
  case vctrs_type_logical: return lgl_equal_scalar_na_equal_p;
  case vctrs_type_integer: return int_equal_scalar_na_equal_p;
  case vctrs_type_double: return dbl_equal_scalar_na_equal_p;
  case vctrs_type_complex: return cpl_equal_scalar_na_equal_p;
  case vctrs_type_character: return chr_equal_scalar_na_equal_p;
  case vctrs_type_raw: return raw_equal_scalar_na_equal_p;
  case vctrs_type_list: return list_equal_scalar_na_equal_p;
  case vctrs_type_dataframe: return df_equal_scalar_na_equal_p;
  default: stop_unimplemented_vctrs_type("new_poly_equal_scalar_na_equal_p", type);
  }
}

static
int nil_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  stop_internal("nil_equal_scalar_na_equal_p", "Can't compare NULL.");
}
static
int lgl_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  return lgl_equal_scalar_na_equal(((const int*) x) + i, ((const int*) y) + j);
}
static
int int_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  return int_equal_scalar_na_equal(((const int*) x) + i, ((const int*) y) + j);
}
static
int dbl_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  return dbl_equal_scalar_na_equal(((const double*) x) + i, ((const double*) y) + j);
}
static
int cpl_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  return cpl_equal_scalar_na_equal(((const Rcomplex*) x) + i, ((const Rcomplex*) y) + j);
}
static
int chr_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  return chr_equal_scalar_na_equal(((const SEXP*) x) + i, ((const SEXP*) y) + j);
}
static
int raw_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  return raw_equal_scalar_na_equal(((const Rbyte*) x) + i, ((const Rbyte*) y) + j);
}
static
int list_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  return list_equal_scalar_na_equal(((const SEXP) x), i, ((const SEXP) y), j);
}
static
int df_equal_scalar_na_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  struct poly_df_data* x_data = (struct poly_df_data*) x;
  struct poly_df_data* y_data = (struct poly_df_data*) y;

  R_len_t n_col = x_data->n_col;
  if (n_col != y_data->n_col) {
    stop_internal("df_equal_scalar_na_equal_p", "`x` and `y` must have the same number of columns.");
  }

  enum vctrs_type* types = x_data->col_types;
  const void** x_ptrs = x_data->col_ptrs;
  const void** y_ptrs = y_data->col_ptrs;

  // `vec_proxy_equal()` flattens data frames so we don't need to
  // worry about df-cols
  for (R_len_t col = 0; col < n_col; ++col) {
    int eq = equal_scalar_na_equal_p(
      types[col],
      R_NilValue, x_ptrs[col], i,
      R_NilValue, y_ptrs[col], j
    );

    if (!eq) {
      return false;
    }
  }

  return true;
}

// -----------------------------------------------------------------------------

static int nil_equal_missing_p(const void* x, R_len_t i);
static int lgl_equal_missing_p(const void* x, R_len_t i);
static int int_equal_missing_p(const void* x, R_len_t i);
static int dbl_equal_missing_p(const void* x, R_len_t i);
static int cpl_equal_missing_p(const void* x, R_len_t i);
static int chr_equal_missing_p(const void* x, R_len_t i);
static int raw_equal_missing_p(const void* x, R_len_t i);
static int list_equal_missing_p(const void* x, R_len_t i);
static int df_equal_missing_p(const void* x, R_len_t i);

// [[ include("poly-op.h") ]]
poly_unary_op_fn_t new_poly_op_equal_missing_p(SEXP proxy) {
  enum vctrs_type type = vec_proxy_typeof(proxy);

  switch (type) {
  case vctrs_type_null: return nil_equal_missing_p;
  case vctrs_type_logical: return lgl_equal_missing_p;
  case vctrs_type_integer: return int_equal_missing_p;
  case vctrs_type_double: return dbl_equal_missing_p;
  case vctrs_type_complex: return cpl_equal_missing_p;
  case vctrs_type_character: return chr_equal_missing_p;
  case vctrs_type_raw: return raw_equal_missing_p;
  case vctrs_type_list: return list_equal_missing_p;
  case vctrs_type_dataframe: return df_equal_missing_p;
  default: stop_unimplemented_vctrs_type("new_poly_equal_missing_p", type);
  }
}

static
int nil_equal_missing_p(const void* x, R_len_t i) {
  stop_internal("nil_equal_missing_p", "Can't compare NULL.");
}
static
int lgl_equal_missing_p(const void* x, R_len_t i) {
  return lgl_equal_scalar_na_equal(((const int*) x) + i, &NA_LOGICAL);
}
static
int int_equal_missing_p(const void* x, R_len_t i) {
  return int_equal_scalar_na_equal(((const int*) x) + i, &NA_INTEGER);
}
static
int dbl_equal_missing_p(const void* x, R_len_t i) {
  return dbl_equal_scalar_na_equal(((const double*) x) + i, &NA_REAL);
}
static
int cpl_equal_missing_p(const void* x, R_len_t i) {
  return cpl_equal_scalar_na_equal(((const Rcomplex*) x) + i, &vctrs_shared_na_cpl);
}
static
int chr_equal_missing_p(const void* x, R_len_t i) {
  return chr_equal_scalar_na_equal(((const SEXP*) x) + i, &NA_STRING);
}
static
int raw_equal_missing_p(const void* x, R_len_t i) {
  return false;
}
static
int list_equal_missing_p(const void* x, R_len_t i) {
  return list_equal_scalar_na_equal(((const SEXP) x), i, vctrs_shared_na_list, 0);
}
static
int df_equal_missing_p(const void* x, R_len_t i) {
  struct poly_df_data* x_data = (struct poly_df_data*) x;

  enum vctrs_type* types = x_data->col_types;
  const void** x_ptrs = x_data->col_ptrs;
  R_len_t n_col = x_data->n_col;

  for (R_len_t col = 0; col < n_col; ++col) {
    enum vctrs_type type = types[col];

    // Raw doesn't have missing values
    if (type == vctrs_type_raw) {
      continue;
    }

    int eq = equal_scalar_na_equal_p(
      type,
      R_NilValue, x_ptrs[col], i,
      R_NilValue, vec_type_missing_value(type), 0
    );

    if (eq) {
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
struct poly_vec* new_poly_vec(SEXP proxy) {
  SEXP self = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct poly_vec)));
  struct poly_vec* p_poly_vec = (struct poly_vec*) RAW(self);

  p_poly_vec->self = self;
  p_poly_vec->vec = proxy;

  enum vctrs_type type = vec_proxy_typeof(proxy);

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
  p_poly_vec->p_vec = (const void*) p_poly_vec->vec;
}
static
void init_df_poly_vec(struct poly_vec* p_poly_vec) {
  SEXP df = p_poly_vec->vec;
  R_len_t n_col = Rf_length(df);

  SEXP self = PROTECT(Rf_allocVector(VECSXP, 4));

  SET_VECTOR_ELT(self, 0, p_poly_vec->self);
  p_poly_vec->self = self;

  SEXP data_handle = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct poly_df_data)));
  struct poly_df_data* data = (struct poly_df_data*) RAW(data_handle);
  SET_VECTOR_ELT(self, 1, data_handle);

  SEXP col_types_handle = PROTECT(Rf_allocVector(RAWSXP, n_col * sizeof(enum vctrs_type)));
  enum vctrs_type* col_types = (enum vctrs_type*) RAW(col_types_handle);
  SET_VECTOR_ELT(self, 2, col_types_handle);

  SEXP col_ptrs_handle = PROTECT(Rf_allocVector(RAWSXP, n_col * sizeof(void*)));
  const void** col_ptrs = (const void**) RAW(col_ptrs_handle);
  SET_VECTOR_ELT(self, 3, col_ptrs_handle);

  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP col = VECTOR_ELT(df, i);
    enum vctrs_type col_type = vec_proxy_typeof(col);

    col_types[i] = col_type;

    if (col_type == vctrs_type_list) {
      col_ptrs[i] = (void*) col;
    } else {
      col_ptrs[i] = r_vec_deref_const(col);
    }
  }

  data->col_types = col_types;
  data->col_ptrs = col_ptrs;
  data->n_col = n_col;

  p_poly_vec->p_vec = (void*) data;

  UNPROTECT(4);
}
