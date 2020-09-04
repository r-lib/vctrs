#include "comparator.h"
#include "vctrs.h"
#include "equal.h"
#include "utils.h"

// -----------------------------------------------------------------------------

static void init_comparator_nil(struct comparator* p_comparator);
static void init_comparator_lgl(struct comparator* p_comparator);
static void init_comparator_int(struct comparator* p_comparator);
static void init_comparator_dbl(struct comparator* p_comparator);
static void init_comparator_cpl(struct comparator* p_comparator);
static void init_comparator_chr(struct comparator* p_comparator);
static void init_comparator_raw(struct comparator* p_comparator);
static void init_comparator_list(struct comparator* p_comparator);
static void init_comparator_df(struct comparator* p_comparator);

// [[ include("comparator.h") ]]
struct comparator* new_comparator(SEXP x) {
  SEXP self = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct comparator)));
  struct comparator* p_comparator = (struct comparator*) RAW(self);

  p_comparator->self = self;

  enum vctrs_type type = vec_proxy_typeof(x);

  switch (type) {
  case vctrs_type_null: init_comparator_nil(p_comparator); break;
  case vctrs_type_logical: init_comparator_lgl(p_comparator); break;
  case vctrs_type_integer: init_comparator_int(p_comparator); break;
  case vctrs_type_double: init_comparator_dbl(p_comparator); break;
  case vctrs_type_complex: init_comparator_cpl(p_comparator); break;
  case vctrs_type_character: init_comparator_chr(p_comparator); break;
  case vctrs_type_raw: init_comparator_raw(p_comparator); break;
  case vctrs_type_list: init_comparator_list(p_comparator); break;
  case vctrs_type_dataframe: init_comparator_df(p_comparator); break;
  default: stop_unimplemented_vctrs_type("new_comparator", type);
  }

  UNPROTECT(1);
  return p_comparator;
}


static int nil_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int nil_equal_missing_p(const void* x, R_len_t i);
static int lgl_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int lgl_equal_missing_p(const void* x, R_len_t i);
static int int_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int int_equal_missing_p(const void* x, R_len_t i);
static int dbl_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int dbl_equal_missing_p(const void* x, R_len_t i);
static int cpl_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int cpl_equal_missing_p(const void* x, R_len_t i);
static int chr_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int chr_equal_missing_p(const void* x, R_len_t i);
static int raw_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int raw_equal_missing_p(const void* x, R_len_t i);
static int list_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int list_equal_missing_p(const void* x, R_len_t i);
static int df_equal_p(const void* x, R_len_t i, const void* y, R_len_t j);
static int df_equal_missing_p(const void* x, R_len_t i);

static
void init_comparator_nil(struct comparator* p_comparator) {
  p_comparator->equal = &nil_equal_p;
  p_comparator->equal_missing = &nil_equal_missing_p;
}
static
void init_comparator_lgl(struct comparator* p_comparator) {
  p_comparator->equal = &lgl_equal_p;
  p_comparator->equal_missing = &lgl_equal_missing_p;
}
static
void init_comparator_int(struct comparator* p_comparator) {
  p_comparator->equal = &int_equal_p;
  p_comparator->equal_missing = &int_equal_missing_p;
}
static
void init_comparator_dbl(struct comparator* p_comparator) {
  p_comparator->equal = dbl_equal_p;
  p_comparator->equal_missing = &dbl_equal_missing_p;
}
static
void init_comparator_cpl(struct comparator* p_comparator) {
  p_comparator->equal = &cpl_equal_p;
  p_comparator->equal_missing = &cpl_equal_missing_p;
}
static
void init_comparator_chr(struct comparator* p_comparator) {
  p_comparator->equal = &chr_equal_p;
  p_comparator->equal_missing = &chr_equal_missing_p;
}
static
void init_comparator_raw(struct comparator* p_comparator) {
  p_comparator->equal = &raw_equal_p;
  p_comparator->equal_missing = &raw_equal_missing_p;
}
static
void init_comparator_list(struct comparator* p_comparator) {
  p_comparator->equal = &list_equal_p;
  p_comparator->equal_missing = &list_equal_missing_p;
}
static
void init_comparator_df(struct comparator* p_comparator) {
  p_comparator->equal = df_equal_p;
  p_comparator->equal_missing = &df_equal_missing_p;
}

// -----------------------------------------------------------------------------

static void init_comparator_vec_nil(struct comparator_vec* p_comparator_vec);
static void init_comparator_vec_lgl(struct comparator_vec* p_comparator_vec);
static void init_comparator_vec_int(struct comparator_vec* p_comparator_vec);
static void init_comparator_vec_dbl(struct comparator_vec* p_comparator_vec);
static void init_comparator_vec_cpl(struct comparator_vec* p_comparator_vec);
static void init_comparator_vec_chr(struct comparator_vec* p_comparator_vec);
static void init_comparator_vec_raw(struct comparator_vec* p_comparator_vec);
static void init_comparator_vec_list(struct comparator_vec* p_comparator_vec);
static void init_comparator_vec_df(struct comparator_vec* p_comparator_vec);

// [[ include("comparator.h") ]]
struct comparator_vec* new_comparator_vec(SEXP x) {
  SEXP self = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct comparator_vec)));
  struct comparator_vec* p_comparator_vec = (struct comparator_vec*) RAW(self);

  p_comparator_vec->self = self;
  p_comparator_vec->vec = x;

  enum vctrs_type type = vec_proxy_typeof(x);

  switch (type) {
  case vctrs_type_null: init_comparator_vec_nil(p_comparator_vec); break;
  case vctrs_type_logical: init_comparator_vec_lgl(p_comparator_vec); break;
  case vctrs_type_integer: init_comparator_vec_int(p_comparator_vec); break;
  case vctrs_type_double: init_comparator_vec_dbl(p_comparator_vec); break;
  case vctrs_type_complex: init_comparator_vec_cpl(p_comparator_vec); break;
  case vctrs_type_character: init_comparator_vec_chr(p_comparator_vec); break;
  case vctrs_type_raw: init_comparator_vec_raw(p_comparator_vec); break;
  case vctrs_type_list: init_comparator_vec_list(p_comparator_vec); break;
  case vctrs_type_dataframe: init_comparator_vec_df(p_comparator_vec); break;
  default: stop_unimplemented_vctrs_type("new_comparator_vec", type);
  }

  // `init_comparator_vec_*()` functions may allocate
  PROTECT(p_comparator_vec->self);

  UNPROTECT(2);
  return p_comparator_vec;
}


static
void init_comparator_vec_nil(struct comparator_vec* p_comparator_vec) {
  p_comparator_vec->vec_p = NULL;
}
static
void init_comparator_vec_lgl(struct comparator_vec* p_comparator_vec) {
  p_comparator_vec->vec_p = (const void*) LOGICAL_RO(p_comparator_vec->vec);
}
static
void init_comparator_vec_int(struct comparator_vec* p_comparator_vec) {
  p_comparator_vec->vec_p = (const void*) INTEGER_RO(p_comparator_vec->vec);
}
static
void init_comparator_vec_dbl(struct comparator_vec* p_comparator_vec) {
  p_comparator_vec->vec_p = (const void*) REAL_RO(p_comparator_vec->vec);
}
static
void init_comparator_vec_cpl(struct comparator_vec* p_comparator_vec) {
  p_comparator_vec->vec_p = (const void*) COMPLEX_RO(p_comparator_vec->vec);
}
static
void init_comparator_vec_chr(struct comparator_vec* p_comparator_vec) {
  p_comparator_vec->vec_p = (const void*) STRING_PTR_RO(p_comparator_vec->vec);
}
static
void init_comparator_vec_raw(struct comparator_vec* p_comparator_vec) {
  p_comparator_vec->vec_p = (const void*) RAW_RO(p_comparator_vec->vec);
}
static
void init_comparator_vec_list(struct comparator_vec* p_comparator_vec) {
  p_comparator_vec->vec_p = (const void*) p_comparator_vec->vec;
}

struct comparator_df_data {
  enum vctrs_type* col_types;
  const void** col_ptrs;
  R_len_t n_col;
};

static
void init_comparator_vec_df(struct comparator_vec* p_comparator_vec) {
  SEXP df = p_comparator_vec->vec;
  R_len_t n_col = Rf_length(df);

  SEXP self = PROTECT(Rf_allocVector(VECSXP, 4));

  SET_VECTOR_ELT(self, 0, p_comparator_vec->self);
  p_comparator_vec->self = self;

  SEXP data_handle = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct comparator_df_data)));
  struct comparator_df_data* data = (struct comparator_df_data*) RAW(data_handle);
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

  p_comparator_vec->vec_p = (void*) data;

  UNPROTECT(4);
}

// -----------------------------------------------------------------------------

static
int nil_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  stop_internal("nil_equal_p", "Can't compare NULL.");
}
static
int nil_equal_missing_p(const void* x, R_len_t i) {
  stop_internal("nil_equal_missing_p", "Can't compare NULL.");
}

static
int lgl_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  return lgl_equal_scalar_na_equal(((const int*) x) + i, ((const int*) y) + j);
}
static
int lgl_equal_missing_p(const void* x, R_len_t i) {
  return lgl_equal_scalar_na_equal(((const int*) x) + i, &NA_LOGICAL);
}

static
int int_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  return int_equal_scalar_na_equal(((const int*) x) + i, ((const int*) y) + j);
}
static
int int_equal_missing_p(const void* x, R_len_t i) {
  return int_equal_scalar_na_equal(((const int*) x) + i, &NA_INTEGER);
}

static
int dbl_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  return dbl_equal_scalar_na_equal(((const double*) x) + i, ((const double*) y) + j);
}
static
int dbl_equal_missing_p(const void* x, R_len_t i) {
  return dbl_equal_scalar_na_equal(((const double*) x) + i, &NA_REAL);
}

static
int cpl_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  return cpl_equal_scalar_na_equal(((const Rcomplex*) x) + i, ((const Rcomplex*) y) + j);
}
static
int cpl_equal_missing_p(const void* x, R_len_t i) {
  return cpl_equal_scalar_na_equal(((const Rcomplex*) x) + i, &vctrs_shared_na_cpl);
}

static
int chr_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  return chr_equal_scalar_na_equal(((const SEXP*) x) + i, ((const SEXP*) y) + j);
}
static
int chr_equal_missing_p(const void* x, R_len_t i) {
  return chr_equal_scalar_na_equal(((const SEXP*) x) + i, &NA_STRING);
}

static
int raw_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  return raw_equal_scalar_na_equal(((const Rbyte*) x) + i, ((const Rbyte*) y) + j);
}
static
int raw_equal_missing_p(const void* x, R_len_t i) {
  return false;
}

static
int list_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  return list_equal_scalar_na_equal(((const SEXP) x), i, ((const SEXP) y), j);
}
static
int list_equal_missing_p(const void* x, R_len_t i) {
  return list_equal_scalar_na_equal(((const SEXP) x), i, vctrs_shared_na_list, 0);
}

static
int df_equal_p(const void* x, R_len_t i, const void* y, R_len_t j) {
  struct comparator_df_data* x_data = (struct comparator_df_data*) x;
  struct comparator_df_data* y_data = (struct comparator_df_data*) y;

  R_len_t n_col = x_data->n_col;
  if (n_col != y_data->n_col) {
    stop_internal("df_equal_p", "`x` and `y` must have the same number of columns.");
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
static
int df_equal_missing_p(const void* x, R_len_t i) {
  struct comparator_df_data* x_data = (struct comparator_df_data*) x;

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
