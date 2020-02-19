#include "vctrs.h"
#include "dictionary.h"
#include "equal.h"
#include "utils.h"

// Initialised at load time
struct vctrs_arg args_needles;
struct vctrs_arg args_haystack;


// http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
int32_t ceil2(int32_t x) {
  x--;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  x++;
  return x;
}

// Dictonary object ------------------------------------------------------------

static struct dictionary* new_dictionary_impl(SEXP x, bool partial);


static int nil_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  Rf_error("Internal error: Shouldn't compare NULL in dictionary.");
}
static int lgl_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  return lgl_equal_scalar(((const int*) x) + i, ((const int*) y) + j, true);
}
static int int_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  return int_equal_scalar(((const int*) x) + i, ((const int*) y) + j, true);
}
static int dbl_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  return dbl_equal_scalar(((const double*) x) + i, ((const double*) y) + j, true);
}
static int cpl_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  return cpl_equal_scalar(((const Rcomplex*) x) + i, ((const Rcomplex*) y) + j, true);
}
static int chr_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  return chr_equal_scalar(((const SEXP*) x) + i, ((const SEXP*) y) + j, true);
}
static int raw_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  return raw_equal_scalar(((const Rbyte*) x) + i, ((const Rbyte*) y) + j, true);
}
static int list_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  return list_equal_scalar(((const SEXP) x), i, ((const SEXP) y), j, true);
}


static void init_dictionary_nil(struct dictionary* d) {
  d->vec_p = NULL;
  d->equal = &nil_equal;
}
static void init_dictionary_lgl(struct dictionary* d) {
  d->vec_p = (const void*) LOGICAL_RO(d->vec);
  d->equal = &lgl_equal;
}
static void init_dictionary_int(struct dictionary* d) {
  d->vec_p = (const void*) INTEGER_RO(d->vec);
  d->equal = &int_equal;
}
static void init_dictionary_dbl(struct dictionary* d) {
  d->vec_p = (const void*) REAL_RO(d->vec);
  d->equal = dbl_equal;
}
static void init_dictionary_cpl(struct dictionary* d) {
  d->vec_p = (const void*) COMPLEX_RO(d->vec);
  d->equal = &cpl_equal;
}
static void init_dictionary_chr(struct dictionary* d) {
  d->vec_p = (const void*) STRING_PTR_RO(d->vec);
  d->equal = &chr_equal;
}
static void init_dictionary_raw(struct dictionary* d) {
  d->vec_p = (const void*) RAW_RO(d->vec);
  d->equal = &raw_equal;
}
static void init_dictionary_list(struct dictionary* d) {
  d->vec_p = (const void*) d->vec;
  d->equal = &list_equal;
}

struct dictionary_df_data {
  enum vctrs_type* col_types;
  void** col_ptrs;
  R_len_t n_col;
};

static int df_equal(const void* x, R_len_t i, const void* y, R_len_t j) {
  struct dictionary_df_data* x_data = (struct dictionary_df_data*) x;
  struct dictionary_df_data* y_data = (struct dictionary_df_data*) y;

  R_len_t n_col = x_data->n_col;
  if (n_col != y_data->n_col) {
    Rf_errorcall(R_NilValue, "Internal error: `x` and `y` must have the same number of columns.");
  }

  enum vctrs_type* types = x_data->col_types;
  void** x_ptrs = x_data->col_ptrs;
  void** y_ptrs = y_data->col_ptrs;

  // `vec_proxy_equal()` flattens data frames so we don't need to
  // worry about df-cols
  for (R_len_t col = 0; col < n_col; ++col) {
    if (!equal_scalar_p(types[col],
                        R_NilValue, x_ptrs[col], i,
                        R_NilValue, y_ptrs[col], j,
                        true)) {
      return false;
    }
  }

  return true;
}
static void init_dictionary_df(struct dictionary* d) {
  SEXP df = d->vec;
  R_len_t n_col = Rf_length(df);

  SEXP data_handle = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct dictionary_df_data)));
  SEXP col_types_handle = PROTECT(Rf_allocVector(RAWSXP, n_col * sizeof(enum vctrs_type)));
  SEXP col_ptrs_handle = PROTECT(Rf_allocVector(RAWSXP, n_col * sizeof(void*)));

  SEXP handle = PROTECT(Rf_allocVector(VECSXP, 4));
  SET_VECTOR_ELT(handle, 0, d->protect);
  SET_VECTOR_ELT(handle, 1, data_handle);
  SET_VECTOR_ELT(handle, 2, col_types_handle);
  SET_VECTOR_ELT(handle, 3, col_ptrs_handle);


  struct dictionary_df_data* data = (struct dictionary_df_data*) RAW(data_handle);
  enum vctrs_type* col_types = (enum vctrs_type*) RAW(col_types_handle);
  void** col_ptrs = (void**) RAW(col_ptrs_handle);

  data->col_types = col_types;
  data->col_ptrs = col_ptrs;
  data->n_col = n_col;

  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP col = VECTOR_ELT(df, i);
    enum vctrs_type col_type = vec_proxy_typeof(col);

    col_types[i] = col_type;

    if (col_type == vctrs_type_list) {
      col_ptrs[i] = col;
    } else {
      col_ptrs[i] = r_vec_deref(col);
    }
  }

  d->protect = handle;
  d->vec_p = data;
  d->equal = df_equal;

  UNPROTECT(4);
}


// Dictionaries must be protected in consistent stack order with
// `PROTECT_DICT()`
struct dictionary* new_dictionary(SEXP x) {
  return new_dictionary_impl(x, false);
}
struct dictionary* new_dictionary_partial(SEXP x) {
  return new_dictionary_impl(x, true);
}

static struct dictionary* new_dictionary_impl(SEXP x, bool partial) {
  SEXP out = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct dictionary)));
  struct dictionary* d = (struct dictionary*) RAW(out);

  d->vec = x;
  d->type = vec_proxy_typeof(x);
  d->protect = out;

  switch (d->type) {
  case vctrs_type_null: init_dictionary_nil(d); break;
  case vctrs_type_logical: init_dictionary_lgl(d); break;
  case vctrs_type_integer: init_dictionary_int(d); break;
  case vctrs_type_double: init_dictionary_dbl(d); break;
  case vctrs_type_complex: init_dictionary_cpl(d); break;
  case vctrs_type_character: init_dictionary_chr(d); break;
  case vctrs_type_raw: init_dictionary_raw(d); break;
  case vctrs_type_list: init_dictionary_list(d); break;
  case vctrs_type_dataframe: init_dictionary_df(d); break;
  default: Rf_error("Internal error: Unimplemented type in `new_dictionary()`.");
  }

  d->used = 0;

  if (partial) {
    d->key = NULL;
    d->size = 0;
  } else {
    // assume worst case, that every value is distinct, aiming for a load factor
    // of at most 77%. We round up to power of 2 to ensure quadratic probing
    // strategy works.
    // Rprintf("size: %i\n", size);
    R_len_t size = ceil2(vec_size(x) / 0.77);
    size = (size < 16) ? 16 : size;

    d->key = (R_len_t*) R_alloc(size, sizeof(R_len_t));
    memset(d->key, DICT_EMPTY, size * sizeof(R_len_t));

    d->size = size;
  }

  R_len_t n = vec_size(x);
  if (n) {
    d->hash = (uint32_t*) R_alloc(n, sizeof(uint32_t));

    if (!(d->hash)) {
      Rf_errorcall(R_NilValue, "Can't allocate hash lookup table. Please free memory.");
    }

    memset(d->hash, 0, n * sizeof(R_len_t));
    hash_fill(d->hash, n, x);
  } else {
    d->hash = NULL;
  }

  UNPROTECT(1);
  return d;
}


// Use hash from `x` but value from `d`. `x` does not need a full
// initialisation of the key vector and can be created with
// `new_dictionary_partial()`.
uint32_t dict_hash_with(struct dictionary* d, struct dictionary* x, R_len_t i) {
  uint32_t hash = x->hash[i];

  // Quadratic probing: will try every slot if d->size is power of 2
  // http://research.cs.vt.edu/AVresearch/hashing/quadratic.php
  for (uint32_t k = 0; k < d->size; ++k) {
    uint32_t probe = (hash + k * (k + 1) / 2) & (d->size - 1);
    // Rprintf("Probe: %i\n", probe);

    // If we circled back to start, dictionary is full
    if (k > 1 && probe == hash) {
      break;
    }

    // Check for unused slot
    R_len_t idx = d->key[probe];
    if (idx == DICT_EMPTY) {
      return probe;
    }

    // Check for same value as there might be a collision
    if (d->equal(d->vec_p, idx, x->vec_p, i)) {
      return probe;
    }

    // Collision. next iteration will find another spot using
    // quadratic probing.
  }

  Rf_errorcall(R_NilValue, "Internal error: Dictionary is full!");
}

uint32_t dict_hash_scalar(struct dictionary* d, R_len_t i) {
  return dict_hash_with(d, d, i);
}


void dict_put(struct dictionary* d, uint32_t hash, R_len_t i) {
  d->key[hash] = i;
  d->used++;
}

// R interface -----------------------------------------------------------------
// TODO: rename to match R function names
// TODO: separate out into individual files

SEXP vctrs_unique_loc(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  struct growable g = new_growable(INTSXP, 256);
  PROTECT_GROWABLE(&g, &nprot);

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
      growable_push_int(&g, i + 1);
    }
  }

  SEXP out = growable_values(&g);

  UNPROTECT(nprot);
  return out;
}

// [[ include("vctrs.h") ]]
SEXP vec_unique(SEXP x) {
  SEXP index = PROTECT(vctrs_unique_loc(x));
  SEXP out = vec_slice(x, index);
  UNPROTECT(1);
  return out;
}

SEXP vctrs_duplicated_any(SEXP x) {
  bool out = duplicated_any(x);
  return Rf_ScalarLogical(out);
}

// [[ include("vctrs.h") ]]
bool duplicated_any(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  bool out = false;

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    } else {
      out = true;
      break;
    }
  }

  UNPROTECT(nprot);
  return out;
}

SEXP vctrs_n_distinct(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    }
  }

  UNPROTECT(nprot);
  return Rf_ScalarInteger(d->used);
}

SEXP vctrs_id(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, n), &nprot);
  int* p_out = INTEGER(out);

  for (int i = 0; i < n; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    }
    p_out[i] = d->key[hash] + 1;
  }

  UNPROTECT(nprot);
  return out;
}

// [[ register() ]]
SEXP vec_match(SEXP needles, SEXP haystack) {
  int nprot = 0;
  int _;
  SEXP type = PROTECT_N(vec_type2(needles, haystack, &args_needles, &args_haystack, &_), &nprot);

  needles = PROTECT_N(vec_cast(needles, type, args_empty, args_empty), &nprot);
  haystack = PROTECT_N(vec_cast(haystack, type, args_empty, args_empty), &nprot);

  needles = PROTECT_N(vec_proxy_equal(needles), &nprot);
  haystack = PROTECT_N(vec_proxy_equal(haystack), &nprot);

  R_len_t n_haystack = vec_size(haystack);
  R_len_t n_needle = vec_size(needles);

  SEXP translated = PROTECT_N(obj_maybe_translate_encoding2(needles, n_needle, haystack, n_haystack), &nprot);
  needles = VECTOR_ELT(translated, 0);
  haystack = VECTOR_ELT(translated, 1);

  struct dictionary* d = new_dictionary(haystack);
  PROTECT_DICT(d, &nprot);

  // Load dictionary with haystack
  for (int i = 0; i < n_haystack; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    }
  }

  struct dictionary* d_needles = new_dictionary_partial(needles);

  // Locate needles
  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, n_needle), &nprot);
  int* p_out = INTEGER(out);

  for (int i = 0; i < n_needle; ++i) {
    uint32_t hash = dict_hash_with(d, d_needles, i);
    if (d->key[hash] == DICT_EMPTY) {
      p_out[i] = NA_INTEGER;
    } else {
      p_out[i] = d->key[hash] + 1;
    }
  }

  UNPROTECT(nprot);
  return out;
}

// [[ register() ]]
SEXP vctrs_in(SEXP needles, SEXP haystack) {
  int nprot = 0;

  int _;
  SEXP type = PROTECT_N(vec_type2(needles, haystack, &args_needles, &args_haystack, &_), &nprot);

  needles = PROTECT_N(vec_cast(needles, type, args_empty, args_empty), &nprot);
  haystack = PROTECT_N(vec_cast(haystack, type, args_empty, args_empty), &nprot);

  needles = PROTECT_N(vec_proxy_equal(needles), &nprot);
  haystack = PROTECT_N(vec_proxy_equal(haystack), &nprot);

  R_len_t n_haystack = vec_size(haystack);
  R_len_t n_needle = vec_size(needles);

  SEXP translated = PROTECT_N(obj_maybe_translate_encoding2(needles, n_needle, haystack, n_haystack), &nprot);
  needles = VECTOR_ELT(translated, 0);
  haystack = VECTOR_ELT(translated, 1);

  struct dictionary* d = new_dictionary(haystack);
  PROTECT_DICT(d, &nprot);

  // Load dictionary with haystack
  for (int i = 0; i < n_haystack; ++i) {
    uint32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
    }
  }

  struct dictionary* d_needles = new_dictionary_partial(needles);
  PROTECT_DICT(d_needles, &nprot);

  // Locate needles
  SEXP out = PROTECT_N(Rf_allocVector(LGLSXP, n_needle), &nprot);
  int* p_out = LOGICAL(out);

  for (int i = 0; i < n_needle; ++i) {
    uint32_t hash = dict_hash_with(d, d_needles, i);
    p_out[i] = (d->key[hash] != DICT_EMPTY);
  }

  UNPROTECT(nprot);
  return out;
}

SEXP vctrs_count(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  SEXP val = PROTECT_N(Rf_allocVector(INTSXP, d->size), &nprot);
  int* p_val = INTEGER(val);

  for (int i = 0; i < n; ++i) {
    int32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
      p_val[hash] = 0;
    }
    p_val[hash]++;
  }

  // Create output
  SEXP out_key = PROTECT_N(Rf_allocVector(INTSXP, d->used), &nprot);
  SEXP out_val = PROTECT_N(Rf_allocVector(INTSXP, d->used), &nprot);
  int* p_out_key = INTEGER(out_key);
  int* p_out_val = INTEGER(out_val);

  int i = 0;
  for (int hash = 0; hash < d->size; ++hash) {
    if (d->key[hash] == DICT_EMPTY)
      continue;

    p_out_key[i] = d->key[hash] + 1;
    p_out_val[i] = p_val[hash];
    i++;
  }

  SEXP out = PROTECT_N(Rf_allocVector(VECSXP, 2), &nprot);
  SET_VECTOR_ELT(out, 0, out_key);
  SET_VECTOR_ELT(out, 1, out_val);
  SEXP names = PROTECT_N(Rf_allocVector(STRSXP, 2), &nprot);
  SET_STRING_ELT(names, 0, Rf_mkChar("key"));
  SET_STRING_ELT(names, 1, Rf_mkChar("val"));
  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(nprot);
  return out;
}

SEXP vctrs_duplicated(SEXP x) {
  int nprot = 0;

  R_len_t n = vec_size(x);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, n), &nprot);

  struct dictionary* d = new_dictionary(x);
  PROTECT_DICT(d, &nprot);

  SEXP val = PROTECT_N(Rf_allocVector(INTSXP, d->size), &nprot);
  int* p_val = INTEGER(val);

  for (int i = 0; i < n; ++i) {
    int32_t hash = dict_hash_scalar(d, i);

    if (d->key[hash] == DICT_EMPTY) {
      dict_put(d, hash, i);
      p_val[hash] = 0;
    }
    p_val[hash]++;
  }

  // Create output
  SEXP out = PROTECT_N(Rf_allocVector(LGLSXP, n), &nprot);
  int* p_out = LOGICAL(out);

  for (int i = 0; i < n; ++i) {
    int32_t hash = dict_hash_scalar(d, i);
    p_out[i] = p_val[hash] != 1;
  }

  UNPROTECT(nprot);
  return out;
}


void vctrs_init_dictionary(SEXP ns) {
  args_needles = new_wrapper_arg(NULL, "needles");
  args_haystack = new_wrapper_arg(NULL, "haystack");
}
