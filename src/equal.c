#include "vctrs.h"

#include "decl/equal-decl.h"

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP ffi_vec_equal(
  SEXP ffi_x,
  SEXP ffi_y,
  SEXP ffi_na_equal,
  SEXP ffi_ptype,
  SEXP ffi_frame
) {
  struct r_lazy error_call = { .x = ffi_frame, .env = r_null };

  const bool na_equal = r_arg_as_bool(ffi_na_equal, "na_equal");

  return vec_equal(
    ffi_x,
    ffi_y,
    na_equal,
    ffi_ptype,
    vec_args.x,
    vec_args.y,
    error_call
  );
}

static inline SEXP lgl_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);
static inline SEXP int_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);
static inline SEXP dbl_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);
static inline SEXP cpl_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);
static inline SEXP chr_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);
static inline SEXP raw_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);
static inline SEXP list_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);
static inline SEXP df_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal);

SEXP vec_equal(
  SEXP x,
  SEXP y,
  bool na_equal,
  SEXP ptype,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy error_call
) {
  SEXP args = PROTECT(r_alloc_list(2));
  r_list_poke(args, 0, x);
  r_list_poke(args, 1, y);

  SEXP names = r_alloc_character(2);
  r_attrib_poke_names(args, names);
  SEXP x_name = PROTECT(vctrs_arg(p_x_arg));
  SEXP y_name = PROTECT(vctrs_arg(p_y_arg));
  r_chr_poke(names, 0, r_chr_get(x_name, 0));
  r_chr_poke(names, 1, r_chr_get(y_name, 0));

  args = PROTECT(vec_cast_common(args, ptype, vec_args.empty, error_call));

  const R_len_t size = vec_size_common(args, -1, vec_args.empty, error_call);

  x = r_list_get(args, 0);
  y = r_list_get(args, 1);

  const bool x_recycles = vec_size(x) == 1;
  const bool y_recycles = vec_size(y) == 1;

  SEXP x_proxy = PROTECT(vec_proxy_equal(x));
  SEXP y_proxy = PROTECT(vec_proxy_equal(y));

  x_proxy = PROTECT(obj_encode_utf8(x_proxy));
  y_proxy = PROTECT(obj_encode_utf8(y_proxy));

  enum vctrs_type type = vec_proxy_typeof(x_proxy);

  SEXP out;

  switch (type) {
  case VCTRS_TYPE_logical: out = lgl_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_integer: out = int_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_double: out = dbl_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_complex: out = cpl_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_character: out = chr_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_raw: out = raw_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_list: out = list_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_dataframe: out = df_equal(x_proxy, y_proxy, size, x_recycles, y_recycles, na_equal); break;
  case VCTRS_TYPE_scalar: r_abort_lazy_call(error_call, "Can't compare scalars with `vec_equal()`.");
  default: stop_unimplemented_vctrs_type("vec_equal", type);
  }

  UNPROTECT(8);
  return out;
}

// -----------------------------------------------------------------------------

#define EQUAL_IMPL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL, EQUAL_NA_PROPAGATE, X_I, Y_I) \
  SEXP out = PROTECT(r_new_logical(size));                                           \
  int* p_out = LOGICAL(out);                                                         \
                                                                                     \
  const CTYPE* p_x = CONST_DEREF(x);                                                 \
  const CTYPE* p_y = CONST_DEREF(y);                                                 \
                                                                                     \
  if (na_equal) {                                                                    \
    for (R_len_t i = 0; i < size; ++i) {                                             \
      p_out[i] = EQUAL_NA_EQUAL(p_x[X_I], p_y[Y_I]);                                 \
    }                                                                                \
  } else {                                                                           \
    for (R_len_t i = 0; i < size; ++i) {                                             \
      p_out[i] = EQUAL_NA_PROPAGATE(p_x[X_I], p_y[Y_I]);                             \
    }                                                                                \
  }                                                                                  \
                                                                                     \
  UNPROTECT(1);                                                                      \
  return out

#define EQUAL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL, EQUAL_NA_PROPAGATE)            \
  if (x_recycles) {                                                              \
    if (y_recycles) {                                                            \
      EQUAL_IMPL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL, EQUAL_NA_PROPAGATE, 0, 0);  \
    } else {                                                                     \
      EQUAL_IMPL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL, EQUAL_NA_PROPAGATE, 0, i);  \
    }                                                                            \
  } else {                                                                       \
    if (y_recycles) {                                                            \
      EQUAL_IMPL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL, EQUAL_NA_PROPAGATE, i, 0);  \
    } else {                                                                     \
      EQUAL_IMPL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL, EQUAL_NA_PROPAGATE, i, i);  \
    }                                                                            \
  }


static inline
SEXP lgl_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  EQUAL(int, LOGICAL_RO, lgl_equal_na_equal, lgl_equal_na_propagate);
}
static inline
SEXP int_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  EQUAL(int, INTEGER_RO, int_equal_na_equal, int_equal_na_propagate);
}
static inline
SEXP dbl_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  EQUAL(double, REAL_RO, dbl_equal_na_equal, dbl_equal_na_propagate);
}
static inline
SEXP cpl_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  EQUAL(Rcomplex, COMPLEX_RO, cpl_equal_na_equal, cpl_equal_na_propagate);
}
static inline
SEXP chr_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  EQUAL(SEXP, STRING_PTR_RO, chr_equal_na_equal, chr_equal_na_propagate);
}
static inline
SEXP raw_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  EQUAL(Rbyte, RAW_RO, raw_equal_na_equal, raw_equal_na_propagate);
}
static inline
SEXP list_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  EQUAL(SEXP, VECTOR_PTR_RO, list_equal_na_equal, list_equal_na_propagate);
}

#undef EQUAL
#undef EQUAL_IMPL

// -----------------------------------------------------------------------------

static
void vec_equal_col_na_equal(
  SEXP x,
  SEXP y,
  bool x_recycles,
  bool y_recycles,
  int* p_out,
  struct df_short_circuit_info* p_info
);

static
void vec_equal_col_na_propagate(
  SEXP x,
  SEXP y,
  bool x_recycles,
  bool y_recycles,
  int* p_out,
  struct df_short_circuit_info* p_info
);

static
SEXP df_equal(SEXP x, SEXP y, R_len_t size, bool x_recycles, bool y_recycles, bool na_equal) {
  int nprot = 0;

  SEXP out = PROTECT_N(r_new_logical(size), &nprot);
  int* p_out = LOGICAL(out);

  // Initialize to "equality" value
  // and only change if we learn that it differs
  for (R_len_t i = 0; i < size; ++i) {
    p_out[i] = 1;
  }

  struct df_short_circuit_info info = new_df_short_circuit_info(size, false);
  struct df_short_circuit_info* p_info = &info;
  PROTECT_DF_SHORT_CIRCUIT_INFO(p_info, &nprot);

  R_len_t n_col = Rf_length(x);

  if (n_col != Rf_length(y)) {
    Rf_errorcall(R_NilValue, "`x` and `y` must have the same number of columns");
  }

  void (*vec_equal_col)(SEXP, SEXP, bool, bool, int*, struct df_short_circuit_info*);

  if (na_equal) {
    vec_equal_col = vec_equal_col_na_equal;
  } else {
    vec_equal_col = vec_equal_col_na_propagate;
  }

  const SEXP* p_x = VECTOR_PTR_RO(x);
  const SEXP* p_y = VECTOR_PTR_RO(y);

  for (R_len_t i = 0; i < n_col; ++i) {
    vec_equal_col(p_x[i], p_y[i], x_recycles, y_recycles, p_out, p_info);

    if (p_info->remaining == 0) {
      break;
    }
  }

  UNPROTECT(nprot);
  return out;
}

// -----------------------------------------------------------------------------

#define EQUAL_COL_IMPL(CTYPE, CONST_DEREF, EQUAL, X_I, Y_I) do { \
  const CTYPE* p_x = CONST_DEREF(x);                             \
  const CTYPE* p_y = CONST_DEREF(y);                             \
                                                                 \
  for (R_len_t i = 0; i < p_info->size; ++i) {                   \
    if (p_info->p_row_known[i]) {                                \
      continue;                                                  \
    }                                                            \
                                                                 \
    int eq = EQUAL(p_x[X_I], p_y[Y_I]);                          \
                                                                 \
    if (eq <= 0) {                                               \
      p_out[i] = eq;                                             \
      p_info->p_row_known[i] = true;                             \
      --p_info->remaining;                                       \
                                                                 \
      if (p_info->remaining == 0) {                              \
        break;                                                   \
      }                                                          \
    }                                                            \
  }                                                              \
} while (0)

#define EQUAL_COL(CTYPE, CONST_DEREF, EQUAL)            \
  if (x_recycles) {                                     \
    if (y_recycles) {                                   \
      EQUAL_COL_IMPL(CTYPE, CONST_DEREF, EQUAL, 0, 0);  \
    } else {                                            \
      EQUAL_COL_IMPL(CTYPE, CONST_DEREF, EQUAL, 0, i);  \
    }                                                   \
  } else {                                              \
    if (y_recycles) {                                   \
      EQUAL_COL_IMPL(CTYPE, CONST_DEREF, EQUAL, i, 0);  \
    } else {                                            \
      EQUAL_COL_IMPL(CTYPE, CONST_DEREF, EQUAL, i, i);  \
    }                                                   \
  }

static
void vec_equal_col_na_equal(
  SEXP x,
  SEXP y,
  bool x_recycles,
  bool y_recycles,
  int* p_out,
  struct df_short_circuit_info* p_info
) {
  switch (vec_proxy_typeof(x)) {
  case VCTRS_TYPE_logical: EQUAL_COL(int, LOGICAL_RO, lgl_equal_na_equal); break;
  case VCTRS_TYPE_integer: EQUAL_COL(int, INTEGER_RO, int_equal_na_equal); break;
  case VCTRS_TYPE_double: EQUAL_COL(double, REAL_RO, dbl_equal_na_equal); break;
  case VCTRS_TYPE_complex: EQUAL_COL(Rcomplex, COMPLEX_RO, cpl_equal_na_equal); break;
  case VCTRS_TYPE_character: EQUAL_COL(SEXP, STRING_PTR_RO, chr_equal_na_equal); break;
  case VCTRS_TYPE_raw: EQUAL_COL(Rbyte, RAW_RO, raw_equal_na_equal); break;
  case VCTRS_TYPE_list: EQUAL_COL(SEXP, VECTOR_PTR_RO, list_equal_na_equal); break;
  case VCTRS_TYPE_dataframe: r_stop_internal("Data frame columns should be flattened already.");
  case VCTRS_TYPE_scalar: Rf_errorcall(R_NilValue, "Can't compare scalars with `vec_equal()`.");
  default: stop_unimplemented_vctrs_type("vec_equal", vec_proxy_typeof(x));
  }
}

static
void vec_equal_col_na_propagate(
  SEXP x,
  SEXP y,
  bool x_recycles,
  bool y_recycles,
  int* p_out,
  struct df_short_circuit_info* p_info
) {
  switch (vec_proxy_typeof(x)) {
  case VCTRS_TYPE_logical: EQUAL_COL(int, LOGICAL_RO, lgl_equal_na_propagate); break;
  case VCTRS_TYPE_integer: EQUAL_COL(int, INTEGER_RO, int_equal_na_propagate); break;
  case VCTRS_TYPE_double: EQUAL_COL(double, REAL_RO, dbl_equal_na_propagate); break;
  case VCTRS_TYPE_complex: EQUAL_COL(Rcomplex, COMPLEX_RO, cpl_equal_na_propagate); break;
  case VCTRS_TYPE_character: EQUAL_COL(SEXP, STRING_PTR_RO, chr_equal_na_propagate); break;
  case VCTRS_TYPE_raw: EQUAL_COL(Rbyte, RAW_RO, raw_equal_na_propagate); break;
  case VCTRS_TYPE_list: EQUAL_COL(SEXP, VECTOR_PTR_RO, list_equal_na_propagate); break;
  case VCTRS_TYPE_dataframe: r_stop_internal("Data frame columns should be flattened already.");
  case VCTRS_TYPE_scalar: Rf_errorcall(R_NilValue, "Can't compare scalars with `vec_equal()`.");
  default: stop_unimplemented_vctrs_type("vec_equal", vec_proxy_typeof(x));
  }
}

#undef EQUAL_COL
#undef EQUAL_COL_IMPL

// -----------------------------------------------------------------------------

// [[ register() ]]
r_obj* ffi_obj_equal(r_obj* x, r_obj* y) {
  return r_lgl(obj_equal(x, y));
}

// [[ include("vctrs.h") ]]
bool obj_equal(r_obj* x, r_obj* y) {
  x = KEEP(obj_encode_utf8(x));
  y = KEEP(obj_encode_utf8(y));

  const bool out = obj_equal_utf8(x, y);

  FREE(2);
  return out;
}

// Assumes `obj_encode_utf8()` has already been called
// [[ include("vctrs.h") ]]
bool obj_equal_utf8(r_obj* x, r_obj* y) {
  const enum r_type type = r_typeof(x);

  // Types must be the same
  if (type != r_typeof(y)) {
    return false;
  }

  // Every type has a chance for an "equal pointer" optimization
  if (x == y) {
    return true;
  }

  switch (type) {
  // Pure pointer comparison. If it didn't early exit in the pointer
  // comparison above, then they must not be equal.
  case R_TYPE_null:
  case R_TYPE_symbol:
  case R_TYPE_special:
  case R_TYPE_builtin:
  case R_TYPE_string:
  case R_TYPE_environment:
  case R_TYPE_pointer:
    return false;

  // Vectors
  case R_TYPE_logical:
  case R_TYPE_integer:
  case R_TYPE_double:
  case R_TYPE_character:
  case R_TYPE_raw:
  case R_TYPE_complex:
  case R_TYPE_list:
    return obj_vec_equal(x, y, type);

  // Expression vectors
  case R_TYPE_expression:
    return obj_expr_equal(x, y);

  // Node like
  case R_TYPE_dots:
  case R_TYPE_call:
  case R_TYPE_pairlist:
  case R_TYPE_bytecode:
    return obj_node_equal(x, y);

  // Functions
  case R_TYPE_closure:
    return obj_fn_equal(x, y);

  default:
    stop_unimplemented_type("obj_equal_utf8", type);
  }
}

// Missingness is never propagated through objects,
// so `na_equal` is always `true` in these macros
#define OBJ_VEC_EQUAL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL) \
  do {                                                    \
    CTYPE const* v_x = CONST_DEREF(x);                    \
    CTYPE const* v_y = CONST_DEREF(y);                    \
                                                          \
    for (r_ssize i = 0; i < n; ++i) {                     \
      if (!EQUAL_NA_EQUAL(v_x[i], v_y[i])) {              \
        return false;                                     \
      }                                                   \
    }                                                     \
    return true;                                          \
  }                                                       \
  while (0)

static inline bool obj_vec_equal(r_obj* x, r_obj* y, enum r_type type) {
  const r_ssize n = r_length(x);

  // Length check
  if (n != r_length(y)) {
    return false;
  }

  // Attribute check
  if (!obj_attrib_equal(x, y)) {
    return false;
  }

  // Data check
  switch (type) {
  case R_TYPE_logical: OBJ_VEC_EQUAL(int, r_lgl_cbegin, lgl_equal_na_equal);
  case R_TYPE_integer: OBJ_VEC_EQUAL(int, r_int_cbegin, int_equal_na_equal);
  case R_TYPE_double: OBJ_VEC_EQUAL(double, r_dbl_cbegin, dbl_equal_na_equal);
  case R_TYPE_character: OBJ_VEC_EQUAL(r_obj*, r_chr_cbegin, chr_equal_na_equal);
  case R_TYPE_raw: OBJ_VEC_EQUAL(Rbyte, r_raw_cbegin, raw_equal_na_equal);
  case R_TYPE_complex: OBJ_VEC_EQUAL(r_complex, r_cpl_cbegin, cpl_equal_na_equal);
  case R_TYPE_list: OBJ_VEC_EQUAL(r_obj*, r_list_cbegin, list_equal_na_equal);
  default: r_stop_unreachable();
  }
}

#undef OBJ_VEC_EQUAL

// Same as implementation for lists where we check length and attributes as
// well, but `VECTOR_PTR_RO()` doesn't support EXPRSXP, so we must use a
// separate loop that uses `VECTOR_ELT()` instead.
static inline bool obj_expr_equal(r_obj* x, r_obj* y) {
  const r_ssize n = r_length(x);

  // Length check
  if (n != r_length(y)) {
    return false;
  }

  // Attribute check
  if (!obj_attrib_equal(x, y)) {
    return false;
  }

  // Data check
  for (r_ssize i = 0; i < n; ++i) {
    r_obj* x_elt = r_list_get(x, i);
    r_obj* y_elt = r_list_get(y, i);

    if (!obj_equal_utf8(x_elt, y_elt)) {
      return false;
    }
  }

  return true;
}

static inline bool obj_node_equal(r_obj* x, r_obj* y) {
  // Attribute check
  if (!obj_attrib_equal(x, y)) {
    return false;
  }

  // Tag check
  if (!obj_equal_utf8(r_node_tag(x), r_node_tag(y))) {
    return false;
  }

  // Value check
  if (!obj_equal_utf8(r_node_car(x), r_node_car(y))) {
    return false;
  }

  // Check rest
  if (!obj_equal_utf8(r_node_cdr(x), r_node_cdr(y))) {
    return false;
  }

  return true;
}

static inline bool obj_fn_equal(r_obj* x, r_obj* y) {
  // Attribute check
  if (!obj_attrib_equal(x, y)) {
    return false;
  }

  // Function body check
  if (!obj_equal_utf8(r_fn_body(x), r_fn_body(y))) {
    return false;
  }

  // Function environment check
  if (!obj_equal_utf8(r_fn_env(x), r_fn_env(y))) {
    return false;
  }

  // Function formals check
  if (!obj_equal_utf8(r_fn_formals(x), r_fn_formals(y))) {
    return false;
  }

  return true;
}

struct attrib_equal_data {
  r_obj* y;
  r_ssize x_size;
};
struct attrib_count_data {
  r_ssize y_size;
};

// Compares attributes of `x` and `y` in an order-independent manner by looking
// up each of `x`'s attributes in `y` by tag, then verifying `y` has no extra
// attributes.
//
// Note that this is not very efficient, but we can't do much better if we want
// order-independent comparisons since we no longer have direct access to the
// pairlists via `ATTRIB()`. It only really affects equality comparisons with
// lists that have elements with many attributes, which is fairly rare.
static inline bool obj_attrib_equal(r_obj* x, r_obj* y) {
  const bool x_has_attrib = r_attrib_has_any(x);
  const bool y_has_attrib = r_attrib_has_any(y);

  if (!x_has_attrib && !y_has_attrib) {
    // Neither have attributes
    return true;
  }

  if (x_has_attrib != y_has_attrib) {
    // One or the other has attributes, but not both
    return false;
  }

  // Ok, now we know both have attributes, compare them
  struct attrib_equal_data equal_data = {
    .y = y,
    .x_size = 0
  };
  r_obj* result = r_attrib_map(x, obj_attrib_equal_cb, &equal_data);

  // We got the signal that an attribute was different
  if (result == r_null) {
    return false;
  }

  // All attributes in `x` equal attributes in `y`.
  // Lastly,  ensure `y` doesn't have more attributes than `x`.
  struct attrib_count_data count_data = {
    .y_size = 0
  };
  r_attrib_map(y, obj_attrib_count_cb, &count_data);

  return equal_data.x_size == count_data.y_size;
}

static r_obj* obj_attrib_equal_cb(r_obj* tag, r_obj* value, void* data) {
  struct attrib_equal_data* p_data = (struct attrib_equal_data*) data;
  p_data->x_size++;

  r_obj* y_value = KEEP(r_attrib_get(p_data->y, tag));

  // Return `r_null` when different to signal that we are done
  SEXP out = obj_equal_utf8(value, y_value) ? NULL : r_null;

  FREE(1);
  return out;
}

static r_obj* obj_attrib_count_cb(r_obj* _tag, r_obj* _value, void* data) {
  struct attrib_count_data* p_data = (struct attrib_count_data*) data;
  p_data->y_size++;

  // Continue
  return NULL;
}
