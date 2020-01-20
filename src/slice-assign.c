#include "vctrs.h"
#include "subscript-loc.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_assign_fallback = NULL;
SEXP fns_vec_assign_fallback = NULL;

// Defined in slice.c
SEXP vec_as_location(SEXP i, R_len_t n, SEXP names);

static SEXP vec_assign_fallback(SEXP x, SEXP index, SEXP value);
SEXP vec_assign_impl(SEXP x, SEXP index, SEXP value, bool clone);
static SEXP lgl_assign(SEXP x, SEXP index, SEXP value, bool clone);
static SEXP int_assign(SEXP x, SEXP index, SEXP value, bool clone);
static SEXP dbl_assign(SEXP x, SEXP index, SEXP value, bool clone);
static SEXP cpl_assign(SEXP x, SEXP index, SEXP value, bool clone);
SEXP chr_assign(SEXP x, SEXP index, SEXP value, bool clone);
static SEXP raw_assign(SEXP x, SEXP index, SEXP value, bool clone);
SEXP list_assign(SEXP x, SEXP index, SEXP value, bool clone);
SEXP df_assign(SEXP x, SEXP index, SEXP value, bool clone);

// [[ register(); include("vctrs.h") ]]
SEXP vec_assign(SEXP x, SEXP index, SEXP value) {
  if (x == R_NilValue) {
    return R_NilValue;
  }

  struct vctrs_arg x_arg = new_wrapper_arg(NULL, "x");
  struct vctrs_arg value_arg = new_wrapper_arg(NULL, "value");
  vec_assert(x, &x_arg);
  vec_assert(value, &value_arg);

  // Take the proxy of the RHS before coercing and recycling
  SEXP value_orig = value;
  value = PROTECT(vec_coercible_cast(value, x, &value_arg, &x_arg));
  SEXP value_proxy = PROTECT(vec_proxy(value));

  // Recycle the proxy of `value`
  index = PROTECT(vec_as_location_opts(index,
                                       vec_size(x),
                                       PROTECT(vec_names(x)),
                                       vec_as_location_default_assign_opts));
  value_proxy = PROTECT(vec_recycle(value_proxy, vec_size(index), &value_arg));

  struct vctrs_proxy_info info = vec_proxy_info(x);

  SEXP out;
  if (vec_requires_fallback(x, info) || has_dim(x)) {
    // Restore the value before falling back to `[<-`
    value = PROTECT(vec_restore(value_proxy, value_orig, R_NilValue));
    out = vec_assign_fallback(x, index, value);
    UNPROTECT(1);
  } else {
    out = PROTECT(vec_assign_impl(info.proxy, index, value_proxy, true));
    out = vec_restore(out, x, R_NilValue);
    UNPROTECT(1);
  }

  UNPROTECT(5);
  return out;
}

/**
 * @param clone Whether to shallow duplicate before assignment. With
 *   data frames, the cloning is recursive. If `false`, make sure you
 *   own the relevant parts of the vector structure (data frame
 *   columns in particular).
 */
SEXP vec_assign_impl(SEXP proxy, SEXP index, SEXP value, bool clone) {
  switch (vec_proxy_typeof(proxy)) {
  case vctrs_type_logical:   return lgl_assign(proxy, index, value, clone);
  case vctrs_type_integer:   return int_assign(proxy, index, value, clone);
  case vctrs_type_double:    return dbl_assign(proxy, index, value, clone);
  case vctrs_type_complex:   return cpl_assign(proxy, index, value, clone);
  case vctrs_type_character: return chr_assign(proxy, index, value, clone);
  case vctrs_type_raw:       return raw_assign(proxy, index, value, clone);
  case vctrs_type_list:      return list_assign(proxy, index, value, clone);
  case vctrs_type_dataframe: return df_assign(proxy, index, value, clone);
  case vctrs_type_s3:
  case vctrs_type_null:      Rf_error("Internal error in `vec_assign_impl()`: Unexpected type %s.",
                                      vec_type_as_str(vec_typeof(proxy)));
  case vctrs_type_scalar:    stop_scalar_type(proxy, args_empty);
  }
  never_reached("vec_assign_impl");
}

#define ASSIGN_INDEX(CTYPE, DEREF, CONST_DEREF)                 \
  R_len_t n = Rf_length(index);                                 \
  int* index_data = INTEGER(index);                             \
                                                                \
  if (n != Rf_length(value)) {                                  \
    Rf_error("Internal error in `vec_assign()`: "               \
             "`value` should have been recycled to fit `x`.");  \
  }                                                             \
                                                                \
  const CTYPE* value_data = CONST_DEREF(value);                 \
  SEXP out = PROTECT(clone ? Rf_shallow_duplicate(x) : x);      \
  CTYPE* out_data = DEREF(out);                                 \
                                                                \
  for (R_len_t i = 0; i < n; ++i) {                             \
    int j = index_data[i];                                      \
    if (j != NA_INTEGER) {                                      \
      out_data[j - 1] = value_data[i];                          \
    }                                                           \
  }                                                             \
                                                                \
  UNPROTECT(1);                                                 \
  return out

#define ASSIGN_COMPACT(CTYPE, DEREF, CONST_DEREF)               \
  int* index_data = INTEGER(index);                             \
  R_len_t start = index_data[0];                                \
  R_len_t n = index_data[1];                                    \
  R_len_t step = index_data[2];                                 \
                                                                \
  if (n != Rf_length(value)) {                                  \
    Rf_error("Internal error in `vec_assign()`: "               \
             "`value` should have been recycled to fit `x`.");  \
  }                                                             \
                                                                \
  const CTYPE* value_data = CONST_DEREF(value);                 \
  SEXP out = PROTECT(clone ? Rf_shallow_duplicate(x) : x);      \
  CTYPE* out_data = DEREF(out) + start;                         \
                                                                \
  for (int i = 0; i < n; ++i, out_data += step, ++value_data) { \
    *out_data = *value_data;                                    \
  }                                                             \
                                                                \
  UNPROTECT(1);                                                 \
  return out

#define ASSIGN(CTYPE, DEREF, CONST_DEREF)       \
  if (is_compact_seq(index)) {                  \
    ASSIGN_COMPACT(CTYPE, DEREF, CONST_DEREF);  \
  } else {                                      \
    ASSIGN_INDEX(CTYPE, DEREF, CONST_DEREF);    \
  }

static SEXP lgl_assign(SEXP x, SEXP index, SEXP value, bool clone) {
  ASSIGN(int, LOGICAL, LOGICAL_RO);
}
static SEXP int_assign(SEXP x, SEXP index, SEXP value, bool clone) {
  ASSIGN(int, INTEGER, INTEGER_RO);
}
static SEXP dbl_assign(SEXP x, SEXP index, SEXP value, bool clone) {
  ASSIGN(double, REAL, REAL_RO);
}
static SEXP cpl_assign(SEXP x, SEXP index, SEXP value, bool clone) {
  ASSIGN(Rcomplex, COMPLEX, COMPLEX_RO);
}
SEXP chr_assign(SEXP x, SEXP index, SEXP value, bool clone) {
  ASSIGN(SEXP, STRING_PTR, STRING_PTR_RO);
}
static SEXP raw_assign(SEXP x, SEXP index, SEXP value, bool clone) {
  ASSIGN(Rbyte, RAW, RAW_RO);
}

#undef ASSIGN
#undef ASSIGN_INDEX
#undef ASSIGN_COMPACT


#define ASSIGN_BARRIER_INDEX(GET, SET)                          \
  R_len_t n = Rf_length(index);                                 \
  int* index_data = INTEGER(index);                             \
                                                                \
  if (n != Rf_length(value)) {                                  \
    Rf_error("Internal error in `vec_assign()`: "               \
             "`value` should have been recycled to fit `x`.");  \
  }                                                             \
                                                                \
  SEXP out = PROTECT(clone ? Rf_shallow_duplicate(x) : x);      \
                                                                \
  for (R_len_t i = 0; i < n; ++i) {                             \
    int j = index_data[i];                                      \
    if (j != NA_INTEGER) {                                      \
      SET(out, j - 1, GET(value, i));                           \
    }                                                           \
  }                                                             \
                                                                \
  UNPROTECT(1);                                                 \
  return out

#define ASSIGN_BARRIER_COMPACT(GET, SET)                        \
  int* index_data = INTEGER(index);                             \
  R_len_t start = index_data[0];                                \
  R_len_t n = index_data[1];                                    \
  R_len_t step = index_data[2];                                 \
                                                                \
  if (n != Rf_length(value)) {                                  \
    Rf_error("Internal error in `vec_assign()`: "               \
             "`value` should have been recycled to fit `x`.");  \
  }                                                             \
                                                                \
  SEXP out = PROTECT(clone ? Rf_shallow_duplicate(x) : x);      \
                                                                \
  for (R_len_t i = 0; i < n; ++i, start += step) {              \
    SET(out, start, GET(value, i));                             \
  }                                                             \
                                                                \
  UNPROTECT(1);                                                 \
  return out

#define ASSIGN_BARRIER(GET, SET)                \
  if (is_compact_seq(index)) {                  \
    ASSIGN_BARRIER_COMPACT(GET, SET);           \
  } else {                                      \
    ASSIGN_BARRIER_INDEX(GET, SET);             \
  }

SEXP list_assign(SEXP x, SEXP index, SEXP value, bool clone) {
  ASSIGN_BARRIER(VECTOR_ELT, SET_VECTOR_ELT);
}

#undef ASSIGN_BARRIER
#undef ASSIGN_BARRIER_INDEX
#undef ASSIGN_BARRIER_COMPACT


/**
 * - `out` and `value` must be rectangular lists.
 * - `value` must have the same size as `index`.
 *
 * [[ include("vctrs.h") ]]
 */
SEXP df_assign(SEXP x, SEXP index, SEXP value, bool clone) {
  SEXP out = PROTECT(clone ? Rf_shallow_duplicate(x) : x);
  R_len_t n = Rf_length(out);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP out_elt = VECTOR_ELT(out, i);
    SEXP value_elt = VECTOR_ELT(value, i);

    // No need to cast or recycle because those operations are
    // recursive and have already been performed. However, proxy and
    // restore are not recursive so need to be done for each element
    // we recurse into.
    SEXP proxy_elt = PROTECT(vec_proxy(out_elt));
    value_elt = PROTECT(vec_proxy(value_elt));

    SEXP assigned = PROTECT(vec_assign_impl(proxy_elt, index, value_elt, clone));
    assigned = vec_restore(assigned, out_elt, R_NilValue);

    SET_VECTOR_ELT(out, i, assigned);
    UNPROTECT(3);
  }

  UNPROTECT(1);
  return out;
}

static SEXP vec_assign_fallback(SEXP x, SEXP index, SEXP value) {
  return vctrs_dispatch3(syms_vec_assign_fallback, fns_vec_assign_fallback,
                         syms_x, x,
                         syms_i, index,
                         syms_value, value);
}


void vctrs_init_slice_assign(SEXP ns) {
  syms_vec_assign_fallback = Rf_install("vec_assign_fallback");
  fns_vec_assign_fallback = Rf_findVar(syms_vec_assign_fallback, ns);
}
