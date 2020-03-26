#include "vctrs.h"
#include "slice-assign.h"
#include "subscript-loc.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_assign_fallback = NULL;
SEXP fns_vec_assign_fallback = NULL;

const struct vec_assign_opts vec_assign_default_opts = {
  .assign_names = false
};

static SEXP vec_assign_fallback(SEXP x, SEXP index, SEXP value);
static SEXP lgl_assign(SEXP x, SEXP index, SEXP value);
static SEXP int_assign(SEXP x, SEXP index, SEXP value);
static SEXP dbl_assign(SEXP x, SEXP index, SEXP value);
static SEXP cpl_assign(SEXP x, SEXP index, SEXP value);
SEXP chr_assign(SEXP x, SEXP index, SEXP value);
static SEXP raw_assign(SEXP x, SEXP index, SEXP value);
SEXP list_assign(SEXP x, SEXP index, SEXP value);

// [[ register() ]]
SEXP vctrs_assign(SEXP x, SEXP index, SEXP value, SEXP x_arg_, SEXP value_arg_) {
  struct vctrs_arg x_arg = vec_as_arg(x_arg_);
  struct vctrs_arg value_arg = vec_as_arg(value_arg_);

  const struct vec_assign_opts opts = new_vec_assign_opts(false,
                                                          &x_arg,
                                                          &value_arg);
  return vec_assign_opts(x, index, value, &opts);
}

// [[ include("vctrs.h") ]]
SEXP vec_assign(SEXP x, SEXP index, SEXP value) {
  return vec_assign_opts(x, index, value, &vec_assign_default_opts);
}

// Exported for testing
// [[ register() ]]
SEXP vctrs_assign_seq(SEXP x, SEXP value, SEXP start, SEXP size, SEXP increasing) {
  R_len_t start_ = r_int_get(start, 0);
  R_len_t size_ = r_int_get(size, 0);
  bool increasing_ = r_lgl_get(increasing, 0);

  SEXP index = PROTECT(compact_seq(start_, size_, increasing_));

  const struct vec_assign_opts* opts = &vec_assign_default_opts;

  // Cast and recycle `value`
  value = PROTECT(vec_coercible_cast(value, x, opts->value_arg, opts->x_arg));
  value = PROTECT(vec_recycle(value, vec_subscript_size(index), opts->value_arg));

  SEXP proxy = PROTECT(vec_proxy(x));
  proxy = PROTECT(vec_proxy_assign_opts(proxy, index, value, opts));

  SEXP out = vec_restore(proxy, x, R_NilValue);

  UNPROTECT(5);
  return out;
}

// [[ include("slice-assign.h") ]]
SEXP vec_assign_opts(SEXP x, SEXP index, SEXP value,
                     const struct vec_assign_opts* opts) {
  if (x == R_NilValue) {
    return R_NilValue;
  }

  vec_assert(x, opts->x_arg);
  vec_assert(value, opts->value_arg);

  index = PROTECT(vec_as_location_opts(index,
                                       vec_size(x),
                                       PROTECT(vec_names(x)),
                                       location_default_assign_opts));

  // Cast and recycle `value`
  value = PROTECT(vec_coercible_cast(value, x, opts->value_arg, opts->x_arg));
  value = PROTECT(vec_recycle(value, vec_size(index), opts->value_arg));

  SEXP proxy = PROTECT(vec_proxy(x));
  proxy = PROTECT(vec_proxy_assign_opts(proxy, index, value, opts));

  SEXP out = vec_restore(proxy, x, R_NilValue);

  UNPROTECT(6);
  return out;
}

// [[ register() ]]
SEXP vctrs_assign_params(SEXP x, SEXP index, SEXP value,
                         SEXP assign_names) {
  const struct vec_assign_opts opts = new_vec_assign_opts(r_bool_as_int(assign_names),
                                                          args_empty,
                                                          args_empty);
  return vec_assign_opts(x, index, value, &opts);
}

static SEXP vec_assign_switch(SEXP proxy, SEXP index, SEXP value,
                              const struct vec_assign_opts* opts) {
  switch (vec_proxy_typeof(proxy)) {
  case vctrs_type_logical:   return lgl_assign(proxy, index, value);
  case vctrs_type_integer:   return int_assign(proxy, index, value);
  case vctrs_type_double:    return dbl_assign(proxy, index, value);
  case vctrs_type_complex:   return cpl_assign(proxy, index, value);
  case vctrs_type_character: return chr_assign(proxy, index, value);
  case vctrs_type_raw:       return raw_assign(proxy, index, value);
  case vctrs_type_list:      return list_assign(proxy, index, value);
  case vctrs_type_dataframe: return df_assign(proxy, index, value, opts);
  case vctrs_type_scalar:    stop_scalar_type(proxy, args_empty);
  default:                   vctrs_stop_unsupported_type(vec_typeof(proxy), "vec_assign_switch()");
  }
  never_reached("vec_assign_switch");
}

SEXP vec_proxy_assign_names(SEXP proxy, SEXP index, SEXP value) {
  SEXP value_nms = PROTECT(vec_names(value));

  if (value_nms == R_NilValue) {
    UNPROTECT(1);
    return proxy;
  }

  SEXP proxy_nms = PROTECT(vec_names(proxy));
  if (proxy_nms == R_NilValue) {
    proxy_nms = Rf_allocVector(STRSXP, vec_size(proxy));
    UNPROTECT(1);
    PROTECT(proxy_nms);
  }

  proxy_nms = PROTECT(chr_assign(proxy_nms, index, value_nms));

  proxy = PROTECT(r_maybe_duplicate(proxy));
  proxy = vec_set_names(proxy, proxy_nms);

  UNPROTECT(4);
  return proxy;
}


// `vec_proxy_assign()` will duplicate the `proxy` if it is referenced or
// marked as not mutable. Otherwise, `vec_proxy_assign()` will assign
// directly into the `proxy`. Even though it can directly assign, the safe
// way to call `vec_proxy_assign()` is to catch and protect its output rather
// than relying on it to assign directly.

/*
 * @param proxy The proxy of the output container
 * @param index The locations to assign `value` to
 * @param value The value to assign into the proxy. Must already be
 *   cast to the type of the true output container, and have been
 *   recycled to the correct size. Should not be proxied, in case
 *   we have to fallback.
 */
SEXP vec_proxy_assign(SEXP proxy, SEXP index, SEXP value) {
  return vec_proxy_assign_opts(proxy, index, value, &vec_assign_default_opts);
}
SEXP vec_proxy_assign_opts(SEXP proxy, SEXP index, SEXP value,
                           const struct vec_assign_opts* opts) {
  struct vctrs_proxy_info value_info = vec_proxy_info(value);

  // If a fallback is required, the `proxy` is identical to the output container
  // because no proxy method was called
  SEXP out = R_NilValue;

  if (vec_requires_fallback(value, value_info) || has_dim(proxy)) {
    index = PROTECT(compact_materialize(index));
    out = PROTECT(vec_assign_fallback(proxy, index, value));
  } else {
    PROTECT(index);
    out = PROTECT(vec_assign_switch(proxy, index, value_info.proxy, opts));
  }

  if (opts->assign_names) {
    out = vec_proxy_assign_names(out, index, value_info.proxy);
  }

  UNPROTECT(2);
  return out;
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
  SEXP out = PROTECT(r_maybe_duplicate(x));                     \
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
  SEXP out = PROTECT(r_maybe_duplicate(x));                     \
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

static SEXP lgl_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(int, LOGICAL, LOGICAL_RO);
}
static SEXP int_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(int, INTEGER, INTEGER_RO);
}
static SEXP dbl_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(double, REAL, REAL_RO);
}
static SEXP cpl_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(Rcomplex, COMPLEX, COMPLEX_RO);
}
SEXP chr_assign(SEXP x, SEXP index, SEXP value) {
  ASSIGN(SEXP, STRING_PTR, STRING_PTR_RO);
}
static SEXP raw_assign(SEXP x, SEXP index, SEXP value) {
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
  SEXP out = PROTECT(r_maybe_duplicate(x));                     \
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
  SEXP out = PROTECT(r_maybe_duplicate(x));                     \
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

SEXP list_assign(SEXP x, SEXP index, SEXP value) {
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
SEXP df_assign(SEXP x, SEXP index, SEXP value,
               const struct vec_assign_opts* opts) {
  SEXP out = PROTECT(r_maybe_duplicate(x));
  R_len_t n = Rf_length(out);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP out_elt = VECTOR_ELT(out, i);
    SEXP value_elt = VECTOR_ELT(value, i);

    // No need to cast or recycle because those operations are
    // recursive and have already been performed. However, proxy and
    // restore are not recursive so need to be done for each element
    // we recurse into. `vec_proxy_assign()` will proxy the `value_elt`.
    SEXP proxy_elt = PROTECT(vec_proxy(out_elt));
    proxy_elt = PROTECT(Rf_shallow_duplicate(proxy_elt));

    SEXP assigned = PROTECT(vec_proxy_assign_opts(proxy_elt, index, value_elt, opts));
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
