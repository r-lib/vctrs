#include "vctrs.h"

// Initialised at load time
SEXP syms_vec_assign_fallback = NULL;
SEXP fns_vec_assign_fallback = NULL;

const struct vec_assign_opts vec_assign_default_opts = {
  .assign_names = false
};

static SEXP vec_assign_fallback(SEXP x, SEXP index, SEXP value);
static SEXP vec_proxy_assign_names(SEXP proxy, SEXP index, SEXP value, const enum vctrs_owned owned);
static SEXP lgl_assign(SEXP x, SEXP index, SEXP value, const enum vctrs_owned owned);
static SEXP int_assign(SEXP x, SEXP index, SEXP value, const enum vctrs_owned owned);
static SEXP dbl_assign(SEXP x, SEXP index, SEXP value, const enum vctrs_owned owned);
static SEXP cpl_assign(SEXP x, SEXP index, SEXP value, const enum vctrs_owned owned);
SEXP chr_assign(SEXP x, SEXP index, SEXP value, const enum vctrs_owned owned);
static SEXP raw_assign(SEXP x, SEXP index, SEXP value, const enum vctrs_owned owned);
SEXP list_assign(SEXP x, SEXP index, SEXP value, const enum vctrs_owned owned);


// [[ include("slice-assign.h") ]]
SEXP vec_assign_opts(SEXP x, SEXP index, SEXP value,
                     const struct vec_assign_opts* opts) {
  if (x == R_NilValue) {
    return R_NilValue;
  }

  // TODO! Error call
  struct r_lazy call = r_lazy_null;

  vec_check_vector(x, opts->x_arg, call);
  vec_check_vector(value, opts->value_arg, call);

  const struct location_opts location_opts = new_location_opts_assign();
  index = PROTECT(vec_as_location_opts(index,
                                       vec_size(x),
                                       PROTECT(vec_names(x)),
                                       &location_opts));

  // Cast and recycle `value`
  value = PROTECT(vec_cast(value, x, opts->value_arg, opts->x_arg, call));
  value = PROTECT(vec_check_recycle(value, vec_size(index), opts->value_arg, call));

  SEXP proxy = PROTECT(vec_proxy(x));
  const enum vctrs_owned owned = vec_owned(proxy);
  proxy = PROTECT(vec_proxy_assign_opts(proxy, index, value, owned, opts));

  SEXP out = vec_restore(proxy, x, R_NilValue, owned);

  UNPROTECT(6);
  return out;
}

// [[ register() ]]
SEXP vctrs_assign(SEXP x, SEXP index, SEXP value, SEXP frame) {
  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy value_arg_ = { .x = syms.value_arg, .env = frame };
  struct vctrs_arg value_arg = new_lazy_arg(&value_arg_);

  const struct vec_assign_opts opts = {
    .assign_names = false,
    .x_arg = &x_arg,
    .value_arg = &value_arg
  };

  return vec_assign_opts(x, index, value, &opts);
}

// [[ register() ]]
SEXP vctrs_assign_params(SEXP x, SEXP index, SEXP value,
                         SEXP assign_names) {
  const struct vec_assign_opts opts =  {
    .assign_names = r_bool_as_int(assign_names)
  };
  return vec_assign_opts(x, index, value, &opts);
}

static SEXP vec_assign_switch(SEXP proxy, SEXP index, SEXP value,
                              const enum vctrs_owned owned,
                              const struct vec_assign_opts* opts) {
  switch (vec_proxy_typeof(proxy)) {
  case vctrs_type_logical:   return lgl_assign(proxy, index, value, owned);
  case vctrs_type_integer:   return int_assign(proxy, index, value, owned);
  case vctrs_type_double:    return dbl_assign(proxy, index, value, owned);
  case vctrs_type_complex:   return cpl_assign(proxy, index, value, owned);
  case vctrs_type_character: return chr_assign(proxy, index, value, owned);
  case vctrs_type_raw:       return raw_assign(proxy, index, value, owned);
  case vctrs_type_list:      return list_assign(proxy, index, value, owned);
  case vctrs_type_dataframe: return df_assign(proxy, index, value, owned, opts);
  case vctrs_type_scalar:    stop_scalar_type(proxy, vec_args.empty, r_lazy_null);
  default:                   stop_unimplemented_vctrs_type("vec_assign_switch", vec_typeof(proxy));
  }
  never_reached("vec_assign_switch");
}

// `vec_proxy_assign_opts()` conditionally duplicates the `proxy` depending
// on a number of factors.
//
// - If a fallback is required, the `proxy` is duplicated at the R level.
// - If `owned` is `VCTRS_OWNED_true`, the `proxy` is typically not duplicated.
//   However, if it is an ALTREP object, it is duplicated because we need to be
//   able to assign into the object it represents, not the ALTREP SEXP itself.
// - If `owned` is `VCTRS_OWNED_false`, the `proxy` is only
//   duplicated if it is referenced, i.e. `MAYBE_REFERENCED()` returns `true`.
//
// In `vec_proxy_assign()`, which is part of the experimental public API,
// ownership is determined with a call to `NO_REFERENCES()`. If there are no
// references, then `VCTRS_OWNED_true` is used, else
// `VCTRS_OWNED_false` is used.
//
// Ownership of the `proxy` must be recursive. For data frames, the `owned`
// argument is passed along to each column.
//
// Practically, we only set `VCTRS_OWNED_true` when we create a fresh data
// structure at the C level and then assign into it to fill it. This happens
// in `vec_c()` and `vec_rbind()`. For data frames, this `owned` parameter
// is particularly important for R 4.0.0 where references are tracked more
// precisely. In R 4.0.0, a freshly created data frame's columns all have a
// refcount of 1 because of the `SET_VECTOR_ELT()` call that set them in the
// data frame. This makes them referenced, but not shared. If
// `VCTRS_OWNED_false` was set and `df_assign()` was used in a loop
// (as it is in `vec_rbind()`), then a copy of each column would be made at
// each iteration of the loop (any time a new set of rows is assigned
// into the output object).
//
// Even though it can directly assign, the safe
// way to call `vec_proxy_assign()` and `vec_proxy_assign_opts()` is to catch
// and protect their output rather than relying on them to assign directly.

/*
 * @param proxy The proxy of the output container
 * @param index The locations to assign `value` to
 * @param value The value to assign into the proxy. Must already be
 *   cast to the type of the true output container, and have been
 *   recycled to the correct size. Should not be proxied, in case
 *   we have to fallback.
 */
SEXP vec_proxy_assign(SEXP proxy, SEXP index, SEXP value) {
  struct vec_assign_opts args = { 0 };
  return vec_proxy_assign_opts(proxy, index, value,
                               vec_owned(proxy),
                               &args);
}
SEXP vec_proxy_assign_opts(SEXP proxy, SEXP index, SEXP value,
                           const enum vctrs_owned owned,
                           const struct vec_assign_opts* opts) {
  int n_protect = 0;

  struct vec_assign_opts mut_opts = *opts;
  bool ignore_outer_names = mut_opts.ignore_outer_names;
  mut_opts.ignore_outer_names = false;

  struct vctrs_proxy_info value_info = vec_proxy_info(value);
  KEEP_N(value_info.shelter, &n_protect);

  if (TYPEOF(proxy) != TYPEOF(value_info.proxy)) {
    r_stop_internal("`proxy` of type `%s` incompatible with `value` proxy of type `%s`.",
                    Rf_type2char(TYPEOF(proxy)),
                    Rf_type2char(TYPEOF(value_info.proxy)));
  }

  // If a fallback is required, the `proxy` is identical to the output container
  // because no proxy method was called
  SEXP out = R_NilValue;

  if (vec_requires_fallback(value, value_info)) {
    index = KEEP_N(compact_materialize(index), &n_protect);
    out = KEEP_N(vec_assign_fallback(proxy, index, value), &n_protect);
  } else if (has_dim(proxy)) {
    out = KEEP_N(vec_assign_shaped(proxy, index, value_info.proxy, owned, &mut_opts), &n_protect);
  } else {
    out = KEEP_N(vec_assign_switch(proxy, index, value_info.proxy, owned, &mut_opts), &n_protect);
  }

  if (!ignore_outer_names && opts->assign_names) {
    out = vec_proxy_assign_names(out, index, value_info.proxy, owned);
  }

  FREE(n_protect);
  return out;
}

#define ASSIGN_INDEX(CTYPE, DEREF, CONST_DEREF)                         \
  R_len_t n = Rf_length(index);                                         \
  int* index_data = INTEGER(index);                                     \
                                                                        \
  if (n != Rf_length(value)) {                                          \
    r_stop_internal("`value` should have been recycled to fit `x`.");   \
  }                                                                     \
                                                                        \
  const CTYPE* value_data = CONST_DEREF(value);                         \
                                                                        \
  SEXP out = PROTECT(vec_clone_referenced(x, owned));                   \
  CTYPE* out_data = DEREF(out);                                         \
                                                                        \
  for (R_len_t i = 0; i < n; ++i) {                                     \
    int j = index_data[i];                                              \
    if (j != NA_INTEGER) {                                              \
      out_data[j - 1] = value_data[i];                                  \
    }                                                                   \
  }                                                                     \
                                                                        \
  UNPROTECT(1);                                                         \
  return out

#define ASSIGN_COMPACT(CTYPE, DEREF, CONST_DEREF)                       \
  int* index_data = INTEGER(index);                                     \
  R_len_t start = index_data[0];                                        \
  R_len_t n = index_data[1];                                            \
  R_len_t step = index_data[2];                                         \
                                                                        \
  if (n != Rf_length(value)) {                                          \
    r_stop_internal("`value` should have been recycled to fit `x`.");   \
  }                                                                     \
                                                                        \
  const CTYPE* value_data = CONST_DEREF(value);                         \
                                                                        \
  SEXP out = PROTECT(vec_clone_referenced(x, owned));                   \
  CTYPE* out_data = DEREF(out) + start;                                 \
                                                                        \
  for (int i = 0; i < n; ++i, out_data += step, ++value_data) {         \
    *out_data = *value_data;                                            \
  }                                                                     \
                                                                        \
  UNPROTECT(1);                                                         \
  return out

#define ASSIGN(CTYPE, DEREF, CONST_DEREF)       \
  if (is_compact_seq(index)) {                  \
    ASSIGN_COMPACT(CTYPE, DEREF, CONST_DEREF);  \
  } else {                                      \
    ASSIGN_INDEX(CTYPE, DEREF, CONST_DEREF);    \
  }

static SEXP lgl_assign(SEXP x, SEXP index, SEXP value, const enum vctrs_owned owned) {
  ASSIGN(int, LOGICAL, LOGICAL_RO);
}
static SEXP int_assign(SEXP x, SEXP index, SEXP value, const enum vctrs_owned owned) {
  ASSIGN(int, INTEGER, INTEGER_RO);
}
static SEXP dbl_assign(SEXP x, SEXP index, SEXP value, const enum vctrs_owned owned) {
  ASSIGN(double, REAL, REAL_RO);
}
static SEXP cpl_assign(SEXP x, SEXP index, SEXP value, const enum vctrs_owned owned) {
  ASSIGN(Rcomplex, COMPLEX, COMPLEX_RO);
}
SEXP chr_assign(SEXP x, SEXP index, SEXP value, const enum vctrs_owned owned) {
  ASSIGN(SEXP, STRING_PTR, STRING_PTR_RO);
}
static SEXP raw_assign(SEXP x, SEXP index, SEXP value, const enum vctrs_owned owned) {
  ASSIGN(Rbyte, RAW, RAW_RO);
}

#undef ASSIGN
#undef ASSIGN_INDEX
#undef ASSIGN_COMPACT


#define ASSIGN_BARRIER_INDEX(GET, SET)                                  \
  R_len_t n = Rf_length(index);                                         \
  int* index_data = INTEGER(index);                                     \
                                                                        \
  if (n != Rf_length(value)) {                                          \
    r_stop_internal("`value` should have been recycled to fit `x`.");   \
  }                                                                     \
                                                                        \
  SEXP out = PROTECT(vec_clone_referenced(x, owned));                   \
                                                                        \
  for (R_len_t i = 0; i < n; ++i) {                                     \
    int j = index_data[i];                                              \
    if (j != NA_INTEGER) {                                              \
      SET(out, j - 1, GET(value, i));                                   \
    }                                                                   \
  }                                                                     \
                                                                        \
  UNPROTECT(1);                                                         \
  return out

#define ASSIGN_BARRIER_COMPACT(GET, SET)                                \
  int* index_data = INTEGER(index);                                     \
  R_len_t start = index_data[0];                                        \
  R_len_t n = index_data[1];                                            \
  R_len_t step = index_data[2];                                         \
                                                                        \
  if (n != Rf_length(value)) {                                          \
    r_stop_internal("`value` should have been recycled to fit `x`.");   \
  }                                                                     \
                                                                        \
  SEXP out = PROTECT(vec_clone_referenced(x, owned));                   \
                                                                        \
  for (R_len_t i = 0; i < n; ++i, start += step) {                      \
    SET(out, start, GET(value, i));                                     \
  }                                                                     \
                                                                        \
  UNPROTECT(1);                                                         \
  return out

#define ASSIGN_BARRIER(GET, SET)                \
  if (is_compact_seq(index)) {                  \
    ASSIGN_BARRIER_COMPACT(GET, SET);           \
  } else {                                      \
    ASSIGN_BARRIER_INDEX(GET, SET);             \
  }

SEXP list_assign(SEXP x, SEXP index, SEXP value, const enum vctrs_owned owned) {
  ASSIGN_BARRIER(VECTOR_ELT, SET_VECTOR_ELT);
}

#undef ASSIGN_BARRIER
#undef ASSIGN_BARRIER_INDEX
#undef ASSIGN_BARRIER_COMPACT


/**
 * - `out` and `value` must be rectangular lists.
 * - `value` must have the same size as `index`.
 *
 * Performance and safety notes:
 * If `x` is a fresh data frame (which would be the case in `vec_c()` and
 * `vec_rbind()`) then `r_clone_referenced()` will return it untouched. Each
 * column will also be fresh, so if `vec_proxy()` just returns its input then
 * `vec_proxy_assign_opts()` will directly assign to that column in `x`. This
 * makes it extremely fast to assign to a data frame.
 *
 * If `x` is referenced already, then `r_clone_referenced()` will call
 * `Rf_shallow_duplicate()`. For lists, this loops over the list and marks
 * each list element with max namedness. This is helpful for us, because
 * it is possible to have a data frame that is itself referenced, with columns
 * that are not (mtcars is an example). If each list element wasn't marked, then
 * `vec_proxy_assign_opts()` would see an unreferenced column and modify it
 * directly, resulting in improper mutable semantics. See #986 for full details.
 *
 * [[ include("vctrs.h") ]]
 */
SEXP df_assign(SEXP x, SEXP index, SEXP value,
               const enum vctrs_owned owned,
               const struct vec_assign_opts* opts) {
  SEXP out = PROTECT(vec_clone_referenced(x, owned));

  R_len_t n = Rf_length(out);

  if (Rf_length(value) != n) {
    r_stop_internal("Can't assign %d columns to df of length %d.",
                    Rf_length(value),
                    n);
  }

  for (R_len_t i = 0; i < n; ++i) {
    SEXP out_elt = VECTOR_ELT(out, i);
    SEXP value_elt = VECTOR_ELT(value, i);

    // No need to cast or recycle because those operations are
    // recursive and have already been performed. However, proxy and
    // restore are not recursive so need to be done for each element
    // we recurse into. `vec_proxy_assign()` will proxy the `value_elt`.
    SEXP proxy_elt = PROTECT(vec_proxy(out_elt));

    SEXP assigned = PROTECT(vec_proxy_assign_opts(proxy_elt, index, value_elt, owned, opts));
    assigned = vec_restore(assigned, out_elt, R_NilValue, owned);

    SET_VECTOR_ELT(out, i, assigned);
    UNPROTECT(2);
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

static
SEXP vec_proxy_assign_names(SEXP proxy,
                            SEXP index,
                            SEXP value,
                            const enum vctrs_owned owned) {
  SEXP value_nms = PROTECT(vec_names(value));

  if (value_nms == R_NilValue) {
    UNPROTECT(1);
    return proxy;
  }

  SEXP proxy_nms = PROTECT(vec_proxy_names(proxy));
  if (proxy_nms == R_NilValue) {
    proxy_nms = PROTECT(Rf_allocVector(STRSXP, vec_size(proxy)));
  } else {
    proxy_nms = PROTECT(vec_clone_referenced(proxy_nms, owned));
  }
  proxy_nms = PROTECT(chr_assign(proxy_nms, index, value_nms, owned));

  proxy = PROTECT(vec_clone_referenced(proxy, owned));
  proxy = vec_proxy_set_names(proxy, proxy_nms, owned);

  UNPROTECT(5);
  return proxy;
}


// Exported for testing
// [[ register() ]]
SEXP vctrs_assign_seq(SEXP x, SEXP value, SEXP start, SEXP size, SEXP increasing) {
  R_len_t start_ = r_int_get(start, 0);
  R_len_t size_ = r_int_get(size, 0);
  bool increasing_ = r_lgl_get(increasing, 0);

  SEXP index = PROTECT(compact_seq(start_, size_, increasing_));

  struct r_lazy call = lazy_calls.vec_assign_seq;

  // TODO! vec_check_assign()
  struct vec_assign_opts args = {
    .x_arg = vec_args.x,
    .value_arg = vec_args.value,
    .call = call
  };

  // Cast and recycle `value`
  value = PROTECT(vec_cast(value, x, args.value_arg, args.x_arg, call));
  value = PROTECT(vec_check_recycle(value, vec_subscript_size(index), args.value_arg, call));

  SEXP proxy = PROTECT(vec_proxy(x));
  const enum vctrs_owned owned = vec_owned(proxy);
  proxy = PROTECT(vec_proxy_assign_opts(proxy, index, value, owned, &args));

  SEXP out = vec_restore(proxy, x, R_NilValue, owned);

  UNPROTECT(5);
  return out;
}


void vctrs_init_slice_assign(SEXP ns) {
  syms_vec_assign_fallback = Rf_install("vec_assign_fallback");
  fns_vec_assign_fallback = Rf_findVar(syms_vec_assign_fallback, ns);
}
