#include "vctrs.h"
#include "decl/slice-assign-decl.h"


// [[ include("slice-assign.h") ]]
r_obj* vec_assign_opts(r_obj* x,
                       r_obj* index,
                       r_obj* value,
                       const struct vec_assign_opts* p_opts) {
  if (x == r_null) {
    return r_null;
  }

  // We won't be proxying recursively
  const bool recursively_proxied = false;

  struct vec_proxy_assign_opts assign_opts = {
    .assign_names = p_opts->assign_names,
    .ignore_outer_names = p_opts->ignore_outer_names,
    .call = p_opts->call,
    .x_arg = p_opts->x_arg,
    .value_arg = p_opts->value_arg,
    .ownership = p_opts->ownership,
    .recursively_proxied = recursively_proxied
  };
  struct vec_restore_opts restore_opts = {
    .ownership = p_opts->ownership,
    .recursively_proxied = recursively_proxied
  };

  if (r_lazy_is_null(p_opts->call)) {
    assign_opts.call = lazy_calls.vec_assign;
    assign_opts.x_arg = vec_args.x;
    assign_opts.value_arg = vec_args.value;
  }

  obj_check_vector(x, assign_opts.x_arg, assign_opts.call);
  obj_check_vector(value, assign_opts.value_arg, assign_opts.call);

  const struct location_opts location_opts = new_location_opts_assign();
  index = KEEP(vec_as_location_opts(
    index,
    vec_size(x),
    KEEP(vec_names(x)),
    &location_opts
  ));

  // Cast and recycle `value`
  value = KEEP(vec_cast(value, x, assign_opts.value_arg, assign_opts.x_arg, assign_opts.call));
  value = KEEP(vec_check_recycle(value, vec_size(index), assign_opts.value_arg, assign_opts.call));

  r_obj* proxy = KEEP(vec_proxy(x));
  proxy = KEEP(vec_proxy_assign_opts(proxy, index, value, &assign_opts));
  r_obj* out = vec_restore_opts(proxy, x, &restore_opts);

  FREE(6);
  return out;
}

// [[ register() ]]
r_obj* ffi_assign(r_obj* x, r_obj* index, r_obj* value, r_obj* frame) {
  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy value_arg_lazy = { .x = syms.value_arg, .env = frame };
  struct vctrs_arg value_arg = new_lazy_arg(&value_arg_lazy);

  struct r_lazy call = { .x = frame, .env = r_null };

  // Comes from the R side, so no known ownership
  enum vctrs_ownership ownership = VCTRS_OWNERSHIP_foreign;

  const struct vec_assign_opts opts = {
    .assign_names = false,
    .ownership = ownership,
    .x_arg = &x_arg,
    .value_arg = &value_arg,
    .call = call
  };

  return vec_assign_opts(x, index, value, &opts);
}

// [[ register() ]]
r_obj* ffi_assign_params(r_obj* x,
                         r_obj* index,
                         r_obj* value,
                         r_obj* assign_names) {
  // Comes from the R side, so no known ownership
  enum vctrs_ownership ownership = VCTRS_OWNERSHIP_foreign;

  const struct vec_assign_opts opts =  {
    .assign_names = r_bool_as_int(assign_names),
    .ownership = ownership,
    .call = lazy_calls.vec_assign_params
  };

  return vec_assign_opts(x, index, value, &opts);
}

static
r_obj* vec_assign_switch(r_obj* proxy,
                         r_obj* index,
                         r_obj* value,
                         const struct vec_proxy_assign_opts* p_opts) {
  switch (vec_proxy_typeof(proxy)) {
  case VCTRS_TYPE_logical:   return lgl_assign(proxy, index, value, p_opts->ownership);
  case VCTRS_TYPE_integer:   return int_assign(proxy, index, value, p_opts->ownership);
  case VCTRS_TYPE_double:    return dbl_assign(proxy, index, value, p_opts->ownership);
  case VCTRS_TYPE_complex:   return cpl_assign(proxy, index, value, p_opts->ownership);
  case VCTRS_TYPE_character: return chr_assign(proxy, index, value, p_opts->ownership);
  case VCTRS_TYPE_raw:       return raw_assign(proxy, index, value, p_opts->ownership);
  case VCTRS_TYPE_list:      return list_assign(proxy, index, value, p_opts->ownership);
  case VCTRS_TYPE_dataframe: return df_assign(proxy, index, value, p_opts);
  case VCTRS_TYPE_scalar:    stop_scalar_type(proxy, vec_args.empty, r_lazy_null);
  default:                   stop_unimplemented_vctrs_type("vec_assign_switch", vec_typeof(proxy));
  }
  r_stop_unreachable();
}

// `vec_proxy_assign_opts()` conditionally duplicates the `proxy` depending
// on a number of factors.
//
// - If a fallback is required, the `proxy` is duplicated at the R level.
// - If `p_opts->ownership` is `VCTRS_OWNERSHIP_deep`, the `proxy` is not duplicated.
//   - Notably, this is passed on to all columns when assigning to a data frame.
//   - If the `proxy` happens to be an ALTREP object, materialization will be
//     forced when we do the actual assignment, but this should really only
//     happen with cheap-to-materialize ALTREP "wrapper" objects since we've
//     claimed that we "own" the `proxy`.
// - If `p_opts->ownership` is `VCTRS_OWNERSHIP_shallow`, the `proxy` container
//   is not duplicated, but in the case of the columns of a data frame the
//   columns are each treated as `VCTRS_OWNERSHIP_foreign`.
// - If `p_opts->ownership` is `VCTRS_OWNERSHIP_foreign`, the `proxy` is only
//   duplicated if it is referenced, i.e. `MAYBE_REFERENCED()` returns `true`.
//
// We only set `VCTRS_OWNERSHIP_deep` when we've created a fresh data structure
// at C level and we are about to fill it. Some examples:
// - vec_c()
// - list_unchop()
// - vec_rbind()
//
// For data frames, this `ownership` parameter is particularly important for R
// 4.0.0 where references are tracked more precisely. In R 4.0.0, a freshly
// created data frame's columns all have a refcount of 1 because of the
// `r_list_poke()` call that set them in the data frame. This makes them
// referenced, but not shared. If `VCTRS_OWNERSHIP_shallow` or
// `VCTRS_OWNERSHIP_foreign` was set and `df_assign()` was used in a loop (as it
// is in `vec_rbind()`), then a copy of each column would be made at each
// iteration of the loop (any time a new set of rows is assigned into the output
// object).
//
// Even though it can directly assign, the safe way to call
// `vec_proxy_assign_opts()` is to catch and protect their output rather than
// relying on them to assign directly.

/*
 * @param proxy The proxy of the output container
 * @param index The locations to assign `value` to
 * @param value The value to assign into the proxy. Must already be
 *   cast to the type of the true output container, and have been
 *   recycled to the correct size. Should not be proxied, in case
 *   we have to fallback.
 * @param p_opts The options to use during the assignment process
 */
r_obj* vec_proxy_assign_opts(r_obj* proxy,
                             r_obj* index,
                             r_obj* value,
                             const struct vec_proxy_assign_opts* p_opts) {
  int n_protect = 0;

  // Ignore vectors marked as fallback because the caller will apply
  // a fallback method instead
  if (vec_is_common_class_fallback(proxy)) {
    return proxy;
  }

  // We only allow `ignore_outer_names` on the "outer" call to
  // `vec_proxy_assign_opts()`. After it has been used once, it is set to
  // `false` for any recursive calls back into this function.
  struct vec_proxy_assign_opts opts_copy = *p_opts;
  const bool ignore_outer_names = opts_copy.ignore_outer_names;
  opts_copy.ignore_outer_names = false;

  struct vctrs_proxy_info value_info = vec_proxy_info(value);
  KEEP_N(value_info.shelter, &n_protect);

  if (r_typeof(proxy) != r_typeof(value_info.proxy)) {
    r_stop_internal("`proxy` of type `%s` incompatible with `value` proxy of type `%s`.",
                    r_type_as_c_string(r_typeof(proxy)),
                    r_type_as_c_string(r_typeof(value_info.proxy)));
  }

  // If a fallback is required, the `proxy` is identical to the output container
  // because no proxy method was called
  r_obj* out = r_null;

  if (vec_requires_fallback(value, value_info)) {
    index = KEEP_N(compact_materialize(index), &n_protect);
    out = KEEP_N(vec_assign_fallback(proxy, index, value), &n_protect);
  } else if (has_dim(proxy)) {
    out = KEEP_N(vec_assign_shaped(proxy, index, value_info.proxy, opts_copy.ownership), &n_protect);
  } else {
    out = KEEP_N(vec_assign_switch(proxy, index, value_info.proxy, &opts_copy), &n_protect);
  }

  if (!ignore_outer_names && p_opts->assign_names) {
    out = vec_proxy_assign_names(out, index, value_info.proxy, opts_copy.ownership);
  }

  FREE(n_protect);
  return out;
}

#define ASSIGN_INDEX(CTYPE, DEREF, CONST_DEREF)                         \
  r_ssize n = r_length(index);                                          \
  int* index_data = r_int_begin(index);                                 \
                                                                        \
  if (n != r_length(value)) {                                           \
    r_stop_internal("`value` should have been recycled to fit `x`.");   \
  }                                                                     \
                                                                        \
  const CTYPE* value_data = CONST_DEREF(value);                         \
                                                                        \
  r_obj* out = KEEP(vec_clone_referenced(x, ownership));                \
  CTYPE* out_data = DEREF(out);                                         \
                                                                        \
  for (r_ssize i = 0; i < n; ++i) {                                     \
    int j = index_data[i];                                              \
    if (j != r_globals.na_int) {                                        \
      out_data[j - 1] = value_data[i];                                  \
    }                                                                   \
  }                                                                     \
                                                                        \
  FREE(1);                                                              \
  return out

#define ASSIGN_COMPACT(CTYPE, DEREF, CONST_DEREF)                       \
  int* index_data = r_int_begin(index);                                 \
  r_ssize start = index_data[0];                                        \
  r_ssize n = index_data[1];                                            \
  r_ssize step = index_data[2];                                         \
                                                                        \
  if (n != r_length(value)) {                                           \
    r_stop_internal("`value` should have been recycled to fit `x`.");   \
  }                                                                     \
                                                                        \
  const CTYPE* value_data = CONST_DEREF(value);                         \
                                                                        \
  r_obj* out = KEEP(vec_clone_referenced(x, ownership));                \
  CTYPE* out_data = DEREF(out) + start;                                 \
                                                                        \
  for (r_ssize i = 0; i < n; ++i, out_data += step, ++value_data) {     \
    *out_data = *value_data;                                            \
  }                                                                     \
                                                                        \
  FREE(1);                                                              \
  return out

#define ASSIGN(CTYPE, DEREF, CONST_DEREF)       \
  if (is_compact_seq(index)) {                  \
    ASSIGN_COMPACT(CTYPE, DEREF, CONST_DEREF);  \
  } else {                                      \
    ASSIGN_INDEX(CTYPE, DEREF, CONST_DEREF);    \
  }

static
r_obj* lgl_assign(r_obj* x, r_obj* index, r_obj* value, const enum vctrs_ownership ownership) {
  ASSIGN(int, LOGICAL, LOGICAL_RO);
}
static
r_obj* int_assign(r_obj* x, r_obj* index, r_obj* value, const enum vctrs_ownership ownership) {
  ASSIGN(int, r_int_begin, INTEGER_RO);
}
static
r_obj* dbl_assign(r_obj* x, r_obj* index, r_obj* value, const enum vctrs_ownership ownership) {
  ASSIGN(double, REAL, REAL_RO);
}
static
r_obj* cpl_assign(r_obj* x, r_obj* index, r_obj* value, const enum vctrs_ownership ownership) {
  ASSIGN(Rcomplex, COMPLEX, COMPLEX_RO);
}
static
r_obj* raw_assign(r_obj* x, r_obj* index, r_obj* value, const enum vctrs_ownership ownership) {
  ASSIGN(Rbyte, RAW, RAW_RO);
}


#define ASSIGN_BARRIER_INDEX(GET, SET)                                  \
  r_ssize n = r_length(index);                                          \
  int* index_data = r_int_begin(index);                                 \
                                                                        \
  if (n != r_length(value)) {                                           \
    r_stop_internal("`value` (size %d) doesn't match `x` (size %d).",   \
                    r_length(value),                                    \
                    n);                                                 \
  }                                                                     \
                                                                        \
  r_obj* out = KEEP(vec_clone_referenced(x, ownership));                \
                                                                        \
  for (r_ssize i = 0; i < n; ++i) {                                     \
    int j = index_data[i];                                              \
    if (j != r_globals.na_int) {                                        \
      SET(out, j - 1, GET(value, i));                                   \
    }                                                                   \
  }                                                                     \
                                                                        \
  FREE(1);                                                              \
  return out

#define ASSIGN_BARRIER_COMPACT(GET, SET)                                \
  int* index_data = r_int_begin(index);                                 \
  r_ssize start = index_data[0];                                        \
  r_ssize n = index_data[1];                                            \
  r_ssize step = index_data[2];                                         \
                                                                        \
  if (n != r_length(value)) {                                           \
    r_stop_internal("`value` (size %d) doesn't match `x` (size %d).",   \
                    r_length(value),                                    \
                    n);                                                 \
  }                                                                     \
                                                                        \
  r_obj* out = KEEP(vec_clone_referenced(x, ownership));                \
                                                                        \
  for (r_ssize i = 0; i < n; ++i, start += step) {                      \
    SET(out, start, GET(value, i));                                     \
  }                                                                     \
                                                                        \
  FREE(1);                                                              \
  return out

#define ASSIGN_BARRIER(GET, SET)                \
  if (is_compact_seq(index)) {                  \
    ASSIGN_BARRIER_COMPACT(GET, SET);           \
  } else {                                      \
    ASSIGN_BARRIER_INDEX(GET, SET);             \
  }

r_obj* chr_assign(r_obj* x, r_obj* index, r_obj* value, const enum vctrs_ownership ownership) {
  ASSIGN_BARRIER(r_chr_get, r_chr_poke);
}
r_obj* list_assign(r_obj* x, r_obj* index, r_obj* value, const enum vctrs_ownership ownership) {
  ASSIGN_BARRIER(r_list_get, r_list_poke);
}

/**
 * Invariants:
 *
 * - `out` and `value` must be rectangular lists.
 * - `value` must have the same size as `index`.
 *
 * Performance and safety notes:
 *
 * In `vec_c()`, `vec_rbind()`, and `list_unchop()` we totally own the data
 * frame and its columns recursively, so we set `VCTRS_OWNERSHIP_deep`. This
 * helps us avoid copies of the columns during restoration even if
 * `NO_REFERENCES()` disagrees (storing the column in a list counts as a
 * reference) (#1151).
 *
 * If we don't own `x` we set `VCTRS_OWNERSHIP_foreign`. This calls
 * `vec_clone_referenced -> r_clone_referenced -> Rf_shallow_duplicate`. For
 * lists (data.frames), this loops over the list and marks each element as
 * referenced. This helps in a particular special case where the data frame
 * itself could be referenced but the columns were not (mtcars was an example at
 * the time, which was likely an R bug). If each list element wasn't marked,
 * then `vec_proxy_assign_opts()` would see an unreferenced column and modify it
 * directly, resulting in improper mutable semantics (#986).
 *
 * [[ include("vctrs.h") ]]
 */
r_obj* df_assign(r_obj* x,
                 r_obj* index,
                 r_obj* value,
                 const struct vec_proxy_assign_opts* p_opts) {
  r_obj* out = KEEP(vec_clone_referenced(x, p_opts->ownership));

  r_ssize n = r_length(out);

  if (r_length(value) != n) {
    r_stop_internal("Can't assign %d columns to df of length %d.",
                    r_length(value),
                    n);
  }

  // During assignment, if we have deep ownership over `x` we can
  // propagate that ownership to the columns, otherwise we have no
  // known ownership over the columns
  enum vctrs_ownership col_ownership;
  switch (p_opts->ownership) {
  case VCTRS_OWNERSHIP_foreign: col_ownership = VCTRS_OWNERSHIP_foreign; break;
  case VCTRS_OWNERSHIP_shallow: col_ownership = VCTRS_OWNERSHIP_foreign; break;
  case VCTRS_OWNERSHIP_deep: col_ownership = VCTRS_OWNERSHIP_deep; break;
  default: r_stop_unreachable();
  }

  const struct vec_proxy_assign_opts col_proxy_assign_opts = {
    .assign_names = p_opts->assign_names,
    .ignore_outer_names = p_opts->ignore_outer_names,
    .call = p_opts->call,
    .x_arg = p_opts->x_arg,
    .value_arg = p_opts->value_arg,
    .ownership = col_ownership,
    .recursively_proxied = p_opts->recursively_proxied
  };

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* out_elt = r_list_get(out, i);
    r_obj* value_elt = r_list_get(value, i);

    // No need to cast or recycle because those operations are
    // recursive and have already been performed. However, proxy and
    // restore are not necessarily recursive and we might need to
    // proxy each element we recurse into.
    //
    // NOTE: `vec_proxy_assign_opts()` proxies `value_elt`.
    r_obj* proxy_elt = KEEP(p_opts->recursively_proxied ? out_elt : vec_proxy(out_elt));

    r_obj* assigned_elt = KEEP(vec_proxy_assign_opts(proxy_elt, index, value_elt, &col_proxy_assign_opts));

    if (!p_opts->recursively_proxied) {
      const struct vec_restore_opts col_restore_opts = {
        .ownership = col_ownership,
        .recursively_proxied = false
      };
      assigned_elt = vec_restore_opts(assigned_elt, out_elt, &col_restore_opts);
    }

    r_list_poke(out, i, assigned_elt);
    FREE(2);
  }

  FREE(1);
  return out;
}

static
r_obj* vec_assign_fallback(r_obj* x, r_obj* index, r_obj* value) {
  return vctrs_dispatch3(syms_vec_assign_fallback, fns_vec_assign_fallback,
                         syms_x, x,
                         syms_i, index,
                         syms_value, value);
}

static
r_obj* vec_proxy_assign_names(r_obj* proxy,
                              r_obj* index,
                              r_obj* value,
                              const enum vctrs_ownership ownership) {
  r_obj* value_nms = KEEP(vec_names(value));

  if (value_nms == r_null) {
    FREE(1);
    return proxy;
  }

  r_obj* proxy_nms = KEEP(vec_proxy_names(proxy));
  if (proxy_nms == r_null) {
    proxy_nms = KEEP(r_alloc_character(vec_size(proxy)));
  } else {
    proxy_nms = KEEP(vec_clone_referenced(proxy_nms, ownership));
  }
  proxy_nms = KEEP(chr_assign(proxy_nms, index, value_nms, ownership));

  proxy = KEEP(vec_clone_referenced(proxy, ownership));
  proxy = vec_proxy_set_names(proxy, proxy_nms, ownership);

  FREE(5);
  return proxy;
}


// Exported for testing
// [[ register() ]]
r_obj* ffi_assign_seq(r_obj* x,
                      r_obj* value,
                      r_obj* ffi_start,
                      r_obj* ffi_size,
                      r_obj* ffi_increasing) {
  r_ssize start = r_int_get(ffi_start, 0);
  r_ssize size = r_int_get(ffi_size, 0);
  bool increasing = r_lgl_get(ffi_increasing, 0);

  struct r_lazy call = lazy_calls.vec_assign_seq;

  r_obj* index = KEEP(compact_seq(start, size, increasing));

  // Comes from the R side, so not owned, and not proxying recursively
  const struct vec_proxy_assign_opts assign_opts = {
    .x_arg = vec_args.x,
    .value_arg = vec_args.value,
    .call = call,
    .ownership = VCTRS_OWNERSHIP_foreign,
    .recursively_proxied = false
  };
  struct vec_restore_opts restore_opts = {
    .ownership = VCTRS_OWNERSHIP_foreign,
    .recursively_proxied = false
  };

  // Cast and recycle `value`
  value = KEEP(vec_cast(value, x, vec_args.value, vec_args.x, call));
  value = KEEP(vec_check_recycle(value, vec_subscript_size(index), vec_args.value, call));

  r_obj* proxy = KEEP(vec_proxy(x));
  proxy = KEEP(vec_proxy_assign_opts(proxy, index, value, &assign_opts));
  r_obj* out = vec_restore_opts(proxy, x, &restore_opts);

  FREE(5);
  return out;
}


void vctrs_init_slice_assign(r_obj* ns) {
  syms_vec_assign_fallback = r_sym("vec_assign_fallback");
  fns_vec_assign_fallback = r_eval(syms_vec_assign_fallback, ns);
}

static r_obj* syms_vec_assign_fallback = NULL;
static r_obj* fns_vec_assign_fallback = NULL;
