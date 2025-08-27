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

  struct r_lazy call = r_lazy_is_null(p_opts->call) ? lazy_calls.vec_assign : p_opts->call;
  struct vctrs_arg* x_arg = r_lazy_is_null(p_opts->call) ? vec_args.x : p_opts->x_arg;
  struct vctrs_arg* value_arg = r_lazy_is_null(p_opts->call) ? vec_args.value : p_opts->value_arg;

  obj_check_vector(x, x_arg, call);
  obj_check_vector(value, value_arg, call);

  const r_ssize x_size = vec_size(x);

  // Determine index style. Logical condition indices follow an optimized path.
  const enum vctrs_index_style index_style = is_condition_index(index, x_size) ?
    VCTRS_INDEX_STYLE_condition :
    VCTRS_INDEX_STYLE_location;

  if (index_style == VCTRS_INDEX_STYLE_location) {
    // Validate and convert to integer locations with `vec_as_location()`
    r_obj* x_names = KEEP(vec_names(x));
    const struct location_opts location_opts = new_location_opts_assign();
    index = vec_as_location_opts(
      index,
      x_size,
      x_names,
      &location_opts
    );
    FREE(1);
  }
  KEEP(index);

  // We won't be proxying recursively
  const bool recursively_proxied = false;

  struct vec_proxy_assign_opts assign_opts = {
    .assign_names = p_opts->assign_names,
    .ignore_outer_names = p_opts->ignore_outer_names,
    .call = call,
    .x_arg = x_arg,
    .value_arg = value_arg,
    .index_style = index_style,
    .slice_value = p_opts->slice_value,
    .ownership = p_opts->ownership,
    .recursively_proxied = recursively_proxied
  };
  struct vec_restore_opts restore_opts = {
    .ownership = p_opts->ownership,
    .recursively_proxied = recursively_proxied
  };

  // Cast `value` and check that it can recycle
  value = KEEP(vec_cast(value, x, assign_opts.value_arg, assign_opts.x_arg, assign_opts.call));

  check_value_recyclable(
    value,
    index,
    x_size,
    assign_opts.slice_value,
    assign_opts.index_style,
    assign_opts.value_arg,
    assign_opts.call
  );

  r_obj* proxy = KEEP(vec_proxy(x));
  proxy = KEEP(vec_proxy_assign_opts(proxy, index, value, &assign_opts));
  r_obj* out = vec_restore_opts(proxy, x, &restore_opts);

  FREE(4);
  return out;
}

// [[ register() ]]
r_obj* ffi_assign(
  r_obj* ffi_x,
  r_obj* ffi_i,
  r_obj* ffi_value,
  r_obj* ffi_slice_value,
  r_obj* ffi_frame
) {
  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = ffi_frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy value_arg_lazy = { .x = syms.value_arg, .env = ffi_frame };
  struct vctrs_arg value_arg = new_lazy_arg(&value_arg_lazy);

  struct r_lazy call = { .x = ffi_frame, .env = r_null };

  const enum assignment_slice_value slice_value =
    r_arg_as_bool(ffi_slice_value, "slice_value") ?
    ASSIGNMENT_SLICE_VALUE_yes :
    ASSIGNMENT_SLICE_VALUE_no;

  // We don't expose this in the R API
  const bool assign_names = false;

  // Comes from the R side, so no known ownership
  const enum vctrs_ownership ownership = VCTRS_OWNERSHIP_foreign;

  const struct vec_assign_opts opts = {
    .assign_names = assign_names,
    .slice_value = slice_value,
    .ownership = ownership,
    .x_arg = &x_arg,
    .value_arg = &value_arg,
    .call = call
  };

  return vec_assign_opts(ffi_x, ffi_i, ffi_value, &opts);
}

// [[ register() ]]
r_obj* ffi_assign_params(
  r_obj* ffi_x,
  r_obj* ffi_index,
  r_obj* ffi_value,
  r_obj* ffi_assign_names,
  r_obj* ffi_slice_value,
  r_obj* ffi_frame
) {
  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = ffi_frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy value_arg_lazy = { .x = syms.value_arg, .env = ffi_frame };
  struct vctrs_arg value_arg = new_lazy_arg(&value_arg_lazy);

  struct r_lazy call = { .x = ffi_frame, .env = r_null };

  const bool assign_names = r_arg_as_bool(ffi_assign_names, "assign_names");

  const enum assignment_slice_value slice_value =
    r_arg_as_bool(ffi_slice_value, "slice_value") ?
    ASSIGNMENT_SLICE_VALUE_yes :
    ASSIGNMENT_SLICE_VALUE_no;

  // Comes from the R side, so no known ownership
  enum vctrs_ownership ownership = VCTRS_OWNERSHIP_foreign;

  const struct vec_assign_opts opts = {
    .assign_names = assign_names,
    .ownership = ownership,
    .slice_value = slice_value,
    .x_arg = &x_arg,
    .value_arg = &value_arg,
    .call = call
  };

  return vec_assign_opts(ffi_x, ffi_index, ffi_value, &opts);
}

static
r_obj* vec_assign_switch(
  r_obj* proxy,
  r_obj* index,
  r_obj* value,
  const struct vec_proxy_assign_opts* p_opts
) {
  switch (vec_proxy_typeof(proxy)) {
  case VCTRS_TYPE_logical:   return lgl_assign(proxy, index, value, p_opts->ownership, p_opts->slice_value, p_opts->index_style);
  case VCTRS_TYPE_integer:   return int_assign(proxy, index, value, p_opts->ownership, p_opts->slice_value, p_opts->index_style);
  case VCTRS_TYPE_double:    return dbl_assign(proxy, index, value, p_opts->ownership, p_opts->slice_value, p_opts->index_style);
  case VCTRS_TYPE_complex:   return cpl_assign(proxy, index, value, p_opts->ownership, p_opts->slice_value, p_opts->index_style);
  case VCTRS_TYPE_character: return chr_assign(proxy, index, value, p_opts->ownership, p_opts->slice_value, p_opts->index_style);
  case VCTRS_TYPE_raw:       return raw_assign(proxy, index, value, p_opts->ownership, p_opts->slice_value, p_opts->index_style);
  case VCTRS_TYPE_list:      return list_assign(proxy, index, value, p_opts->ownership, p_opts->slice_value, p_opts->index_style);
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
 *   cast to the type of the true output container, and have already
 *   been checked for recyclability (either size 1 or size of `index`).
 *   Should not be proxied, in case we have to fallback.
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
    out = KEEP_N(vec_assign_fallback(proxy, index, value, opts_copy.slice_value, opts_copy.index_style), &n_protect);
  } else if (has_dim(proxy)) {
    out = KEEP_N(vec_assign_shaped(proxy, index, value_info.proxy, opts_copy.ownership, opts_copy.slice_value, opts_copy.index_style), &n_protect);
  } else {
    out = KEEP_N(vec_assign_switch(proxy, index, value_info.proxy, &opts_copy), &n_protect);
  }

  if (!ignore_outer_names && p_opts->assign_names) {
    out = vec_proxy_assign_names(out, index, value_info.proxy, opts_copy.ownership, opts_copy.slice_value, opts_copy.index_style);
  }

  FREE(n_protect);
  return out;
}

#define ASSIGN_LOCATION_INDEX(CTYPE, DEREF, CONST_DEREF, VALUE_LOC)     \
  const r_ssize index_size = r_length(index);                           \
  const int* index_data = r_int_cbegin(index);                          \
                                                                        \
  const CTYPE* value_data = CONST_DEREF(value);                         \
                                                                        \
  r_obj* out = KEEP(vec_clone_referenced(x, ownership));                \
  CTYPE* out_data = DEREF(out);                                         \
                                                                        \
  for (r_ssize index_loc = 0; index_loc < index_size; ++index_loc) {    \
    const int index_elt = index_data[index_loc];                        \
    if (index_elt != r_globals.na_int) {                                \
      const r_ssize out_loc = index_elt - 1;                            \
      out_data[out_loc] = value_data[VALUE_LOC];                        \
    }                                                                   \
  }                                                                     \
                                                                        \
  FREE(1);                                                              \
  return out

#define ASSIGN_LOCATION_COMPACT(CTYPE, DEREF, CONST_DEREF, VALUE_LOC)   \
  const int* index_data = r_int_cbegin(index);                          \
  const r_ssize start = index_data[0];                                  \
  const r_ssize index_size = index_data[1];                             \
  const r_ssize step = index_data[2];                                   \
                                                                        \
  const CTYPE* value_data = CONST_DEREF(value);                         \
                                                                        \
  r_obj* out = KEEP(vec_clone_referenced(x, ownership));                \
  CTYPE* out_data = DEREF(out);                                         \
  r_ssize out_loc = start;                                              \
                                                                        \
  for (r_ssize index_loc = 0; index_loc < index_size; ++index_loc) {    \
    out_data[out_loc] = value_data[VALUE_LOC];                          \
    out_loc += step;                                                    \
  }                                                                     \
                                                                        \
  FREE(1);                                                              \
  return out

#define ASSIGN_LOCATION(CTYPE, DEREF, CONST_DEREF)                               \
  const r_ssize value_size = r_length(value);                                    \
  check_assign_sizes(x, index, value_size, slice_value, index_style);            \
                                                                                 \
  if (is_compact_seq(index)) {                                                   \
    if (value_size == 1) {                                                       \
      ASSIGN_LOCATION_COMPACT(CTYPE, DEREF, CONST_DEREF, 0);                     \
    } else if (should_slice_value(slice_value)) {                                \
      ASSIGN_LOCATION_COMPACT(CTYPE, DEREF, CONST_DEREF, out_loc);               \
    } else {                                                                     \
      ASSIGN_LOCATION_COMPACT(CTYPE, DEREF, CONST_DEREF, index_loc);             \
    }                                                                            \
  } else {                                                                       \
    if (value_size == 1) {                                                       \
      ASSIGN_LOCATION_INDEX(CTYPE, DEREF, CONST_DEREF, 0);                       \
    } else if (should_slice_value(slice_value)) {                                \
      ASSIGN_LOCATION_INDEX(CTYPE, DEREF, CONST_DEREF, out_loc);                 \
    } else {                                                                     \
      ASSIGN_LOCATION_INDEX(CTYPE, DEREF, CONST_DEREF, index_loc);               \
    }                                                                            \
  }

#define ASSIGN_CONDITION_INDEX(CTYPE, DEREF, CONST_DEREF, VALUE_INCR)   \
  const r_ssize index_size = r_length(index);                           \
  const int* index_data = r_lgl_cbegin(index);                          \
                                                                        \
  const CTYPE* value_data = CONST_DEREF(value);                         \
                                                                        \
  r_obj* out = KEEP(vec_clone_referenced(x, ownership));                \
  CTYPE* out_data = DEREF(out);                                         \
                                                                        \
  r_ssize value_loc = 0;                                                \
                                                                        \
  for (r_ssize index_loc = 0; index_loc < index_size; ++index_loc) {    \
    const int index_elt = index_data[index_loc];                        \
    if (index_elt == 1) {                                               \
      out_data[index_loc] = value_data[value_loc];                      \
    }                                                                   \
    value_loc += VALUE_INCR;                                            \
  }                                                                     \
                                                                        \
  FREE(1);                                                              \
  return out

#define ASSIGN_CONDITION(CTYPE, DEREF, CONST_DEREF)                              \
  const r_ssize value_size = r_length(value);                                    \
  check_assign_sizes(x, index, value_size, slice_value, index_style);            \
                                                                                 \
  if (is_compact_seq(index)) {                                                   \
    r_stop_internal(                                                             \
      "Compact sequence `index` are not supported in the condition path."        \
    );                                                                           \
  } else {                                                                       \
    if (value_size == 1) {                                                       \
      ASSIGN_CONDITION_INDEX(CTYPE, DEREF, CONST_DEREF, 0);                      \
    } else if (should_slice_value(slice_value)) {                                \
      ASSIGN_CONDITION_INDEX(CTYPE, DEREF, CONST_DEREF, 1);                      \
    } else {                                                                     \
      ASSIGN_CONDITION_INDEX(CTYPE, DEREF, CONST_DEREF, index_elt != 0);         \
    }                                                                            \
  }

#define ASSIGN(CTYPE, DEREF, CONST_DEREF)                               \
  switch (index_style) {                                                \
  case VCTRS_INDEX_STYLE_location: {                                    \
    ASSIGN_LOCATION(CTYPE, DEREF, CONST_DEREF);                         \
  }                                                                     \
  case VCTRS_INDEX_STYLE_condition: {                                   \
    ASSIGN_CONDITION(CTYPE, DEREF, CONST_DEREF);                        \
  }                                                                     \
  default: r_stop_unreachable();                                        \
  }

static
r_obj* lgl_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  ASSIGN(int, LOGICAL, LOGICAL_RO);
}
static
r_obj* int_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  ASSIGN(int, r_int_begin, INTEGER_RO);
}
static
r_obj* dbl_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  ASSIGN(double, REAL, REAL_RO);
}
static
r_obj* cpl_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  ASSIGN(Rcomplex, COMPLEX, COMPLEX_RO);
}
static
r_obj* raw_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  ASSIGN(Rbyte, RAW, RAW_RO);
}


#define ASSIGN_BARRIER_LOCATION_INDEX(CTYPE, CONST_DEREF, SET, VALUE_LOC)  \
  const r_ssize index_size = r_length(index);                              \
  const int* index_data = r_int_cbegin(index);                             \
                                                                           \
  CTYPE const* value_data = CONST_DEREF(value);                            \
                                                                           \
  r_obj* out = KEEP(vec_clone_referenced(x, ownership));                   \
                                                                           \
  for (r_ssize index_loc = 0; index_loc < index_size; ++index_loc) {       \
    const int index_elt = index_data[index_loc];                           \
    if (index_elt != r_globals.na_int) {                                   \
      const r_ssize out_loc = index_elt - 1;                               \
      SET(out, out_loc, value_data[VALUE_LOC]);                            \
    }                                                                      \
  }                                                                        \
                                                                           \
  FREE(1);                                                                 \
  return out

#define ASSIGN_BARRIER_LOCATION_COMPACT(CTYPE, CONST_DEREF, SET, VALUE_LOC)  \
  const int* index_data = r_int_cbegin(index);                               \
  const r_ssize start = index_data[0];                                       \
  const r_ssize index_size = index_data[1];                                  \
  const r_ssize step = index_data[2];                                        \
                                                                             \
  CTYPE const* value_data = CONST_DEREF(value);                              \
                                                                             \
  r_obj* out = KEEP(vec_clone_referenced(x, ownership));                     \
  r_ssize out_loc = start;                                                   \
                                                                             \
  for (r_ssize index_loc = 0; index_loc < index_size; ++index_loc) {         \
    SET(out, out_loc, value_data[VALUE_LOC]);                                \
    out_loc += step;                                                         \
  }                                                                          \
                                                                             \
  FREE(1);                                                                   \
  return out


#define ASSIGN_BARRIER_LOCATION(CTYPE, CONST_DEREF, SET)                       \
  const r_ssize value_size = r_length(value);                                  \
  check_assign_sizes(x, index, value_size, slice_value, index_style);          \
                                                                               \
  if (is_compact_seq(index)) {                                                 \
    if (value_size == 1) {                                                     \
      ASSIGN_BARRIER_LOCATION_COMPACT(CTYPE, CONST_DEREF, SET, 0);             \
    } else if (should_slice_value(slice_value)) {                              \
      ASSIGN_BARRIER_LOCATION_COMPACT(CTYPE, CONST_DEREF, SET, out_loc);       \
    } else {                                                                   \
      ASSIGN_BARRIER_LOCATION_COMPACT(CTYPE, CONST_DEREF, SET, index_loc);     \
    }                                                                          \
  } else {                                                                     \
    if (value_size == 1) {                                                     \
      ASSIGN_BARRIER_LOCATION_INDEX(CTYPE, CONST_DEREF, SET, 0);               \
    } else if (should_slice_value(slice_value)) {                              \
      ASSIGN_BARRIER_LOCATION_INDEX(CTYPE, CONST_DEREF, SET, out_loc);         \
    } else {                                                                   \
      ASSIGN_BARRIER_LOCATION_INDEX(CTYPE, CONST_DEREF, SET, index_loc);       \
    }                                                                          \
  }

#define ASSIGN_BARRIER_CONDITION_INDEX(CTYPE, CONST_DEREF, SET, VALUE_INCR)     \
  const r_ssize index_size = r_length(index);                                   \
  const int* index_data = r_lgl_cbegin(index);                                  \
                                                                                \
  CTYPE const* value_data = CONST_DEREF(value);                                 \
                                                                                \
  r_obj* out = KEEP(vec_clone_referenced(x, ownership));                        \
                                                                                \
  r_ssize value_loc = 0;                                                        \
                                                                                \
  for (r_ssize index_loc = 0; index_loc < index_size; ++index_loc) {            \
    const int index_elt = index_data[index_loc];                                \
    if (index_elt == 1) {                                                       \
      SET(out, index_loc, value_data[value_loc]);                               \
    }                                                                           \
    value_loc += VALUE_INCR;                                                    \
  }                                                                             \
                                                                                \
  FREE(1);                                                                      \
  return out

#define ASSIGN_BARRIER_CONDITION(CTYPE, CONST_DEREF, SET)                      \
  const r_ssize value_size = r_length(value);                                  \
  check_assign_sizes(x, index, value_size, slice_value, index_style);          \
                                                                               \
  if (is_compact_seq(index)) {                                                 \
    r_stop_internal(                                                           \
      "Compact sequence `index` are not supported in the condition path."      \
    );                                                                         \
  } else {                                                                     \
    if (value_size == 1) {                                                     \
      ASSIGN_BARRIER_CONDITION_INDEX(CTYPE, CONST_DEREF, SET, 0);              \
    } else if (should_slice_value(slice_value)) {                              \
      ASSIGN_BARRIER_CONDITION_INDEX(CTYPE, CONST_DEREF, SET, 1);              \
    } else {                                                                   \
      ASSIGN_BARRIER_CONDITION_INDEX(CTYPE, CONST_DEREF, SET, index_elt != 0); \
    }                                                                          \
  }

#define ASSIGN_BARRIER(CTYPE, CONST_DEREF, SET)                         \
  switch (index_style) {                                                \
  case VCTRS_INDEX_STYLE_location: {                                    \
    ASSIGN_BARRIER_LOCATION(CTYPE, CONST_DEREF, SET);                   \
  }                                                                     \
  case VCTRS_INDEX_STYLE_condition: {                                   \
    ASSIGN_BARRIER_CONDITION(CTYPE, CONST_DEREF, SET);                  \
  }                                                                     \
  default: r_stop_unreachable();                                        \
  }

r_obj* chr_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  ASSIGN_BARRIER(r_obj*, r_chr_cbegin, r_chr_poke);
}
r_obj* list_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  ASSIGN_BARRIER(r_obj*, r_list_cbegin, r_list_poke);
}

/**
 * Invariants:
 *
 * - `out` and `value` must be rectangular lists.
 * - `value` must have the same size as `index` or be size 1.
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
    .slice_value = p_opts->slice_value,
    .call = p_opts->call,
    .x_arg = p_opts->x_arg,
    .value_arg = p_opts->value_arg,
    .index_style = p_opts->index_style,
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
r_obj* vec_assign_fallback(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  r_obj* ffi_slice_value;
  switch (slice_value) {
  case ASSIGNMENT_SLICE_VALUE_no: ffi_slice_value = r_false; break;
  case ASSIGNMENT_SLICE_VALUE_yes: ffi_slice_value = r_true; break;
  default: r_stop_unreachable();
  }

  r_obj* ffi_index_style;
  switch (index_style) {
  case VCTRS_INDEX_STYLE_location: ffi_index_style = chrs.location; break;
  case VCTRS_INDEX_STYLE_condition: ffi_index_style = chrs.condition; break;
  default: r_stop_unreachable();
  }

  return vctrs_dispatch5(syms_vec_assign_fallback, fns_vec_assign_fallback,
                         syms_x, x,
                         syms_i, index,
                         syms_slice_value, ffi_slice_value,
                         syms_index_style, ffi_index_style,
                         syms_value, value);
}

static
r_obj* vec_proxy_assign_names(
  r_obj* proxy,
  r_obj* index,
  r_obj* value,
  enum vctrs_ownership ownership,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
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
  proxy_nms = KEEP(chr_assign(
    proxy_nms,
    index,
    value_nms,
    ownership,
    slice_value,
    index_style
  ));

  proxy = KEEP(vec_clone_referenced(proxy, ownership));
  proxy = vec_proxy_set_names(proxy, proxy_nms, ownership);

  FREE(5);
  return proxy;
}

// Helper for determining if we have a logical "condition" index we can optimize
// via `VCTRS_INDEX_STYLE_condition`. Otherwise we use `vec_as_location()` and
// convert to integer locations.
//
// Optimization avoids a `which(i)` conversion in `vec_as_location()`,
// which helps in two ways:
// - We don't allocate an integer vector of locations where the vector is `TRUE`
// - We don't perform extra passes through `i`, typically a `which()` call requires
//   2 passes over `i`
//
// Restrictions:
// - Must be logical
// - Can't be an array
// - Can't be an object (objects go through `vec_as_location()` casting)
// - Must be the same size as `x` (i.e. no scalar `TRUE`)
//
// Notably allowed:
// - Can have other attributes, including names
bool is_condition_index(r_obj* index, r_ssize size) {
  if (r_typeof(index) != R_TYPE_logical) {
    return false;
  }
  if (has_dim(index)) {
    return false;
  }
  if (r_is_object(index)) {
    return false;
  }
  if (r_length(index) != size) {
    return false;
  }
  return true;
}

// Cheap internal checks done right before assignment to avoid R crashes in corrupt cases
void check_assign_sizes(
  r_obj* x,
  r_obj* index,
  r_ssize value_size,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style
) {
  switch (index_style) {
  case VCTRS_INDEX_STYLE_location: {
    switch (slice_value) {
    case ASSIGNMENT_SLICE_VALUE_no: {
      if (value_size != 1 && value_size != vec_subscript_size(index)) {
        r_stop_internal("`value` should have been recycled to match `index`.");
      }
      return;
    }
    case ASSIGNMENT_SLICE_VALUE_yes: {
      if (value_size != 1 && value_size != vec_size(x)) {
        r_stop_internal("`value` should have been recycled to match `x`.");
      }
      return;
    }
    default: r_stop_unreachable();
    }
  }
  case VCTRS_INDEX_STYLE_condition: {
    switch (slice_value) {
    case ASSIGNMENT_SLICE_VALUE_no: {
      // In theory, we'd check `value_size` against the number of `TRUE`
      // values in `index`, but this is too expensive, so we rely on
      // the caller to check this.
      return;
    }
    case ASSIGNMENT_SLICE_VALUE_yes: {
      if (value_size != 1 && value_size != vec_size(x)) {
        r_stop_internal("`value` should have been recycled to match `x`.");
      }
      return;
    }
    default: r_stop_unreachable();
    }
  }
  default: r_stop_unreachable();
  }
}

// Checks that `value` has the correct size
//
// Note that `index` must have already been converted to positive integer indices
// with `vec_as_location()`, because that can change its size.
//
// Note that `index` can be a `compact_seq()`, so we need `vec_subscript_size()`.
void check_value_recyclable(
  r_obj* value,
  r_obj* index,
  r_ssize x_size,
  enum assignment_slice_value slice_value,
  enum vctrs_index_style index_style,
  struct vctrs_arg* p_value_arg,
  struct r_lazy call
) {
  r_ssize check_size;

  switch (slice_value) {
  case ASSIGNMENT_SLICE_VALUE_no: {
    switch (index_style) {
    case VCTRS_INDEX_STYLE_location: check_size = vec_subscript_size(index); break;
    case VCTRS_INDEX_STYLE_condition: check_size = r_lgl_sum(index, true); break;
    default: r_stop_unreachable();
    }
    break;
  }
  case ASSIGNMENT_SLICE_VALUE_yes: check_size = x_size; break;
  default: r_stop_unreachable();
  }

  vec_check_recyclable(value, check_size, p_value_arg, call);
}

// Exported for testing
// [[ register() ]]
r_obj* ffi_assign_seq(
  r_obj* x,
  r_obj* value,
  r_obj* ffi_start,
  r_obj* ffi_size,
  r_obj* ffi_increasing,
  r_obj* ffi_slice_value
) {
  r_ssize start = r_int_get(ffi_start, 0);
  r_ssize size = r_int_get(ffi_size, 0);
  bool increasing = r_lgl_get(ffi_increasing, 0);

  struct r_lazy call = lazy_calls.vec_assign_seq;

  const enum assignment_slice_value slice_value =
    r_arg_as_bool(ffi_slice_value, "slice_value") ?
    ASSIGNMENT_SLICE_VALUE_yes :
    ASSIGNMENT_SLICE_VALUE_no;

  r_obj* index = KEEP(compact_seq(start, size, increasing));
  const enum vctrs_index_style index_style = VCTRS_INDEX_STYLE_location;

  // Comes from the R side, so not owned, and not proxying recursively
  const struct vec_proxy_assign_opts assign_opts = {
    .x_arg = vec_args.x,
    .value_arg = vec_args.value,
    .call = call,
    .slice_value = slice_value,
    .index_style = index_style,
    .ownership = VCTRS_OWNERSHIP_foreign,
    .recursively_proxied = false
  };
  struct vec_restore_opts restore_opts = {
    .ownership = VCTRS_OWNERSHIP_foreign,
    .recursively_proxied = false
  };

  const r_ssize x_size = vec_size(x);

  // Cast `value` and check that it can recycle
  value = KEEP(vec_cast(value, x, vec_args.value, vec_args.x, call));

  check_value_recyclable(
    value,
    index,
    x_size,
    assign_opts.slice_value,
    assign_opts.index_style,
    assign_opts.value_arg,
    assign_opts.call
  );

  r_obj* proxy = KEEP(vec_proxy(x));
  proxy = KEEP(vec_proxy_assign_opts(proxy, index, value, &assign_opts));
  r_obj* out = vec_restore_opts(proxy, x, &restore_opts);

  FREE(4);
  return out;
}


void vctrs_init_slice_assign(r_obj* ns) {
  syms_vec_assign_fallback = r_sym("vec_assign_fallback");
  fns_vec_assign_fallback = r_eval(syms_vec_assign_fallback, ns);
}

static r_obj* syms_vec_assign_fallback = NULL;
static r_obj* fns_vec_assign_fallback = NULL;
