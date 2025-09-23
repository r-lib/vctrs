#include "vctrs.h"
#include "type-data-frame.h"
#include "altrep.h"

#define SLICE_SUBSCRIPT(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE)     \
  const CTYPE* data = CONST_DEREF(x);                                   \
  r_ssize n = r_length(subscript);                                      \
  int* subscript_data = r_int_begin(subscript);                         \
                                                                        \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, n));                          \
  CTYPE* out_data = DEREF(out);                                         \
                                                                        \
  for (r_ssize i = 0; i < n; ++i, ++subscript_data, ++out_data) {       \
    int j = *subscript_data;                                            \
    *out_data = (j == r_globals.na_int) ? NA_VALUE : data[j - 1];       \
  }                                                                     \
                                                                        \
  FREE(1);                                                              \
  return out

#define SLICE_COMPACT_REP(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE)   \
  const CTYPE* data = CONST_DEREF(x);                                   \
                                                                        \
  int* subscript_data = r_int_begin(subscript);                         \
  r_ssize j = subscript_data[0];                                        \
  r_ssize n = subscript_data[1];                                        \
                                                                        \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, n));                          \
  CTYPE* out_data = DEREF(out);                                         \
                                                                        \
  CTYPE elt = (j == r_globals.na_int) ? NA_VALUE : data[j - 1];         \
                                                                        \
  for (r_ssize i = 0; i < n; ++i, ++out_data) {                         \
    *out_data = elt;                                                    \
  }                                                                     \
                                                                        \
  FREE(1);                                                              \
  return out

#define SLICE_COMPACT_SEQ(RTYPE, CTYPE, DEREF, CONST_DEREF)     \
  int* subscript_data = r_int_begin(subscript);                 \
  r_ssize start = subscript_data[0];                            \
  r_ssize n = subscript_data[1];                                \
  r_ssize step = subscript_data[2];                             \
                                                                \
  const CTYPE* data = CONST_DEREF(x) + start;                   \
                                                                \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, n));                  \
  CTYPE* out_data = DEREF(out);                                 \
                                                                \
  for (int i = 0; i < n; ++i, ++out_data, data += step) {       \
    *out_data = *data;                                          \
  }                                                             \
                                                                \
  FREE(1);                                                      \
  return out

#define SLICE(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE)               \
  if (!materialize && ALTREP(x)) {                                      \
    r_obj* alt_subscript = KEEP(vec_subscript_materialize(subscript));  \
    r_obj* out = ALTVEC_EXTRACT_SUBSET_PROXY(x, alt_subscript, r_null); \
    FREE(1);                                                            \
    if (out != NULL) {                                                  \
      return out;                                                       \
    }                                                                   \
  }                                                                     \
  if (is_compact_rep(subscript)) {                                      \
    SLICE_COMPACT_REP(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE);      \
  } else if (is_compact_seq(subscript)) {                               \
    SLICE_COMPACT_SEQ(RTYPE, CTYPE, DEREF, CONST_DEREF);                \
  } else {                                                              \
    SLICE_SUBSCRIPT(RTYPE, CTYPE, DEREF, CONST_DEREF, NA_VALUE);        \
  }

static
r_obj* lgl_slice(r_obj* x, r_obj* subscript, enum vctrs_materialize materialize) {
  SLICE(R_TYPE_logical, int, r_lgl_begin, r_lgl_cbegin, r_globals.na_lgl);
}
static
r_obj* int_slice(r_obj* x, r_obj* subscript, enum vctrs_materialize materialize) {
  SLICE(R_TYPE_integer, int, r_int_begin, r_int_cbegin, r_globals.na_int);
}
static
r_obj* dbl_slice(r_obj* x, r_obj* subscript, enum vctrs_materialize materialize) {
  SLICE(R_TYPE_double, double, r_dbl_begin, r_dbl_cbegin, r_globals.na_dbl);
}
static
r_obj* cpl_slice(r_obj* x, r_obj* subscript, enum vctrs_materialize materialize) {
  SLICE(R_TYPE_complex, r_complex, r_cpl_begin, r_cpl_cbegin, r_globals.na_cpl);
}
static
r_obj* raw_slice(r_obj* x, r_obj* subscript, enum vctrs_materialize materialize) {
  SLICE(R_TYPE_raw, char, (char*) r_raw_begin, (char*) r_raw_cbegin, 0);
}


#define SLICE_BARRIER_SUBSCRIPT(RTYPE, CONST_DEREF, SET, NA_VALUE)      \
  r_obj* const * data = CONST_DEREF(x);                                 \
                                                                        \
  r_ssize n = r_length(subscript);                                      \
  int* subscript_data = r_int_begin(subscript);                         \
                                                                        \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, n));                          \
                                                                        \
  for (r_ssize i = 0; i < n; ++i, ++subscript_data) {                   \
    int j = *subscript_data;                                            \
    r_obj* elt = (j == r_globals.na_int) ? NA_VALUE : data[j - 1];      \
    SET(out, i, elt);                                                   \
  }                                                                     \
                                                                        \
  FREE(1);                                                              \
  return out


#define SLICE_BARRIER_COMPACT_REP(RTYPE, CONST_DEREF, SET, NA_VALUE)    \
  r_obj* const * data = CONST_DEREF(x);                                 \
                                                                        \
  int* subscript_data = r_int_begin(subscript);                         \
  r_ssize j = subscript_data[0];                                        \
  r_ssize n = subscript_data[1];                                        \
                                                                        \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, n));                          \
                                                                        \
  r_obj* elt = (j == r_globals.na_int) ? NA_VALUE : data[j - 1];        \
                                                                        \
  for (r_ssize i = 0; i < n; ++i) {                                     \
    SET(out, i, elt);                                                   \
  }                                                                     \
                                                                        \
  FREE(1);                                                              \
  return out

#define SLICE_BARRIER_COMPACT_SEQ(RTYPE, CONST_DEREF, SET)      \
  r_obj* const * data = CONST_DEREF(x);                         \
                                                                \
  int* subscript_data = r_int_begin(subscript);                 \
  r_ssize start = subscript_data[0];                            \
  r_ssize n = subscript_data[1];                                \
  r_ssize step = subscript_data[2];                             \
                                                                \
  r_obj* out = KEEP(r_alloc_vector(RTYPE, n));                  \
                                                                \
  for (r_ssize i = 0; i < n; ++i, start += step) {              \
    SET(out, i, data[start]);                                   \
  }                                                             \
                                                                \
  FREE(1);                                                      \
  return out

#define SLICE_BARRIER(RTYPE, CONST_DEREF, SET, NA_VALUE)                \
  if (is_compact_rep(subscript)) {                                      \
    SLICE_BARRIER_COMPACT_REP(RTYPE, CONST_DEREF, SET, NA_VALUE);       \
  } else if (is_compact_seq(subscript)) {                               \
    SLICE_BARRIER_COMPACT_SEQ(RTYPE, CONST_DEREF, SET);                 \
  } else {                                                              \
    SLICE_BARRIER_SUBSCRIPT(RTYPE, CONST_DEREF, SET, NA_VALUE);         \
  }

static
r_obj* chr_slice(r_obj* x, r_obj* subscript, enum vctrs_materialize materialize) {
  SLICE_BARRIER(R_TYPE_character, r_chr_cbegin, r_chr_poke, r_globals.na_str);
}
static
r_obj* chr_names_slice(r_obj* x, r_obj* subscript, enum vctrs_materialize materialize) {
  SLICE_BARRIER(R_TYPE_character, r_chr_cbegin, r_chr_poke, r_strs.empty);
}
static
r_obj* list_slice(r_obj* x, r_obj* subscript) {
  SLICE_BARRIER(R_TYPE_list, r_list_cbegin, r_list_poke, r_null);
}


static
r_obj* df_slice(r_obj* x, r_obj* subscript) {
  r_ssize n = r_length(x);
  r_ssize size = df_size(x);

  r_obj* out = KEEP(r_alloc_list(n));

  // FIXME: Should that be restored?
  r_obj* nms = r_names(x);
  r_attrib_poke(out, r_syms.names, nms);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* elt = r_list_get(x, i);

    if (vec_size(elt) != size) {
      r_stop_internal("Column `%s` (size %" R_PRI_SSIZE ") must match the data frame (size %" R_PRI_SSIZE ").",
                      r_chr_get_c_string(nms, i),
                      vec_size(elt),
                      size);
    }

    r_obj* sliced = vec_slice_unsafe(elt, subscript);
    r_list_poke(out, i, sliced);
  }

  init_data_frame(out, vec_subscript_size(subscript));

  r_obj* row_nms = KEEP(df_rownames(x));
  if (r_typeof(row_nms) == R_TYPE_character) {
    row_nms = slice_rownames(row_nms, subscript);
    r_attrib_poke(out, r_syms.row_names, row_nms);
  }

  FREE(2);
  return out;
}


r_obj* vec_slice_fallback(r_obj* x, r_obj* subscript) {
  // TODO - Remove once bit64 is updated on CRAN. Special casing integer64
  // objects to ensure correct slicing with `NA_integer_`.
  if (is_integer64(x)) {
    return vctrs_dispatch2(syms.vec_slice_fallback_integer64, fns.vec_slice_fallback_integer64,
                           syms_x, x,
                           syms_i, subscript);
  }

  return vctrs_dispatch2(syms.vec_slice_fallback, fns.vec_slice_fallback,
                         syms_x, x,
                         syms_i, subscript);
}

static
r_obj* vec_slice_dispatch(r_obj* x, r_obj* subscript) {
  // TODO - Remove once bit64 is updated on CRAN. Special casing integer64
  // objects to ensure correct slicing with `NA_integer_`.
  if (is_integer64(x)) {
    return vctrs_dispatch2(syms.vec_slice_dispatch_integer64, fns.vec_slice_dispatch_integer64,
                           syms_x, x,
                           syms_i, subscript);
  }

  return vctrs_dispatch2(syms_bracket, fns_bracket,
                         syms_x, x,
                         syms_i, subscript);
}

bool vec_requires_fallback(r_obj* x, struct vctrs_proxy_info info) {
  return r_is_object(x) &&
    !info.had_proxy_method &&
    info.type != VCTRS_TYPE_dataframe;
}

r_obj* vec_slice_base(enum vctrs_type type,
                      r_obj* x,
                      r_obj* subscript,
                      enum vctrs_materialize materialize) {
  switch (type) {
  case VCTRS_TYPE_logical:   return lgl_slice(x, subscript, materialize);
  case VCTRS_TYPE_integer:   return int_slice(x, subscript, materialize);
  case VCTRS_TYPE_double:    return dbl_slice(x, subscript, materialize);
  case VCTRS_TYPE_complex:   return cpl_slice(x, subscript, materialize);
  case VCTRS_TYPE_character: return chr_slice(x, subscript, materialize);
  case VCTRS_TYPE_raw:       return raw_slice(x, subscript, materialize);
  case VCTRS_TYPE_list:      return list_slice(x, subscript);
  default: stop_unimplemented_vctrs_type("vec_slice_base", type);
  }
}

r_obj* slice_names(r_obj* names, r_obj* subscript) {
  if (names == r_null) {
    return names;
  } else {
    // Ensures `NA_integer_` subscripts utilize `""` as the name
    return chr_names_slice(names, subscript, VCTRS_MATERIALIZE_false);
  }
}
r_obj* slice_rownames(r_obj* names, r_obj* subscript) {
  if (names == r_null) {
    return names;
  }

  names = KEEP(chr_slice(names, subscript, VCTRS_MATERIALIZE_false));

  // Rownames can't contain `NA` or duplicates
  names = vec_as_unique_names(names, true);

  FREE(1);
  return names;
}

r_obj* vec_slice_unsafe(r_obj* x, r_obj* subscript) {
  int nprot = 0;

  struct vctrs_proxy_info info = vec_proxy_info(x);
  KEEP_N_PROXY_INFO(info, &nprot);

  // Fallback to `[` if the class doesn't implement a proxy. This is
  // to be maximally compatible with existing classes.
  if (vec_requires_fallback(x, info)) {
    if (info.type == VCTRS_TYPE_scalar) {
      obj_check_vector(x, NULL, r_lazy_null);
    }

    subscript = KEEP_N(vec_subscript_materialize(subscript), &nprot);

    r_obj* out;

    if (has_dim(x)) {
      out = KEEP_N(vec_slice_fallback(x, subscript), &nprot);
    } else {
      out = KEEP_N(vec_slice_dispatch(x, subscript), &nprot);
    }

    // Take over attribute restoration only if there is no `[` method
    if (!vec_is_restored(out, x)) {
      // Sliced `out` comes from R, so is foreign. Technically not proxied at all,
      // so "restoring" is a bit of a hack, but we only restore if it looks like the
      // `[` result is missing attributes.
      struct vec_restore_opts restore_opts = {
        .ownership = VCTRS_OWNERSHIP_foreign,
        .recursively_proxied = false
      };

      out = vec_restore_opts(out, x, &restore_opts);
    }

    FREE(nprot);
    return out;
  }

  switch (info.type) {
  case VCTRS_TYPE_null:
    r_stop_internal("Unexpected `NULL`.");

  case VCTRS_TYPE_logical:
  case VCTRS_TYPE_integer:
  case VCTRS_TYPE_double:
  case VCTRS_TYPE_complex:
  case VCTRS_TYPE_character:
  case VCTRS_TYPE_raw:
  case VCTRS_TYPE_list: {
    r_obj* out;

    if (has_dim(x)) {
      out = KEEP_N(vec_slice_shaped(info.type, info.proxy, subscript), &nprot);

      r_obj* names = KEEP_N(r_attrib_get(x, r_syms.dim_names), &nprot);
      if (names != r_null) {
        names = KEEP_N(r_clone(names), &nprot);
        r_obj* row_names = r_list_get(names, 0);
        row_names = KEEP_N(slice_names(row_names, subscript), &nprot);
        r_list_poke(names, 0, row_names);
        r_attrib_poke(out, r_syms.dim_names, names);
      }
    } else {
      out = KEEP_N(vec_slice_base(info.type, info.proxy, subscript, VCTRS_MATERIALIZE_false), &nprot);

      r_obj* names = KEEP_N(r_names(x), &nprot);
      names = KEEP_N(slice_names(names, subscript), &nprot);
      r_attrib_poke_names(out, names);
    }

    // Sliced `out` is a fresh object from `vec_slice_base()` or
    // `vec_slice_shaped()` that we control (and we even modify names directly
    // above). For atomics, shallow and deep ownership are the same. We mark as
    // shallow just for consistency with the data frame path.
    struct vec_restore_opts restore_opts = {
      .ownership = VCTRS_OWNERSHIP_shallow,
      .recursively_proxied = false
    };

    out = vec_restore_opts(out, x, &restore_opts);

    FREE(nprot);
    return out;
  }

  case VCTRS_TYPE_dataframe: {
    r_obj* out = KEEP_N(df_slice(info.proxy, subscript), &nprot);

    // Sliced `out` is a fresh list container from `df_slice()`, but we don't
    // necessarily own the sliced columns (an individual column could have gone
    // through the fallback path) so we set shallow ownership. This is fine, we
    // don't restore recursively here, so only the list container will need to
    // be modified during restoration.
    struct vec_restore_opts restore_opts = {
      .ownership = VCTRS_OWNERSHIP_shallow,
      .recursively_proxied = false
    };

    out = vec_restore_opts(out, x, &restore_opts);

    FREE(nprot);
    return out;
  }

  default:
    stop_unimplemented_vctrs_type("vec_slice_impl", info.type);
  }
}

bool vec_is_restored(r_obj* x, r_obj* to) {
  // Don't restore if there is an actual `[` method that ignored
  // attributes. Some methods like [.ts intentionally strip the class
  // and attributes. FIXME: This branch is now probably sufficient.
  if (s3_find_method("[", to, base_method_table) != r_null) {
    return true;
  }

  r_obj* attrib = r_attrib(x);

  if (attrib == r_null) {
    return false;
  }

  // Class is restored if it contains any other attributes than names.
  // We might want to add support for data frames later on.
  r_obj* node = attrib;
  while (node != r_null) {
    if (r_node_tag(node) == r_syms.names) {
      node = r_node_cdr(node);
      continue;
    }
    return true;
  }

  return false;
}

r_obj* ffi_slice(r_obj* x,
                 r_obj* i,
                 r_obj* frame) {
  struct vec_slice_opts opts = {
    .x_arg = vec_args.x,
    .i_arg = vec_args.i,
    .call = {.x = r_syms.error_call, .env = frame}
  };
  return vec_slice_opts(x, i, &opts);
}

r_obj* vec_slice_opts(r_obj* x,
                      r_obj* i,
                      const struct vec_slice_opts* opts) {
  obj_check_vector(x, opts->x_arg, opts->call);

  r_obj* names = KEEP(vec_names(x));
  i = KEEP(vec_as_location_ctxt(i,
                                vec_size(x),
                                names,
                                opts->i_arg,
                                opts->call));

  r_obj* out = vec_slice_unsafe(x, i);

  FREE(2);
  return out;
}

// Reverse a vector
r_obj* vec_reverse(r_obj* x) {
  const r_ssize size = vec_size(x);
  const r_ssize start = (size == 0) ? 0 : size - 1;
  const bool increasing = false;
  r_obj* index = KEEP(compact_seq(start, size, increasing));

  r_obj* out = vec_slice_unsafe(x, index);

  FREE(1);
  return out;
}

r_obj* vec_init(r_obj* x, r_ssize n) {
  obj_check_vector(x, vec_args.x, lazy_calls.vec_init);

  if (n < 0) {
    r_abort_lazy_call(lazy_calls.vec_init,
                      "%s must be a positive integer.",
                      r_c_str_format_error_arg("n"));
  }

  r_obj* i = KEEP(compact_rep(r_globals.na_int, n));
  r_obj* out = vec_slice_unsafe(x, i);

  FREE(1);
  return out;
}

// [[ register() ]]
r_obj* ffi_init(r_obj* x, r_obj* ffi_n, r_obj* ffi_frame) {
  struct r_lazy call = { .x = ffi_frame, .env = r_null };
  r_ssize n = vec_as_short_length(ffi_n, vec_args.n, call);

  r_obj* out = vec_init(x, n);

  return out;
}

// Exported for testing
// [[ register() ]]
r_obj* ffi_slice_seq(r_obj* x,
                     r_obj* ffi_start,
                     r_obj* ffi_size,
                     r_obj* ffi_increasing) {
  r_ssize start = r_int_get(ffi_start, 0);
  r_ssize size = r_int_get(ffi_size, 0);
  bool increasing = r_lgl_get(ffi_increasing, 0);

  r_obj* subscript = KEEP(compact_seq(start, size, increasing));
  r_obj* out = vec_slice_unsafe(x, subscript);

  FREE(1);
  return out;
}

// Exported for testing
// [[ register() ]]
r_obj* ffi_slice_rep(r_obj* x, r_obj* ffi_i, r_obj* ffi_n) {
  r_ssize i = r_int_get(ffi_i, 0);
  r_ssize n = r_int_get(ffi_n, 0);

  r_obj* subscript = KEEP(compact_rep(i, n));
  r_obj* out = vec_slice_unsafe(x, subscript);

  FREE(1);
  return out;
}


void vctrs_init_slice(r_obj* ns) {
  syms.vec_slice_dispatch_integer64 = r_sym("vec_slice_dispatch_integer64");
  syms.vec_slice_fallback = r_sym("vec_slice_fallback");
  syms.vec_slice_fallback_integer64 = r_sym("vec_slice_fallback_integer64");

  fns.vec_slice_dispatch_integer64 = r_eval(syms.vec_slice_dispatch_integer64, ns);
  fns.vec_slice_fallback = r_eval(syms.vec_slice_fallback, ns);
  fns.vec_slice_fallback_integer64 = r_eval(syms.vec_slice_fallback_integer64, ns);
}
