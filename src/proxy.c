#include "vctrs.h"
#include "type-data-frame.h"
#include "decl/proxy-decl.h"


// [[ register() ]]
r_obj* vec_proxy(r_obj* x) {
  return vec_proxy_2(x, false);
}

// [[ register() ]]
r_obj* vec_proxy_recurse(r_obj* x) {
  return vec_proxy_2(x, true);
}

static
r_obj* vec_proxy_2(r_obj* x, bool recurse) {
  struct vctrs_type_info info = vec_type_info(x);
  KEEP(info.shelter);

  switch (info.type) {
  case VCTRS_TYPE_dataframe: {
    r_obj* out = recurse ? df_proxy_recurse(x) : x;
    FREE(1);
    return out;
  }

  case VCTRS_TYPE_s3: {
    r_obj* out = KEEP(vec_proxy_invoke(x, info.proxy_method));
    if (!is_data_frame(out)) {
      FREE(2);
      return out;
    }

    out = KEEP(recurse ? df_proxy_recurse(out) : out);

    FREE(3);
    return out;
  }

  default:
    FREE(1);
    return x;
  }
}

// Recurse into data frames
static
r_obj* df_proxy_recurse(r_obj* x) {
  r_obj* out = KEEP(vec_clone_referenced(x, VCTRS_OWNED_false));

  for (r_ssize i = 0, n = r_length(out); i < n; ++i) {
    r_obj* col = r_list_get(out, i);
    r_list_poke(out, i, vec_proxy_recurse(col));
  }

  FREE(1);
  return out;
}

// [[ register() ]]
r_obj* vec_proxy_equal(r_obj* x) {
  r_obj* out = KEEP(vec_proxy_equal_impl(x));

  if (is_data_frame(out)) {
    // Automatically proxy df-proxies recursively.
    // Also flattens and unwraps them (#1537, #1664).
    out = df_proxy(out, VCTRS_PROXY_KIND_equal);
  }

  FREE(1);
  return out;
}

// [[ register() ]]
r_obj* vec_proxy_compare(r_obj* x) {
  r_obj* out = KEEP(vec_proxy_compare_impl(x));

  if (is_data_frame(out)) {
    // Automatically proxy df-proxies recursively.
    // Also flattens and unwraps them (#1537, #1664).
    out = df_proxy(out, VCTRS_PROXY_KIND_compare);
  }

  FREE(1);
  return out;
}

// [[ register() ]]
r_obj* vec_proxy_order(r_obj* x) {
  r_obj* out = KEEP(vec_proxy_order_impl(x));

  if (is_data_frame(out)) {
    // Automatically proxy df-proxies recursively.
    // Also flattens and unwraps them (#1537, #1664).
    out = df_proxy(out, VCTRS_PROXY_KIND_order);
  }

  FREE(1);
  return out;
}


// Non-recursive variants called by the fallback path to ensure we only
// fallback on the container itself (like a df or rcrd) and not its elements
// (like columns or fields)
#define VEC_PROXY_KIND_IMPL(METHOD, INVOKE) do { \
  r_obj* method = KEEP(METHOD(x));               \
  r_obj* out = INVOKE(x, method);                \
  FREE(1);                                       \
  return out;                                    \
} while (0)                                      \

static inline
r_obj* vec_proxy_equal_impl(r_obj* x) {
  VEC_PROXY_KIND_IMPL(vec_proxy_equal_method, vec_proxy_equal_invoke);
}
static inline
r_obj* vec_proxy_compare_impl(r_obj* x) {
  VEC_PROXY_KIND_IMPL(vec_proxy_compare_method, vec_proxy_compare_invoke);
}
static inline
r_obj* vec_proxy_order_impl(r_obj* x) {
  VEC_PROXY_KIND_IMPL(vec_proxy_order_method, vec_proxy_order_invoke);
}

#undef VEC_PROXY_KIND_IMPL


r_obj* vec_proxy_method(r_obj* x) {
  return s3_find_method("vec_proxy", x, vctrs_method_table);
}

// This should be faster than normal dispatch but also means that
// proxy methods can't call `NextMethod()`. This could be changed if
// it turns out a problem.
r_obj* vec_proxy_invoke(r_obj* x, r_obj* method) {
  if (method == r_null) {
    return x;
  } else {
    return vctrs_dispatch1(syms_vec_proxy, method, syms_x, x);
  }
}

static inline
r_obj* vec_proxy_method_impl(r_obj* x, const char* generic, r_obj* fn_proxy_array) {
  r_obj* cls = KEEP(s3_get_class(x));
  r_obj* method = s3_class_find_method(generic, cls, vctrs_method_table);

  if (method != r_null) {
    FREE(1);
    return method;
  }

  /* FIXME: Stopgap check for bare arrays */
  /* which equality functions don't handle well */
  if (vec_dim_n(x) > 1) {
    FREE(1);
    return fn_proxy_array;
  }

  FREE(1);
  return r_null;
}

static inline
r_obj* vec_proxy_equal_method(r_obj* x) {
  return vec_proxy_method_impl(x, "vec_proxy_equal", fns_vec_proxy_equal_array);
}
static inline
r_obj* vec_proxy_compare_method(r_obj* x) {
  return vec_proxy_method_impl(x, "vec_proxy_compare", fns_vec_proxy_compare_array);
}
static inline
r_obj* vec_proxy_order_method(r_obj* x) {
  return vec_proxy_method_impl(x, "vec_proxy_order", fns_vec_proxy_order_array);
}

static inline
r_obj* vec_proxy_invoke_impl(r_obj* x,
                             r_obj* method,
                             r_obj* vec_proxy_sym,
                             r_obj* (*vec_proxy_impl_fn)(r_obj*)) {
  if (method != r_null) {
    return vctrs_dispatch1(vec_proxy_sym, method, syms_x, x);
  }

  /* Fallback on S3 objects with no proxy */
  if (vec_typeof(x) == VCTRS_TYPE_s3) {
    return vec_proxy_impl_fn(x);
  } else {
    return x;
  }
}

static inline
r_obj* vec_proxy_equal_invoke(r_obj* x, r_obj* method) {
  return vec_proxy_invoke_impl(x, method, syms_vec_proxy_equal, vec_proxy);
}
static inline
r_obj* vec_proxy_compare_invoke(r_obj* x, r_obj* method) {
  return vec_proxy_invoke_impl(x, method, syms_vec_proxy_compare, &vec_proxy_equal_impl);
}
static inline
r_obj* vec_proxy_order_invoke(r_obj* x, r_obj* method) {
  return vec_proxy_invoke_impl(x, method, syms_vec_proxy_order, &vec_proxy_compare_impl);
}


#define DF_PROXY(PROXY) do {                                   \
  const r_ssize n_cols = r_length(x);                          \
  r_obj* const* v_x = r_list_cbegin(x);                        \
                                                               \
  for (r_ssize i = 0; i < n_cols; ++i) {                       \
    r_obj* col = v_x[i];                                       \
    r_list_poke(x, i, PROXY(col));                             \
  }                                                            \
} while (0)

static inline
r_obj* df_proxy(r_obj* x, enum vctrs_proxy_kind kind) {
  x = KEEP(r_clone_referenced(x));

  switch (kind) {
  case VCTRS_PROXY_KIND_equal: DF_PROXY(vec_proxy_equal); break;
  case VCTRS_PROXY_KIND_compare: DF_PROXY(vec_proxy_compare); break;
  case VCTRS_PROXY_KIND_order: DF_PROXY(vec_proxy_order); break;
  }

  x = KEEP(df_flatten(x));
  x = vec_proxy_unwrap(x);

  FREE(2);
  return x;
}

r_obj* ffi_df_proxy(r_obj* x, r_obj* kind) {
  if (!r_is_number(kind)) {
    r_stop_internal("`kind` must be a single integer.");
  }

  enum vctrs_proxy_kind c_kind = r_int_get(kind, 0);

  return df_proxy(x, c_kind);
}

r_obj* vec_proxy_unwrap(r_obj* x) {
  if (r_typeof(x) == R_TYPE_list && r_length(x) == 1 && is_data_frame(x)) {
    x = vec_proxy_unwrap(r_list_get(x, 0));
  }
  return x;
}


r_obj* ffi_unset_s4(r_obj* x) {
  x = r_clone_referenced(x);
  r_unmark_s4(x);
  return x;
}


void vctrs_init_data(r_obj* ns) {
  syms_vec_proxy = r_sym("vec_proxy");

  syms_vec_proxy_equal = r_sym("vec_proxy_equal");
  syms_vec_proxy_equal_array = r_sym("vec_proxy_equal.array");

  syms_vec_proxy_compare = r_sym("vec_proxy_compare");
  syms_vec_proxy_compare_array = r_sym("vec_proxy_compare.array");

  syms_vec_proxy_order = r_sym("vec_proxy_order");
  syms_vec_proxy_order_array = r_sym("vec_proxy_order.array");

  fns_vec_proxy_equal_array = r_env_get(ns, syms_vec_proxy_equal_array);
  fns_vec_proxy_compare_array = r_env_get(ns, syms_vec_proxy_compare_array);
  fns_vec_proxy_order_array = r_env_get(ns, syms_vec_proxy_order_array);
}

r_obj* syms_vec_proxy = NULL;
r_obj* syms_vec_proxy_equal = NULL;
r_obj* syms_vec_proxy_equal_array = NULL;
r_obj* syms_vec_proxy_compare = NULL;
r_obj* syms_vec_proxy_compare_array = NULL;
r_obj* syms_vec_proxy_order = NULL;
r_obj* syms_vec_proxy_order_array = NULL;

r_obj* fns_vec_proxy_equal_array = NULL;
r_obj* fns_vec_proxy_compare_array = NULL;
r_obj* fns_vec_proxy_order_array = NULL;
