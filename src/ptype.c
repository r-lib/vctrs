#include "vctrs.h"
#include "proxy-data.h"

#include "decl/ptype-decl.h"


// [[ register() ]]
r_obj* ffi_ptype(r_obj* x, r_obj* x_arg_ffi, r_obj* frame) {
  struct vctrs_arg x_arg = vec_as_arg(x_arg_ffi);
  struct r_lazy call = { .x = r_syms.call, .env = frame };
  return vec_ptype(x, &x_arg, call);
}

/// Compute the prototype of `x`
///
/// Morally, a prototype is a size 0 representation of the minimum amount of
/// information needed to uniquely identify the type of `x`.
///
/// - For `NULL`, `NULL`
///
/// - For unclassed vectors, a size 0 vector
///   - All attributes are cleared
///     - Notably, `names` are cleared
///
/// - For unclassed arrays, a size 0 array
///   - Almost all attributes are cleared
///     - `dim` is retained to hold the "shape", as that is part of the type
///     - Notably, `dimnames` are cleared
///
/// - For bare data frames / tibbles, a size 0 data frame / tibble
///   - Almost all attributes are cleared
///   - `names` are retained to hold the column names, which are part of the type
///   - `class` is retained, as that is part of the type
///   - `vec_ptype()` is called on each column
///
/// - For S3 objects
///
///   - If a `vec_ptype()` S3 method is implemented, we call that. No further
///     checking is done, for maximum performance. We rely on the package author
///     to implement a well formed ptype that strips all unrelated attributes.
///
///   - Otherwise, we do the following:
///
///     ```r
///     # Retrieve R object appropriate for C level manipulation
///     proxy <- vec_proxy(x)
///
///     # Strip that R object down to its most "native" form
///     # - For vectors, only `names` are retained
///     # - For arrays, only `dim` and `dimnames` are retained
///     # - For data frames, only `names`, `row.names`, and the `"data.frame"` class are retained
///     proxy <- proxy_data(proxy)
///
///     # Compute ptype of this "native" form
///     proxy <- vec_ptype(proxy)
///
///     # Restore back to original type
///     vec_restore(proxy, x)
///     ```
///
///     Note that this is a "best effort" fallback that does retain all attributes
///     from `x` except for the `names`, `dimnames`, or `row.names`.
///     This is a "best effort" fallback that does retain all other attributes, as we
///     have no way to know what is an extraneous attribute (beyond names), and
///     what is an attribute that is core to the type itself.
r_obj* vec_ptype(r_obj* x, struct vctrs_arg* p_x_arg, struct r_lazy call) {
  switch (vec_typeof(x)) {
  case VCTRS_TYPE_null: {
    return r_null;
  }
  case VCTRS_TYPE_unspecified: {
    return vctrs_shared_empty_uns;
  }
  case VCTRS_TYPE_logical: {
    return vec_shaped_ptype(r_globals.empty_lgl, x);
  }
  case VCTRS_TYPE_integer: {
    return vec_shaped_ptype(r_globals.empty_int, x);
  }
  case VCTRS_TYPE_double: {
    return vec_shaped_ptype(r_globals.empty_dbl, x);
  }
  case VCTRS_TYPE_complex: {
    return vec_shaped_ptype(r_globals.empty_cpl, x);
  }
  case VCTRS_TYPE_character: {
    return vec_shaped_ptype(r_globals.empty_chr, x);
  }
  case VCTRS_TYPE_raw: {
    return vec_shaped_ptype(r_globals.empty_raw, x);
  }
  case VCTRS_TYPE_list: {
    return vec_shaped_ptype(r_globals.empty_list, x);
  }
  case VCTRS_TYPE_dataframe: {
    return df_ptype(x, /*tibble=*/false);
  }
  case VCTRS_TYPE_s3: {
    switch (class_type(x)) {
    case VCTRS_CLASS_none: {
      r_stop_unreachable();
    }
    case VCTRS_CLASS_bare_data_frame: {
      r_stop_unreachable();
    }
    case VCTRS_CLASS_bare_tibble: {
      return df_ptype(x, /*tibble=*/true);
    }
    default: {
      obj_check_vector(x, VCTRS_ALLOW_NULL_no, p_x_arg, call);
      return s3_ptype(x);
    }
    }
  }
  case VCTRS_TYPE_scalar: {
    stop_scalar_type(x, p_x_arg, call);
  }
  default: {
    r_stop_unreachable();
  }
  }
}

static
r_obj* s3_ptype(r_obj* x) {
  // Use `vec_ptype()` S3 method if one exists.
  // For maximal performance, no additional checking is done.
  r_obj* method = vec_ptype_method(x);
  if (method != r_null) {
    KEEP(method);
    r_obj* out = vec_ptype_invoke(x, method);
    FREE(1);
    return out;
  }

  // Otherwise use "fallback" approach of calling `vec_ptype()` on the proxy's
  // native data. `proxy_data()` prevents this from being an infloop.
  r_obj* proxy = KEEP(vec_proxy(x));
  proxy = KEEP(proxy_data(proxy));
  proxy = KEEP(vec_ptype(proxy, vec_args.empty, r_lazy_null));

  // Must be `VCTRS_OWNERSHIP_foreign`, as `vec_ptype()` returns shared objects
  struct vec_restore_opts opts = {
    .ownership = VCTRS_OWNERSHIP_foreign,
    .recursively_proxied = false
  };

  r_obj* out = vec_restore_opts(proxy, x, &opts);

  FREE(3);
  return out;
}

static inline
r_obj* vec_ptype_method(r_obj* x) {
  r_obj* cls = KEEP(s3_get_class(x));
  r_obj* method = s3_class_find_method("vec_ptype", cls, vctrs_method_table);
  FREE(1);
  return method;
}

static inline
r_obj* vec_ptype_invoke(r_obj* x, r_obj* method) {
  return vctrs_dispatch1(syms_vec_ptype, method, syms_x, x);
}

static inline
r_obj* df_ptype(r_obj* x, bool tibble) {
  r_obj* out = KEEP(map(x, &col_ptype));
  if (tibble) {
    r_init_tibble(out, 0);
  } else {
    r_init_data_frame(out, 0);
  }
  FREE(1);
  return out;
}

static inline
r_obj* col_ptype(r_obj* x) {
  return vec_ptype(x, vec_args.empty, r_lazy_null);
}

r_obj* vec_ptype_final(r_obj* x, struct vctrs_arg* p_x_arg, struct r_lazy call) {
  r_obj* out = KEEP(vec_ptype(x, p_x_arg, call));
  out = vec_ptype_finalise(out);
  FREE(1);
  return out;
}

void vctrs_init_ptype(r_obj* ns) {
  syms_vec_ptype = r_sym("vec_ptype");
}

static r_obj* syms_vec_ptype = NULL;
