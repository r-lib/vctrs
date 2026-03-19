#include "vctrs.h"
#include "type-data-frame.h"
#include "decl/proxy-restore-decl.h"

r_obj* ffi_vec_restore(r_obj* x, r_obj* to) {
  // Never own user objects, so foreign ownership.
  // Hooked to `vec_restore()`, which is called after an R level non-recursive
  // `vec_proxy()`, so not recursively proxied.
  struct vec_restore_opts opts = {
    .ownership = VCTRS_OWNERSHIP_foreign,
    .recursively_proxied = false
  };
  return vec_restore_opts(x, to, &opts);
}

// Exposed for testing
r_obj* ffi_vec_restore_recurse(r_obj* x, r_obj* to) {
  // Never own user objects, but we are restoring recursively here for testing purposes
  struct vec_restore_opts opts = {
    .ownership = VCTRS_OWNERSHIP_foreign,
    .recursively_proxied = true
  };
  return vec_restore_opts(x, to, &opts);
}

// Restoration with full options
//
// This gives you the ability to perform a recursive restore, which is needed
// when `vec_proxy_recurse()` is used, like in `vec_c()` and `vec_rbind()`. In
// that case you also specify `VCTRS_OWNERSHIP_deep` to indicate full ownership
// to avoid copies (of data frame columns in particular) during the restoration
// process.
r_obj* vec_restore_opts(
  r_obj* x,
  r_obj* to,
  const struct vec_restore_opts* p_opts
) {
  enum vctrs_class_type to_type = class_type(to);

  switch (to_type) {
  case VCTRS_CLASS_bare_factor:
  case VCTRS_CLASS_bare_ordered:
  case VCTRS_CLASS_none: return vec_restore_default(x, to, p_opts->ownership);
  case VCTRS_CLASS_bare_date: return vec_date_restore(x, to, p_opts->ownership);
  case VCTRS_CLASS_bare_posixct: return vec_posixct_restore(x, to, p_opts->ownership);
  case VCTRS_CLASS_bare_posixlt: return vec_posixlt_restore(x, to, p_opts->ownership);
  case VCTRS_CLASS_bare_data_frame:
  case VCTRS_CLASS_bare_tibble: return vec_bare_df_restore(x, to, p_opts);
  case VCTRS_CLASS_data_frame: return vec_df_restore(x, to, p_opts);
  default:
    if (p_opts->recursively_proxied && is_data_frame(x)) {
      return vec_df_restore(x, to, p_opts);
    } else {
      return vec_restore_dispatch(x, to);
    }
  }
}


static
r_obj* vec_restore_dispatch(r_obj* x, r_obj* to) {
  return vctrs_dispatch2(syms_vec_restore_dispatch, fns_vec_restore_dispatch,
                         syms_x, x,
                         syms_to, to);
}

struct vec_restore_collect_data {
  r_obj* names;
  r_obj* dim;
  r_obj* dim_names;
  r_obj* row_names;
};

static r_obj* vec_restore_collect_cb(r_obj* tag, r_obj* value, void* data) {
  struct vec_restore_collect_data* p_data = (struct vec_restore_collect_data*) data;

  if (tag == r_syms.names) {
    p_data->names = value;
    return NULL;
  }
  if (tag == r_syms.dim) {
    p_data->dim = value;
    return NULL;
  }
  if (tag == r_syms.dim_names) {
    p_data->dim_names = value;
    return NULL;
  }
  if (tag == r_syms.row_names) {
    p_data->row_names = value;
    return NULL;
  }

  return NULL;
}

// Default algorithm to restore `x` to the type of `to`
//
// 4 attributes from `x` are retained:
// - `names`
// - `dim`
// - `dimnames`
// - `row.names`
//
// All other `x` attributes are cleared and are replaced with `to`'s attributes.
//
// Duplicates `x` as needed according to `ownership`. Recursive ownership is
// never useful here, because we only touch the container, not the elements inside.
r_obj* vec_restore_default(r_obj* x, r_obj* to, enum vctrs_ownership ownership) {
  if (x == to) {
    // Rare but useful pointer comparison
    return x;
  }

  if (!r_attrib_has_any(x) && !r_attrib_has_any(to)) {
    // No one has attributes (nothing to clear, nothing to add).
    // Don't need to worry about OBJECT/S4 bit, you'd always have at least one attribute.
    return x;
  }

  // Note: Could this be `R_tryWrap()` one day? For backwards compatibility with
  // R < 4.6, maybe we would have our own ALTREP wrapper class implementation?
  // https://github.com/wch/r-source/commit/84293ec070c219b9ad2df44ae84d3f0f58d5ce7c
  x = KEEP(vec_clone_referenced(x, ownership));

  // In one pass, collect attributes we want to retain from `x`
  struct vec_restore_collect_data data = {
    .names = r_null,
    .dim = r_null,
    .dim_names = r_null,
    .row_names = r_null
  };
  r_attrib_map(x, vec_restore_collect_cb, &data);
  r_obj* names = KEEP(data.names);
  r_obj* dim = KEEP(data.dim);
  r_obj* dim_names = KEEP(data.dim_names);
  r_obj* row_names = KEEP(data.row_names);

  if (dim == r_null) {
    // This is a vector, clear any `dim` or `dim_names`
    dim_names = r_null;
  } else {
    // This is an array, clear any `names` or `row_names`
    names = r_null;
    row_names = r_null;
  }

  // We don't actually retain row names if `to` isn't a data frame
  if (row_names != r_null && !is_data_frame(to)) {
    row_names = r_null;
  }

  // Copy over all attributes from `to`
  //
  // Uses `SHALLOW_DUPLICATE_ATTRIB()`. Notably:
  // - Shallow clones attribute pairlist from `to`
  // - `SET_ATTRIB()` for efficiency and avoids
  //   `Rf_getAttrib()`/`Rf_setAttrib()` funny business
  // - `SET_OBJECT()`
  // - `SET_S4_OBJECT()` / `UNSET_S4_OBJECT()`
  r_attrib_clone_from(x, to);

  // Retain specific attributes from `x`
  //
  // We must set all 4! If `to` had `names` but `x` does not, then `names` will
  // be `r_null` and will clear the `names` brought over by
  // `r_attrib_clone_from()`.
  r_attrib_poke(x, r_syms.names, names);
  r_attrib_poke(x, r_syms.dim, dim);
  r_attrib_poke(x, r_syms.dim_names, dim_names);
  r_attrib_poke(x, r_syms.row_names, row_names);

  FREE(5);
  return x;
}

r_obj* ffi_vec_restore_default(r_obj* x, r_obj* to) {
  // Never own user objects
  return vec_restore_default(x, to, VCTRS_OWNERSHIP_foreign);
}


r_obj* vec_df_restore(r_obj* x,
                      r_obj* to,
                      const struct vec_restore_opts* p_opts) {
  r_obj* out = KEEP(vec_bare_df_restore(x, to, p_opts));
  out = vec_restore_dispatch(out, to);
  FREE(1);
  return out;
}

r_obj* vec_bare_df_restore(r_obj* x,
                           r_obj* to,
                           const struct vec_restore_opts* p_opts) {
  if (r_typeof(x) != R_TYPE_list) {
    r_stop_internal("Attempt to restore data frame from a %s.",
                    r_type_as_c_string(r_typeof(x)));
  }

  int n_prot = 0;

  if (!is_data_frame(to)) {
    to = KEEP_N(vec_proxy(to), &n_prot);
    if (!is_data_frame(to)) {
      r_stop_internal("Expected restoration target to have a df proxy.");
    }
  }

  if (p_opts->recursively_proxied) {
    r_ssize n_cols = r_length(x);
    if (n_cols != r_length(to)) {
      r_stop_internal("Shape of `x` doesn't match `to` in recursive df restoration.");
    };

    r_obj* const * v_x = r_list_cbegin(x);
    r_obj* const * v_to = r_list_cbegin(to);

    // During restoration, if we have deep ownership over `x` we can
    // propagate that ownership to the columns, otherwise we have no
    // known ownership over the columns
    enum vctrs_ownership col_ownership;
    switch (p_opts->ownership) {
    case VCTRS_OWNERSHIP_foreign: col_ownership = VCTRS_OWNERSHIP_foreign; break;
    case VCTRS_OWNERSHIP_shallow: col_ownership = VCTRS_OWNERSHIP_foreign; break;
    case VCTRS_OWNERSHIP_deep: col_ownership = VCTRS_OWNERSHIP_deep; break;
    default: r_stop_unreachable();
    }

    const struct vec_restore_opts col_opts = {
      .ownership = col_ownership,
      .recursively_proxied = p_opts->recursively_proxied
    };

    for (r_ssize i = 0; i < n_cols; ++i) {
      r_obj* x_restored = vec_restore_opts(v_x[i], v_to[i], &col_opts);
      r_list_poke(x, i, x_restored);
    }
  }

  x = KEEP(vec_restore_default(x, to, p_opts->ownership));

  if (r_attrib_get(x, r_syms.names) == r_null) {
    r_obj* names = KEEP(r_alloc_character(r_length(x)));
    r_attrib_poke(x, r_syms.names, names);
    FREE(1);
  }

  r_obj* rownames = KEEP(df_rownames(x));

  if (rownames == r_null) {
    r_ssize size = df_raw_size(x);
    init_compact_rownames(x, size);
  } else if (rownames_type(rownames) == ROWNAMES_TYPE_identifiers) {
    rownames = KEEP(vec_as_names(rownames, p_unique_repair_silent_opts));
    x = vec_proxy_set_names(x, rownames, p_opts->ownership);
    FREE(1);
  }

  FREE(2);
  FREE(n_prot);
  return x;
}

// Mapped to `vec_restore.data.frame()`, which is called after non-recursive R
// level `vec_proxy()`, so we don't need recursion here
r_obj* ffi_vec_bare_df_restore(r_obj* x, r_obj* to) {
  // Never own user objects
  const struct vec_restore_opts opts = {
    .ownership = VCTRS_OWNERSHIP_foreign,
    .recursively_proxied = false
  };
  return vec_bare_df_restore(x, to, &opts);
}

void vctrs_init_proxy_restore(r_obj* ns) {
  syms_vec_restore_dispatch = r_sym("vec_restore_dispatch");
  fns_vec_restore_dispatch = r_eval(syms_vec_restore_dispatch, ns);
}

static r_obj* syms_vec_restore_dispatch = NULL;
static r_obj* fns_vec_restore_dispatch = NULL;
