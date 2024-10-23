#include "vctrs.h"
#include "type-data-frame.h"
#include "decl/proxy-restore-decl.h"

// FIXME: Having `owned` as an argument to `vec_restore()` may be
// unnecessary once we have recursive proxy / restore mechanisms.
// It currently helps resolve performance issues in `vec_rbind()`'s usage of
// `df_assign()`, which repeatedly proxies and restores each column,
// causing duplication to occur. Passing `owned` through here allows us to
// call `vec_clone_referenced()`, which won't attempt to clone if we know we
// own the object. See #1151.
r_obj* vec_restore(r_obj* x, r_obj* to, enum vctrs_owned owned) {
  return vec_restore_4(x, to, owned, VCTRS_RECURSE_false);
}
r_obj* vec_restore_recurse(r_obj* x, r_obj* to, enum vctrs_owned owned) {
  return vec_restore_4(x, to, owned, VCTRS_RECURSE_true);
}

r_obj* ffi_vec_restore(r_obj* x, r_obj* to) {
  return vec_restore(x, to, vec_owned(x));
}
r_obj* ffi_vec_restore_recurse(r_obj* x, r_obj* to) {
  return vec_restore_recurse(x, to, vec_owned(x));
}

static
r_obj* vec_restore_4(r_obj* x,
                     r_obj* to,
                     enum vctrs_owned owned,
                     enum vctrs_recurse recurse) {
  enum vctrs_class_type to_type = class_type(to);

  switch (to_type) {
  case VCTRS_CLASS_bare_factor:
  case VCTRS_CLASS_bare_ordered:
  case VCTRS_CLASS_none: return vec_restore_default(x, to, owned);
  case VCTRS_CLASS_bare_date: return vec_date_restore(x, to, owned);
  case VCTRS_CLASS_bare_posixct: return vec_posixct_restore(x, to, owned);
  case VCTRS_CLASS_bare_posixlt: return vec_posixlt_restore(x, to, owned);
  case VCTRS_CLASS_bare_data_frame:
  case VCTRS_CLASS_bare_tibble: return vec_bare_df_restore(x, to, owned, recurse);
  case VCTRS_CLASS_data_frame: return vec_df_restore(x, to, owned, recurse);
  default:
    if (recurse && is_data_frame(x)) {
      return vec_df_restore(x, to, owned, recurse);
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


// Copy attributes except names and dim. This duplicates `x` if needed.
r_obj* vec_restore_default(r_obj* x, r_obj* to, enum vctrs_owned owned) {
  r_obj* attrib = r_attrib(to);

  const bool is_s4 = IS_S4_OBJECT(to);

  if (attrib == r_null && !is_s4) {
    return x;
  }

  int n_prot = 0;

  attrib = KEEP(r_clone(attrib));
  ++n_prot;

  x = KEEP(vec_clone_referenced(x, owned));
  ++n_prot;

  // Remove vectorised attributes which might be incongruent after reshaping.
  // Shouldn't matter for GNU R but other R implementations might have checks.
  // Also record class to set it later with `r_attrib_poke()`. This restores
  // the OBJECT bit and is likely more compatible with other implementations.
  r_obj* cls = r_null;

  {
    r_obj* node = attrib;
    r_obj* prev = r_null;

    while (node != r_null) {
      r_obj* tag = r_node_tag(node);

      // Skip special attributes
      if (tag == r_syms.names || tag == r_syms.dim ||
          tag == r_syms.dim_names || tag == r_syms.class_ ||
          tag == r_syms.row_names) {
        if (tag == r_syms.class_) {
          cls = r_node_car(node);
        }

        if (prev == r_null) {
          attrib = r_node_cdr(attrib);
        } else {
          r_node_poke_cdr(prev, r_node_cdr(node));
        }

        node = r_node_cdr(node);
        continue;
      }

      prev = node;
      node = r_node_cdr(node);
    }
  }

  // Copy attributes but keep names and dims. Don't restore names for
  // shaped objects since those are generated from dimnames.
  r_obj* dim = KEEP(r_attrib_get(x, r_syms.dim));
  ++n_prot;

  if (dim == r_null) {
    r_obj* nms = KEEP(r_attrib_get(x, r_syms.names));

    // Check if `to` is a data frame early. If `x` and `to` point
    // to the same reference, then `r_poke_attrib()` would alter `to`.
    r_obj* rownms = KEEP(df_rownames(x));
    const bool restore_rownms = rownms != r_null && is_data_frame(to);

    r_poke_attrib(x, attrib);

    r_attrib_poke(x, r_syms.names, nms);

    // Don't restore row names if `to` isn't a data frame
    if (restore_rownms) {
      r_attrib_poke(x, r_syms.row_names, rownms);
    }
    FREE(2);
  } else {
    r_obj* dimnames = KEEP(r_attrib_get(x, r_syms.dim_names));

    r_poke_attrib(x, attrib);

    r_attrib_poke(x, r_syms.dim, dim);
    r_attrib_poke(x, r_syms.dim_names, dimnames);
    FREE(1);
  }

  if (cls != r_null) {
    r_attrib_poke(x, r_syms.class_, cls);
  }

  if (is_s4) {
    r_mark_s4(x);
  }

  FREE(n_prot);
  return x;
}

r_obj* ffi_vec_restore_default(r_obj* x, r_obj* to) {
  return vec_restore_default(x, to, vec_owned(x));
}


r_obj* vec_df_restore(r_obj* x,
                      r_obj* to,
                      enum vctrs_owned owned,
                      enum vctrs_recurse recurse) {
  r_obj* out = KEEP(vec_bare_df_restore(x, to, owned, recurse));
  out = vec_restore_dispatch(out, to);
  FREE(1);
  return out;
}

r_obj* vec_bare_df_restore(r_obj* x,
                           r_obj* to,
                           enum vctrs_owned owned,
                           enum vctrs_recurse recurse) {
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

  if (recurse) {
    r_ssize n_cols = r_length(x);
    if (n_cols != r_length(to)) {
      r_stop_internal("Shape of `x` doesn't match `to` in recursive df restoration.");
    };

    r_obj* const * v_x = r_list_cbegin(x);
    r_obj* const * v_to = r_list_cbegin(to);

    for (r_ssize i = 0; i < n_cols; ++i) {
      r_obj* x_restored = vec_restore_recurse(v_x[i], v_to[i], owned);
      r_list_poke(x, i, x_restored);
    }
  }

  x = KEEP(vec_restore_default(x, to, owned));

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
    x = vec_proxy_set_names(x, rownames, owned);
    FREE(1);
  }

  FREE(2);
  FREE(n_prot);
  return x;
}

r_obj* ffi_vec_bare_df_restore(r_obj* x, r_obj* to) {
  return vec_bare_df_restore(x,
                             to,
                             vec_owned(x),
                             VCTRS_RECURSE_false);
}


void vctrs_init_proxy_restore(r_obj* ns) {
  syms_vec_restore_dispatch = r_sym("vec_restore_dispatch");
  fns_vec_restore_dispatch = r_eval(syms_vec_restore_dispatch, ns);
}

static r_obj* syms_vec_restore_dispatch = NULL;
static r_obj* fns_vec_restore_dispatch = NULL;
