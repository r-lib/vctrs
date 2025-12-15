#include "proxy-data.h"
#include "dim.h"
#include "type-data-frame.h"
#include "utils.h"

#include "decl/proxy-data-decl.h"

r_obj* ffi_proxy_data(r_obj* x) {
  return proxy_data(x);
}

r_obj* proxy_data(r_obj* x) {
  if (is_data_frame(x)) {
    return df_proxy_data(x);
  } else if (has_dim(x)) {
    return array_proxy_data(x);
  } else {
    return atomic_proxy_data(x);
  }
}

static inline
r_obj* df_proxy_data(r_obj* x) {
  // We do need to protect these, as we are about to clear `x`
  r_obj* names = KEEP(r_names(x));
  r_obj* row_names = KEEP(df_rownames(x));
  const enum rownames_type row_names_type = rownames_type(row_names);
  const r_ssize size = rownames_size(row_names, row_names_type);

  // TODO!: At least add documentation on how this always ALTREP clones,
  // and some golden tests about it for `proxy_data()` itself.
  //
  // Presumably we are modifying `x` in place, which WE created
  // at the C level but ALSO need it to remain unmodified for
  // some future use of our own.

  // This clones `x` as required, and creates a cheap ALTREP shallow duplicate
  // of `x` with its own attribute pairlist
  r_obj* out = KEEP(vec_set_attributes(x, r_null));

  r_attrib_poke_names(out, names);
  r_init_data_frame(out, size);

  if (row_names_type == ROWNAMES_TYPE_identifiers) {
    r_attrib_poke(out, r_syms.row_names, row_names);
  }

  FREE(3);
  return out;
}

static inline
r_obj* array_proxy_data(r_obj* x) {
  // We do need to protect these, as we are about to clear `x`
  r_obj* dim = KEEP(r_dim(x));
  r_obj* dim_names = KEEP(r_dim_names(x));

  // This clones `x` as required, and creates a cheap ALTREP shallow duplicate
  // of `x` with its own attribute pairlist
  r_obj* out = KEEP(vec_set_attributes(x, r_null));

  r_attrib_poke_dim(out, dim);
  r_attrib_poke_dim_names(out, dim_names);

  FREE(3);
  return out;
}

static inline
r_obj* atomic_proxy_data(r_obj* x) {
  // We do need to protect this, as we are about to clear `x`
  r_obj* names = KEEP(r_names(x));

  // This clones `x` as required, and creates a cheap ALTREP shallow duplicate
  // of `x` with its own attribute pairlist
  r_obj* out = KEEP(vec_set_attributes(x, r_null));

  r_attrib_poke_names(out, names);

  FREE(2);
  return out;
}
