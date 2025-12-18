#include "unstructure.h"
#include "dim.h"
#include "utils.h"

#include "decl/unstructure-decl.h"

r_obj* ffi_vec_unstructure(r_obj* x) {
  return vec_unstructure(x);
}

r_obj* vec_unstructure(r_obj* x) {
  // Not using `obj_check_vector()`, that would infloop and doesn't have
  // the right semantics, the proxy isn't involved here. Note that using
  // `r_typeof()` does support S4 objects that are "built on" an atomic
  // vector type using `contains`.
  switch (r_typeof(x)) {
  case R_TYPE_logical:
  case R_TYPE_integer:
  case R_TYPE_double:
  case R_TYPE_complex:
  case R_TYPE_raw:
  case R_TYPE_character:
  case R_TYPE_list:
    break;
  default:
    stop_unsupported_storage_type(x);
  }

  if (is_data_frame(x)) {
    return df_unstructure(x);
  } else if (has_dim(x)) {
    return array_unstructure(x);
  } else {
    return atomic_unstructure(x);
  }
}

static inline
r_obj* atomic_unstructure(r_obj* x) {
  if (
    !r_is_object(x) &&
    !r_is_s4(x) &&
    has_unstructured_atomic_attributes(x)
  ) {
    // Already has the right attributes
    return x;
  }

  // Protect this as we are about to clear `x`
  r_obj* names = KEEP(r_names(x));

  // - ALTREP shallow clone `x`
  // - Clears OBJECT
  // - Clears S4
  // - Clears attributes
  r_obj* out = KEEP(r_set_attributes(x, r_null));

  if (names != r_null) {
    r_attrib_poke_names(out, names);
  }

  FREE(2);
  return out;
}

static inline
r_obj* array_unstructure(r_obj* x) {
  if (
    !r_is_object(x) &&
    !r_is_s4(x) &&
    has_unstructured_array_attributes(x) &&
    has_unstructured_array_dim_names(x)
  ) {
    // Already has the right attributes
    return x;
  }

  // Protect these as we are about to clear `x`
  r_obj* dim = KEEP(r_dim(x));
  r_obj* dim_names = KEEP(r_dim_names(x));
  dim_names = KEEP(dim_names_unstructure(dim_names));

  // - ALTREP shallow clone `x`
  // - Clears OBJECT
  // - Clears S4
  // - Clears attributes
  r_obj* out = KEEP(r_set_attributes(x, r_null));

  if (dim != r_null) {
    r_attrib_poke_dim(out, dim);
  }
  if (dim_names != r_null) {
    r_attrib_poke_dim_names(out, dim_names);
  }

  FREE(4);
  return out;
}

static inline
r_obj* df_unstructure(r_obj* x) {
  if (
    !r_is_s4(x) &&
    has_unstructured_data_frame_attributes(x) &&
    has_unstructured_data_frame_class(x)
  ) {
    // Already has the right attributes
    return x;
  }

  // Protect these as we are about to clear `x`
  r_obj* names = KEEP(r_names(x));

  // Using `r_attrib_get()` bypasses `Rf_getAttrib()`, so compact automatic
  // rownames of the form `c(NA, -n)` are left untouched. Calling
  // `Rf_setAttrib()` with these row names retains compact automatic row names
  // on the output. We may have to change this if we lose `ATTRIB()` for good.
  // https://github.com/r-devel/r-dev-day/issues/146
  r_obj* row_names = KEEP(r_attrib_get(x, r_syms.row_names));

  // - ALTREP shallow clone `x`
  // - Clears OBJECT
  // - Clears S4
  // - Clears attributes
  r_obj* out = KEEP(r_set_attributes(x, r_null));

  r_attrib_poke_names(out, names);
  r_attrib_poke(out, r_syms.row_names, row_names);
  r_attrib_poke_class(out, r_classes.data_frame);

  FREE(3);
  return out;
}

// Only `names`
static inline
bool has_unstructured_atomic_attributes(r_obj* x) {
  r_obj* node = r_attrib(x);

  while (node != r_null) {
    r_obj* tag = r_node_tag(node);
    if (tag != r_syms.names) {
      return false;
    }
    node = r_node_cdr(node);
  }

  return true;
}

// Only `dim` and `dimnames`
static inline
bool has_unstructured_array_attributes(r_obj* x) {
  r_obj* node = r_attrib(x);

  while (node != r_null) {
    r_obj* tag = r_node_tag(node);
    if (tag != r_syms.dim && tag != r_syms.dim_names) {
      return false;
    }
    node = r_node_cdr(node);
  }

  return true;
}

static inline
bool has_unstructured_array_dim_names(r_obj* x) {
  r_obj* dim_names = r_dim_names(x);
  return dim_names_are_unstructured(dim_names);
}

static inline
r_obj* dim_names_unstructure(r_obj* dim_names) {
  if (dim_names_are_unstructured(dim_names)) {
    return dim_names;
  }

  r_obj* const* v_dim_names = r_list_cbegin(dim_names);
  const r_ssize dimensionality = r_length(dim_names);

  if (dimensionality == 0) {
    r_stop_internal("Dimensionality of an array should be at least 1.");
  }

  r_obj* row_names = v_dim_names[0];

  if (row_names == r_null) {
    // Clear `dimnames`, there are no row names
    return r_null;
  }

  // Elements are initialized to `NULL`, only pull over row names
  r_obj* out = KEEP(r_alloc_list(dimensionality));
  r_list_poke(out, 0, row_names);

  FREE(1);
  return out;
}

// Unstructured `dimnames` are:
// - `NULL`
// - `list(<rownames>, <zero or more NULL>)`
//
// Notably not unstructured:
// - `list(<rownames>, <colnames>)` // remove column names
// - `list(NULL, <colnames>)` // no row names, remove entirely
// - `list(NULL, NULL)` // no row names, remove entirely
// - `list()` // not possible
// - `list(x = <rownames>)` // "meta" names on dimnames need to be removed
static inline
bool dim_names_are_unstructured(r_obj* dim_names) {
  if (dim_names == r_null) {
    return true;
  }

  if (r_names(dim_names) != r_null) {
    // We have to clear the meta names at the very least
    return false;
  }

  if (r_typeof(dim_names) != R_TYPE_list) {
    r_stop_internal("`dimnames` must be a list.");
  }

  r_obj* const* v_dim_names = r_list_cbegin(dim_names);
  const r_ssize dimensionality = r_length(dim_names);

  if (dimensionality == 0) {
    r_stop_internal("Dimensionality of an array should be at least 1.");
  }

  if (v_dim_names[0] == r_null) {
    // i.e. `list(NULL, <colnames>)` or somehow `list(NULL, NULL)`.
    // In this case `dimnames` should be totally removed as there are no row names.
    return false;
  }

  // Skipping `v_dim_names[0]`
  for (r_ssize i = 1; i < dimensionality; ++i) {
    if (v_dim_names[i] != r_null) {
      return false;
    }
  }

  return true;
}

// Only `names` and `rownames` and `class`
static inline
bool has_unstructured_data_frame_attributes(r_obj* x) {
  r_obj* node = r_attrib(x);

  while (node != r_null) {
    r_obj* tag = r_node_tag(node);
    if (tag != r_syms.names && tag != r_syms.row_names && tag != r_syms.class_) {
      return false;
    }
    node = r_node_cdr(node);
  }

  return true;
}

// `class` must be exactly `"data.frame"`
static inline
bool has_unstructured_data_frame_class(r_obj* x) {
  r_obj* class = r_class(x);

  if (r_typeof(class) != R_TYPE_character) {
    return false;
  }

  if (r_length(class) != 1) {
    return false;
  }

  const char* class_string = r_chr_get_c_string(class, 0);
  const bool is_data_frame = strcmp(class_string, "data.frame") == 0;

  return is_data_frame;
}

static inline
r_no_return
void stop_unsupported_storage_type(r_obj* x) {
  r_obj* syms[2] = {
    syms_x,
    NULL
  };
  r_obj* args[2] = {
    KEEP(r_protect(x)),
    NULL
  };

  r_obj* ffi_call = KEEP(r_call_n(
    syms_stop_unsupported_storage_type,
    syms,
    args
  ));
  r_eval(ffi_call, vctrs_ns_env);

  r_stop_unreachable();
}
