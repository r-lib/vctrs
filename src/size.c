#include <rlang.h>
#include "vctrs.h"
#include "type-data-frame.h"
#include "utils.h"
#include "slice.h"
#include "decl/size-decl.h"

r_ssize vec_size(r_obj* x) {
  int nprot = 0;

  struct vctrs_proxy_info info = vec_proxy_info(x);
  PROTECT_PROXY_INFO(&info, &nprot);

  r_obj* data = info.proxy;

  r_ssize size;
  switch (info.type) {
  case vctrs_type_null:
    size = 0;
    break;
  case vctrs_type_logical:
  case vctrs_type_integer:
  case vctrs_type_double:
  case vctrs_type_complex:
  case vctrs_type_character:
  case vctrs_type_raw:
  case vctrs_type_list:
    size = vec_raw_size(data);
    break;

  case vctrs_type_dataframe:
    size = df_size(data);
    break;

  default:
    stop_scalar_type(x, NULL, r_lazy_null);
}

  FREE(nprot);
  return size;
}
// [[ register() ]]
SEXP vctrs_size(SEXP x) {
  return Rf_ScalarInteger(vec_size(x));
}

static
r_ssize vec_raw_size(r_obj* x) {
  r_obj* dimensions = r_dim(x);

  if (dimensions == r_null || r_length(dimensions) == 0) {
    return r_length(x);
  }

  if (r_typeof(dimensions) != R_TYPE_integer) {
    r_stop_internal("vec_raw_size", "Corrupt vector, `dim` attribute is not an integer vector.");
  }

  return r_int_get(dimensions, 0);
}

// [[ register() ]]
r_obj* ffi_list_sizes(r_obj* x, r_obj* frame) {
  struct r_lazy call = { .x = frame, .env = r_null };
  return list_sizes(x, call);
}

static
r_obj* list_sizes(r_obj* x, struct r_lazy call) {
  if (!vec_is_list(x)) {
    r_abort_lazy_call(call, "`x` must be a list.");
  }

  r_ssize size = vec_size(x);
  r_obj* const * v_x = r_list_cbegin(x);

  r_obj* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_begin(out);

  r_attrib_poke_names(out, vec_names(x));

  for (R_len_t i = 0; i < size; ++i) {
    // TODO! Error call
    v_out[i] = vec_size(v_x[i]);
  }

  FREE(1);
  return out;
}

r_ssize df_rownames_size(r_obj* x) {
  for (r_obj* attr = r_attrib(x);
       attr != r_null;
       attr = r_node_cdr(attr)) {
    if (r_node_tag(attr) != r_syms.row_names) {
      continue;
    }

    return rownames_size(r_node_car(attr));
  }

  return -1;
}

// For performance, avoid Rf_getAttrib() because it automatically transforms
// the rownames into an integer vector
r_ssize df_size(r_obj* x) {
  r_ssize n = df_rownames_size(x);

  if (n < 0) {
    r_stop_internal("df_size", "Corrupt data frame: row.names are missing");
  }

  return n;
}
// Supports bare lists as well
r_ssize df_raw_size(r_obj* x) {
  r_ssize n = df_rownames_size(x);
  if (n >= 0) {
    return n;
  }

  return df_raw_size_from_list(x);
}

r_ssize df_raw_size_from_list(r_obj* x) {
  if (r_length(x) >= 1) {
    return vec_size(r_list_get(x, 0));
  } else {
    return 0;
  }
}

// [[ register() ]]
SEXP vctrs_df_size(SEXP x) {
  return r_int(df_raw_size(x));
}


r_obj* vec_recycle2(r_obj* x,
                    r_ssize size,
                    struct vctrs_arg* x_arg,
                    struct r_lazy call) {
  if (x == r_null) {
    return r_null;
  }

  r_ssize n_x = vec_size(x);

  if (n_x == size) {
    return x;
  }

  if (n_x == 1L) {
    r_obj* i = KEEP(compact_rep(1, size));
    r_obj* out = vec_slice_unsafe(x, i);

    FREE(1);
    return out;
  }

  stop_recycle_incompatible_size(n_x, size, x_arg, call);
}

// [[ register() ]]
r_obj* ffi_recycle(r_obj* x,
                   r_obj* size_obj,
                   r_obj* frame) {
  if (x == r_null || size_obj == r_null) {
    return r_null;
  }

  struct r_lazy recycle_call = { .x = frame, .env = r_null };

  size_obj = KEEP(vec_cast(size_obj,
                           vctrs_shared_empty_int,
                           args_empty,
                           args_empty,
                           recycle_call));
  R_len_t size = r_int_get(size_obj, 0);
  FREE(1);

  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy call = { .x = syms_call, .env = frame };

  return vec_recycle2(x, size, &x_arg, call);
}

r_obj* vec_recycle_fallback(r_obj* x,
                            r_ssize size,
                            struct vctrs_arg* x_arg) {
  if (x == r_null) {
    return r_null;
  }

  r_ssize x_size = vec_size(x);

  if (x_size == size) {
    return x;
  }

  if (x_size == 1) {
    r_obj* subscript = KEEP(r_alloc_integer(size));
    r_int_fill(subscript, 1, size);

    r_obj* out = vec_slice_fallback(x, subscript);

    FREE(1);
    return out;
  }

  stop_recycle_incompatible_size(x_size, size, x_arg, r_lazy_null);
}


r_ssize size_validate(r_obj* size, const char* arg) {
  // TODO! Error call
  struct r_lazy call = r_lazy_null;

  size = vec_cast(size,
                  vctrs_shared_empty_int,
                  args_empty,
                  args_empty,
                  call);

  if (r_length(size) != 1) {
    r_abort_lazy_call(call, "`%s` must be a single integer.", arg);
  }

  int out = r_int_get(size, 0);

  if (out == r_globals.na_int) {
    r_abort_lazy_call(call, "`%s` can't be missing.", arg);
  }

  return out;
}
