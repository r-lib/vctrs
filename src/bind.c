#include "vctrs.h"
#include "type-data-frame.h"
#include "decl/bind-decl.h"

// [[ register(external = TRUE) ]]
r_obj* ffi_rbind(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  struct r_lazy call = { .x = env, .env = r_null };

  r_obj* xs = KEEP(rlang_env_dots_list(env));
  r_obj* ptype = r_node_car(args); args = r_node_cdr(args);
  r_obj* names_to = r_node_car(args); args = r_node_cdr(args);
  r_obj* name_repair = r_node_car(args); args = r_node_cdr(args);
  r_obj* name_spec = r_node_car(args);

  if (names_to != r_null) {
    if (r_inherits(names_to, "rlang_zap")) {
      r_attrib_poke_names(xs, r_null);
      names_to = r_null;
    } else if (r_is_string(names_to)) {
      names_to = r_chr_get(names_to, 0);
    } else {
      r_abort_lazy_call(call,
                        "%s must be `NULL`, a string, or an `rlang::zap()` object.",
                        r_c_str_format_error_arg(".names_to"));
    }
  }

  struct name_repair_opts name_repair_opts = validate_bind_name_repair(name_repair, false);
  KEEP(name_repair_opts.shelter);

  name_repair_opts.call = call;

  r_obj* out = vec_rbind(xs,
                         ptype,
                         names_to,
                         &name_repair_opts,
                         name_spec,
                         call);

  FREE(2);
  return out;
}

static
r_obj* vec_rbind(r_obj* xs,
                 r_obj* ptype,
                 r_obj* names_to,
                 struct name_repair_opts* name_repair,
                 r_obj* name_spec,
                 struct r_lazy call) {
  // In case `.arg` is added later on
  struct vctrs_arg* p_arg = vec_args.empty;

  int n_prot = 0;
  r_ssize n_inputs = r_length(xs);

  for (r_ssize i = 0; i < n_inputs; ++i) {
    r_list_poke(xs, i, as_df_row(r_list_get(xs, i),
                                 name_repair,
                                 call));
  }

  // The common type holds information about common column names,
  // types, etc. Each element of `xs` needs to be cast to that type
  // before assignment.
  ptype = vec_ptype_common_params(xs,
                                  ptype,
                                  DF_FALLBACK_DEFAULT,
                                  S3_FALLBACK_true,
                                  p_arg,
                                  call);
  KEEP_N(ptype, &n_prot);

  r_ssize n_cols = r_length(ptype);

  if (ptype == r_null) {
    FREE(n_prot);
    return new_data_frame(vctrs_shared_empty_list, 0);
  }
  if (r_typeof(ptype) == R_TYPE_logical && !n_cols) {
    ptype = as_df_row_impl(vctrs_shared_na_lgl,
                           name_repair,
                           call);
    KEEP_N(ptype, &n_prot);
  }
  if (!is_data_frame(ptype)) {
    r_abort_lazy_call(call, "Can't bind objects that are not coercible to a data frame.");
  }

  bool assign_names = !r_inherits(name_spec, "rlang_zap");

  bool has_names_to = names_to != r_null;
  r_ssize names_to_loc = 0;

  if (has_names_to) {
    if (!assign_names) {
      r_abort_lazy_call(call,
                        "Can't zap outer names when %s is supplied.",
                        r_c_str_format_error_arg(".names_to"));
    }

    r_obj* ptype_nms = KEEP(r_names(ptype));
    names_to_loc = r_chr_find(ptype_nms, names_to);
    FREE(1);

    if (names_to_loc < 0) {
      ptype = cbind_names_to(r_names(xs) != r_null,
                             names_to,
                             ptype,
                             call);
      KEEP_N(ptype, &n_prot);
      names_to_loc = 0;
    }
  }

  // Must happen after the `names_to` column has been added to `ptype`
  xs = vec_cast_common_params(xs,
                              ptype,
                              DF_FALLBACK_DEFAULT,
                              S3_FALLBACK_true,
                              vec_args.empty,
                              call);
  KEEP_N(xs, &n_prot);

  // Find individual input sizes and total size of output
  r_ssize n_rows = 0;

  r_obj* ns_placeholder = KEEP_N(r_alloc_integer(n_inputs), &n_prot);
  int* ns = r_int_begin(ns_placeholder);

  for (r_ssize i = 0; i < n_inputs; ++i) {
    r_obj* elt = r_list_get(xs, i);
    r_ssize size = (elt == r_null) ? 0 : vec_size(elt);
    n_rows += size;
    ns[i] = size;
  }

  r_obj* proxy = KEEP_N(vec_proxy(ptype), &n_prot);
  if (!is_data_frame(proxy)) {
    r_abort_lazy_call(call, "Can't fill a data frame that doesn't have a data frame proxy.");
  }

  r_keep_loc out_pi;
  r_obj* out = vec_init(proxy, n_rows);
  KEEP_HERE(out, &out_pi);
  ++n_prot;

  r_obj* loc = KEEP_N(compact_seq(0, 0, true), &n_prot);
  int* p_loc = r_int_begin(loc);

  r_obj* row_names = r_null;
  r_keep_loc rownames_pi;
  KEEP_HERE(row_names, &rownames_pi);
  ++n_prot;

  r_obj* names_to_col = r_null;
  enum r_type names_to_type = 99;
  void* p_names_to_col = NULL;
  const void* p_index = NULL;

  r_obj* xs_names = KEEP_N(r_names(xs), &n_prot);
  bool xs_is_named = xs_names != r_null;

  if (has_names_to) {
    r_obj* index = r_null;
    if (xs_is_named) {
      index = xs_names;
    } else {
      index = KEEP_N(r_alloc_integer(n_inputs), &n_prot);
      r_int_fill_seq(index, 1, n_inputs);
    }
    names_to_type = r_typeof(index);
    names_to_col = KEEP_N(r_alloc_vector(names_to_type, n_rows), &n_prot);

    p_index = r_vec_deref_barrier_const(index);
    p_names_to_col = r_vec_deref_barrier(names_to_col);

    xs_names = r_null;
    xs_is_named = false;
  }

  r_obj* const * p_xs_names = NULL;
  if (xs_is_named) {
    p_xs_names = r_chr_cbegin(xs_names);
  }

  // Compact sequences use 0-based counters
  r_ssize counter = 0;

  const struct vec_assign_opts bind_assign_opts = {
    .assign_names = assign_names,
    // Unlike in `vec_c()` we don't need to ignore outer names because
    // `df_assign()` doesn't deal with those
    .ignore_outer_names = false
  };

  for (r_ssize i = 0; i < n_inputs; ++i) {
    r_ssize size = ns[i];
    if (!size) {
      continue;
    }
    r_obj* x = r_list_get(xs, i);

    init_compact_seq(p_loc, counter, size, true);

    // Total ownership of `out` because it was freshly created with `vec_init()`
    out = df_assign(out, loc, x, VCTRS_OWNED_true, &bind_assign_opts);
    KEEP_AT(out, out_pi);

    if (assign_names) {
      r_obj* outer = xs_is_named ? p_xs_names[i] : r_null;
      r_obj* inner = KEEP(vec_names(x));
      r_obj* x_nms = KEEP(apply_name_spec(name_spec, outer, inner, size));

      if (x_nms != r_null) {
        R_LAZY_ALLOC(row_names, rownames_pi, R_TYPE_character, n_rows);

        // If there is no name to assign, skip the assignment since
        // `out_names` already contains empty strings
        if (inner != chrs_empty) {
          row_names = chr_assign(row_names, loc, x_nms, VCTRS_OWNED_true);
          KEEP_AT(row_names, rownames_pi);
        }
      }

      FREE(2);
    }

    // Assign current name to group vector, if supplied
    if (has_names_to) {
      r_vec_fill(names_to_type, p_names_to_col, counter, p_index, i, size);
    }

    counter += size;
  }

  if (row_names != r_null) {
    r_attrib_poke(out, r_syms.row_names, row_names);
  }

  if (has_names_to) {
    out = df_poke(out, names_to_loc, names_to_col);
    KEEP_AT(out, out_pi);
  }

  // Not optimal. Happens after the fallback columns have been
  // assigned already, ideally they should be ignored. Also this is
  // currently not recursive. Should we deal with this during
  // restoration?
  for (r_ssize i = 0; i < n_cols; ++i) {
    r_obj* col = r_list_get(ptype, i);

    if (vec_is_common_class_fallback(col)) {
      r_obj* col_xs = KEEP(list_pluck(xs, i));
      r_obj* col_out = vec_c_fallback(col, col_xs, name_spec, name_repair);
      r_list_poke(out, i, col_out);
      FREE(1);
    }
  }

  r_obj* r_n_rows = KEEP_N(r_int(n_rows), &n_prot);
  out = vec_restore(out, ptype, r_n_rows, VCTRS_OWNED_true);

  FREE(n_prot);
  return out;
}

static
r_obj* as_df_row(r_obj* x,
                 struct name_repair_opts* name_repair,
                 struct r_lazy call) {
  if (vec_is_unspecified(x) && r_names(x) == r_null) {
    return x;
  } else {
    return as_df_row_impl(x, name_repair, call);
  }
}

static
r_obj* as_df_row_impl(r_obj* x,
                      struct name_repair_opts* name_repair,
                      struct r_lazy call) {
  if (x == r_null) {
    return x;
  }
  if (is_data_frame(x)) {
    return df_repair_names(x, name_repair);
  }

  int nprot = 0;

  r_obj* dim = vec_bare_dim(x);
  r_ssize ndim = (dim == r_null) ? 1 : r_length(dim);

  if (ndim > 2) {
    r_abort_lazy_call(call, "Can't bind arrays.");
  }
  if (ndim == 2) {
    r_obj* names = KEEP_N(vec_unique_colnames(x, name_repair->quiet), &nprot);
    r_obj* out = KEEP_N(r_as_data_frame(x), &nprot);
    r_attrib_poke_names(out, names);
    FREE(nprot);
    return out;
  }

  r_obj* nms = KEEP_N(vec_names(x), &nprot);

  if (dim != r_null) {
    x = KEEP_N(r_clone_referenced(x), &nprot);
    r_attrib_poke(x, r_syms.dim, r_null);
    r_attrib_poke(x, r_syms.dim_names, r_null);
  }

  // Remove names as they are promoted to data frame column names
  if (nms != r_null) {
    x = KEEP_N(vec_set_names(x, r_null), &nprot);
  }

  if (nms == r_null) {
    nms = KEEP_N(vec_unique_names(x, name_repair->quiet), &nprot);
  } else {
    nms = KEEP_N(vec_as_names(nms, name_repair), &nprot);
  }

  x = KEEP_N(vec_chop(x, r_null), &nprot);
  r_attrib_poke_names(x, nms);
  x = new_data_frame(x, 1);

  FREE(nprot);
  return x;
}

// [[ register() ]]
r_obj* ffi_as_df_row(r_obj* x, r_obj* quiet, r_obj* frame) {
  struct name_repair_opts name_repair_opts = {
    .type = name_repair_unique,
    .fn = r_null,
    .quiet = r_lgl_get(quiet, 0)
  };
  struct r_lazy call = { .x = frame, .env = r_null };
  return as_df_row(x, &name_repair_opts, call);
}

static
r_obj* cbind_names_to(bool has_names,
                      r_obj* names_to,
                      r_obj* ptype,
                      struct r_lazy call) {
  r_obj* index_ptype = has_names ? vctrs_shared_empty_chr : vctrs_shared_empty_int;

  r_obj* tmp = KEEP(r_alloc_list(2));
  r_list_poke(tmp, 0, index_ptype);
  r_list_poke(tmp, 1, ptype);

  r_obj* tmp_nms = KEEP(r_alloc_character(2));
  r_chr_poke(tmp_nms, 0, names_to);
  r_chr_poke(tmp_nms, 1, strings_empty);

  r_attrib_poke_names(tmp, tmp_nms);

  r_obj* out = vec_cbind(tmp, r_null, r_null, NULL, call);

  FREE(2);
  return out;
}


// [[ register(external = TRUE) ]]
r_obj* ffi_cbind(r_obj* ffi_call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  struct r_lazy call = { .x = env, .env = r_null };

  r_obj* xs = KEEP(rlang_env_dots_list(env));
  r_obj* ptype = r_node_car(args); args = r_node_cdr(args);
  r_obj* size = r_node_car(args); args = r_node_cdr(args);
  r_obj* name_repair = r_node_car(args);

  struct name_repair_opts name_repair_opts = validate_bind_name_repair(name_repair, true);
  KEEP(name_repair_opts.shelter);
  name_repair_opts.call = call;

  r_obj* out = vec_cbind(xs, ptype, size, &name_repair_opts, call);

  FREE(2);
  return out;
}

static
r_obj* vec_cbind(r_obj* xs,
                 r_obj* ptype,
                 r_obj* size,
                 struct name_repair_opts* name_repair,
                 struct r_lazy call) {
  // In case `.arg` is added later on
  struct vctrs_arg* p_arg = vec_args.empty;

  r_ssize n = r_length(xs);

  // Find the common container type of inputs
  r_obj* rownames = r_null;
  r_obj* containers = KEEP(map_with_data(xs, &cbind_container_type, &rownames));
  ptype = KEEP(cbind_container_type(ptype, &rownames));

  r_obj* type = KEEP(vec_ptype_common_params(containers,
                                             ptype,
                                             DF_FALLBACK_DEFAULT,
                                             S3_FALLBACK_false,
                                             p_arg,
                                             call));
  if (type == r_null) {
    type = new_data_frame(vctrs_shared_empty_list, 0);
  } else if (!is_data_frame(type)) {
    type = r_as_data_frame(type);
  }
  FREE(1);
  KEEP(type);


  r_ssize nrow;
  if (size == r_null) {
    nrow = vec_check_size_common(xs, 0, p_arg, call);
  } else {
    nrow = vec_as_short_length(size, vec_args.dot_size, call);
  }

  if (rownames != r_null && r_length(rownames) != nrow) {
    rownames = KEEP(vec_check_recycle(rownames, nrow, vec_args.empty, call));
    rownames = vec_as_unique_names(rownames, false);
    FREE(1);
  }
  KEEP(rownames);

  // Convert inputs to data frames, validate, and collect total number of columns
  r_obj* xs_names = KEEP(r_names(xs));
  bool has_names = xs_names != r_null;
  r_obj* const* xs_names_p = has_names ? r_chr_cbegin(xs_names) : NULL;

  r_ssize ncol = 0;
  for (r_ssize i = 0; i < n; ++i) {
    r_obj* x = r_list_get(xs, i);

    if (x == r_null) {
      continue;
    }

    x = KEEP(vec_check_recycle(x, nrow, vec_args.empty, r_lazy_null));

    r_obj* outer_name = has_names ? xs_names_p[i] : strings_empty;
    bool allow_packing;
    x = KEEP(as_df_col(x, outer_name, &allow_packing, call));

    // Remove outer name of column vectors because they shouldn't be repacked
    if (has_names && !allow_packing) {
      r_chr_poke(xs_names, i, strings_empty);
    }

    r_list_poke(xs, i, x);
    FREE(2);

    // Named inputs are packed in a single column
    r_ssize x_ncol = outer_name == strings_empty ? r_length(x) : 1;
    ncol += x_ncol;
  }


  // Fill in columns
  r_keep_loc out_pi;
  r_obj* out = r_alloc_list(ncol);
  KEEP_HERE(out, &out_pi);
  init_data_frame(out, nrow);

  r_keep_loc names_pi;
  r_obj* names = r_alloc_character(ncol);
  KEEP_HERE(names, &names_pi);

  r_obj* idx = KEEP(compact_seq(0, 0, true));
  int* idx_ptr = r_int_begin(idx);

  r_ssize counter = 0;

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* x = r_list_get(xs, i);

    if (x == r_null) {
      continue;
    }

    r_obj* outer_name = has_names ? xs_names_p[i] : strings_empty;
    if (outer_name != strings_empty) {
      r_list_poke(out, counter, x);
      r_chr_poke(names, counter, outer_name);
      ++counter;
      continue;
    }

    r_ssize xn = r_length(x);
    init_compact_seq(idx_ptr, counter, xn, true);

    // Total ownership of `out` because it was freshly created with `r_alloc_vector()`
    out = list_assign(out, idx, x, VCTRS_OWNED_true);
    KEEP_AT(out, out_pi);

    r_obj* xnms = KEEP(r_names(x));
    if (xnms != r_null) {
      names = chr_assign(names, idx, xnms, VCTRS_OWNED_true);
      KEEP_AT(names, names_pi);
    }
    FREE(1);

    counter += xn;
  }

  names = KEEP(vec_as_names(names, name_repair));
  r_attrib_poke(out, r_syms.names, names);

  if (rownames != r_null) {
    r_attrib_poke(out, r_syms.row_names, rownames);
  }

  out = vec_restore(out, type, r_null, VCTRS_OWNED_true);

  FREE(9);
  return out;
}

r_obj* vec_cbind_frame_ptype(r_obj* x) {
  return vctrs_dispatch1(syms_vec_cbind_frame_ptype,
                         fns_vec_cbind_frame_ptype,
                         syms_x,
                         x);
}

static
r_obj* cbind_container_type(r_obj* x, void* data) {
  if (is_data_frame(x)) {
    r_obj* rn = df_rownames(x);

    if (rownames_type(rn) == ROWNAMES_IDENTIFIERS) {
      r_obj** learned_rn_p = (r_obj**) data;
      r_obj* learned_rn = *learned_rn_p;

      if (learned_rn == r_null) {
        *learned_rn_p = rn;
      }
    }

    return vec_cbind_frame_ptype(x);
  } else {
    return r_null;
  }
}


// [[ register() ]]
r_obj* ffi_as_df_col(r_obj* x, r_obj* outer, r_obj* frame) {
  struct r_lazy call = { .x = frame, .env = r_null };
  bool allow_pack;
  return as_df_col(x, r_chr_get(outer, 0), &allow_pack, call);
}

static
r_obj* as_df_col(r_obj* x,
                 r_obj* outer,
                 bool* allow_pack,
                 struct r_lazy call) {
  if (is_data_frame(x)) {
    *allow_pack = true;
    return r_clone(x);
  }

  r_ssize ndim = vec_bare_dim_n(x);
  if (ndim > 2) {
    r_abort_lazy_call(call, "Can't bind arrays.");
  }
  if (ndim > 0) {
    *allow_pack = true;
    return shaped_as_df_col(x, outer);
  }

  *allow_pack = false;
  return vec_as_df_col(x, outer);
}

static
r_obj* shaped_as_df_col(r_obj* x, r_obj* outer) {
  // If packed, store array as a column
  if (outer != strings_empty) {
    return x;
  }

  // If unpacked, transform to data frame first. We repair names
  // after unpacking and concatenation.
  r_obj* out = KEEP(r_as_data_frame(x));

  // Remove names if they were repaired by `as.data.frame()`
  if (colnames(x) == r_null) {
    r_attrib_poke_names(out, r_null);
  }

  FREE(1);
  return out;
}

static
r_obj* vec_as_df_col(r_obj* x, r_obj* outer) {
  r_obj* out = KEEP(r_alloc_list(1));
  r_list_poke(out, 0, x);

  if (outer != strings_empty) {
    r_obj* names = KEEP(r_str_as_character(outer));
    r_attrib_poke_names(out, names);
    FREE(1);
  }

  init_data_frame(out, r_length(x));

  FREE(1);
  return out;
}

static
struct name_repair_opts validate_bind_name_repair(r_obj* name_repair, bool allow_minimal) {
  struct name_repair_opts opts = new_name_repair_opts(name_repair,
                                                      vec_args.empty,
                                                      false,
                                                      r_lazy_null);

  switch (opts.type) {
  case name_repair_custom:
  case name_repair_unique:
  case name_repair_universal:
  case name_repair_check_unique:
    break;
  case name_repair_minimal:
    if (allow_minimal) break; // else fallthrough
  default:
    if (allow_minimal) {
      r_abort_call(r_null,
                   "`.name_repair` can't be `\"%s\"`.\n"
                   "It must be one of `\"unique\"`, `\"universal\"`, `\"check_unique\"`, or `\"minimal\"`.",
                   name_repair_arg_as_c_string(opts.type));
    } else {
      r_abort_call(r_null,
                   "`.name_repair` can't be `\"%s\"`.\n"
                   "It must be one of `\"unique\"`, `\"universal\"`, or `\"check_unique\"`.",
                   name_repair_arg_as_c_string(opts.type));
    }
  }

  return opts;
}

void vctrs_init_bind(r_obj* ns) {
  syms_vec_cbind_frame_ptype = r_sym("vec_cbind_frame_ptype");
  fns_vec_cbind_frame_ptype = r_env_get(ns, syms_vec_cbind_frame_ptype);
}

static
r_obj* syms_vec_cbind_frame_ptype = NULL;

static
r_obj* fns_vec_cbind_frame_ptype = NULL;
