#include "vctrs.h"
#include "type-data-frame.h"
#include "decl/type-data-frame-decl.h"

bool is_data_frame(r_obj* x) {
  if (r_typeof(x) != R_TYPE_list) {
    return false;
  }

  enum vctrs_class_type type = class_type(x);
  return
    type == VCTRS_CLASS_bare_data_frame ||
    type == VCTRS_CLASS_bare_tibble ||
    type == VCTRS_CLASS_data_frame;
}

bool is_native_df(r_obj* x) {
  enum vctrs_class_type type = class_type(x);
  return
    type == VCTRS_CLASS_bare_data_frame ||
    type == VCTRS_CLASS_bare_tibble;
}

bool is_bare_data_frame(r_obj* x) {
  return class_type(x) == VCTRS_CLASS_bare_data_frame;
}

bool is_bare_tibble(r_obj* x) {
  return class_type(x) == VCTRS_CLASS_bare_tibble;
}

r_obj* new_data_frame(r_obj* x, r_ssize n) {
  x = KEEP(r_clone_referenced(x));
  init_data_frame(x, n);

  FREE(1);
  return x;
}

// [[ register() ]]
r_obj* ffi_new_data_frame(r_obj* args) {
  args = r_node_cdr(args);

  r_obj* x = r_node_car(args); args = r_node_cdr(args);
  r_obj* n = r_node_car(args); args = r_node_cdr(args);
  r_obj* cls = r_node_car(args); args = r_node_cdr(args);
  r_obj* attrib = args;

  r_keep_loc pi;
  KEEP_HERE(attrib, &pi);

  if (r_typeof(x) != R_TYPE_list) {
    r_abort_call(r_null, "`x` must be a list");
  }

  bool has_names = false;
  bool has_rownames = false;
  r_ssize size = df_size_from_list(x, n);

  r_obj* out = KEEP(r_clone_referenced(x));

  for (r_obj* node = attrib; node != r_null; node = r_node_cdr(node)) {
    r_obj* tag = r_node_tag(node);

    // We might add dynamic dots later on
    if (tag == r_syms.class_) {
      r_stop_internal("Can't supply `class` in `...`.");
    }

    if (tag == r_syms.names) {
      has_names = true;
      continue;
    }

    if (tag == r_syms.row_names) {
      // "row.names" is checked for consistency with n (if provided)
      if (size != rownames_size(r_node_car(node)) && n != r_null) {
        r_abort_call(r_null, "`n` and `row.names` must be consistent.");
      }

      has_rownames = true;
      continue;
    }
  }

  // Take names from `x` if `attrib` doesn't have any
  if (!has_names) {
    r_obj* nms = vctrs_shared_empty_chr;
    if (r_length(out)) {
      nms = r_names(out);
    }
    KEEP(nms);

    if (nms != r_null) {
      attrib = r_new_node(nms, attrib);
      r_node_poke_tag(attrib, r_syms.names);
      KEEP_AT(attrib, pi);
    }

    FREE(1);
  }

  if (!has_rownames) {
    r_obj* rn = KEEP(new_compact_rownames(size));
    attrib = r_new_node(rn, attrib);
    r_node_poke_tag(attrib, r_syms.row_names);

    FREE(1);
    KEEP_AT(attrib, pi);
  }

  if (cls == r_null) {
    cls = classes_data_frame;
  } else {
    cls = c_data_frame_class(cls);
  }
  KEEP(cls);

  attrib = r_new_node(cls, attrib);
  r_node_poke_tag(attrib, r_syms.class_);

  FREE(1);
  KEEP_AT(attrib, pi);


  r_poke_attrib(out, attrib);
  r_mark_object(out);

  FREE(2);
  return out;
}

static
r_ssize df_size_from_list(r_obj* x, r_obj* n) {
  if (n == r_null) {
    if (is_data_frame(x)) {
      return df_size(x);
    } else {
      return df_raw_size_from_list(x);
    }
  } else {
    return df_size_from_n(n);
  }
}

static
r_ssize df_size_from_n(r_obj* n) {
  if (r_typeof(n) != R_TYPE_integer || r_length(n) != 1) {
    r_abort("`n` must be an integer of size 1.");
  }

  r_ssize out = r_int_get(n, 0);

  if (out == r_globals.na_int) {
    r_abort("`n` can't be missing.");
  }
  if (out < 0) {
    r_abort("`n` can't be negative.");
  }

  return out;
}

static
r_obj* c_data_frame_class(r_obj* cls) {
  if (r_typeof(cls) != R_TYPE_character) {
    r_abort_call(r_null, "`class` must be NULL or a character vector");
  }
  return chr_c(cls, classes_data_frame);
}


// [[ register() ]]
r_obj* ffi_data_frame(r_obj* x,
                      r_obj* size,
                      r_obj* name_repair,
                      r_obj* frame) {
  struct r_lazy call = { .x = syms_dot_call, .env = frame };

  struct name_repair_opts name_repair_opts = new_name_repair_opts(name_repair,
                                                                  vec_args.dot_name_repair,
                                                                  false,
                                                                  call);
  KEEP(name_repair_opts.shelter);

  r_ssize c_size = 0;
  if (size == r_null) {
    c_size = vec_check_size_common(x, 0, vec_args.empty, call);
  } else {
    c_size = vec_as_short_length(size, vec_args.dot_size, call);
  }

  r_obj* out = data_frame(x, c_size, &name_repair_opts, call);

  FREE(1);
  return out;
}

static
r_obj* data_frame(r_obj* x,
                  r_ssize size,
                  const struct name_repair_opts* p_name_repair_opts,
                  struct r_lazy call) {
  const bool unpack = true;
  r_obj* out = KEEP(df_list(x, size, unpack, p_name_repair_opts, call));
  out = new_data_frame(out, size);
  FREE(1);
  return out;
}


// [[ register() ]]
r_obj* ffi_df_list(r_obj* x,
                   r_obj* size,
                   r_obj* unpack,
                   r_obj* name_repair,
                   r_obj* frame) {
  struct r_lazy call = { .x = syms_dot_call, .env = frame };

  struct name_repair_opts name_repair_opts = new_name_repair_opts(name_repair,
                                                                  vec_args.dot_name_repair,
                                                                  false,
                                                                  call);
  KEEP(name_repair_opts.shelter);

  r_ssize c_size = 0;
  if (size == r_null) {
    c_size = vec_check_size_common(x, 0, vec_args.empty, call);
  } else {
    c_size = vec_as_short_length(size, vec_args.dot_size, call);
  }

  const bool c_unpack = r_arg_as_bool(unpack, ".unpack");

  r_obj* out = df_list(x, c_size, c_unpack, &name_repair_opts, call);

  FREE(1);
  return out;
}

static
r_obj* df_list(r_obj* x,
               r_ssize size,
               bool unpack,
               const struct name_repair_opts* p_name_repair_opts,
               struct r_lazy call) {
  if (r_typeof(x) != R_TYPE_list) {
    r_stop_internal("`x` must be a list.");
  }

  x = KEEP(vec_check_recycle_common(x, size, vec_args.empty, call));

  r_ssize n_cols = r_length(x);

  // Unnamed columns are auto-named with `""`
  if (r_names(x) == r_null) {
    r_obj* names = KEEP(r_new_character(n_cols));
    r_attrib_poke_names(x, names);
    FREE(1);
  }

  x = KEEP(df_list_drop_null(x));

  if (unpack) {
    x = df_list_unpack(x);
  }
  KEEP(x);

  r_obj* names = KEEP(r_names(x));
  names = KEEP(vec_as_names(names, p_name_repair_opts));
  r_attrib_poke_names(x, names);

  FREE(5);
  return x;
}

static
r_obj* df_list_drop_null(r_obj* x) {
  r_ssize n_cols = r_length(x);
  r_ssize count = 0;

  for (r_ssize i = 0; i < n_cols; ++i) {
    count += r_list_get(x, i) == r_null;
  }

  if (count == 0) {
    return x;
  }

  r_obj* names = KEEP(r_names(x));
  r_obj* const * p_names = r_chr_cbegin(names);

  r_ssize n_out = n_cols - count;
  r_obj* out = KEEP(r_alloc_list(n_out));
  r_obj* out_names = KEEP(r_alloc_character(n_out));
  r_ssize out_i = 0;

  for (r_ssize i = 0; i < n_cols; ++i) {
    r_obj* col = r_list_get(x, i);

    if (col != r_null) {
      r_list_poke(out, out_i, col);
      r_chr_poke(out_names, out_i, p_names[i]);
      ++out_i;
    }
  }

  r_attrib_poke_names(out, out_names);

  FREE(3);
  return out;
}

static
r_obj* df_list_unpack(r_obj* x) {
  r_obj* names = KEEP(r_names(x));
  r_obj* const * p_names = r_chr_cbegin(names);

  bool any_needs_unpack = false;
  r_ssize n_cols = r_length(x);
  r_ssize i = 0;

  for (; i < n_cols; ++i) {
    // Only unpack unnamed data frames
    if (p_names[i] != strings_empty) {
      continue;
    }

    r_obj* col = r_list_get(x, i);

    if (is_data_frame(col)) {
      any_needs_unpack = true;
      break;
    }
  }

  if (!any_needs_unpack) {
    FREE(1);
    return x;
  }

  r_obj* unpack = KEEP(r_new_logical(n_cols));
  int* p_unpack = LOGICAL(unpack);

  for (r_ssize j = 0; j < n_cols; ++j) {
    p_unpack[j] = 0;
  }

  r_ssize width = i;

  for (; i < n_cols; ++i) {
    // Only unpack unnamed data frames
    if (p_names[i] != strings_empty) {
      ++width;
      continue;
    }

    r_obj* col = r_list_get(x, i);

    if (is_data_frame(col)) {
      width += r_length(col);
      p_unpack[i] = 1;
    } else {
      ++width;
    }
  }

  r_obj* out = KEEP(r_new_list(width));
  r_obj* out_names = KEEP(r_new_character(width));

  r_ssize loc = 0;

  // Unpack loop
  for (r_ssize i = 0; i < n_cols; ++i) {
    if (!p_unpack[i]) {
      r_list_poke(out, loc, r_list_get(x, i));
      r_chr_poke(out_names, loc, p_names[i]);
      ++loc;
      continue;
    }

    r_obj* col = r_list_get(x, i);
    r_obj* col_names = KEEP(r_names(col));

    if (r_typeof(col_names) != R_TYPE_character) {
      r_stop_internal(
        "Encountered corrupt data frame. "
        "Data frames must have character column names."
      );
    }

    r_obj* const * p_col_names = r_chr_cbegin(col_names);
    r_ssize col_i = 0;

    r_ssize stop = loc + r_length(col);

    for (; loc < stop; ++loc, ++col_i) {
      r_list_poke(out, loc, r_list_get(col, col_i));
      r_chr_poke(out_names, loc, p_col_names[col_i]);
    }

    loc = stop;
    FREE(1);
  }

  r_attrib_poke_names(out, out_names);

  FREE(4);
  return out;
}


enum rownames_type rownames_type(r_obj* x) {
  switch (r_typeof(x)) {
  case R_TYPE_character:
    return ROWNAMES_IDENTIFIERS;
  case R_TYPE_integer:
    if (r_length(x) == 2 && r_int_begin(x)[0] == r_globals.na_int) {
      return ROWNAMES_AUTOMATIC_COMPACT;
    } else {
      return ROWNAMES_AUTOMATIC;
    }
  default:
    r_stop_internal("Unexpected type `%s`.", Rf_type2char(r_typeof(x)));
  }
}

static
r_ssize compact_rownames_length(r_obj* x) {
  return abs(r_int_get(x, 1));
}

// [[ include("type-data-frame.h") ]]
r_ssize rownames_size(r_obj* rn) {
  switch (rownames_type(rn)) {
  case ROWNAMES_IDENTIFIERS:
  case ROWNAMES_AUTOMATIC:
    return r_length(rn);
  case ROWNAMES_AUTOMATIC_COMPACT:
    return compact_rownames_length(rn);
  }

  never_reached("rownames_size");
}

// [[ include("type-data-frame.h") ]]
void init_data_frame(r_obj* x, r_ssize n) {
  r_attrib_poke(x, r_syms.class_, classes_data_frame);
  init_bare_data_frame(x, n);
}
// [[ include("type-data-frame.h") ]]
void init_tibble(r_obj* x, r_ssize n) {
  r_attrib_poke(x, r_syms.class_, classes_tibble);
  init_bare_data_frame(x, n);
}

static
void init_bare_data_frame(r_obj* x, r_ssize n) {
  if (r_length(x) == 0) {
    r_attrib_poke(x, r_syms.names, vctrs_shared_empty_chr);
  }

  init_compact_rownames(x, n);
}

// [[ include("type-data-frame.h") ]]
void init_compact_rownames(r_obj* x, r_ssize n) {
  r_obj* rn = KEEP(new_compact_rownames(n));
  r_attrib_poke(x, r_syms.row_names, rn);
  FREE(1);
}

static
r_obj* new_compact_rownames(r_ssize n) {
  if (n <= 0) {
    return vctrs_shared_empty_int;
  }

  r_obj* out = r_alloc_integer(2);
  int* out_data = r_int_begin(out);
  out_data[0] = r_globals.na_int;
  out_data[1] = -n;
  return out;
}


// vctrs type methods ------------------------------------------------

// [[ register() ]]
r_obj* ffi_df_ptype2_opts(r_obj* x, r_obj* y, r_obj* opts, r_obj* frame) {
  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy y_arg_ = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_);

  struct r_lazy call = { .x = r_syms.call, .env = frame };

  const struct ptype2_opts c_opts = new_ptype2_opts(x,
                                                    y,
                                                    &x_arg,
                                                    &y_arg,
                                                    call,
                                                    opts);

  return df_ptype2(&c_opts);
}

r_obj* df_ptype2(const struct ptype2_opts* opts) {
  r_obj* x_names = KEEP(r_names(opts->x));
  r_obj* y_names = KEEP(r_names(opts->y));

  r_obj* out = r_null;

  if (equal_object(x_names, y_names)) {
    out = df_ptype2_loop(opts, x_names);
  } else {
    out = df_ptype2_match(opts, x_names, y_names);
  }

  FREE(2);
  return out;
}

r_obj* df_ptype2_match(const struct ptype2_opts* opts,
                     r_obj* x_names,
                     r_obj* y_names) {
  r_obj* x = opts->x;
  r_obj* y = opts->y;

  r_obj* x_dups_pos = KEEP(vec_match(x_names, y_names));
  r_obj* y_dups_pos = KEEP(vec_match(y_names, x_names));

  int* x_dups_pos_data = r_int_begin(x_dups_pos);
  int* y_dups_pos_data = r_int_begin(y_dups_pos);

  r_ssize x_len = r_length(x_names);
  r_ssize y_len = r_length(y_names);

  // Count columns that are only in `y`
  r_ssize rest_len = 0;
  for (r_ssize i = 0; i < y_len; ++i) {
    if (y_dups_pos_data[i] == r_globals.na_int) {
      ++rest_len;
    }
  }

  r_ssize out_len = x_len + rest_len;
  r_obj* out = KEEP(r_alloc_list(out_len));
  r_obj* nms = KEEP(r_alloc_character(out_len));
  r_attrib_poke(out, r_syms.names, nms);

  r_ssize i = 0;
  r_ssize y_arg_loc = 0;
  struct vctrs_arg* named_x_arg = new_subscript_arg(opts->p_x_arg, x_names, x_len, &i);
  KEEP(named_x_arg->shelter);

  struct vctrs_arg* named_y_arg = new_subscript_arg(opts->p_y_arg, y_names, y_len, &y_arg_loc);
  KEEP(named_y_arg->shelter);

  // Fill in prototypes of all the columns that are in `x`, in order
  for (; i < x_len; ++i) {
    r_ssize dup = x_dups_pos_data[i];

    r_obj* col = r_list_get(x, i);
    struct ptype2_opts col_opts = *opts;
    col_opts.x = col;
    col_opts.p_x_arg = named_x_arg;

    r_obj* type;
    if (dup == r_globals.na_int) {
      col_opts.y = vctrs_shared_empty_uns;
      col_opts.p_y_arg = NULL;
      type = vec_ptype2_from_unspecified(&col_opts,
                                         vec_typeof(col),
                                         col,
                                         named_x_arg);
    } else {
      // 1-based index
      --dup;
      y_arg_loc = dup;

      col_opts.y = r_list_get(y, dup);
      col_opts.p_y_arg = named_y_arg;

      int _left;
      type = vec_ptype2_opts(&col_opts, &_left);
    }

    r_list_poke(out, i, type);
    r_chr_poke(nms, i, r_chr_get(x_names, i));
  }

  // Fill in prototypes of the columns that are only in `y`
  for (r_ssize j = 0; i < out_len; ++j) {
    r_ssize dup = y_dups_pos_data[j];

    if (dup == r_globals.na_int) {
      r_obj* col = r_list_get(y, j);
      y_arg_loc = j;

      struct ptype2_opts col_opts = *opts;
      col_opts.y = col;
      col_opts.p_y_arg = named_y_arg;
      col_opts.x = vctrs_shared_empty_uns;
      col_opts.p_x_arg = NULL;
      r_obj* type = vec_ptype2_from_unspecified(&col_opts,
                                              vec_typeof(col),
                                              col,
                                              named_y_arg);

      r_list_poke(out, i, type);
      r_chr_poke(nms, i, r_chr_get(y_names, j));
      ++i;
    }
  }

  init_data_frame(out, 0);

  FREE(6);
  return out;
}

static
r_obj* df_ptype2_loop(const struct ptype2_opts* opts,
                      r_obj* names) {
  r_obj* x = opts->x;
  r_obj* y = opts->y;

  r_ssize len = r_length(names);

  r_obj* out = KEEP(r_alloc_list(len));
  r_attrib_poke(out, r_syms.names, names);

  r_ssize i = 0;

  struct vctrs_arg* named_x_arg = new_subscript_arg_vec(opts->p_x_arg, out, &i);
  KEEP(named_x_arg->shelter);

  struct vctrs_arg* named_y_arg = new_subscript_arg_vec(opts->p_y_arg, out, &i);
  KEEP(named_y_arg->shelter);

  for (; i < len; ++i) {
    struct ptype2_opts col_opts = *opts;
    col_opts.x = r_list_get(x, i);
    col_opts.y = r_list_get(y, i);
    col_opts.p_x_arg = named_x_arg;
    col_opts.p_y_arg = named_y_arg;
    int _left;

    r_obj* type = vec_ptype2_opts(&col_opts, &_left);

    r_list_poke(out, i, type);
  }

  init_data_frame(out, 0);

  FREE(3);
  return out;
}

// [[ register() ]]
r_obj* ffi_df_cast_opts(r_obj* x, r_obj* to, r_obj* opts, r_obj* frame) {
  struct r_lazy x_arg_ = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_);

  struct r_lazy to_arg_ = { .x = syms.to_arg, .env = frame };
  struct vctrs_arg to_arg = new_lazy_arg(&to_arg_);

  struct r_lazy call = { .x = r_syms.call, .env = frame };

  struct cast_opts c_opts = new_cast_opts(x,
                                          to,
                                          &x_arg,
                                          &to_arg,
                                          call,
                                          opts);

  return df_cast_opts(&c_opts);
}

// Take all columns of `to` and preserve the order. Common columns are
// cast to their types in `to`. Extra `x` columns are dropped and
// cause a lossy cast. Extra `to` columns are filled with missing
// values.
// [[ include("cast.h") ]]
r_obj* df_cast_opts(const struct cast_opts* opts) {
  r_obj* x_names = KEEP(r_names(opts->x));
  r_obj* to_names = KEEP(r_names(opts->to));

  if (x_names == r_null || to_names == r_null) {
    r_stop_internal("Data frame must have names.");
  }

  r_obj* out = r_null;

  if (equal_object(x_names, to_names)) {
    out = df_cast_loop(opts, x_names);
  } else {
    out = df_cast_match(opts, x_names, to_names);
  }

  FREE(2);
  return out;
}

static
r_obj* df_cast_match(const struct cast_opts* opts,
                     r_obj* x_names,
                     r_obj* to_names) {
  r_obj* x = opts->x;
  r_obj* to = opts->to;

  r_obj* to_dups_pos = KEEP(vec_match(to_names, x_names));
  int* to_dups_pos_data = r_int_begin(to_dups_pos);

  r_ssize to_len = r_length(to_dups_pos);
  r_obj* out = KEEP(r_alloc_list(to_len));
  r_attrib_poke(out, r_syms.names, to_names);

  r_ssize size = df_size(x);
  r_ssize common_len = 0;

  r_ssize i = 0;
  r_ssize x_arg_loc = 0;

  struct vctrs_arg* named_x_arg = new_subscript_arg(opts->p_x_arg, x_names, r_length(x_names), &x_arg_loc);
  KEEP(named_x_arg->shelter);

  struct vctrs_arg* named_to_arg = new_subscript_arg(opts->p_to_arg, to_names, to_len, &i);
  KEEP(named_to_arg->shelter);

  for (; i < to_len; ++i) {
    r_ssize pos = to_dups_pos_data[i];

    r_obj* col;
    if (pos == r_globals.na_int) {
      r_obj* to_col = r_list_get(to, i);
      col = vec_init(to_col, size);

      // FIXME: Need to initialise the vector because we currently use
      // `vec_assign()` in `vec_rbind()` before falling back. Attach
      // an attribute to recognise unspecified vectors in
      // `base_c_invoke()`.
      if (opts->fallback.s3 && vec_is_common_class_fallback(to_col)) {
        KEEP(col);
        r_attrib_poke(col, r_sym("vctrs:::unspecified"), vctrs_shared_true);
        FREE(1);
      }
    } else {
      --pos; // 1-based index
      ++common_len;
      x_arg_loc = pos;

      struct cast_opts col_opts = {
        .x = r_list_get(x, pos),
        .to = r_list_get(to, i),
        .p_x_arg = named_x_arg,
        .p_to_arg = named_to_arg,
        .call = opts->call,
        .fallback = opts->fallback
      };
      col = vec_cast_opts(&col_opts);
    }

    r_list_poke(out, i, col);
  }

  // Restore data frame size before calling `vec_restore()`. `x` and
  // `to` might not have any columns to compute the original size.
  init_data_frame(out, size);
  r_attrib_poke(out, r_syms.row_names, df_rownames(x));

  r_ssize extra_len = r_length(x) - common_len;
  if (extra_len) {
    r_obj* ffi_x_arg = KEEP(vctrs_arg(opts->p_x_arg));
    r_obj* ffi_to_arg = KEEP(vctrs_arg(opts->p_to_arg));
    r_obj* ffi_call = KEEP(r_lazy_eval(opts->call));
    out = vctrs_dispatch6(syms_df_lossy_cast, fns_df_lossy_cast,
                          syms_out, out,
                          syms_x, x,
                          syms_to, to,
                          syms_x_arg, ffi_x_arg,
                          syms_to_arg, ffi_to_arg,
                          syms_call, ffi_call);
    FREE(3);
  }

  FREE(4);
  return out;
}

static
r_obj* df_cast_loop(const struct cast_opts* opts, r_obj* names) {
  r_obj* x = opts->x;
  r_obj* to = opts->to;

  r_ssize len = r_length(names);

  r_obj* out = KEEP(r_alloc_list(len));
  r_attrib_poke(out, r_syms.names, names);

  r_ssize size = df_size(x);

  r_ssize i = 0;
  struct vctrs_arg* named_x_arg = new_subscript_arg(opts->p_x_arg, names, len, &i);
  KEEP(named_x_arg->shelter);

  struct vctrs_arg* named_to_arg = new_subscript_arg(opts->p_to_arg, names, len, &i);
  KEEP(named_to_arg->shelter);

  for (; i < len; ++i) {
    struct cast_opts col_opts = {
      .x = r_list_get(x, i),
      .to = r_list_get(to, i),
      .p_x_arg = named_x_arg,
      .p_to_arg = named_to_arg,
      .call = opts->call,
      .fallback = opts->fallback
    };
    r_obj* col = vec_cast_opts(&col_opts);

    r_list_poke(out, i, col);
  }

  // Restore data frame size before calling `vec_restore()`. `x` and
  // `to` might not have any columns to compute the original size.
  init_data_frame(out, size);
  r_attrib_poke(out, r_syms.row_names, df_rownames(x));

  FREE(3);
  return out;
}

// If negative index, value is appended
r_obj* df_poke(r_obj* x, r_ssize i, r_obj* value) {
  if (i >= 0) {
    r_list_poke(x, i, value);
    return x;
  }

  r_ssize ncol = r_length(x);

  r_obj* tmp = KEEP(r_resize(x, ncol + 1));
  Rf_copyMostAttrib(x, tmp);
  x = tmp;

  r_list_poke(x, ncol, value);

  FREE(1);
  return x;
}
r_obj* df_poke_at(r_obj* x, r_obj* name, r_obj* value) {
  r_obj* names = KEEP(r_names(x));
  r_ssize i = r_chr_find(names, name);
  FREE(1);

  x = KEEP(df_poke(x, i, value));

  if (i < 0) {
    r_obj* names = KEEP(r_names(x));
    r_chr_poke(names, r_length(x) - 1, name);
    FREE(1);
  }

  FREE(1);
  return x;
}

static inline
r_ssize df_flat_width(r_obj* x) {
  r_ssize n = r_length(x);
  r_ssize out = n;

  r_obj* const * v_x = r_list_cbegin(x);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* col = v_x[i];
    if (is_data_frame(col)) {
      out = out + df_flat_width(col) - 1;
    }
  }

  return out;
}

struct flatten_info {
  bool flatten;
  r_ssize width;
};

static inline
struct flatten_info df_flatten_info(r_obj* x) {
  bool flatten = false;

  r_ssize n = r_length(x);
  r_ssize width = n;

  r_obj* const * v_x = r_list_cbegin(x);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* col = v_x[i];
    if (is_data_frame(col)) {
      flatten = true;
      width = width + df_flat_width(col) - 1;
    }
  }

  return (struct flatten_info){flatten, width};
}

// [[ register() ]]
r_obj* ffi_df_flatten_info(r_obj* x) {
  struct flatten_info info = df_flatten_info(x);

  r_obj* out = KEEP(r_alloc_list(2));
  r_list_poke(out, 0, r_lgl(info.flatten));
  r_list_poke(out, 1, r_int(info.width));

  FREE(1);
  return out;
}

// Might return duplicate names. Currently only used for equality
// proxy so this doesn't matter. A less bare bone version would repair
// names.
//
// [[ register() ]]
r_obj* df_flatten(r_obj* x) {
  struct flatten_info info = df_flatten_info(x);

  if (!info.flatten) {
    return x;
  }

  r_obj* out = KEEP(r_alloc_list(info.width));
  r_obj* out_names = KEEP(r_alloc_character(info.width));
  r_attrib_poke_names(out, out_names);

  df_flatten_loop(x, out, out_names, 0);
  init_data_frame(out, df_size(x));

  FREE(2);
  return out;
}

static
r_ssize df_flatten_loop(r_obj* x,
                        r_obj* out,
                        r_obj* out_names,
                        r_ssize counter) {
  r_ssize n = r_length(x);
  r_obj* x_names = KEEP(r_names(x));

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* col = r_list_get(x, i);

    if (is_data_frame(col)) {
      counter = df_flatten_loop(col, out, out_names, counter);
    } else {
      r_list_poke(out, counter, col);
      r_chr_poke(out_names, counter, r_chr_get(x_names, i));
      ++counter;
    }
  }

  FREE(1);
  return counter;
}

r_obj* df_repair_names(r_obj* x, struct name_repair_opts* name_repair) {
  r_obj* nms = KEEP(r_names(x));
  r_obj* repaired = KEEP(vec_as_names(nms, name_repair));

  // Should this go through proxy and restore so that classes can
  // update metadata and check invariants when special columns are
  // renamed?
  if (nms != repaired) {
    x = KEEP(r_clone_referenced(x));
    r_attrib_poke_names(x, repaired);
    FREE(1);
  }

  FREE(2);
  return x;
}


void vctrs_init_type_data_frame(r_obj* ns) {
  syms_df_lossy_cast = r_sym("df_lossy_cast");
  fns_df_lossy_cast = r_eval(syms_df_lossy_cast, ns);
}

static r_obj* syms_df_lossy_cast = NULL;
static r_obj* fns_df_lossy_cast = NULL;
