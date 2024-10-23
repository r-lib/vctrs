#include "vctrs.h"
#include "decl/subscript-loc-decl.h"


r_obj* vec_as_location(r_obj* subscript, r_ssize n, r_obj* names) {
  const struct location_opts opts = { 0 };
  return vec_as_location_opts(subscript,
                              n,
                              names,
                              &opts);
}

r_obj* vec_as_location_ctxt(r_obj* subscript,
                            r_ssize n,
                            r_obj* names,
                            struct vctrs_arg* arg,
                            struct r_lazy call) {
  struct location_opts opts = {
    .subscript_opts = {
      .subscript_arg = arg,
      .call = call
    }
  };
  return vec_as_location_opts(subscript,
                              n,
                              names,
                              &opts);
}

r_obj* vec_as_location_opts(r_obj* subscript,
                            r_ssize n,
                            r_obj* names,
                            const struct location_opts* opts) {

  ERR err = NULL;
  subscript = vec_as_subscript_opts(subscript, &opts->subscript_opts, &err);
  KEEP2(subscript, err);

  if (err) {
    r_cnd_signal(err);
    r_stop_unreachable();
  }

  r_obj* out = r_null;
  switch (r_typeof(subscript)) {
  case R_TYPE_null: out = r_globals.empty_int; break;
  case R_TYPE_logical: out = lgl_as_location(subscript, n, opts); break;
  case R_TYPE_integer: out = int_as_location(subscript, n, opts); break;
  case R_TYPE_double: out = dbl_as_location(subscript, n, opts); break;
  case R_TYPE_character: out = chr_as_location(subscript, names, opts); break;
  default: r_stop_unimplemented_type(r_typeof(subscript));
  }

  FREE(2);
  return out;
}


static
r_obj* lgl_as_location(r_obj* subscript,
                       r_ssize n,
                       const struct location_opts* opts) {
  r_ssize subscript_n = r_length(subscript);

  if (subscript_n == n) {
    bool na_propagate = false;

    switch (opts->missing) {
    case SUBSCRIPT_MISSING_PROPAGATE: na_propagate = true; break;
    case SUBSCRIPT_MISSING_REMOVE: break;
    case SUBSCRIPT_MISSING_ERROR: {
      if (lgl_any_na(subscript)) {
        stop_subscript_missing(subscript, opts);
      }
      break;
    }
    }

    return r_lgl_which(subscript, na_propagate);
  }

  /* A single `TRUE` or `FALSE` index is recycled to the full vector
   * size. This means `TRUE` is synonym for missing index (subscript.e. no
   * subsetting) and `FALSE` is synonym for empty index.
   *
   * We could return the missing argument as sentinel to avoid
   * materialising the index vector for the `TRUE` case but this would
   * make `vec_as_location()` an option type just to optimise a rather
   * uncommon case.
   */
  if (subscript_n == 1) {
    int elt = r_lgl_get(subscript, 0);

    r_ssize recycle_size = n;

    r_obj* out = r_null;
    r_keep_loc out_shelter;
    KEEP_HERE(out, &out_shelter);

    if (elt == r_globals.na_lgl) {
      switch (opts->missing) {
      case SUBSCRIPT_MISSING_PROPAGATE: {
        out = r_alloc_integer(n);
        KEEP_AT(out, out_shelter);
        r_int_fill(out, r_globals.na_int, n);
        break;
      }
      case SUBSCRIPT_MISSING_REMOVE: {
        out = r_copy(r_globals.empty_int);
        KEEP_AT(out, out_shelter);
        recycle_size = 0;
        break;
      }
      case SUBSCRIPT_MISSING_ERROR: {
        stop_subscript_missing(subscript, opts);
      }
      }
    } else if (elt) {
      out = r_alloc_integer(n);
      KEEP_AT(out, out_shelter);
      r_int_fill_seq(out, 1, n);
    } else {
      out = r_copy(r_globals.empty_int);
      KEEP_AT(out, out_shelter);
      recycle_size = 0;
    }

    r_obj* nms = KEEP(r_names(subscript));
    if (nms != R_NilValue) {
      r_obj* recycled_nms = r_alloc_character(recycle_size);
      r_attrib_poke_names(out, recycled_nms);
      r_chr_fill(recycled_nms, r_chr_get(nms, 0), recycle_size);
    }

    FREE(2);
    return out;
  }

  r_obj* n_obj = KEEP(r_int(n));
  stop_indicator_size(subscript, n_obj, opts);

  r_stop_unreachable();
}

static
r_obj* int_as_location(r_obj* subscript,
                       r_ssize n,
                       const struct location_opts* opts) {
  const int* data = r_int_cbegin(subscript);
  r_ssize loc_n = r_length(subscript);

  // Zeros need to be filtered out from the subscript vector.
  // `int_invert_location()` filters them out for negative indices, but
  // positive indices need to go through and `int_filter_zero()`.
  r_ssize n_zero = 0;

  r_ssize n_oob = 0;
  r_ssize n_missing = 0;

  for (r_ssize i = 0; i < loc_n; ++i, ++data) {
    int elt = *data;

    if (elt == r_globals.na_int) {
      switch (opts->missing) {
      case SUBSCRIPT_MISSING_PROPAGATE: break;
      case SUBSCRIPT_MISSING_REMOVE: ++n_missing; break;
      case SUBSCRIPT_MISSING_ERROR: stop_subscript_missing(subscript, opts);
      }
    } else if (elt == 0) {
      switch (opts->loc_zero) {
      case LOC_ZERO_REMOVE: ++n_zero; break;
      case LOC_ZERO_ERROR: stop_location_zero(subscript, opts);
      case LOC_ZERO_IGNORE: break;
      }
    } else if (elt < 0) {
      switch (opts->loc_negative) {
      case LOC_NEGATIVE_INVERT: return int_invert_location(subscript, n, opts);
      case LOC_NEGATIVE_ERROR: stop_location_negative(subscript, opts);
      case LOC_NEGATIVE_IGNORE: {
        if (abs(elt) > n) {
          switch (opts->loc_oob) {
          case LOC_OOB_ERROR: stop_subscript_oob_location(subscript, n, opts);
          case LOC_OOB_EXTEND: stop_subscript_negative_oob_location(subscript, n, opts);
          case LOC_OOB_REMOVE: ++n_oob; break;
          }
        }
        break;
      }
      }
    } else if (elt > n) {
      switch (opts->loc_oob) {
      case LOC_OOB_ERROR: stop_subscript_oob_location(subscript, n, opts);
      case LOC_OOB_EXTEND: ++n_oob; break;
      case LOC_OOB_REMOVE: ++n_oob; break;
      }
    }
  }

  r_keep_loc subscript_shelter;
  KEEP_HERE(subscript, &subscript_shelter);

  if (n_missing > 0) {
    subscript = int_filter_missing(subscript, n_missing);
    KEEP_AT(subscript, subscript_shelter);
  }

  if (n_zero > 0) {
    subscript = int_filter_zero(subscript, n_zero);
    KEEP_AT(subscript, subscript_shelter);
  }

  if (n_oob > 0) {
    switch (opts->loc_oob) {
    case LOC_OOB_ERROR: {
      r_stop_internal("An error should have been thrown on the first OOB value.");
    }
    case LOC_OOB_EXTEND: {
      int_check_consecutive(subscript, n, n_oob, opts);
      break;
    }
    case LOC_OOB_REMOVE: {
      subscript = int_filter_oob(subscript, n, n_oob);
      KEEP_AT(subscript, subscript_shelter);
      break;
    }
    }
  }

  FREE(1);
  return subscript;
}

static
r_obj* int_invert_location(r_obj* subscript,
                           r_ssize n,
                           const struct location_opts* opts) {
  const int* data = r_int_cbegin(subscript);
  r_ssize loc_n = r_length(subscript);

  r_obj* sel = KEEP(r_alloc_logical(n));
  r_lgl_fill(sel, 1, n);

  int* sel_data = r_lgl_begin(sel);

  for (r_ssize i = 0; i < loc_n; ++i, ++data) {
    int j = *data;

    if (j == r_globals.na_int) {
      // Following base R by erroring on `missing = "propagate"`, e.g. `1[c(NA, -1)]`.
      // Doesn't make sense to invert an `NA`, so we can't meaningfully propagate.
      switch (opts->missing) {
      case SUBSCRIPT_MISSING_PROPAGATE: stop_location_negative_missing(subscript, opts);
      case SUBSCRIPT_MISSING_REMOVE: continue;
      case SUBSCRIPT_MISSING_ERROR: stop_location_negative_missing(subscript, opts);
      }
    }

    if (j >= 0) {
      if (j == 0) {
        switch (opts->loc_zero) {
        case LOC_ZERO_REMOVE: continue;
        case LOC_ZERO_IGNORE: continue;
        case LOC_ZERO_ERROR: stop_location_zero(subscript, opts);
        }
      } else {
        stop_location_negative_positive(subscript, opts);
      }
    }

    j = -j;
    if (j > n) {
      switch (opts->loc_oob) {
      case LOC_OOB_REMOVE: {
        continue;
      }
      case LOC_OOB_EXTEND:
      case LOC_OOB_ERROR: {
        // Setting `oob` to `"error"` and `"extend"` result in errors here,
        // because extending with a negative subscript is nonsensical
        stop_subscript_negative_oob_location(subscript, n, opts);
      }
      }
    }

    sel_data[j - 1] = 0;
  }

  r_obj* out = lgl_as_location(sel, n, opts);

  FREE(1);
  return out;
}

static
r_obj* int_filter(r_obj* subscript, r_ssize n_filter, int value) {
  const r_ssize size = r_length(subscript);
  const int* v_subscript = r_int_cbegin(subscript);

  r_obj* out = KEEP(r_alloc_integer(size - n_filter));
  int* v_out = r_int_begin(out);

  r_obj* names = r_names(subscript);
  const bool has_names = names != r_null;
  r_obj* const* v_names = NULL;
  r_obj* out_names = r_null;
  if (has_names) {
    v_names = r_chr_cbegin(names);
    out_names = r_alloc_character(size - n_filter);
    r_attrib_poke_names(out, out_names);
  }

  r_ssize j = 0;

  for (r_ssize i = 0; i < size; ++i) {
    const int elt = v_subscript[i];

    if (elt != value) {
      v_out[j] = elt;

      if (has_names) {
        r_chr_poke(out_names, j, v_names[i]);
      }

      ++j;
    }
  }

  FREE(1);
  return out;
}
static
r_obj* int_filter_zero(r_obj* subscript, r_ssize n_zero) {
  return int_filter(subscript, n_zero, 0);
}
static
r_obj* int_filter_missing(r_obj* subscript, r_ssize n_missing) {
  return int_filter(subscript, n_missing, r_globals.na_int);
}

static
r_obj* int_filter_oob(r_obj* subscript, r_ssize n, r_ssize n_oob) {
  const r_ssize n_subscript = r_length(subscript);
  const r_ssize n_out = n_subscript - n_oob;

  const int* v_subscript = r_int_cbegin(subscript);

  r_obj* out = KEEP(r_alloc_integer(n_out));
  int* v_out = r_int_begin(out);

  r_obj* names = r_names(subscript);
  const bool has_names = names != r_null;
  r_obj* const* v_names = NULL;
  r_obj* out_names = r_null;
  if (has_names) {
    v_names = r_chr_cbegin(names);
    out_names = r_alloc_character(n_out);
    r_attrib_poke_names(out, out_names);
  }

  r_ssize j = 0;

  for (r_ssize i = 0; i < n_subscript; ++i) {
    const int elt = v_subscript[i];

    if (abs(elt) <= n || elt == r_globals.na_int) {
      v_out[j] = elt;

      if (has_names) {
        r_chr_poke(out_names, j, v_names[i]);
      }

      ++j;
    }
  }

  FREE(1);
  return out;
}

static
void int_check_consecutive(r_obj* subscript,
                           r_ssize n,
                           r_ssize n_extend,
                           const struct location_opts* opts) {

  r_obj* extended = KEEP(r_alloc_integer(n_extend));
  int* p_extended = r_int_begin(extended);
  int i_extend = 0;
  int new_n = n;

  int* p_subscript = r_int_begin(subscript);

  r_ssize n_subscript = Rf_length(subscript);
  for (r_ssize i = 0; i < n_subscript; ++i) {
    int elt = p_subscript[i];

    // Missing value also covered here
    if (elt <= n) {
      continue;
    }

    // Special case: appending in ascending sequence at the end
    // should not require any sorting
    if (elt - 1 == new_n) {
      ++new_n;
      --n_extend;
    } else {
      p_extended[i_extend++] = elt - 1;
    }
  }

  if (n_extend != i_extend) {
    r_stop_internal("int_check_consecutive",
                    "n_extend (%d) != i_extend (%d).",
                    n_extend,
                    i_extend);
  }

  if (i_extend == 0) {
    FREE(1);
    return;
  }

  // Only the first i_extend entries of the array are populated,
  // the rest is never touched.
  qsort(p_extended, i_extend, sizeof(int), &qsort_int_compare_scalar);

  for (r_ssize i = 0; i < i_extend; ++i) {
    int elt = p_extended[i];

    if (elt != new_n + i) {
      stop_location_oob_non_consecutive(subscript, n, opts);
    }
  }

  FREE(1);
}

static
r_obj* dbl_as_location(r_obj* subscript,
                       r_ssize n,
                       const struct location_opts* opts) {
  subscript = KEEP(vec_cast(subscript,
                            r_globals.empty_int,
                            vec_args.empty,
                            vec_args.empty,
                            r_lazy_null));
  subscript = int_as_location(subscript, n, opts);

  FREE(1);
  return subscript;
}

static
r_obj* chr_as_location(r_obj* subscript,
                       r_obj* names,
                       const struct location_opts* opts) {
  if (names == R_NilValue) {
    r_abort("Can't use character names to index an unnamed vector.");
  }
  if (r_typeof(names) != R_TYPE_character) {
    r_abort("`names` must be a character vector.");
  }

  bool remove_missing = false;

  r_obj* matched = KEEP(Rf_match(names, subscript, r_globals.na_int));
  r_attrib_poke_names(matched, r_names(subscript));

  r_ssize n = r_length(matched);
  int* p = r_int_begin(matched);
  r_obj* const * ip = r_chr_cbegin(subscript);

  for (r_ssize k = 0; k < n; ++k) {
    const r_obj* elt = ip[k];

    if (elt == r_strs.empty) {
      // `""` never matches, even if `names` contains a `""` name
      stop_subscript_empty(subscript, opts);
    }

    if (elt == r_globals.na_str) {
      // `NA_character_` never matches, even if `names` contains a missing name
      p[k] = r_globals.na_int;

      switch (opts->missing) {
      case SUBSCRIPT_MISSING_PROPAGATE: continue;
      case SUBSCRIPT_MISSING_REMOVE: remove_missing = true; continue;
      case SUBSCRIPT_MISSING_ERROR: stop_subscript_missing(subscript, opts);
      }
    }

    if (p[k] == r_globals.na_int) {
      stop_subscript_oob_name(subscript, names, opts);
    }
  }

  if (remove_missing) {
    if (opts->missing != SUBSCRIPT_MISSING_REMOVE) {
      r_stop_internal("`missing = 'remove'` must be set if `n_missing > 0`.");
    }

    r_obj* not_missing = KEEP(vec_detect_complete(matched));
    matched = KEEP(vec_slice(matched, not_missing));

    FREE(2);
  }
  KEEP(matched);

  FREE(2);
  return matched;
}

// [[ register() ]]
r_obj* ffi_as_location(r_obj* subscript,
                       r_obj* ffi_n,
                       r_obj* names,
                       r_obj* loc_negative,
                       r_obj* loc_oob,
                       r_obj* loc_zero,
                       r_obj* missing,
                       r_obj* frame) {
  r_ssize n = 0;

  if (ffi_n == r_null && r_typeof(subscript) == R_TYPE_character) {
    n = r_length(subscript);
  } else {
    if (r_is_object(ffi_n) || r_typeof(ffi_n) != R_TYPE_integer) {
      ffi_n = vec_cast(ffi_n,
                       r_globals.empty_int,
                       vec_args.n,
                       vec_args.empty,
                       (struct r_lazy) { .x = frame, .env = r_null });
    }
    KEEP(ffi_n);

    if (r_length(ffi_n) != 1) {
      r_stop_internal("`n` must be a scalar number.");
    }

    n = r_int_get(ffi_n, 0);
    FREE(1);
  }

  struct r_lazy arg_ = { .x = syms.arg, .env = frame };
  struct vctrs_arg arg = new_lazy_arg(&arg_);

  struct r_lazy call = (struct r_lazy) { .x = syms_call, .env = frame };

  struct location_opts opts = {
    .subscript_opts = {
      .subscript_arg = &arg,
      .call          = call
    },
    .missing        = parse_subscript_arg_missing(missing, call),
    .loc_negative   = parse_loc_negative(loc_negative, call),
    .loc_oob        = parse_loc_oob(loc_oob, call),
    .loc_zero       = parse_loc_zero(loc_zero, call)
  };

  return vec_as_location_opts(subscript, n, names, &opts);
}

static
enum subscript_missing parse_subscript_arg_missing(r_obj* x,
                                                   struct r_lazy call) {
  if (r_typeof(x) != R_TYPE_character || r_length(x) == 0) {
    stop_subscript_arg_missing(call);
  }

  const char* str = r_chr_get_c_string(x, 0);

  if (!strcmp(str, "propagate")) return SUBSCRIPT_MISSING_PROPAGATE;
  if (!strcmp(str, "remove")) return SUBSCRIPT_MISSING_REMOVE;
  if (!strcmp(str, "error")) return SUBSCRIPT_MISSING_ERROR;
  stop_subscript_arg_missing(call);

  r_stop_unreachable();
}
static
enum num_loc_negative parse_loc_negative(r_obj* x,
                                         struct r_lazy call) {
  if (r_typeof(x) != R_TYPE_character || r_length(x) == 0) {
    stop_bad_negative(call);
  }

  const char* str = r_chr_get_c_string(x, 0);

  if (!strcmp(str, "invert")) return LOC_NEGATIVE_INVERT;
  if (!strcmp(str, "error")) return LOC_NEGATIVE_ERROR;
  if (!strcmp(str, "ignore")) return LOC_NEGATIVE_IGNORE;
  stop_bad_negative(call);

  r_stop_unreachable();
}
static
enum num_loc_oob parse_loc_oob(r_obj* x,
                               struct r_lazy call) {
  if (r_typeof(x) != R_TYPE_character || r_length(x) == 0) {
    stop_bad_oob(call);
  }

  const char* str = r_chr_get_c_string(x, 0);

  if (!strcmp(str, "error")) return LOC_OOB_ERROR;
  if (!strcmp(str, "remove")) return LOC_OOB_REMOVE;
  if (!strcmp(str, "extend")) return LOC_OOB_EXTEND;
  stop_bad_oob(call);

  r_stop_unreachable();
}
static
enum num_loc_zero parse_loc_zero(r_obj* x,
                                 struct r_lazy call) {
  if (r_typeof(x) != R_TYPE_character || r_length(x) == 0) {
    stop_bad_zero(call);
  }

  const char* str = r_chr_get_c_string(x, 0);

  if (!strcmp(str, "remove")) return LOC_ZERO_REMOVE;
  if (!strcmp(str, "error")) return LOC_ZERO_ERROR;
  if (!strcmp(str, "ignore")) return LOC_ZERO_IGNORE;
  stop_bad_zero(call);

  r_stop_unreachable();
}

static
void stop_subscript_arg_missing(struct r_lazy call) {
  r_abort_call(call.env, "`missing` must be one of \"propagate\", \"remove\", or \"error\".");
}
static
void stop_bad_negative(struct r_lazy call) {
  r_abort_call(call.env, "`negative` must be one of \"invert\", \"error\", or \"ignore\".");
}
static
void stop_bad_oob(struct r_lazy call) {
  r_abort_call(call.env, "`oob` must be one of \"error\", \"remove\", or \"extend\".");
}
static
void stop_bad_zero(struct r_lazy call) {
  r_abort_call(call.env, "`zero` must be one of \"remove\", \"error\", or \"ignore\".");
}

static
void stop_subscript_missing(r_obj* i,
                            const struct location_opts* opts) {
  r_obj* call = KEEP(r_lazy_eval(opts->subscript_opts.call));
  vctrs_eval_mask2(r_sym("stop_subscript_missing"),
                   syms_i, i,
                   syms_call, call);
  r_stop_unreachable();
}

static
void stop_subscript_empty(r_obj* i,
                          const struct location_opts* opts) {
  r_obj* call = KEEP(r_lazy_eval(opts->subscript_opts.call));
  vctrs_eval_mask2(r_sym("stop_subscript_empty"),
                   syms_i, i,
                   syms_call, call);
  r_stop_unreachable();
}

static
void stop_location_negative_missing(r_obj* i,
                                    const struct location_opts* opts) {
  r_obj* arg = KEEP(vctrs_arg(opts->subscript_opts.subscript_arg));
  r_obj* call = KEEP(r_lazy_eval(opts->subscript_opts.call));
  vctrs_eval_mask4(r_sym("stop_location_negative_missing"),
                   syms_i, i,
                   syms_subscript_arg, arg,
                   syms_call, call,
                   syms_subscript_action, get_opts_action(&opts->subscript_opts));
  r_stop_unreachable();
}
static
void stop_location_negative_positive(r_obj* i,
                                     const struct location_opts* opts) {
  r_obj* arg = KEEP(vctrs_arg(opts->subscript_opts.subscript_arg));
  r_obj* call = KEEP(r_lazy_eval(opts->subscript_opts.call));
  vctrs_eval_mask4(r_sym("stop_location_negative_positive"),
                   syms_i, i,
                   syms_subscript_arg, arg,
                   syms_call, call,
                   syms_subscript_action, get_opts_action(&opts->subscript_opts));
  r_stop_unreachable();
}

static
void stop_subscript_oob_location(r_obj* i,
                                 r_ssize size,
                                 const struct location_opts* opts) {
  r_obj* size_obj = KEEP(r_int(size));
  r_obj* arg = KEEP(vctrs_arg(opts->subscript_opts.subscript_arg));
  r_obj* call = KEEP(r_lazy_eval(opts->subscript_opts.call));
  vctrs_eval_mask6(r_sym("stop_subscript_oob"),
                   syms_i, i,
                   syms_subscript_type, chrs_numeric,
                   syms_size, size_obj,
                   syms_subscript_action, get_opts_action(&opts->subscript_opts),
                   syms_subscript_arg, arg,
                   syms_call, call);
  r_stop_unreachable();
}
static
void stop_subscript_negative_oob_location(r_obj* i,
                                          r_ssize size,
                                          const struct location_opts* opts) {
  struct location_opts error_opts = *opts;
  error_opts.subscript_opts.action = SUBSCRIPT_ACTION_NEGATE;
  stop_subscript_oob_location(i, size, &error_opts);
}
static
void stop_subscript_oob_name(r_obj* i,
                             r_obj* names,
                             const struct location_opts* opts) {
  r_obj* arg = KEEP(vctrs_arg(opts->subscript_opts.subscript_arg));
  r_obj* call = KEEP(r_lazy_eval(opts->subscript_opts.call));
  vctrs_eval_mask6(r_sym("stop_subscript_oob"),
                   syms_i, i,
                   syms_subscript_type, chrs_character,
                   syms_names, names,
                   syms_subscript_action, get_opts_action(&opts->subscript_opts),
                   syms_subscript_arg, arg,
                   syms_call, call);
  r_stop_unreachable();
}

static
void stop_location_negative(r_obj* i,
                            const struct location_opts* opts) {
  r_obj* arg = KEEP(vctrs_arg(opts->subscript_opts.subscript_arg));
  r_obj* call = KEEP(r_lazy_eval(opts->subscript_opts.call));
  vctrs_eval_mask4(r_sym("stop_location_negative"),
                   syms_i, i,
                   syms_subscript_action, get_opts_action(&opts->subscript_opts),
                   syms_subscript_arg, arg,
                   syms_call, call);
  r_stop_unreachable();
}

static
void stop_location_zero(r_obj* i,
                        const struct location_opts* opts) {
  r_obj* arg = KEEP(vctrs_arg(opts->subscript_opts.subscript_arg));
  r_obj* call = KEEP(r_lazy_eval(opts->subscript_opts.call));
  vctrs_eval_mask4(r_sym("stop_location_zero"),
                   syms_i, i,
                   syms_subscript_action, get_opts_action(&opts->subscript_opts),
                   syms_subscript_arg, arg,
                   syms_call, call);
  r_stop_unreachable();
}

static
void stop_indicator_size(r_obj* i,
                         r_obj* n,
                         const struct location_opts* opts) {
  r_obj* arg = KEEP(vctrs_arg(opts->subscript_opts.subscript_arg));
  r_obj* call = KEEP(r_lazy_eval(opts->subscript_opts.call));
  vctrs_eval_mask5(r_sym("stop_indicator_size"),
                   syms_i, i,
                   syms_n, n,
                   syms_subscript_action, get_opts_action(&opts->subscript_opts),
                   syms_subscript_arg, arg,
                   syms_call, call);
  r_stop_unreachable();
}

static
void stop_location_oob_non_consecutive(r_obj* i,
                                       r_ssize size,
                                       const struct location_opts* opts) {
  r_obj* size_obj = KEEP(r_int(size));
  r_obj* arg = KEEP(vctrs_arg(opts->subscript_opts.subscript_arg));
  r_obj* call = KEEP(r_lazy_eval(opts->subscript_opts.call));
  vctrs_eval_mask5(r_sym("stop_location_oob_non_consecutive"),
                   syms_i, i,
                   syms_size, size_obj,
                   syms_subscript_action, get_opts_action(&opts->subscript_opts),
                   syms_subscript_arg, arg,
                   syms_call, call);

  FREE(1);
  r_stop_unreachable();
}

void vctrs_init_subscript_loc(r_obj* ns) { }
