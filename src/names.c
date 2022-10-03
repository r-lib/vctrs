#include <ctype.h>
#include "vctrs.h"
#include "type-data-frame.h"
#include "decl/names-decl.h"

// 3 leading '.' + 1 trailing '\0' + 24 characters
#define MAX_IOTA_SIZE 28


r_obj* vec_as_names(r_obj* names, const struct name_repair_opts* opts) {
  if (!opts) {
    return names;
  }
  switch (opts->type) {
  case NAME_REPAIR_none: return names;
  case NAME_REPAIR_minimal: return ffi_as_minimal_names(names);
  case NAME_REPAIR_unique: return vec_as_unique_names(names, opts->quiet);
  case NAME_REPAIR_universal: return vec_as_universal_names(names, opts->quiet);
  case NAME_REPAIR_check_unique: return check_unique_names(names, opts);
  case NAME_REPAIR_custom: return vec_as_custom_names(names, opts);
  }
  r_stop_unreachable();
}

r_obj* ffi_vec_as_names(r_obj* names,
                        r_obj* repair,
                        r_obj* ffi_quiet,
                        r_obj* frame) {
  if (!r_is_bool(ffi_quiet)) {
    r_abort("`quiet` must a boolean value.");
  }
  bool quiet = r_lgl_get(ffi_quiet, 0);

  struct r_lazy call = (struct r_lazy) { .x = r_syms.call, .env = frame };
  struct r_lazy repair_arg = { .x = syms.repair_arg, .env = frame };

  struct name_repair_opts repair_opts = new_name_repair_opts(repair,
                                                             repair_arg,
                                                             quiet,
                                                             call);
  KEEP(repair_opts.shelter);

  r_obj* out = vec_as_names(names, &repair_opts);

  FREE(1);
  return out;
}


struct repair_error_info {
  r_obj* shelter;
  r_obj* repair_arg;
  r_obj* call;
  r_obj* input_error_repair_arg;
  r_obj* input_error_call;
};

struct repair_error_info new_repair_error_info(struct name_repair_opts* p_opts) {
  struct repair_error_info out;

  out.shelter = r_new_list(4);
  KEEP(out.shelter);

  out.repair_arg = r_lazy_eval(p_opts->name_repair_arg);
  r_list_poke(out.shelter, 0, out.repair_arg);

  out.call = r_lazy_eval(p_opts->call);
  r_list_poke(out.shelter, 1, out.call);

  // If this is NULL, the `repair` value has been hard-coded by the
  // frontend. Input errors are internal, and we provide no
  // recommendation to fix user errors by providing a different value
  // for `repair`.
  if (out.repair_arg == r_null) {
    out.input_error_repair_arg = chrs.repair;
    r_list_poke(out.shelter, 2, out.input_error_repair_arg);

    out.input_error_call = r_call(r_sym("vec_as_names"));
    r_list_poke(out.shelter, 3, out.input_error_call);
  } else {
    out.input_error_repair_arg = r_lazy_eval(p_opts->name_repair_arg);
    r_list_poke(out.shelter, 2, out.input_error_repair_arg);

    out.input_error_call = r_lazy_eval(p_opts->call);
    r_list_poke(out.shelter, 3, out.input_error_call);
  }

  FREE(1);
  return out;
}


r_obj* vec_as_universal_names(r_obj* names, bool quiet) {
  r_obj* quiet_obj = KEEP(r_lgl(quiet));
  r_obj* out = vctrs_dispatch2(syms_as_universal_names, fns_as_universal_names,
                               syms_names, names,
                               syms_quiet, quiet_obj);
  FREE(1);
  return out;
}

static
r_obj* check_unique_names(r_obj* names,
                          const struct name_repair_opts* opts) {
  r_obj* ffi_arg = KEEP(r_lazy_eval(opts->name_repair_arg));
  r_obj* ffi_call = KEEP(r_lazy_eval(opts->call));

  r_obj* out = KEEP(vctrs_dispatch3(syms_check_unique_names, fns_check_unique_names,
                                    syms_names, names,
                                    r_syms.arg, ffi_arg,
                                    syms_call, ffi_call));

  // Restore visibility
  r_eval(r_null, r_envs.empty);

  FREE(3);
  return out;
}

r_obj* vec_as_custom_names(r_obj* names, const struct name_repair_opts* opts) {
  names = KEEP(ffi_as_minimal_names(names));

  // Don't use vctrs dispatch utils because we match argument positionally
  r_obj* call = KEEP(r_call2(syms_repair, syms_names));
  r_obj* mask = KEEP(r_new_environment(R_GlobalEnv));
  r_env_poke(mask, syms_repair, opts->fn);
  r_env_poke(mask, syms_names, names);
  r_obj* out = KEEP(r_eval(call, mask));

  vec_validate_minimal_names(out, r_length(names), opts->call);

  FREE(4);
  return out;
}

static
r_obj* vec_names_impl(r_obj* x, bool proxy) {
  bool has_class = r_is_object(x);

  if (has_class && r_inherits(x, "data.frame")) {
    // Only return row names if they are character. Data frames with
    // automatic row names are treated as unnamed.
    r_obj* rn = df_rownames(x);
    if (rownames_type(rn) == ROWNAMES_TYPE_identifiers) {
      return rn;
    } else {
      return r_null;
    }
  }

  if (vec_bare_dim(x) == r_null) {
    if (!proxy && has_class) {
      return vctrs_dispatch1(syms_names, fns_names, syms_x, x);
    } else {
      return r_names(x);
    }
  }

  r_obj* dimnames = KEEP(r_attrib_get(x, r_syms.dim_names));
  if (dimnames == r_null || r_length(dimnames) < 1) {
    FREE(1);
    return r_null;
  }

  r_obj* out = r_list_get(dimnames, 0);
  FREE(1);
  return out;
}

// [[ register() ]]
r_obj* vec_names(r_obj* x) {
  return vec_names_impl(x, false);
}
r_obj* vec_proxy_names(r_obj* x) {
  return vec_names_impl(x, true);
}

r_obj* vec_names2(r_obj* x) {
  r_obj* names = vec_names(x);
  if (names == r_null) {
    return r_alloc_character(vec_size(x));
  } else {
    return names;
  }
}

r_obj* ffi_as_minimal_names(r_obj* names) {
  if (r_typeof(names) != R_TYPE_character) {
    r_abort("`names` must be a character vector");
  }

  r_ssize i = 0;
  r_ssize n = r_length(names);
  r_obj* const * v_names = r_chr_cbegin(names);

  for (; i < n; ++i) {
    if (v_names[i] == r_globals.na_str) {
      break;
    }
  }
  if (i == n) {
    return names;
  }

  names = KEEP(r_clone(names));

  for (; i < n; ++i) {
    if (v_names[i] == r_globals.na_str) {
      r_chr_poke(names, i, strings_empty);
    }
  }

  FREE(1);
  return names;
}

r_obj* ffi_minimal_names(r_obj* x) {
  r_obj* names = KEEP(vec_names(x));

  if (names == r_null) {
    names = r_alloc_character(vec_size(x));
  } else {
    names = ffi_as_minimal_names(names);
  }

  FREE(1);
  return names;
}


// From dictionary.c
r_obj* vctrs_duplicated(r_obj* x);

// [[ include("vctrs.h") ]]
r_obj* vec_as_unique_names(r_obj* names, bool quiet) {
  if (is_unique_names(names) && !any_has_suffix(names)) {
    return names;
  } else {
    return(as_unique_names_impl(names, quiet));
  }
}

// [[ include("vctrs.h") ]]
bool is_unique_names(r_obj* names) {
  if (r_typeof(names) != R_TYPE_character) {
    r_abort("`names` must be a character vector");
  }

  r_ssize n = r_length(names);
  r_obj* const * v_names = r_chr_cbegin(names);

  if (duplicated_any(names)) {
    return false;
  }

  for (r_ssize i = 0; i < n; ++i) {
    if (needs_suffix(v_names[i])) {
      return false;
    }
  }

  return true;
}

bool any_has_suffix(r_obj* names) {
  r_ssize n = r_length(names);
  r_obj* const * v_names = r_chr_cbegin(names);

  for (r_ssize i = 0; i < n; ++i) {
    if (suffix_pos(r_str_c_string(v_names[i])) >= 0) {
      return true;
    }
  }

  return false;
}

r_obj* as_unique_names_impl(r_obj* names, bool quiet) {
  r_ssize n = r_length(names);

  r_obj* new_names = KEEP(r_clone(names));
  r_obj* const * v_new_names = r_chr_cbegin(new_names);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* elt = v_new_names[i];

    // Set `NA` and dots values to "" so they get replaced by `...n`
    // later on
    if (needs_suffix(elt)) {
      elt = strings_empty;
      r_chr_poke(new_names, i, elt);
      continue;
    }

    // Strip `...n` suffixes
    const char* nm = r_str_c_string(elt);
    int pos = suffix_pos(nm);
    if (pos >= 0) {
      elt = Rf_mkCharLenCE(nm, pos, Rf_getCharCE(elt));
      r_chr_poke(new_names, i, elt);
      continue;
    }
  }

  // Append all duplicates with a suffix

  r_obj* dups = KEEP(vctrs_duplicated(new_names));
  const int* dups_ptr = r_lgl_cbegin(dups);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* elt = v_new_names[i];

    if (elt != strings_empty && !dups_ptr[i]) {
      continue;
    }

    const char* name = r_str_c_string(elt);

    int size = strlen(name);
    int buf_size = size + MAX_IOTA_SIZE;

    R_CheckStack2(buf_size);
    char buf[buf_size];
    buf[0] = '\0';

    memcpy(buf, name, size);
    int remaining = buf_size - size;

    int needed = snprintf(buf + size, remaining, "...%d", (int) i + 1);
    if (needed >= remaining) {
      stop_large_name();
    }

    r_chr_poke(new_names, i, Rf_mkCharLenCE(buf, size + needed, Rf_getCharCE(elt)));
  }

  if (!quiet) {
    describe_repair(names, new_names);
  }

  FREE(2);
  return new_names;
}

r_obj* vctrs_as_unique_names(r_obj* names, r_obj* quiet) {
  r_obj* out = KEEP(vec_as_unique_names(names, r_lgl_get(quiet, 0)));
  FREE(1);
  return out;
}

r_obj* vctrs_is_unique_names(r_obj* names) {
  bool out = is_unique_names(names);
  return r_lgl(out);
}

static
bool is_dotdotint(const char* name) {
  int n = strlen(name);

  if (n < 3) {
    return false;
  }
  if (name[0] != '.' || name[1] != '.') {
    return false;
  }

  if (name[2] == '.') {
    name += 3;
  } else {
    name += 2;
  }

  return (bool) strtol(name, NULL, 10);
}

static
ptrdiff_t suffix_pos(const char* name) {
  int n = strlen(name);

  const char* suffix_end = NULL;
  int in_dots = 0;
  bool in_digits = false;

  for (const char* ptr = name + n - 1; ptr >= name; --ptr) {
    char c = *ptr;

    if (in_digits) {
      if (c == '.') {
        in_digits = false;
        in_dots = 1;
        continue;
      }

      if (isdigit(c)) {
        continue;
      }

      goto done;
    }

    switch (in_dots) {
    case 0:
      if (isdigit(c)) {
        in_digits = true;
        continue;
      }
      goto done;
    case 1:
    case 2:
      if (c == '.') {
        ++in_dots;
        continue;
      }
      goto done;
    case 3:
      suffix_end = ptr + 1;
      if (isdigit(c)) {
        in_dots = 0;
        in_digits = true;
        continue;
      }
      goto done;

    default:
      r_stop_internal("Unexpected state.");
    }}

 done:
  if (suffix_end) {
    return suffix_end - name;
  } else {
    return -1;
  }
}

static void stop_large_name() {
  r_abort("Can't tidy up name because it is too large.");
}

static bool needs_suffix(r_obj* str) {
  return
    str == r_globals.na_str ||
    str == strings_dots ||
    str == strings_empty ||
    is_dotdotint(r_str_c_string(str));
}

r_obj* ffi_unique_names(r_obj* x, r_obj* quiet) {
  return vec_unique_names(x, LOGICAL(quiet)[0]);
}

r_obj* vec_unique_names(r_obj* x, bool quiet) {
  r_obj* names = KEEP(vec_names(x));
  r_obj* out = vec_unique_names_impl(names, vec_size(x), quiet);
  FREE(1);
  return out;
}
r_obj* vec_unique_colnames(r_obj* x, bool quiet) {
  r_obj* names = KEEP(colnames(x));
  r_obj* out = vec_unique_names_impl(names, Rf_ncols(x), quiet);
  FREE(1);
  return out;
}

static
r_obj* vec_unique_names_impl(r_obj* names, r_ssize n, bool quiet) {
  r_obj* out;
  if (names == r_null) {
    out = KEEP(names_iota(n));
    if (!quiet) {
      describe_repair(names, out);
    }
  } else {
    out = KEEP(vec_as_unique_names(names, quiet));
  }

  FREE(1);
  return(out);
}

static
r_obj* names_iota(r_ssize n) {
  char buf[MAX_IOTA_SIZE];
  r_obj* nms = r_chr_iota(n, buf, MAX_IOTA_SIZE, "...");

  if (nms == r_null) {
    r_abort("Too many names to repair.");
  }

  return nms;
}



static
void describe_repair(r_obj* old_names, r_obj* new_names) {
  r_obj* call = KEEP(r_call3(r_sym("describe_repair"),
                             old_names,
                             new_names));
  r_eval(call, vctrs_ns_env);

  // To reset visibility when called from a `.External2()`
  r_eval(r_null, r_envs.empty);

  FREE(1);
}


r_obj* ffi_outer_names(r_obj* names, r_obj* outer, r_obj* n) {
  if (names != r_null && r_typeof(names) != R_TYPE_character) {
    r_stop_internal("`names` must be `NULL` or a string.");
  }
  if (!r_is_number(n)) {
    r_stop_internal("`n` must be a single integer.");
  }

  if (outer != r_null) {
    outer = r_chr_get(outer, 0);
  }

  return outer_names(names, outer, r_int_get(n, 0));
}

r_obj* outer_names(r_obj* names, r_obj* outer, r_ssize n) {
  if (outer == r_null) {
    return names;
  }
  if (r_typeof(outer) != R_TYPE_string) {
    r_stop_internal("`outer` must be a scalar string.");
  }

  if (outer == strings_empty || outer == r_globals.na_str) {
    return names;
  }

  if (r_is_empty_names(names)) {
    if (n == 1) {
      return r_str_as_character(outer);
    } else {
      return r_seq_chr(r_str_c_string(outer), n);
    }
  } else {
    return r_chr_paste_prefix(names, r_str_c_string(outer), "..");
  }
}

r_obj* ffi_apply_name_spec(r_obj* name_spec, r_obj* outer, r_obj* inner, r_obj* n) {
  return apply_name_spec(name_spec, r_chr_get(outer, 0), inner, r_int_get(n, 0));
}

r_obj* apply_name_spec(r_obj* name_spec, r_obj* outer, r_obj* inner, r_ssize n) {
  if (r_inherits(name_spec, "rlang_zap")) {
    return r_null;
  }

  if (outer == r_null) {
    return inner;
  }
  if (r_typeof(outer) != R_TYPE_string) {
    r_stop_internal("`outer` must be a scalar string.");
  }

  if (outer == strings_empty || outer == r_globals.na_str) {
    if (inner == r_null) {
      return chrs_empty;
    } else {
      return inner;
    }
  }

  if (r_is_empty_names(inner)) {
    if (n == 0) {
      return r_globals.empty_chr;
    }
    if (n == 1) {
      return r_str_as_character(outer);
    }
    inner = KEEP(r_seq(1, n + 1));
  } else {
    inner = KEEP(inner);
  }

  switch (r_typeof(name_spec)) {
  case R_TYPE_closure:
    break;
  case R_TYPE_character:
    name_spec = glue_as_name_spec(name_spec);
    break;
  default:
    name_spec = r_as_function(name_spec, ".name_spec");
    break;
  case R_TYPE_null: {
    const char* reason;
    if (n > 1) {
      reason = "a vector of length > 1";
    } else {
      reason = "a named vector";
    }
    r_abort("Can't merge the outer name `%s` with %s.\n"
            "Please supply a `.name_spec` specification.",
            r_str_c_string(outer),
            reason);
  }}
  KEEP(name_spec);

  r_obj* outer_chr = KEEP(r_str_as_character(outer));

  r_obj* out = KEEP(vctrs_dispatch2(syms_dot_name_spec, name_spec,
                                    syms_outer, outer_chr,
                                    syms_inner, inner));
  out = vec_recycle(out, n);

  if (out != r_null) {
    if (r_typeof(out) != R_TYPE_character) {
      r_abort("`.name_spec` must return a character vector.");
    }
    if (r_length(out) != n) {
      r_abort("`.name_spec` must return a character vector as long as `inner`.");
    }
  }

  FREE(4);
  return out;
}


static
r_obj* glue_as_name_spec(r_obj* spec) {
  if (!r_is_string(spec)) {
    r_abort("Glue specification in `.name_spec` must be a single string.");
  }
  return vctrs_dispatch1(syms_glue_as_name_spec, fns_glue_as_name_spec,
                         syms_internal_spec, spec);
}

#define VCTRS_PASTE_BUFFER_MAX_SIZE 4096
char vctrs_paste_buffer[VCTRS_PASTE_BUFFER_MAX_SIZE];

r_obj* r_chr_paste_prefix(r_obj* names, const char* prefix, const char* sep) {
  int n_prot = 0;

  names = KEEP_N(r_clone(names), &n_prot);
  r_ssize n = r_length(names);

  int outer_len = strlen(prefix);
  int names_len = r_chr_max_len(names);

  int sep_len = strlen(sep);
  int total_len = outer_len + names_len + sep_len + 1;

  char* buf = vctrs_paste_buffer;
  if (total_len > VCTRS_PASTE_BUFFER_MAX_SIZE) {
    r_obj* buf_box = KEEP_N(
      r_alloc_raw(total_len * sizeof(char)),
      &n_prot
    );
    buf = (char*) RAW(buf_box);
  }

  buf[total_len - 1] = '\0';
  char* bufp = buf;

  memcpy(bufp, prefix, outer_len); bufp += outer_len;

  for (int i = 0; i < sep_len; ++i) {
    *bufp++ = sep[i];
  }

  r_obj* const* p_names = r_chr_cbegin(names);

  for (r_ssize i = 0; i < n; ++i) {
    const char* inner = r_str_c_string(p_names[i]);
    int inner_n = strlen(inner);

    memcpy(bufp, inner, inner_n);
    bufp[inner_n] = '\0';

    r_chr_poke(names, i, r_str(buf));
  }

  FREE(n_prot);
  return names;
}

r_obj* ffi_chr_paste_prefix(r_obj* names, r_obj* prefix, r_obj* sep) {
  return r_chr_paste_prefix(names,
                            r_chr_get_c_string(prefix, 0),
                            r_chr_get_c_string(sep, 0));
}

r_obj* r_seq_chr(const char* prefix, r_ssize n) {
  int total_len = 24 + strlen(prefix) + 1;

  R_CheckStack2(total_len);
  char buf[total_len];

  return r_chr_iota(n, buf, total_len, prefix);
}


static
r_obj* set_rownames_dispatch(r_obj* x, r_obj* names) {
  return vctrs_dispatch2(syms_set_rownames_dispatch, fns_set_rownames_dispatch,
                         syms_x, x,
                         syms_names, names);
}

static
r_obj* set_names_dispatch(r_obj* x, r_obj* names) {
  return vctrs_dispatch2(syms_set_names_dispatch, fns_set_names_dispatch,
                         syms_x, x,
                         syms_names, names);
}

static
void check_names(r_obj* x, r_obj* names) {
  if (names == r_null) {
    return;
  }

  if (r_typeof(names) != R_TYPE_character) {
    r_abort(
      "`names` must be a character vector, not a %s.",
      r_type_as_c_string(r_typeof(names))
    );
  }

  r_ssize x_size = vec_size(x);
  r_ssize names_size = vec_size(names);

  if (x_size != names_size) {
    r_abort(
      "The size of `names`, %i, must be the same as the size of `x`, %i.",
      names_size,
      x_size
    );
  }
}

r_obj* vec_set_rownames(r_obj* x, r_obj* names, bool proxy, const enum vctrs_owned owned) {
  if (!proxy && r_is_object(x)) {
    return set_rownames_dispatch(x, names);
  }

  int nprot = 0;

  r_obj* dim_names = r_attrib_get(x, r_syms.dim_names);

  // Early exit when no new row names and no existing row names
  if (names == r_null) {
    if (dim_names == r_null || r_list_get(dim_names, 0) == r_null) {
      return x;
    }
  }

  x = KEEP_N(vec_clone_referenced(x, owned), &nprot);

  if (dim_names == r_null) {
    dim_names = KEEP_N(r_alloc_list(vec_dim_n(x)), &nprot);
  } else {
    // Also clone attribute
    dim_names = KEEP_N(r_clone(dim_names), &nprot);
  }

  r_list_poke(dim_names, 0, names);

  r_attrib_poke(x, r_syms.dim_names, dim_names);

  FREE(nprot);
  return x;
}

r_obj* vec_set_df_rownames(r_obj* x, r_obj* names, bool proxy, const enum vctrs_owned owned) {
  if (names == r_null) {
    if (rownames_type(df_rownames(x)) != ROWNAMES_TYPE_identifiers) {
      return(x);
    }

    x = KEEP(vec_clone_referenced(x, owned));
    init_compact_rownames(x, vec_size(x));

    FREE(1);
    return x;
  }

  // Repair row names silently
  if (!proxy) {
    names = vec_as_names(names, p_unique_repair_silent_opts);
  }
  KEEP(names);

  x = KEEP(vec_clone_referenced(x, owned));
  r_attrib_poke(x, r_syms.row_names, names);

  FREE(2);
  return x;
}

// FIXME: Do we need to get the vec_proxy() and only fall back if it doesn't
// exist? See #526 and #531 for discussion and the related issue.
r_obj* vec_set_names_impl(r_obj* x, r_obj* names, bool proxy, const enum vctrs_owned owned) {
  check_names(x, names);

  if (is_data_frame(x)) {
    return vec_set_df_rownames(x, names, proxy, owned);
  }

  if (has_dim(x)) {
    return vec_set_rownames(x, names, proxy, owned);
  }

  if (!proxy && r_is_object(x)) {
    return set_names_dispatch(x, names);
  }

  // Early exit if no new names and no existing names
  if (names == r_null && r_attrib_get(x, r_syms.names) == r_null) {
    return x;
  }

  if (owned) {
    // Possibly skip the cloning altogether
    x = KEEP(vec_clone_referenced(x, owned));
    r_attrib_poke(x, r_syms.names, names);
  } else {
    // We need to clone, but to do this we will use `names<-`
    // which can perform a cheaper ALTREP shallow duplication
    x = KEEP(set_names_dispatch(x, names));
  }

  FREE(1);
  return x;
}
// [[ register() ]]
r_obj* vec_set_names(r_obj* x, r_obj* names) {
  return vec_set_names_impl(x, names, false, VCTRS_OWNED_false);
}
r_obj* vec_proxy_set_names(r_obj* x, r_obj* names, const enum vctrs_owned owned) {
  return vec_set_names_impl(x, names, true, owned);
}


r_obj* vctrs_validate_name_repair_arg(r_obj* arg) {
  struct name_repair_opts opts = new_name_repair_opts(arg,
                                                      r_lazy_null,
                                                      true,
                                                      r_lazy_null);
  if (opts.type == NAME_REPAIR_custom) {
    return opts.fn;
  } else if (r_length(arg) != 1) {
    return r_str_as_character(r_str(name_repair_arg_as_c_string(opts.type)));
  } else {
    return arg;
  }
}

void stop_name_repair(struct name_repair_opts* p_opts) {
  struct repair_error_info info = new_repair_error_info(p_opts);
  KEEP(info.shelter);

  r_abort_call(info.input_error_call,
               "%s must be a string or a function. See `?vctrs::vec_as_names`.",
               r_format_error_arg(info.input_error_repair_arg));
}

struct name_repair_opts new_name_repair_opts(r_obj* name_repair,
                                             struct r_lazy name_repair_arg,
                                             bool quiet,
                                             struct r_lazy call) {
  struct name_repair_opts opts = {
    .shelter = r_null,
    .type = 0,
    .fn = r_null,
    .name_repair_arg = name_repair_arg,
    .quiet = quiet,
    .call = call
  };

  switch (r_typeof(name_repair)) {
  case R_TYPE_character: {
    if (!r_length(name_repair)) {
      stop_name_repair(&opts);
    }

    r_obj* c = r_chr_get(name_repair, 0);

    if (c == strings_none) {
      opts.type = NAME_REPAIR_none;
    } else if (c == strings_minimal) {
      opts.type = NAME_REPAIR_minimal;
    } else if (c == strings_unique) {
      opts.type = NAME_REPAIR_unique;
    } else if (c == strings_universal) {
      opts.type = NAME_REPAIR_universal;
    } else if (c == strings_check_unique) {
      opts.type = NAME_REPAIR_check_unique;
    } else if (c == strings_unique_quiet) {
      opts.type = NAME_REPAIR_unique;
      opts.quiet = true;
    } else if (c == strings_universal_quiet) {
      opts.type = NAME_REPAIR_universal;
      opts.quiet = true;
    } else {
      struct repair_error_info info = new_repair_error_info(&opts);
      KEEP(info.shelter);
      r_abort_call(info.input_error_call,
                   "%s can't be \"%s\". See `?vctrs::vec_as_names`.",
                   r_format_error_arg(info.input_error_repair_arg),
                   r_str_c_string(c));
    }

    return opts;
  }

  case R_TYPE_call:
    opts.fn = r_as_function(name_repair, ".name_repair");
    opts.shelter = opts.fn;
    opts.type = NAME_REPAIR_custom;
    return opts;

  case R_TYPE_closure:
    opts.fn = name_repair;
    opts.type = NAME_REPAIR_custom;
    return opts;

  default:
    stop_name_repair(&opts);
  }

  r_stop_unreachable();
}

const char* name_repair_arg_as_c_string(enum name_repair_type type) {
  switch (type) {
  case NAME_REPAIR_none: return "none";
  case NAME_REPAIR_minimal: return "minimal";
  case NAME_REPAIR_unique: return "unique";
  case NAME_REPAIR_universal: return "universal";
  case NAME_REPAIR_check_unique: return "check_unique";
  case NAME_REPAIR_custom: return "custom";
  }
  r_stop_unreachable();
}

static
void vec_validate_minimal_names(r_obj* names, r_ssize n, struct r_lazy call) {
  if (names == r_null) {
    r_abort_lazy_call(call, "Names repair functions can't return `NULL`.");
  }

  if (r_typeof(names) != R_TYPE_character) {
    r_abort_lazy_call(call, "Names repair functions must return a character vector.");
  }

  if (n >= 0 && r_length(names) != n) {
    r_abort_lazy_call(call,
                      "Repaired names have length %d instead of length %d.",
                      r_length(names),
                      n);
  }

  if (r_chr_has_string(names, r_globals.na_str)) {
    r_abort_lazy_call(call, "Names repair functions can't return `NA` values.");
  }
}
r_obj* vctrs_validate_minimal_names(r_obj* names, r_obj* n_) {
  r_ssize n = -1;

  if (r_typeof(n_) == R_TYPE_integer) {
    if (r_length(n_) != 1) {
      r_stop_internal("`n` must be a single number.");
    }
    n = r_int_get(n_, 0);
  }

  vec_validate_minimal_names(names, n, r_lazy_null);
  return names;
}


struct name_repair_opts unique_repair_default_opts;
struct name_repair_opts unique_repair_silent_opts;
struct name_repair_opts no_repair_opts;

void vctrs_init_names(r_obj* ns) {
  syms_set_rownames_dispatch = r_sym("set_rownames_dispatch");
  syms_set_names_dispatch = r_sym("set_names_dispatch");
  syms_as_universal_names = r_sym("as_universal_names");
  syms_check_unique_names = r_sym("validate_unique");

  fns_set_rownames_dispatch = r_env_get(ns, syms_set_rownames_dispatch);
  fns_set_names_dispatch = r_env_get(ns, syms_set_names_dispatch);
  fns_as_universal_names = r_env_get(ns, syms_as_universal_names);
  fns_check_unique_names = r_env_get(ns, syms_check_unique_names);

  syms_glue_as_name_spec = r_sym("glue_as_name_spec");
  fns_glue_as_name_spec = r_env_get(ns, syms_glue_as_name_spec);
  syms_internal_spec = r_sym("_spec");

  unique_repair_default_opts.type = NAME_REPAIR_unique;
  unique_repair_default_opts.fn = r_null;
  unique_repair_default_opts.quiet = false;

  unique_repair_silent_opts.type = NAME_REPAIR_unique;
  unique_repair_silent_opts.fn = r_null;
  unique_repair_silent_opts.quiet = true;

  no_repair_opts.type = NAME_REPAIR_none;
  no_repair_opts.fn = r_null;
  no_repair_opts.quiet = true;
}

static r_obj* syms_as_universal_names = NULL;
static r_obj* syms_check_unique_names = NULL;
static r_obj* syms_glue_as_name_spec = NULL;
static r_obj* syms_internal_spec = NULL;
static r_obj* syms_set_rownames_dispatch = NULL;
static r_obj* syms_set_names_dispatch = NULL;

static r_obj* fns_as_universal_names = NULL;
static r_obj* fns_check_unique_names = NULL;
static r_obj* fns_glue_as_name_spec = NULL;
static r_obj* fns_set_rownames_dispatch = NULL;
static r_obj* fns_set_names_dispatch = NULL;
