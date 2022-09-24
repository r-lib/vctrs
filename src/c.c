#include "vctrs.h"
#include "decl/c-decl.h"

r_obj* vec_c(r_obj* xs,
             r_obj* ptype,
             r_obj* name_spec,
             const struct name_repair_opts* name_repair) {
  struct fallback_opts opts = {
    .df = DF_FALLBACK_DEFAULT,
    .s3 = S3_FALLBACK_true
  };
  return vec_c_opts(xs, ptype, name_spec, name_repair, &opts);
}

r_obj* vec_c_opts(r_obj* xs,
                  r_obj* ptype,
                  r_obj* name_spec,
                  const struct name_repair_opts* name_repair,
                  const struct fallback_opts* fallback_opts) {
  struct ptype_common_opts ptype_opts = {
    .fallback = *fallback_opts
  };

  r_obj* orig_ptype = ptype;
  ptype = KEEP(vec_ptype_common_opts(xs, orig_ptype, &ptype_opts));

  if (ptype == r_null) {
    FREE(1);
    return r_null;
  }

  if (needs_vec_c_fallback(ptype)) {
    r_obj* out = vec_c_fallback(ptype, xs, name_spec, name_repair);
    FREE(1);
    return out;
  }
  // FIXME: Needed for dplyr::summarise() which passes a non-fallback ptype
  if (needs_vec_c_homogeneous_fallback(xs, ptype)) {
    r_obj* out = vec_c_fallback_invoke(xs, name_spec);
    FREE(1);
    return out;
  }

  // FIXME: If data frame, recompute ptype without common class
  // fallback. Should refactor this to allow common class fallback
  // with data frame columns.
  //
  // FIXME: If `ptype` is a `vctrs_vctr` class without a
  // `vec_ptype2()` method, the common type is a common class
  // fallback. To avoid infinit recursion through `c.vctrs_vctr()`, we
  // bail out from `needs_vec_c_fallback()`. In this case recurse with
  // fallback disabled as well.
  if ((is_data_frame(ptype) && fallback_opts->s3 == S3_FALLBACK_true) ||
      vec_is_common_class_fallback(ptype)) {
    ptype_opts.fallback.s3 = S3_FALLBACK_false;
    ptype = KEEP(vec_ptype_common_opts(xs, orig_ptype, &ptype_opts));
  } else {
    ptype = KEEP(vec_ptype_common_opts(xs, ptype, &ptype_opts));
  }

  // Find individual input sizes and total size of output
  r_ssize n = r_length(xs);
  r_ssize out_size = 0;

  // Caching the sizes causes an extra allocation but it improves performance
  r_obj* sizes = KEEP(r_alloc_integer(n));
  int* p_sizes = r_int_begin(sizes);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* x = r_list_get(xs, i);
    r_ssize size = (x == r_null) ? 0 : vec_size(x);
    out_size += size;
    p_sizes[i] = size;
  }

  r_obj* out = vec_init(ptype, out_size);
  r_keep_loc out_pi;
  KEEP_HERE(out, &out_pi);

  out = vec_proxy_recurse(out);
  KEEP_AT(out, out_pi);

  r_obj* loc = KEEP(compact_seq(0, 0, true));
  int* p_loc = r_int_begin(loc);

  bool assign_names = !r_inherits(name_spec, "rlang_zap");
  r_obj* xs_names = KEEP(r_names(xs));
  bool xs_is_named = xs_names != r_null && !is_data_frame(ptype);

  r_obj* out_names = r_null;
  r_keep_loc out_names_pi;
  KEEP_HERE(r_null, &out_names_pi);

  // Compact sequences use 0-based counters
  r_ssize counter = 0;

  const struct vec_assign_opts c_assign_opts = {
    .recursive = true,
    .assign_names = assign_names,
    .ignore_outer_names = true
  };

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* x = r_list_get(xs, i);
    r_ssize size = p_sizes[i];

    init_compact_seq(p_loc, counter, size, true);

    if (assign_names) {
      r_obj* outer = xs_is_named ? r_chr_get(xs_names, i) : r_null;
      r_obj* inner = KEEP(vec_names(x));
      r_obj* x_nms = KEEP(apply_name_spec(name_spec, outer, inner, size));

      if (x_nms != r_null) {
        R_LAZY_ALLOC(out_names, out_names_pi, R_TYPE_character, out_size);

        // If there is no name to assign, skip the assignment since
        // `out_names` already contains empty strings
        if (x_nms != chrs_empty) {
          out_names = chr_assign(out_names, loc, x_nms, VCTRS_OWNED_true);
          KEEP_AT(out_names, out_names_pi);
        }
      }

      FREE(2);
    }

    if (!size) {
      continue;
    }

    struct cast_opts opts = (struct cast_opts) {
      .x = x,
      .to = ptype,
      .fallback = *fallback_opts
    };
    x = KEEP(vec_cast_opts(&opts));

    // Total ownership of `out` because it was freshly created with `vec_init()`
    out = vec_proxy_assign_opts(out, loc, x, VCTRS_OWNED_true, &c_assign_opts);
    KEEP_AT(out, out_pi);

    counter += size;
    FREE(1);
  }

  out = KEEP(vec_restore_recurse(out, ptype, VCTRS_OWNED_true));

  if (out_names != r_null) {
    out_names = KEEP(vec_as_names(out_names, name_repair));
    out = vec_set_names(out, out_names);
    FREE(1);
  } else if (!assign_names) {
    // FIXME: `vec_ptype2()` doesn't consistently zaps names, so `out`
    // might have been initialised with names. This branch can be
    // removed once #1020 is resolved.
    out = vec_set_names(out, r_null);
  }

  FREE(8);
  return out;
}

r_obj* ffi_vec_c(r_obj* call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  r_obj* xs = KEEP(rlang_env_dots_list(env));
  r_obj* ptype = KEEP(r_eval(r_node_car(args), env)); args = r_node_cdr(args);
  r_obj* name_spec = KEEP(r_eval(r_node_car(args), env)); args = r_node_cdr(args);
  r_obj* name_repair = KEEP(r_eval(r_node_car(args), env));

  struct name_repair_opts name_repair_opts =
    new_name_repair_opts(name_repair,
                         r_lazy_null,
                         // think about this 'false'
                         false,
                         r_lazy_null);
  KEEP(name_repair_opts.shelter);

  r_obj* out = vec_c(xs, ptype, name_spec, &name_repair_opts);

  FREE(5);
  return out;
}


bool needs_vec_c_fallback(r_obj* ptype) {
  if (!vec_is_common_class_fallback(ptype)) {
    return false;
  }

  // Suboptimal: Prevent infinite recursion through `vctrs_vctr` method
  r_obj* class = r_attrib_get(ptype, syms_fallback_class);
  class = r_chr_get(class, r_length(class) - 1);

  return class != strings_vctrs_vctr;
}

bool needs_vec_c_homogeneous_fallback(r_obj* xs, r_obj* ptype) {
  if (!r_length(xs)) {
    return false;
  }

  r_obj* x = list_first_non_null(xs, NULL);
  if (!vec_is_vector(x)) {
    return false;
  }

  // Never fall back for `vctrs_vctr` classes to avoid infinite
  // recursion through `c.vctrs_vctr()`
  if (r_inherits(x, "vctrs_vctr")) {
    return false;
  }

  if (ptype != r_null) {
    r_obj* x_class = KEEP(r_class(x));
    r_obj* ptype_class = KEEP(r_class(ptype));
    bool equal = equal_object(x_class, ptype_class);
    FREE(2);

    if (!equal) {
      return false;
    }
  }

  return
    !vec_implements_ptype2(x) &&
    list_is_homogeneously_classed(xs) &&
    vec_implements_base_c(x);
}

static inline
bool vec_implements_base_c(r_obj* x) {
  if (!r_is_object(x)) {
    return false;
  }

  if (IS_S4_OBJECT(x)) {
    return s4_find_method(x, s4_c_method_table) != r_null;
  } else {
    return s3_find_method("c", x, base_method_table) != r_null;
  }
}

static inline
bool class_implements_base_c(r_obj* cls) {
  if (s3_class_find_method("c", cls, base_method_table) != r_null) {
    return true;
  }
  if (s4_class_find_method(cls, s4_c_method_table) != r_null) {
    return true;
  }
  return false;
}


r_obj* vec_c_fallback(r_obj* ptype,
                      r_obj* xs,
                      r_obj* name_spec,
                      const struct name_repair_opts* name_repair) {
  r_obj* class = KEEP(r_attrib_get(ptype, syms_fallback_class));
  bool implements_c = class_implements_base_c(class);
  FREE(1);

  if (implements_c) {
    return vec_c_fallback_invoke(xs, name_spec);
  } else {
    struct ptype_common_opts ptype_opts = {
      .fallback = {
        .df = DF_FALLBACK_none,
        .s3 = S3_FALLBACK_false
      }
    };

    // Should cause a common type error, unless another fallback
    // kicks in (for instance, homogeneous class with homogeneous
    // attributes)
    vec_ptype_common_opts(xs, r_null, &ptype_opts);

    // Suboptimal: Call `vec_c()` again to combine vector with
    // homogeneous class fallback
    return vec_c_opts(xs, r_null, name_spec, name_repair, &ptype_opts.fallback);
  }
}

r_obj* vec_c_fallback_invoke(r_obj* xs, r_obj* name_spec) {
  r_obj* x = list_first_non_null(xs, NULL);

  if (vctrs_debug_verbose) {
    r_printf("Falling back to `base::c()` for class `%s`.\n",
             r_chr_get_c_string(r_class(x), 0));
  }

  int err_type = vec_c_fallback_validate_args(x, name_spec);
  if (err_type) {
    stop_vec_c_fallback(xs, err_type);
  }

  r_obj* call = KEEP(r_call2(r_sym("base_c_invoke"), xs));
  r_obj* out = r_eval(call, vctrs_ns_env);

  FREE(1);
  return out;
}

static inline
int vec_c_fallback_validate_args(r_obj* x, r_obj* name_spec) {
  if (name_spec != r_null) {
    return 2;
  }
  return 0;
}

static
void stop_vec_c_fallback(r_obj* xs, int err_type) {
  r_obj* common_class = KEEP(r_class(list_first_non_null(xs, NULL)));
  const char* class_str = r_chr_get_c_string(common_class, 0);

  const char* msg = NULL;
  switch (err_type) {
  case 2: msg = "Can't use a name specification with non-vctrs types."; break;
  case 3: msg = "Can't find vctrs or base methods for concatenation."; break;
  default: msg = "Internal error: Unexpected error type."; break;
  }

  r_abort("%s\n"
          "vctrs methods must be implemented for class `%s`.\n"
          "See <https://vctrs.r-lib.org/articles/s3-vector.html>.",
          msg,
          class_str);
}
