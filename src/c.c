#include "vctrs.h"
#include "c.h"
#include "ptype-common.h"
#include "slice-assign.h"
#include "owned.h"
#include "utils.h"


// [[ register(external = TRUE) ]]
SEXP vctrs_c(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP xs = PROTECT(rlang_env_dots_list(env));
  SEXP ptype = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP name_spec = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP name_repair = PROTECT(Rf_eval(CAR(args), env));

  struct name_repair_opts name_repair_opts = new_name_repair_opts(name_repair, args_empty, false);
  PROTECT_NAME_REPAIR_OPTS(&name_repair_opts);

  SEXP out = vec_c(xs, ptype, name_spec, &name_repair_opts);

  UNPROTECT(5);
  return out;
}


// [[ include("vctrs.h") ]]
SEXP vec_c(SEXP xs,
           SEXP ptype,
           SEXP name_spec,
           const struct name_repair_opts* name_repair) {
  struct fallback_opts opts = {
    .df = DF_FALLBACK_DEFAULT,
    .s3 = S3_FALLBACK_true
  };
  return vec_c_opts(xs, ptype, name_spec, name_repair, &opts);
}

SEXP vec_c_opts(SEXP xs,
                SEXP ptype,
                SEXP name_spec,
                const struct name_repair_opts* name_repair,
                const struct fallback_opts* fallback_opts) {
  SEXP orig_ptype = ptype;
  ptype = PROTECT(vec_ptype_common_opts(xs, orig_ptype, fallback_opts));

  if (ptype == R_NilValue) {
    UNPROTECT(1);
    return R_NilValue;
  }

  if (needs_vec_c_fallback(ptype)) {
    SEXP out = vec_c_fallback(ptype, xs, name_spec, name_repair);
    UNPROTECT(1);
    return out;
  }
  // FIXME: Needed for dplyr::summarise() which passes a non-fallback ptype
  if (needs_vec_c_homogeneous_fallback(xs, ptype)) {
    SEXP out = vec_c_fallback_invoke(xs, name_spec);
    UNPROTECT(1);
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
    struct fallback_opts d_fallback_opts = *fallback_opts;
    d_fallback_opts.s3 = S3_FALLBACK_false;
    ptype = PROTECT(vec_ptype_common_opts(xs, orig_ptype, &d_fallback_opts));
  } else {
    ptype = PROTECT(vec_ptype_common_opts(xs, ptype, fallback_opts));
  }

  // Find individual input sizes and total size of output
  R_len_t n = Rf_length(xs);
  R_len_t out_size = 0;

  // Caching the sizes causes an extra allocation but it improves performance
  SEXP sizes = PROTECT(Rf_allocVector(INTSXP, n));
  int* p_sizes = INTEGER(sizes);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP x = VECTOR_ELT(xs, i);
    R_len_t size = (x == R_NilValue) ? 0 : vec_size(x);
    out_size += size;
    p_sizes[i] = size;
  }

  SEXP out = vec_init(ptype, out_size);
  PROTECT_INDEX out_pi;
  PROTECT_WITH_INDEX(out, &out_pi);

  out = vec_proxy(out);
  REPROTECT(out, out_pi);

  SEXP idx = PROTECT(compact_seq(0, 0, true));
  int* p_idx = INTEGER(idx);

  SEXP xs_names = PROTECT(r_names(xs));
  bool assign_names = !Rf_inherits(name_spec, "rlang_zap");
  bool has_names = assign_names && (xs_names != R_NilValue || list_has_inner_vec_names(xs, n));
  has_names = has_names && !is_data_frame(ptype);

  PROTECT_INDEX out_names_pi;
  SEXP out_names = has_names ? Rf_allocVector(STRSXP, out_size) : R_NilValue;
  PROTECT_WITH_INDEX(out_names, &out_names_pi);

  // Compact sequences use 0-based counters
  R_len_t counter = 0;

  const struct vec_assign_opts c_assign_opts = {
    .assign_names = assign_names
  };

  for (R_len_t i = 0; i < n; ++i) {
    R_len_t size = p_sizes[i];
    if (!size) {
      continue;
    }

    struct cast_opts opts = (struct cast_opts) {
      .x = VECTOR_ELT(xs, i),
      .to = ptype,
      .fallback = *fallback_opts
    };
    SEXP x = PROTECT(vec_cast_opts(&opts));

    init_compact_seq(p_idx, counter, size, true);

    // Total ownership of `out` because it was freshly created with `vec_init()`
    out = vec_proxy_assign_opts(out, idx, x, VCTRS_OWNED_true, &c_assign_opts);
    REPROTECT(out, out_pi);

    // FIXME: This work happens in parallel to the names assignment in
    // `vec_proxy_assign()`. We should add a way to instruct
    // proxy-assign to ignore the outermost names (but still assign
    // inner names in case of data frames).
    if (has_names) {
      SEXP outer = xs_names == R_NilValue ? R_NilValue : STRING_ELT(xs_names, i);
      SEXP inner = PROTECT(vec_names(x));
      SEXP x_nms = PROTECT(apply_name_spec(name_spec, outer, inner, size));

      if (x_nms != R_NilValue) {
        out_names = chr_assign(out_names, idx, x_nms, VCTRS_OWNED_true);
        REPROTECT(out_names, out_names_pi);
      }

      UNPROTECT(2);
    }

    counter += size;
    UNPROTECT(1);
  }

  out = PROTECT(vec_restore(out, ptype, R_NilValue, VCTRS_OWNED_true));

  if (has_names) {
    out_names = PROTECT(vec_as_names(out_names, name_repair));
    out = vec_set_names(out, out_names);
    UNPROTECT(1);
  } else if (!assign_names) {
    // FIXME: `vec_ptype2()` doesn't consistently zaps names, so `out`
    // might have been initialised with names. This branch can be
    // removed once #1020 is resolved.
    out = vec_set_names(out, R_NilValue);
  }

  UNPROTECT(8);
  return out;
}

static inline bool vec_implements_base_c(SEXP x);

// [[ include("c.h") ]]
bool needs_vec_c_fallback(SEXP ptype) {
  if (!vec_is_common_class_fallback(ptype)) {
    return false;
  }

  // Suboptimal: Prevent infinite recursion through `vctrs_vctr` method
  SEXP class = PROTECT(Rf_getAttrib(ptype, syms_fallback_class));
  class = r_chr_get(class, r_length(class) - 1);

  if (class == strings_vctrs_vctr) {
    UNPROTECT(1);
    return false;
  }

  UNPROTECT(1);
  return true;
}

// [[ include("c.h") ]]
bool needs_vec_c_homogeneous_fallback(SEXP xs, SEXP ptype) {
  if (!Rf_length(xs)) {
    return false;
  }

  SEXP x = list_first_non_null(xs, NULL);
  if (!vec_is_vector(x)) {
    return false;
  }

  // Never fall back for `vctrs_vctr` classes to avoid infinite
  // recursion through `c.vctrs_vctr()`
  if (Rf_inherits(x, "vctrs_vctr")) {
    return false;
  }

  if (ptype != R_NilValue) {
    SEXP x_class = PROTECT(r_class(x));
    SEXP ptype_class = PROTECT(r_class(ptype));
    bool equal = equal_object(x_class, ptype_class);
    UNPROTECT(2);

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
bool vec_implements_base_c(SEXP x) {
  if (!OBJECT(x)) {
    return false;
  }

  if (IS_S4_OBJECT(x)) {
    return s4_find_method(x, s4_c_method_table) != R_NilValue;
  } else {
    return s3_find_method("c", x, base_method_table) != R_NilValue;
  }
}
static inline
bool class_implements_base_c(SEXP cls) {
  if (s3_class_find_method("c", cls, base_method_table) != R_NilValue) {
    return true;
  }
  if (s4_class_find_method(cls, s4_c_method_table) != R_NilValue) {
    return true;
  }
  return false;
}

static inline int vec_c_fallback_validate_args(SEXP x, SEXP name_spec);
static inline void stop_vec_c_fallback(SEXP xs, int err_type);

// [[ include("c.h") ]]
SEXP vec_c_fallback(SEXP ptype,
                    SEXP xs,
                    SEXP name_spec,
                    const struct name_repair_opts* name_repair) {
  SEXP class = PROTECT(Rf_getAttrib(ptype, syms_fallback_class));
  bool implements_c = class_implements_base_c(class);
  UNPROTECT(1);

  if (implements_c) {
    return vec_c_fallback_invoke(xs, name_spec);
  } else {
    struct fallback_opts fallback_opts = {
      .df = DF_FALLBACK_none,
      .s3 = S3_FALLBACK_false
    };

    // Should cause a common type error, unless another fallback
    // kicks in (for instance, homogeneous class with homogeneous
    // attributes)
    vec_ptype_common_opts(xs, R_NilValue, &fallback_opts);

    // Suboptimal: Call `vec_c()` again to combine vector with
    // homogeneous class fallback
    return vec_c_opts(xs, R_NilValue, name_spec, name_repair, &fallback_opts);
  }
}

// [[ include("c.h") ]]
SEXP vec_c_fallback_invoke(SEXP xs, SEXP name_spec) {
  SEXP x = list_first_non_null(xs, NULL);

  if (vctrs_debug_verbose) {
    Rprintf("Falling back to `base::c()` for class `%s`.\n",
            r_chr_get_c_string(r_class(x), 0));
  }

  int err_type = vec_c_fallback_validate_args(x, name_spec);
  if (err_type) {
    stop_vec_c_fallback(xs, err_type);
  }

  SEXP call = PROTECT(Rf_lang2(Rf_install("base_c_invoke"), xs));
  SEXP out = Rf_eval(call, vctrs_ns_env);

  UNPROTECT(1);
  return out;
}

static inline
int vec_c_fallback_validate_args(SEXP x, SEXP name_spec) {
  if (name_spec != R_NilValue) {
    return 2;
  }
  return 0;
}

static void stop_vec_c_fallback(SEXP xs, int err_type) {
  SEXP common_class = PROTECT(r_class(list_first_non_null(xs, NULL)));
  const char* class_str = r_chr_get_c_string(common_class, 0);

  const char* msg = NULL;
  switch (err_type) {
  case 2: msg = "Can't use a name specification with non-vctrs types."; break;
  case 3: msg = "Can't find vctrs or base methods for concatenation."; break;
  default: msg = "Internal error: Unexpected error type."; break;
  }

  Rf_errorcall(R_NilValue,
               "%s\n"
               "vctrs methods must be implemented for class `%s`.\n"
               "See <https://vctrs.r-lib.org/articles/s3-vector.html>.",
               msg,
               class_str);
}
