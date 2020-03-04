#include "vctrs.h"
#include "utils.h"

// From type.c
SEXP vctrs_type_common_impl(SEXP dots, SEXP ptype);


// [[ register(external = TRUE) ]]
SEXP vctrs_c(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP xs = PROTECT(rlang_env_dots_list(env));
  SEXP ptype = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP name_spec = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  SEXP name_repair = PROTECT(Rf_eval(CAR(args), env));

  struct name_repair_opts name_repair_opts = new_name_repair_opts(name_repair, false);
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
  R_len_t n = Rf_length(xs);

  if (needs_vec_c_fallback(xs)) {
    return vec_c_fallback(xs, ptype, name_spec);
  }

  ptype = PROTECT(vctrs_type_common_impl(xs, ptype));

  if (ptype == R_NilValue) {
    UNPROTECT(1);
    return R_NilValue;
  }

  // Find individual input sizes and total size of output
  R_len_t out_size = 0;

  SEXP ns_placeholder = PROTECT(Rf_allocVector(INTSXP, n));
  int* ns = INTEGER(ns_placeholder);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = VECTOR_ELT(xs, i);
    R_len_t size = (elt == R_NilValue) ? 0 : vec_size(elt);
    out_size += size;
    ns[i] = size;
  }

  PROTECT_INDEX out_pi;
  SEXP out = vec_init(ptype, out_size);
  PROTECT_WITH_INDEX(out, &out_pi);
  out = vec_proxy(out);
  REPROTECT(out, out_pi);

  SEXP idx = PROTECT(compact_seq(0, 0, true));
  int* idx_ptr = INTEGER(idx);

  SEXP xs_names = PROTECT(r_names(xs));
  bool has_names = xs_names != R_NilValue || list_has_inner_vec_names(xs, n);
  has_names = has_names && !is_data_frame(ptype);

  PROTECT_INDEX out_names_pi;
  SEXP out_names = has_names ? Rf_allocVector(STRSXP, out_size) : R_NilValue;
  PROTECT_WITH_INDEX(out_names, &out_names_pi);

  // Compact sequences use 0-based counters
  R_len_t counter = 0;

  for (R_len_t i = 0; i < n; ++i) {
    R_len_t size = ns[i];
    if (!size) {
      continue;
    }

    // TODO
    SEXP x = VECTOR_ELT(xs, i);
    SEXP elt = PROTECT(vec_cast(x, ptype, args_empty, args_empty));

    init_compact_seq(idx_ptr, counter, size, true);

    out = vec_proxy_assign(out, idx, elt);
    REPROTECT(out, out_pi);

    if (has_names) {
      SEXP outer = xs_names == R_NilValue ? R_NilValue : STRING_ELT(xs_names, i);
      SEXP inner = PROTECT(vec_names(x));
      SEXP x_nms = PROTECT(apply_name_spec(name_spec, outer, inner, size));
      if (x_nms != R_NilValue) {
        out_names = chr_assign(out_names, idx, x_nms);
        REPROTECT(out_names, out_names_pi);
      }
      UNPROTECT(2);
    }

    counter += size;
    UNPROTECT(1);
  }

  out = PROTECT(vec_restore(out, ptype, R_NilValue));

  if (has_names) {
    out_names = PROTECT(vec_as_names(out_names, name_repair));
    out = vec_set_names(out, out_names);
    REPROTECT(out, out_pi);
    UNPROTECT(1);
  }

  UNPROTECT(7);
  return out;
}

// [[ include("vctrs.h") ]]
bool needs_vec_c_fallback(SEXP xs) {
  if (!Rf_length(xs)) {
    return false;
  }

  SEXP x = list_first_non_null(xs, NULL);
  if (!vec_is_vector(x)) {
    return false;
  }

  return
    !vec_implements_ptype2(x) &&
    list_is_s3_homogeneous(xs);
}

static inline bool vec_implements_base_c(SEXP x);
static inline int vec_c_fallback_validate_args(SEXP ptype, SEXP name_spec);
static inline void stop_vec_c_fallback(SEXP xs, int err_type);

// [[ include("vctrs.h") ]]
SEXP vec_c_fallback(SEXP xs, SEXP ptype, SEXP name_spec) {
  int err_type = vec_c_fallback_validate_args(ptype, name_spec);
  if (err_type) {
    stop_vec_c_fallback(xs, err_type);
  }

  SEXP args = PROTECT(Rf_coerceVector(xs, LISTSXP));
  args = PROTECT(node_compact_d(args));

  if (!vec_implements_base_c(CAR(args))) {
    stop_vec_c_fallback(xs, 3);
  }

  SEXP call = PROTECT(Rf_lcons(Rf_install("c"), args));

  // Dispatch in the base namespace which inherits from the global env
  SEXP out = Rf_eval(call, R_BaseNamespace);

  UNPROTECT(3);
  return out;
}

static inline bool vec_implements_base_c(SEXP x) {
  return
    OBJECT(x) &&
    s3_find_method("c", x, base_method_table) != R_NilValue;
}

static inline int vec_c_fallback_validate_args(SEXP ptype, SEXP name_spec) {
  if (ptype != R_NilValue) {
    return 1;
  }
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
  case 1: msg = "Can't specify a prototype with non-vctrs types."; break;
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
