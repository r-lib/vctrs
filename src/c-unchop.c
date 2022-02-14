#include "vctrs.h"
#include "c.h"
#include "ptype-common.h"
#include "slice.h"
#include "slice-assign.h"
#include "owned.h"
#include "utils.h"

// Defined in slice-chop.c
SEXP vec_as_indices(SEXP indices, R_len_t n, SEXP names);


static SEXP vec_unchop(SEXP x,
                       SEXP indices,
                       SEXP ptype,
                       SEXP name_spec,
                       const struct name_repair_opts* name_repair);

// [[ register() ]]
SEXP vctrs_unchop(SEXP x, SEXP indices, SEXP ptype, SEXP name_spec, SEXP name_repair) {
  struct name_repair_opts name_repair_opts = new_name_repair_opts(name_repair,
                                                                  args_empty,
                                                                  false,
                                                                  r_lazy_null);
  KEEP(name_repair_opts.shelter);

  SEXP out = vec_unchop(x, indices, ptype, name_spec, &name_repair_opts);

  FREE(1);
  return out;
}

enum fallback_homogeneous {
  FALLBACK_HOMOGENEOUS_false = 0,
  FALLBACK_HOMOGENEOUS_true
};
static SEXP vec_unchop_fallback(SEXP ptype,
                                SEXP x,
                                SEXP indices,
                                SEXP name_spec,
                                const struct name_repair_opts* name_repair,
                                enum fallback_homogeneous homogenous);

static SEXP vec_unchop(SEXP xs,
                       SEXP indices,
                       SEXP ptype,
                       SEXP name_spec,
                       const struct name_repair_opts* name_repair) {
  if (!vec_is_list(xs)) {
    Rf_errorcall(R_NilValue, "`x` must be a list");
  }

  if (indices == R_NilValue) {
    return vec_c(xs, ptype, name_spec, name_repair);
  }

  R_len_t xs_size = vec_size(xs);

  // Apply size/type checking to `indices` before possibly exiting early from
  // having a `NULL` common type
  if (xs_size != vec_size(indices)) {
    Rf_errorcall(R_NilValue, "`x` and `indices` must be lists of the same size");
  }

  if (!vec_is_list(indices)) {
    Rf_errorcall(R_NilValue, "`indices` must be a list of integers, or `NULL`");
  }

  ptype = PROTECT(vec_ptype_common_params(xs,
                                          ptype,
                                          DF_FALLBACK_DEFAULT,
                                          S3_FALLBACK_true,
                                          args_empty,
                                          r_lazy_null));

  if (needs_vec_c_fallback(ptype)) {
    SEXP out = vec_unchop_fallback(ptype, xs, indices, name_spec, name_repair, FALLBACK_HOMOGENEOUS_false);
    UNPROTECT(1);
    return out;
  }
  // FIXME: Needed for dplyr::summarise() which passes a non-fallback ptype
  if (needs_vec_c_homogeneous_fallback(xs, ptype)) {
    SEXP out = vec_unchop_fallback(ptype, xs, indices, name_spec, name_repair, FALLBACK_HOMOGENEOUS_true);
    UNPROTECT(1);
    return out;
  }

  if (ptype == R_NilValue) {
    UNPROTECT(1);
    return R_NilValue;
  }

  xs = PROTECT(vec_cast_common(xs, ptype, r_lazy_null));

  bool assign_names = !Rf_inherits(name_spec, "rlang_zap");
  SEXP xs_names = PROTECT(r_names(xs));
  bool xs_is_named = xs_names != R_NilValue && !is_data_frame(ptype);

  R_len_t out_size = 0;

  // `out_size` is computed from `indices`
  for (R_len_t i = 0; i < xs_size; ++i) {
    SEXP x = VECTOR_ELT(xs, i);

    if (x == R_NilValue) {
      continue;
    }

    R_len_t index_size = Rf_length(VECTOR_ELT(indices, i));
    out_size += index_size;

    // Each element of `xs` is recycled to its corresponding index's size
    x = vec_recycle(x, index_size, args_empty);
    SET_VECTOR_ELT(xs, i, x);
  }

  SEXP locs = PROTECT(vec_as_indices(indices, out_size, R_NilValue));

  SEXP proxy = vec_proxy(ptype);
  PROTECT_INDEX proxy_pi;
  PROTECT_WITH_INDEX(proxy, &proxy_pi);

  proxy = vec_init(proxy, out_size);
  REPROTECT(proxy, proxy_pi);

  SEXP out_names = R_NilValue;
  PROTECT_INDEX out_names_pi;
  PROTECT_WITH_INDEX(out_names, &out_names_pi);

  const struct vec_assign_opts unchop_assign_opts = {
    .assign_names = assign_names,
    .ignore_outer_names = true
  };

  for (R_len_t i = 0; i < xs_size; ++i) {
    SEXP x = VECTOR_ELT(xs, i);

    if (x == R_NilValue) {
      continue;
    }

    SEXP loc = VECTOR_ELT(locs, i);

    if (assign_names) {
      R_len_t size = Rf_length(loc);
      SEXP outer = xs_is_named ? STRING_ELT(xs_names, i) : R_NilValue;
      SEXP inner = PROTECT(vec_names(x));
      SEXP x_nms = PROTECT(apply_name_spec(name_spec, outer, inner, size));

      if (x_nms != R_NilValue) {
        R_LAZY_ALLOC(out_names, out_names_pi, STRSXP, out_size);

        // If there is no name to assign, skip the assignment since
        // `out_names` already contains empty strings
        if (x_nms != chrs_empty) {
          out_names = chr_assign(out_names, loc, x_nms, VCTRS_OWNED_true);
          REPROTECT(out_names, out_names_pi);
        }
      }

      UNPROTECT(2);
    }

    // Total ownership of `proxy` because it was freshly created with `vec_init()`
    proxy = vec_proxy_assign_opts(proxy, loc, x, VCTRS_OWNED_true, &unchop_assign_opts);
    REPROTECT(proxy, proxy_pi);
  }

  SEXP out_size_sexp = PROTECT(r_int(out_size));

  SEXP out = PROTECT(vec_restore(proxy, ptype, out_size_sexp, VCTRS_OWNED_true));

  if (out_names != R_NilValue) {
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

// This is essentially:
// vec_slice_fallback(vec_c_fallback_invoke(!!!x), order(vec_c(!!!indices)))
// with recycling of each element of `x` to the corresponding index size
static SEXP vec_unchop_fallback(SEXP ptype,
                                SEXP x,
                                SEXP indices,
                                SEXP name_spec,
                                const struct name_repair_opts* name_repair,
                                enum fallback_homogeneous homogeneous) {
  R_len_t x_size = vec_size(x);
  x = PROTECT(r_clone_referenced(x));

  R_len_t out_size = 0;

  // Recycle `x` elements to the size of their corresponding index
  for (R_len_t i = 0; i < x_size; ++i) {
    SEXP elt = VECTOR_ELT(x, i);

    R_len_t index_size = vec_size(VECTOR_ELT(indices, i));
    out_size += index_size;

    SET_VECTOR_ELT(x, i, vec_recycle_fallback(elt, index_size, args_empty));
  }

  indices = PROTECT(vec_as_indices(indices, out_size, R_NilValue));

  SEXP out = R_NilValue;
  if (homogeneous) {
    out = PROTECT(vec_c_fallback_invoke(x, name_spec));
  } else {
    out = PROTECT(vec_c_fallback(ptype, x, name_spec, name_repair));
  }

  const struct name_repair_opts name_repair_opts = {
    .type = name_repair_none,
    .fn = R_NilValue
  };

  indices = PROTECT(vec_c(
    indices,
    vctrs_shared_empty_int,
    R_NilValue,
    &name_repair_opts
  ));

  const int* p_indices = INTEGER(indices);

  SEXP locations = PROTECT(Rf_allocVector(INTSXP, out_size));
  int* p_locations = INTEGER(locations);

  // Initialize with missing to handle locations that are never selected
  for (R_len_t i = 0; i < out_size; ++i) {
    p_locations[i] = NA_INTEGER;
  }

  for (R_len_t i = 0; i < out_size; ++i) {
    const int index = p_indices[i];

    if (index == NA_INTEGER) {
      continue;
    }

    p_locations[index - 1] = i + 1;
  }

  out = PROTECT(vec_slice_fallback(out, locations));

  UNPROTECT(6);
  return out;
}
