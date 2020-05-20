#include "vctrs.h"
#include "slice.h"
#include "slice-assign.h"
#include "subscript-loc.h"
#include "ptype-common.h"
#include "type-data-frame.h"
#include "utils.h"
#include "dim.h"

/*
 * @member proxy_info The result of `vec_proxy_info(x)`.
 * @member restore_size The restore size used in each call to `vec_restore()`.
 *   Will always be 1 for `indices = NULL`.
 * @member p_restore_size A pointer to update the restore size.
 * @member index The current index value. If `indices` are provided, this is
 *   the i-th element of indices. For the default of `indices = NULL`, this
 *   starts at 0 and is incremented by 1 repeatedly through `p_index`.
 * @member p_index A pointer to increment the `index` value for the default
 *   case.
 * @member has_indices Whether indices were provided.
 * @member out_size The size of `out`. Will be `vec_size(x)` in the default
 *   case, otherwise will be `vec_size(indices)`.
 * @member out The list container for the result.
 */
struct vctrs_chop_info {
  struct vctrs_proxy_info proxy_info;
  SEXP restore_size;
  int* p_restore_size;
  SEXP index;
  int* p_index;
  bool has_indices;
  R_len_t out_size;
  SEXP out;
};

#define PROTECT_CHOP_INFO(info, n) do {       \
  PROTECT_PROXY_INFO(&(info)->proxy_info, n); \
  PROTECT((info)->restore_size);              \
  PROTECT((info)->index);                     \
  PROTECT((info)->out);                       \
  *n += 3;                                    \
} while (0)                                   \

static struct vctrs_chop_info init_chop_info(SEXP x, SEXP indices) {
  int nprot = 0;

  struct vctrs_chop_info info;

  info.proxy_info = vec_proxy_info(x);
  PROTECT_PROXY_INFO(&info.proxy_info, &nprot);

  info.restore_size = PROTECT_N(r_int(1), &nprot);
  info.p_restore_size = INTEGER(info.restore_size);

  info.index = PROTECT_N(r_int(0), &nprot);
  info.p_index = INTEGER(info.index);

  if (indices == R_NilValue) {
    info.out_size = vec_size(x);
    info.has_indices = false;
  } else {
    info.out_size = vec_size(indices);
    info.has_indices = true;
  }

  info.out = PROTECT_N(Rf_allocVector(VECSXP, info.out_size), &nprot);

  UNPROTECT(nprot);
  return info;
}

// -----------------------------------------------------------------------------

static SEXP chop(SEXP x, SEXP indices, struct vctrs_chop_info info);
static SEXP chop_shaped(SEXP x, SEXP indices, struct vctrs_chop_info info);
static SEXP chop_df(SEXP x, SEXP indices, struct vctrs_chop_info info);
static SEXP chop_fallback(SEXP x, SEXP indices, struct vctrs_chop_info info);
static SEXP chop_fallback_shaped(SEXP x, SEXP indices, struct vctrs_chop_info info);

static SEXP vec_chop_base(SEXP x, SEXP indices, struct vctrs_chop_info info);

static SEXP vec_as_indices(SEXP indices, R_len_t n, SEXP names);

// [[ register() ]]
SEXP vctrs_chop_seq(SEXP x, SEXP starts, SEXP sizes, SEXP increasings) {
  int* p_starts = INTEGER(starts);
  int* p_sizes = INTEGER(sizes);
  int* p_increasings = LOGICAL(increasings);

  int n = Rf_length(starts);

  SEXP indices = PROTECT(Rf_allocVector(VECSXP, n));

  for (int i = 0; i < n; ++i) {
    SEXP index = compact_seq(p_starts[i], p_sizes[i], p_increasings[i]);
    SET_VECTOR_ELT(indices, i, index);
  }

  SEXP out = PROTECT(vec_chop(x, indices));

  UNPROTECT(2);
  return out;
}

// [[ register() ]]
SEXP vctrs_chop(SEXP x, SEXP indices) {
  R_len_t n = vec_size(x);
  SEXP names = PROTECT(vec_names(x));

  indices = PROTECT(vec_as_indices(indices, n, names));

  SEXP out = PROTECT(vec_chop(x, indices));

  UNPROTECT(3);
  return out;
}

// [[ include("vctrs.h") ]]
SEXP vec_chop(SEXP x, SEXP indices) {
  int nprot = 0;

  struct vctrs_chop_info info = init_chop_info(x, indices);
  PROTECT_CHOP_INFO(&info, &nprot);

  SEXP out = PROTECT_N(vec_chop_base(x, indices, info), &nprot);

  UNPROTECT(nprot);
  return out;
}

static SEXP vec_chop_base(SEXP x, SEXP indices, struct vctrs_chop_info info) {
  struct vctrs_proxy_info proxy_info = info.proxy_info;

  // Fallback to `[` if the class doesn't implement a proxy. This is
  // to be maximally compatible with existing classes.
  if (vec_requires_fallback(x, proxy_info)) {
    if (proxy_info.type == vctrs_type_scalar) {
      Rf_errorcall(R_NilValue, "Can't slice a scalar");
    }

    if (info.has_indices) {
      for (int i = 0; i < info.out_size; ++i) {
        SEXP index = VECTOR_ELT(indices, i);

        if (is_compact(index)) {
          SET_VECTOR_ELT(indices, i, compact_materialize(index));
        }
      }
    }

    if (has_dim(x)) {
      return chop_fallback_shaped(x, indices, info);
    }

    return chop_fallback(x, indices, info);
  }

  switch (proxy_info.type) {
  case vctrs_type_logical:
  case vctrs_type_integer:
  case vctrs_type_double:
  case vctrs_type_complex:
  case vctrs_type_character:
  case vctrs_type_raw:
  case vctrs_type_list: {
    if (has_dim(x)) {
      return chop_shaped(x, indices, info);
    }

    return chop(x, indices, info);
  }
  case vctrs_type_dataframe: {
    return chop_df(x, indices, info);
  }
  default:
    vec_assert(x, args_empty);
    Rf_error(
      "Internal error: Unexpected type `%s` for vector proxy in `vec_chop()`",
      vec_type_as_str(proxy_info.type)
    );
  }
}

static SEXP chop(SEXP x, SEXP indices, struct vctrs_chop_info info) {
  SEXP proxy = info.proxy_info.proxy;
  SEXP names = PROTECT(Rf_getAttrib(proxy, R_NamesSymbol));

  for (R_len_t i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      info.index = VECTOR_ELT(indices, i);
      *info.p_restore_size = vec_subscript_size(info.index);
    } else {
      ++(*info.p_index);
    }

    SEXP elt = PROTECT(vec_slice_base(info.proxy_info.type, proxy, info.index));

    if (names != R_NilValue) {
      SEXP elt_names = PROTECT(slice_names(names, info.index));
      r_poke_names(elt, elt_names);
      UNPROTECT(1);
    }

    elt = vec_restore(elt, x, info.restore_size);

    SET_VECTOR_ELT(info.out, i, elt);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return info.out;
}

static SEXP chop_df(SEXP x, SEXP indices, struct vctrs_chop_info info) {
  SEXP proxy = info.proxy_info.proxy;

  int n_cols = Rf_length(proxy);

  SEXP col_names = PROTECT(Rf_getAttrib(proxy, R_NamesSymbol));
  SEXP row_names = PROTECT(df_rownames(proxy));

  bool has_row_names = TYPEOF(row_names) == STRSXP;

  // Pre-load the `out` container with lists that will become data frames
  for (R_len_t i = 0; i < info.out_size; ++i) {
    SEXP elt = PROTECT(Rf_allocVector(VECSXP, n_cols));

    Rf_setAttrib(elt, R_NamesSymbol, col_names);

    if (has_row_names) {
      if (info.has_indices) {
        info.index = VECTOR_ELT(indices, i);
      } else {
        ++(*info.p_index);
      }

      Rf_setAttrib(elt, R_RowNamesSymbol, slice_rownames(row_names, info.index));
    }

    SET_VECTOR_ELT(info.out, i, elt);
    UNPROTECT(1);
  }

  // Split each column according to the indices, and then assign the results
  // into the appropriate data frame column in the `out` list
  for (int i = 0; i < n_cols; ++i) {
    SEXP col = VECTOR_ELT(proxy, i);
    SEXP split = PROTECT(vec_chop(col, indices));

    for (int j = 0; j < info.out_size; ++j) {
      SEXP elt = VECTOR_ELT(info.out, j);
      SET_VECTOR_ELT(elt, i, VECTOR_ELT(split, j));
    }

    UNPROTECT(1);
  }

  // Restore each data frame
  for (int i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      *info.p_restore_size = vec_subscript_size(VECTOR_ELT(indices, i));
    }

    SEXP elt = VECTOR_ELT(info.out, i);
    elt = vec_restore(elt, x, info.restore_size);
    SET_VECTOR_ELT(info.out, i, elt);
  }

  UNPROTECT(2);
  return info.out;
}

static SEXP chop_shaped(SEXP x, SEXP indices, struct vctrs_chop_info info) {
  SEXP proxy = info.proxy_info.proxy;
  SEXP dim_names = PROTECT(Rf_getAttrib(proxy, R_DimNamesSymbol));

  SEXP row_names = R_NilValue;
  if (dim_names != R_NilValue) {
    row_names = VECTOR_ELT(dim_names, 0);
  }

  for (R_len_t i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      info.index = VECTOR_ELT(indices, i);
      *info.p_restore_size = vec_subscript_size(info.index);
    } else {
      ++(*info.p_index);
    }

    SEXP elt = PROTECT(vec_slice_shaped(info.proxy_info.type, proxy, info.index));

    if (dim_names != R_NilValue) {
      if (row_names != R_NilValue) {
        SEXP new_dim_names = PROTECT(Rf_shallow_duplicate(dim_names));
        SEXP new_row_names = PROTECT(slice_names(row_names, info.index));

        SET_VECTOR_ELT(new_dim_names, 0, new_row_names);
        Rf_setAttrib(elt, R_DimNamesSymbol, new_dim_names);
        UNPROTECT(2);
      } else {
        Rf_setAttrib(elt, R_DimNamesSymbol, dim_names);
      }
    }

    elt = vec_restore(elt, x, info.restore_size);

    SET_VECTOR_ELT(info.out, i, elt);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return info.out;
}

static SEXP chop_fallback(SEXP x, SEXP indices, struct vctrs_chop_info info) {
  // Evaluate in a child of the global environment to allow dispatch
  // to custom functions. We define `[` to point to its base
  // definition to ensure consistent look-up. This is the same logic
  // as in `vctrs_dispatch_n()`, reimplemented here to allow repeated
  // evaluations in a loop.
  SEXP env = PROTECT(r_new_environment(R_GlobalEnv, 2));
  Rf_defineVar(syms_x, x, env);
  Rf_defineVar(syms_i, info.index, env);

  // Construct call with symbols, not values, for performance.
  // TODO - Remove once bit64 is updated on CRAN. Special casing integer64
  // objects to ensure correct slicing with `NA_integer_`.
  SEXP call;
  if (is_integer64(x)) {
    call = PROTECT(Rf_lang3(syms_vec_slice_dispatch_integer64, syms_x, syms_i));
    Rf_defineVar(syms_vec_slice_dispatch_integer64, fns_vec_slice_dispatch_integer64, env);
  } else {
    call = PROTECT(Rf_lang3(syms_bracket, syms_x, syms_i));
    Rf_defineVar(syms_bracket, fns_bracket, env);
  }

  for (R_len_t i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      info.index = VECTOR_ELT(indices, i);
      *info.p_restore_size = vec_size(info.index);

      // Update `i` binding with the new index value
      Rf_defineVar(syms_i, info.index, env);
    } else {
      ++(*info.p_index);
    }

    SEXP elt = PROTECT(Rf_eval(call, env));

    // Restore attributes only if `[` fallback doesn't
    if (ATTRIB(elt) == R_NilValue) {
      elt = vec_restore(elt, x, info.restore_size);
    }

    SET_VECTOR_ELT(info.out, i, elt);
    UNPROTECT(1);
  }

  UNPROTECT(2);
  return info.out;
}

static SEXP chop_fallback_shaped(SEXP x, SEXP indices, struct vctrs_chop_info info) {
  for (R_len_t i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      info.index = VECTOR_ELT(indices, i);
    } else {
      ++(*info.p_index);
    }

    // `vec_slice_fallback()` will also `vec_restore()` for us
    SEXP elt = PROTECT(vec_slice_fallback(x, info.index));

    SET_VECTOR_ELT(info.out, i, elt);
    UNPROTECT(1);
  }

  return info.out;
}

// -----------------------------------------------------------------------------

static SEXP vec_unchop(SEXP x,
                       SEXP indices,
                       SEXP ptype,
                       SEXP name_spec,
                       const struct name_repair_opts* name_repair);

// [[ register() ]]
SEXP vctrs_unchop(SEXP x, SEXP indices, SEXP ptype, SEXP name_spec, SEXP name_repair) {
  struct name_repair_opts name_repair_opts = new_name_repair_opts(name_repair, args_empty, false);
  PROTECT_NAME_REPAIR_OPTS(&name_repair_opts);

  SEXP out = vec_unchop(x, indices, ptype, name_spec, &name_repair_opts);

  UNPROTECT(1);
  return out;
}

static inline bool needs_vec_unchop_fallback(SEXP x, SEXP ptype);
static SEXP vec_unchop_fallback(SEXP x, SEXP indices, SEXP name_spec);

static SEXP vec_unchop(SEXP x,
                       SEXP indices,
                       SEXP ptype,
                       SEXP name_spec,
                       const struct name_repair_opts* name_repair) {
  if (!vec_is_list(x)) {
    Rf_errorcall(R_NilValue, "`x` must be a list");
  }

  if (indices == R_NilValue) {
    return vec_c(x, ptype, name_spec, name_repair);
  }

  R_len_t x_size = vec_size(x);

  // Apply size/type checking to `indices` before possibly exiting early from
  // having a `NULL` common type
  if (x_size != vec_size(indices)) {
    Rf_errorcall(R_NilValue, "`x` and `indices` must be lists of the same size");
  }

  if (!vec_is_list(indices)) {
    Rf_errorcall(R_NilValue, "`indices` must be a list of integers, or `NULL`");
  }

  if (needs_vec_unchop_fallback(x, ptype)) {
    return vec_unchop_fallback(x, indices, name_spec);
  }

  ptype = PROTECT(vec_ptype_common_params(x, ptype, DF_FALLBACK_WARN, S3_FALLBACK_false));

  if (ptype == R_NilValue) {
    UNPROTECT(1);
    return R_NilValue;
  }

  x = PROTECT(vec_cast_common(x, ptype));

  SEXP x_names = PROTECT(r_names(x));

  bool has_outer_names = (x_names != R_NilValue);
  bool assign_names = !Rf_inherits(name_spec, "rlang_zap");
  bool has_names =
    assign_names &&
    !is_data_frame(ptype) &&
    (has_outer_names || list_has_inner_vec_names(x, x_size));

  // Element sizes are only required for applying the `name_spec`
  SEXP sizes = vctrs_shared_empty_int;
  if (has_names) {
    sizes = Rf_allocVector(INTSXP, x_size);
  }
  PROTECT(sizes);
  int* p_sizes = INTEGER(sizes);

  R_len_t out_size = 0;

  // `out_size` is computed from `indices`
  for (R_len_t i = 0; i < x_size; ++i) {
    SEXP elt = VECTOR_ELT(x, i);

    if (elt == R_NilValue) {
      continue;
    }

    R_len_t index_size = vec_size(VECTOR_ELT(indices, i));
    out_size += index_size;

    if (has_names) {
      p_sizes[i] = index_size;
    }

    // Each element of `x` is recycled to its corresponding index's size
    elt = vec_recycle(elt, index_size, args_empty);
    SET_VECTOR_ELT(x, i, elt);
  }

  indices = PROTECT(vec_as_indices(indices, out_size, R_NilValue));

  PROTECT_INDEX proxy_pi;
  SEXP proxy = vec_proxy(ptype);
  PROTECT_WITH_INDEX(proxy, &proxy_pi);
  proxy = vec_init(proxy, out_size);
  REPROTECT(proxy, proxy_pi);

  PROTECT_INDEX out_names_pi;
  SEXP out_names = vctrs_shared_empty_chr;
  if (has_names) {
    out_names = Rf_allocVector(STRSXP, out_size);
  }
  PROTECT_WITH_INDEX(out_names, &out_names_pi);

  const struct vec_assign_opts unchop_assign_opts = {
    .assign_names = assign_names
  };

  for (R_len_t i = 0; i < x_size; ++i) {
    SEXP elt = VECTOR_ELT(x, i);

    if (elt == R_NilValue) {
      continue;
    }

    SEXP index = VECTOR_ELT(indices, i);

    // Total ownership of `proxy` because it was freshly created with `vec_init()`
    proxy = vec_proxy_assign_opts(proxy, index, elt, vctrs_ownership_total, &unchop_assign_opts);
    REPROTECT(proxy, proxy_pi);

    if (has_names) {
      R_len_t size = p_sizes[i];
      SEXP outer = (has_outer_names) ? STRING_ELT(x_names, i) : R_NilValue;
      SEXP inner = PROTECT(vec_names(elt));
      SEXP elt_names = PROTECT(apply_name_spec(name_spec, outer, inner, size));
      if (elt_names != R_NilValue) {
        out_names = chr_assign(out_names, index, elt_names, vctrs_ownership_total);
        REPROTECT(out_names, out_names_pi);
      }
      UNPROTECT(2);
    }
  }

  SEXP out_size_sexp = PROTECT(r_int(out_size));

  SEXP out = PROTECT(vec_restore(proxy, ptype, out_size_sexp));

  if (has_names) {
    out_names = vec_as_names(out_names, name_repair);
    REPROTECT(out_names, out_names_pi);
    out = vec_set_names(out, out_names);
  } else if (!assign_names) {
    // FIXME: `vec_ptype2()` doesn't consistently zaps names, so `out`
    // might have been initialised with names. This branch can be
    // removed once #1020 is resolved.
    out = vec_set_names(out, R_NilValue);
  }

  UNPROTECT(9);
  return out;
}

// Unchopping is a just version of `vec_c()` that controls the ordering,
// so they both fallback to `c()` in the same situations
static inline bool needs_vec_unchop_fallback(SEXP x, SEXP ptype) {
  return needs_vec_c_homogeneous_fallback(x, ptype);
}

// This is essentially:
// vec_slice_fallback(vec_c_fallback_invoke(!!!x), order(vec_c(!!!indices)))
// with recycling of each element of `x` to the corresponding index size
static SEXP vec_unchop_fallback(SEXP x, SEXP indices, SEXP name_spec) {
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

  SEXP out = PROTECT(vec_c_fallback_invoke(x, name_spec));

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

// -----------------------------------------------------------------------------

static SEXP vec_as_indices(SEXP indices, R_len_t n, SEXP names) {
  if (indices == R_NilValue) {
    return indices;
  }

  if (TYPEOF(indices) != VECSXP) {
    Rf_errorcall(R_NilValue, "`indices` must be a list of index values, or `NULL`.");
  }

  indices = PROTECT(r_clone_referenced(indices));

  R_len_t size = vec_size(indices);

  const struct subscript_opts subscript_opts = {
    .action = SUBSCRIPT_ACTION_DEFAULT,
    .logical = SUBSCRIPT_TYPE_ACTION_ERROR,
    .numeric = SUBSCRIPT_TYPE_ACTION_CAST,
    .character = SUBSCRIPT_TYPE_ACTION_ERROR,
    .subscript_arg = NULL
  };

  // Restrict index values to positive integer locations
  const struct location_opts opts = {
    .subscript_opts = &subscript_opts,
    .missing = SUBSCRIPT_MISSING_PROPAGATE,
    .loc_negative = LOC_NEGATIVE_ERROR,
    .loc_oob = LOC_OOB_ERROR,
    .loc_zero = LOC_ZERO_ERROR
  };

  for (int i = 0; i < size; ++i) {
    SEXP index = VECTOR_ELT(indices, i);
    index = vec_as_location_opts(index, n, names, &opts);
    SET_VECTOR_ELT(indices, i, index);
  }

  UNPROTECT(1);
  return indices;
}
