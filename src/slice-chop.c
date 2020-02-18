#include "vctrs.h"
#include "slice.h"
#include "subscript-loc.h"
#include "type-data-frame.h"
#include "utils.h"

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
  SEXP elt;
  SEXP names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));

  for (R_len_t i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      info.index = VECTOR_ELT(indices, i);
      *info.p_restore_size = vec_subscript_size(info.index);
    } else {
      ++(*info.p_index);
    }

    elt = PROTECT(vec_slice_base(info.proxy_info.type, info.proxy_info.proxy, info.index));

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
  SEXP elt;

  int n_cols = Rf_length(x);

  SEXP col_names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
  SEXP row_names = PROTECT(df_rownames(x));

  bool has_row_names = TYPEOF(row_names) == STRSXP;

  // Pre-load the `out` container with lists that will become data frames
  for (R_len_t i = 0; i < info.out_size; ++i) {
    elt = PROTECT(Rf_allocVector(VECSXP, n_cols));

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
    SEXP col = VECTOR_ELT(info.proxy_info.proxy, i);
    SEXP split = PROTECT(vec_chop(col, indices));

    for (int j = 0; j < info.out_size; ++j) {
      elt = VECTOR_ELT(info.out, j);
      SET_VECTOR_ELT(elt, i, VECTOR_ELT(split, j));
    }

    UNPROTECT(1);
  }

  // Restore each data frame
  for (int i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      *info.p_restore_size = vec_subscript_size(VECTOR_ELT(indices, i));
    }

    elt = VECTOR_ELT(info.out, i);
    elt = vec_restore(elt, x, info.restore_size);
    SET_VECTOR_ELT(info.out, i, elt);
  }

  UNPROTECT(2);
  return info.out;
}

static SEXP chop_shaped(SEXP x, SEXP indices, struct vctrs_chop_info info) {
  SEXP elt;

  SEXP dim_names = PROTECT(Rf_getAttrib(x, R_DimNamesSymbol));

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

    elt = PROTECT(vec_slice_shaped(info.proxy_info.type, info.proxy_info.proxy, info.index));

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
  SEXP elt;

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

    elt = PROTECT(Rf_eval(call, env));

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
  SEXP elt;

  for (R_len_t i = 0; i < info.out_size; ++i) {
    if (info.has_indices) {
      info.index = VECTOR_ELT(indices, i);
    } else {
      ++(*info.p_index);
    }

    // `vec_slice_fallback()` will also `vec_restore()` for us
    elt = PROTECT(vec_slice_fallback(x, info.index));

    SET_VECTOR_ELT(info.out, i, elt);
    UNPROTECT(1);
  }

  return info.out;
}

// -----------------------------------------------------------------------------

// From type.c
SEXP vctrs_type_common_impl(SEXP dots, SEXP ptype);

// From slice-assign.c
SEXP vec_assign_impl(SEXP proxy, SEXP index, SEXP value, bool clone);

static SEXP vec_unchop(SEXP x, SEXP indices, SEXP ptype, const struct name_repair_opts* name_repair);

// [[ register() ]]
SEXP vctrs_unchop(SEXP x, SEXP indices, SEXP ptype, SEXP name_repair) {
  struct name_repair_opts name_repair_opts = new_name_repair_opts(name_repair, false);
  PROTECT_NAME_REPAIR_OPTS(&name_repair_opts);

  SEXP out = vec_unchop(x, indices, ptype, &name_repair_opts);

  UNPROTECT(1);
  return out;
}

static SEXP vec_unchop_indices(SEXP out,
                               SEXP* p_out_names,
                               PROTECT_INDEX out_pi,
                               SEXP x,
                               R_len_t x_size,
                               bool is_shaped,
                               bool has_inner_names,
                               SEXP indices);

static SEXP vec_unchop_sequentially(SEXP out,
                                    SEXP* p_out_names,
                                    PROTECT_INDEX out_pi,
                                    SEXP x,
                                    R_len_t x_size,
                                    bool is_shaped,
                                    bool has_inner_names);

static SEXP vec_unchop(SEXP x, SEXP indices, SEXP ptype, const struct name_repair_opts* name_repair) {
  if (TYPEOF(x) != VECSXP) {
    Rf_errorcall(R_NilValue, "`x` must be a list");
  }

  R_len_t x_size = vec_size(x);

  R_len_t out_size = 0;
  for (R_len_t i = 0; i < x_size; ++i) {
    out_size += vec_size(VECTOR_ELT(x, i));
  }

  indices = PROTECT(vec_as_indices(indices, out_size, R_NilValue));
  bool null_indices = (indices == R_NilValue);

  if (!null_indices && x_size != vec_size(indices)) {
    Rf_errorcall(R_NilValue, "`x` and `indices` must be lists of the same size");
  }

  ptype = PROTECT(vctrs_type_common_impl(x, ptype));
  x = PROTECT(vec_cast_common(x, ptype));

  PROTECT_INDEX out_pi;
  SEXP out = vec_init(ptype, out_size);
  PROTECT_WITH_INDEX(out, &out_pi);
  out = vec_proxy(out);
  REPROTECT(out, out_pi);

  bool is_shaped = has_dim(ptype);
  bool has_inner_names = !is_data_frame(ptype) && list_has_inner_names(x, x_size);

  SEXP out_names = vctrs_shared_empty_chr;
  if (has_inner_names) {
    out_names = Rf_allocVector(STRSXP, out_size);
  }
  PROTECT(out_names);

  if (null_indices) {
    out = vec_unchop_sequentially(out, &out_names, out_pi, x, x_size, is_shaped, has_inner_names);
  } else {
    out = vec_unchop_indices(out, &out_names, out_pi, x, x_size, is_shaped, has_inner_names, indices);
  }
  REPROTECT(out, out_pi);

  out = vec_restore(out, ptype, r_int(out_size));
  REPROTECT(out, out_pi);

  if (has_inner_names) {
    out_names = PROTECT(vec_as_names(out_names, name_repair));
    out = vec_set_names(out, out_names);
    REPROTECT(out, out_pi);
    UNPROTECT(1);
  }

  UNPROTECT(5);
  return out;
}

static SEXP vec_unchop_indices(SEXP out,
                               SEXP* p_out_names,
                               PROTECT_INDEX out_pi,
                               SEXP x,
                               R_len_t x_size,
                               bool is_shaped,
                               bool has_inner_names,
                               SEXP indices) {
  // Modified in place
  SEXP out_names = *p_out_names;

  for (R_len_t i = 0; i < x_size; ++i) {
    SEXP elt = VECTOR_ELT(x, i);
    SEXP index = VECTOR_ELT(indices, i);

    if (is_shaped) {
      out = vec_assign(out, index, elt);
      REPROTECT(out, out_pi);
    } else {
      vec_assign_impl(out, index, elt, false);
    }

    if (has_inner_names) {
      SEXP elt_names = PROTECT(vec_names(elt));
      if (elt_names != R_NilValue) {
        vec_assign_impl(out_names, index, elt_names, false);
      }
      UNPROTECT(1);
    }
  }

  return out;
}


static SEXP vec_unchop_sequentially(SEXP out,
                                    SEXP* p_out_names,
                                    PROTECT_INDEX out_pi,
                                    SEXP x,
                                    R_len_t x_size,
                                    bool is_shaped,
                                    bool has_inner_names) {
  // Modified in place
  SEXP out_names = *p_out_names;

  // Compact sequences use 0-based counters
  R_len_t counter = 0;

  SEXP index = PROTECT(compact_seq(0, 0, true));
  int* p_index = INTEGER(index);

  for (R_len_t i = 0; i < x_size; ++i) {
    SEXP elt = VECTOR_ELT(x, i);
    R_len_t elt_size = vec_size(elt);

    init_compact_seq(p_index, counter, elt_size, true);

    if (is_shaped) {
      SEXP index2 = PROTECT(compact_materialize(index));
      out = vec_assign(out, index2, elt);
      REPROTECT(out, out_pi);
      UNPROTECT(1);
    } else {
      vec_assign_impl(out, index, elt, false);
    }

    if (has_inner_names) {
      SEXP elt_names = PROTECT(vec_names(elt));
      if (elt_names != R_NilValue) {
        vec_assign_impl(out_names, index, elt_names, false);
      }
      UNPROTECT(1);
    }

    counter += elt_size;
  }

  UNPROTECT(1);
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

  SEXP index;
  indices = PROTECT(r_maybe_duplicate(indices));

  R_len_t size = vec_size(indices);

  for (int i = 0; i < size; ++i) {
    index = VECTOR_ELT(indices, i);
    SET_VECTOR_ELT(indices, i, vec_as_location(index, n, names));
  }

  UNPROTECT(1);
  return indices;
}
