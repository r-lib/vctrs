#include "if-else.h"
#include "vctrs.h"

#include "decl/if-else-decl.h"

r_obj* ffi_vec_if_else(
  r_obj* ffi_condition,
  r_obj* ffi_true,
  r_obj* ffi_false,
  r_obj* ffi_missing,
  r_obj* ffi_ptype,
  r_obj* ffi_frame
) {
  struct r_lazy condition_arg_lazy = { .x = syms.condition_arg, .env = ffi_frame };
  struct vctrs_arg condition_arg = new_lazy_arg(&condition_arg_lazy);

  struct r_lazy true_arg_lazy = { .x = syms.true_arg, .env = ffi_frame };
  struct vctrs_arg true_arg = new_lazy_arg(&true_arg_lazy);

  struct r_lazy false_arg_lazy = { .x = syms.false_arg, .env = ffi_frame };
  struct vctrs_arg false_arg = new_lazy_arg(&false_arg_lazy);

  struct r_lazy missing_arg_lazy = { .x = syms.missing_arg, .env = ffi_frame };
  struct vctrs_arg missing_arg = new_lazy_arg(&missing_arg_lazy);

  struct r_lazy error_call = { .x = r_syms.error_call, .env = ffi_frame };

  return vec_if_else(
    ffi_condition,
    ffi_true,
    ffi_false,
    ffi_missing,
    ffi_ptype,
    &condition_arg,
    &true_arg,
    &false_arg,
    &missing_arg,
    error_call
  );
}

r_obj* vec_if_else(
  r_obj* condition,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  r_obj* ptype,
  struct vctrs_arg* p_condition_arg,
  struct vctrs_arg* p_true_arg,
  struct vctrs_arg* p_false_arg,
  struct vctrs_arg* p_missing_arg,
  struct r_lazy error_call
) {
  obj_check_vector(condition, VCTRS_ALLOW_NULL_no, p_condition_arg, error_call);
  check_condition_index(condition, p_condition_arg, error_call);

  const r_ssize size = r_length(condition);

  const bool has_missing = missing != r_null;

  obj_check_vector(true_, VCTRS_ALLOW_NULL_no, p_true_arg, error_call);
  obj_check_vector(false_, VCTRS_ALLOW_NULL_no, p_false_arg, error_call);
  if (has_missing) {
    obj_check_vector(missing, VCTRS_ALLOW_NULL_no, p_missing_arg, error_call);
  }

  ptype = KEEP(ptype_finalize(
    ptype,
    true_,
    false_,
    missing,
    has_missing,
    p_true_arg,
    p_false_arg,
    p_missing_arg,
    error_call
  ));

  r_obj* out;

  if (ptype_is_atomic(ptype)) {
    out = atomic_if_else(
      condition,
      true_,
      false_,
      missing,
      ptype,
      size,
      p_true_arg,
      p_false_arg,
      p_missing_arg,
      error_call,
      has_missing
    );
  } else {
    out = generic_if_else(
      condition,
      true_,
      false_,
      missing,
      ptype,
      size,
      p_true_arg,
      p_false_arg,
      p_missing_arg,
      error_call
    );
  }

  FREE(1);
  return out;
}

static
r_obj* generic_if_else(
  r_obj* condition,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  r_obj* ptype,
  r_ssize size,
  struct vctrs_arg* p_true_arg,
  struct vctrs_arg* p_false_arg,
  struct vctrs_arg* p_missing_arg,
  struct r_lazy error_call
) {
  r_obj* conditions = KEEP(r_alloc_list(2));
  r_list_poke(conditions, 0, condition);
  // TODO: This is another place where we could use a compact-condition
  // if `list_combine()` could take them as `indices`. Would be 3x less
  // memory, which probably does matter quite a bit for simple cases.
  r_list_poke(conditions, 1, r_lgl_invert(condition));

  r_obj* values = KEEP(r_alloc_list(2));
  r_list_poke(values, 0, true_);
  r_list_poke(values, 1, false_);

  // Must materialize tags to set names on the list, for use in error messages
  r_obj* true_arg = KEEP(vctrs_arg(p_true_arg));
  r_obj* false_arg = KEEP(vctrs_arg(p_false_arg));

  r_obj* names = r_alloc_character(2);
  r_attrib_poke_names(values, names);
  r_chr_poke(names, 0, r_chr_get(true_arg, 0));
  r_chr_poke(names, 1, r_chr_get(false_arg, 0));

  // We want `missing` to be used
  const enum list_combine_unmatched unmatched = LIST_COMBINE_UNMATCHED_default;

  // This won't matter, there are no overlaps, so we choose the default "cheaper" option
  const enum list_combine_multiple multiple = LIST_COMBINE_MULTIPLE_first;

  // All of `true`, `false`, and `missing` are size 1 or size `size`
  const enum assignment_slice_value slice_values = ASSIGNMENT_SLICE_VALUE_yes;

  // We want error messages from outer names, but don't want them on the output
  r_obj* name_spec = name_spec_inner;

  // No name repair
  const struct name_repair_opts* p_name_repair_opts = p_no_repair_opts;

  // Don't use outer names if any errors occur
  struct vctrs_arg* p_values_arg = vec_args.empty;
  struct vctrs_arg* p_conditions_arg = vec_args.empty;

  r_obj* out = list_combine(
    values,
    conditions,
    size,
    missing,
    unmatched,
    multiple,
    slice_values,
    ptype,
    name_spec,
    p_name_repair_opts,
    p_values_arg,
    p_conditions_arg,
    p_missing_arg,
    error_call
  );

  FREE(4);
  return out;
}

static
r_obj* atomic_if_else(
  r_obj* condition,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  r_obj* ptype,
  r_ssize size,
  struct vctrs_arg* p_true_arg,
  struct vctrs_arg* p_false_arg,
  struct vctrs_arg* p_missing_arg,
  struct r_lazy error_call,
  bool has_missing
) {
  int n_prot = 0;

  // `true`, `false`, and `missing` must all recycle to the size of `condition`
  const r_ssize true_size = vec_check_recyclable(true_, size, VCTRS_ALLOW_NULL_no, p_true_arg, error_call);
  const r_ssize false_size = vec_check_recyclable(false_, size, VCTRS_ALLOW_NULL_no, p_false_arg, error_call);
  const r_ssize missing_size = has_missing ? vec_check_recyclable(missing, size, VCTRS_ALLOW_NULL_no, p_missing_arg, error_call) : 0;

  // Grab names before casting as casting may drop them
  // https://github.com/r-lib/vctrs/issues/623
  r_obj* true_names = r_names(true_);
  r_obj* false_names = r_names(false_);
  r_obj* missing_names = has_missing ? r_names(missing) : r_null;

  const bool has_true_names = true_names != r_null;
  const bool has_false_names = false_names != r_null;
  const bool has_missing_names = missing_names != r_null;

  true_ = KEEP_N(
    vec_cast(
      true_,
      ptype,
      p_true_arg,
      vec_args.empty,
      error_call
    ),
    &n_prot
  );

  false_ = KEEP_N(
    vec_cast(
      false_,
      ptype,
      p_false_arg,
      vec_args.empty,
      error_call
    ),
    &n_prot
  );

  if (has_missing) {
    missing = KEEP_N(
      vec_cast(
        missing,
        ptype,
        p_missing_arg,
        vec_args.empty,
        error_call
      ),
      &n_prot
    );
  }

  r_obj* out = atomic_if_else_switch(
    r_typeof(ptype),
    condition,
    true_,
    false_,
    missing,
    size,
    true_size,
    false_size,
    missing_size,
    true_names,
    false_names,
    missing_names,
    has_missing,
    has_true_names,
    has_false_names,
    has_missing_names
  );

  FREE(n_prot);
  return out;
}

// Extremely sensitive loop!
//
// In our testing, using a ternary style works best, and somehow
// works even better than using explicit if/else
#define ATOMIC_IF_ELSE_LOOP(                                                               \
  ELT_TRUE,                                                                                \
  ELT_FALSE,                                                                               \
  ELT_MISSING,                                                                             \
  CTYPE,                                                                                   \
  DEREF,                                                                                   \
  SET                                                                                      \
) do {                                                                                     \
  DEREF;                                                                                   \
  for (r_ssize i = 0; i < size; ++i) {                                                     \
    const int cnd = v_condition[i];                                                        \
    CTYPE elt = (cnd == 1) ? ELT_TRUE : (cnd == 0) ? ELT_FALSE : ELT_MISSING;              \
    SET;                                                                                   \
  }                                                                                        \
} while (0)

// `r_chr_poke()` takes the majority of the time in the named case. We don't
// want to slow down the main data loop with extra branching, so we only do this
// if have to and just loop through a second time.
#define ATOMIC_IF_ELSE_NAMES_LOOP(                                                         \
  ELT_TRUE,                                                                                \
  ELT_FALSE,                                                                               \
  ELT_MISSING                                                                              \
) do {                                                                                     \
  for (r_ssize i = 0; i < size; ++i) {                                                     \
    const int cnd = v_condition[i];                                                        \
    r_obj* elt = (cnd == 1) ? ELT_TRUE : (cnd == 0) ? ELT_FALSE : ELT_MISSING;             \
    r_chr_poke(names, i, elt);                                                             \
  }                                                                                        \
} while (0)

// Core routing logic for atomic if-else
//
// Yes, this is insane. Yes, it is worth it to maximally avoid branching.
#define ATOMIC_IF_ELSE(                                                                     \
  CTYPE,                                                                                    \
  DEREF,                                                                                    \
  SET,                                                                                      \
  CONST_DEREF,                                                                              \
  ALLOC,                                                                                    \
  MISSING                                                                                   \
) do {                                                                                      \
  const int* v_condition = r_lgl_cbegin(condition);                                         \
                                                                                            \
  if (!has_missing) {                                                                       \
    missing = MISSING;                                                                      \
    missing_size = 1;                                                                       \
    has_missing_names = false;                                                              \
  }                                                                                         \
                                                                                            \
  const bool true_recycles = true_size == 1;                                                \
  const bool false_recycles = false_size == 1;                                              \
  const bool missing_recycles = missing_size == 1;                                          \
                                                                                            \
  CTYPE const* v_true = CONST_DEREF(true_);                                                 \
  CTYPE const* v_false = CONST_DEREF(false_);                                               \
  CTYPE const* v_missing = CONST_DEREF(missing);                                            \
                                                                                            \
  const bool has_names = has_true_names || has_false_names || has_missing_names;            \
                                                                                            \
  true_names = has_true_names ? true_names : r_chrs.empty_string;                           \
  false_names = has_false_names ? false_names : r_chrs.empty_string;                        \
  missing_names = has_missing_names ? missing_names : r_chrs.empty_string;                  \
                                                                                            \
  const bool true_names_recycles = has_true_names ? true_recycles : true;                   \
  const bool false_names_recycles = has_false_names ? false_recycles : true;                \
  const bool missing_names_recycles = has_missing_names ? missing_recycles : true;          \
                                                                                            \
  r_obj* const* v_true_names = r_chr_cbegin(true_names);                                    \
  r_obj* const* v_false_names = r_chr_cbegin(false_names);                                  \
  r_obj* const* v_missing_names = r_chr_cbegin(missing_names);                              \
                                                                                            \
  r_obj* out = KEEP(ALLOC(size));                                                           \
                                                                                            \
  if (true_recycles) {                                                                      \
    if (false_recycles) {                                                                   \
      if (missing_recycles) {                                                               \
        ATOMIC_IF_ELSE_LOOP(v_true[0], v_false[0], v_missing[0], CTYPE, DEREF, SET);        \
      } else {                                                                              \
        ATOMIC_IF_ELSE_LOOP(v_true[0], v_false[0], v_missing[i], CTYPE, DEREF, SET);        \
      }                                                                                     \
    } else {                                                                                \
      if (missing_recycles) {                                                               \
        ATOMIC_IF_ELSE_LOOP(v_true[0], v_false[i], v_missing[0], CTYPE, DEREF, SET);        \
      } else {                                                                              \
        ATOMIC_IF_ELSE_LOOP(v_true[0], v_false[i], v_missing[i], CTYPE, DEREF, SET);        \
      }                                                                                     \
    }                                                                                       \
  } else {                                                                                  \
    if (false_recycles) {                                                                   \
      if (missing_recycles) {                                                               \
        ATOMIC_IF_ELSE_LOOP(v_true[i], v_false[0], v_missing[0], CTYPE, DEREF, SET);        \
      } else {                                                                              \
        ATOMIC_IF_ELSE_LOOP(v_true[i], v_false[0], v_missing[i], CTYPE, DEREF, SET);        \
      }                                                                                     \
    } else {                                                                                \
      if (missing_recycles) {                                                               \
        ATOMIC_IF_ELSE_LOOP(v_true[i], v_false[i], v_missing[0], CTYPE, DEREF, SET);        \
      } else {                                                                              \
        ATOMIC_IF_ELSE_LOOP(v_true[i], v_false[i], v_missing[i], CTYPE, DEREF, SET);        \
      }                                                                                     \
    }                                                                                       \
  }                                                                                         \
                                                                                            \
  if (has_names) {                                                                          \
    r_obj* names = r_alloc_character(size);                                                 \
    r_attrib_poke_names(out, names);                                                        \
                                                                                            \
    if (true_names_recycles) {                                                              \
      if (false_names_recycles) {                                                           \
        if (missing_names_recycles) {                                                       \
          ATOMIC_IF_ELSE_NAMES_LOOP(v_true_names[0], v_false_names[0], v_missing_names[0]); \
        } else {                                                                            \
          ATOMIC_IF_ELSE_NAMES_LOOP(v_true_names[0], v_false_names[0], v_missing_names[i]); \
        }                                                                                   \
      } else {                                                                              \
        if (missing_names_recycles) {                                                       \
          ATOMIC_IF_ELSE_NAMES_LOOP(v_true_names[0], v_false_names[i], v_missing_names[0]); \
        } else {                                                                            \
          ATOMIC_IF_ELSE_NAMES_LOOP(v_true_names[0], v_false_names[i], v_missing_names[i]); \
        }                                                                                   \
      }                                                                                     \
    } else {                                                                                \
      if (false_names_recycles) {                                                           \
        if (missing_names_recycles) {                                                       \
          ATOMIC_IF_ELSE_NAMES_LOOP(v_true_names[i], v_false_names[0], v_missing_names[0]); \
        } else {                                                                            \
          ATOMIC_IF_ELSE_NAMES_LOOP(v_true_names[i], v_false_names[0], v_missing_names[i]); \
        }                                                                                   \
      } else {                                                                              \
        if (missing_names_recycles) {                                                       \
          ATOMIC_IF_ELSE_NAMES_LOOP(v_true_names[i], v_false_names[i], v_missing_names[0]); \
        } else {                                                                            \
          ATOMIC_IF_ELSE_NAMES_LOOP(v_true_names[i], v_false_names[i], v_missing_names[i]); \
        }                                                                                   \
      }                                                                                     \
    }                                                                                       \
  }                                                                                         \
                                                                                            \
  FREE(1);                                                                                  \
  return out;                                                                               \
} while (0)

static
r_obj* atomic_if_else_switch(
  enum r_type type,
  r_obj* condition,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  r_ssize size,
  r_ssize true_size,
  r_ssize false_size,
  r_ssize missing_size,
  r_obj* true_names,
  r_obj* false_names,
  r_obj* missing_names,
  bool has_missing,
  bool has_true_names,
  bool has_false_names,
  bool has_missing_names
) {
  switch (type) {
  case R_TYPE_logical: ATOMIC_IF_ELSE(
    int,
    int* v_out = r_lgl_begin(out),
    v_out[i] = elt,
    r_lgl_cbegin,
    r_alloc_logical,
    vctrs_shared_missing_lgl
  );
  case R_TYPE_integer: ATOMIC_IF_ELSE(
    int,
    int* v_out = r_int_begin(out),
    v_out[i] = elt,
    r_int_cbegin,
    r_alloc_integer,
    vctrs_shared_missing_int
  );
  case R_TYPE_double: ATOMIC_IF_ELSE(
    double,
    double* v_out = r_dbl_begin(out),
    v_out[i] = elt,
    r_dbl_cbegin,
    r_alloc_double,
    vctrs_shared_missing_dbl
  );
  case R_TYPE_complex: ATOMIC_IF_ELSE(
    r_complex,
    r_complex* v_out = r_cpl_begin(out),
    v_out[i] = elt,
    r_cpl_cbegin,
    r_alloc_complex,
    vctrs_shared_missing_cpl
  );
  case R_TYPE_raw: ATOMIC_IF_ELSE(
    Rbyte,
    Rbyte* v_out = r_raw_begin(out),
    v_out[i] = elt,
    r_raw_cbegin,
    r_alloc_raw,
    vctrs_shared_missing_raw
  );
  case R_TYPE_character: ATOMIC_IF_ELSE(
    r_obj*,
    NULL,
    r_chr_poke(out, i, elt),
    r_chr_cbegin,
    r_alloc_character,
    vctrs_shared_missing_chr
  );
  case R_TYPE_list: ATOMIC_IF_ELSE(
    r_obj*,
    NULL,
    r_list_poke(out, i, elt),
    r_list_cbegin,
    r_alloc_list,
    vctrs_shared_missing_list
  );
  default: r_stop_unreachable();
  }
}

#undef ATOMIC_IF_ELSE_LOOP
#undef ATOMIC_IF_ELSE_NAMES_LOOP
#undef ATOMIC_IF_ELSE

// Determine if `ptype` can use the fast path or not
//
// We are extremely strict here, only allowing the fast
// path for the absolute simplest cases of:
//
// - Atomic vectors, i.e. no classed objects
// - No dim, i.e. no matrix or array
//
// Notably having extraneous attributes on an atomic vector is allowed, but they
// are dropped from the output, which is in line with our general belief that
// extraneous attributes are not part of the ptype.
//
// Names are always retained.
static
bool ptype_is_atomic(r_obj* ptype) {
  if (r_is_object(ptype)) {
    return false;
  }
  if (has_dim(ptype)) {
    return false;
  }
  return true;
}

static
r_obj* ptype_finalize(
  r_obj* ptype,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  bool has_missing,
  struct vctrs_arg* p_true_arg,
  struct vctrs_arg* p_false_arg,
  struct vctrs_arg* p_missing_arg,
  struct r_lazy error_call
) {
  if (ptype != r_null) {
    // Performs scalar checks and whatnot
    return vec_ptype_final(ptype, vec_args.ptype, error_call);
  }

  int n_prot = 0;

  // Common type of `true` and `false`
  int left;
  ptype = KEEP_N(
    vec_ptype2(
      true_,
      false_,
      p_true_arg,
      p_false_arg,
      error_call,
      S3_FALLBACK_false,
      &left
    ),
    &n_prot
  );

  // Mix in `missing` if needed
  if (has_missing) {
    // Same logic as `vec_ptype_common()`
    // 1 = `x` won
    // 0 = `y` won
    // -1 = same type, stick with `x`
    struct vctrs_arg* p_ptype_arg;
    switch (left) {
    case 1: p_ptype_arg = p_true_arg; break;
    case 0: p_ptype_arg = p_false_arg; break;
    case -1: p_ptype_arg = p_true_arg; break;
    default: r_stop_unreachable();
    }

    ptype = KEEP_N(
      vec_ptype2(
        ptype,
        missing,
        p_ptype_arg,
        p_missing_arg,
        error_call,
        S3_FALLBACK_false,
        &left
      ),
      &n_prot
    );
  }

  // Finalize on the way out
  ptype = vec_ptype_finalise(ptype);

  FREE(n_prot);
  return ptype;
}
