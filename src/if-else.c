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

  struct r_lazy call = { .x = r_syms.call, .env = ffi_frame };

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
    call
  );
}

static
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
  struct r_lazy call
) {
  obj_check_vector(condition, p_condition_arg, call);
  check_logical(condition, p_condition_arg, call);

  const bool has_missing = missing != r_null;

  obj_check_vector(true_, p_true_arg, call);
  obj_check_vector(false_, p_false_arg, call);
  if (has_missing) {
    obj_check_vector(missing, p_missing_arg, call);
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
    call
  ));

  r_obj* out;

  if (ptype_is_atomic(ptype)) {
    out = atomic_if_else(
      condition,
      true_,
      false_,
      missing,
      ptype,
      p_true_arg,
      p_false_arg,
      p_missing_arg,
      call,
      has_missing
    );
  } else {
    out = generic_if_else(
      condition,
      true_,
      false_,
      missing,
      ptype,
      p_true_arg,
      p_false_arg,
      p_missing_arg,
      call
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
  struct vctrs_arg* p_true_arg,
  struct vctrs_arg* p_false_arg,
  struct vctrs_arg* p_missing_arg,
  struct r_lazy call
) {
  const r_ssize size = r_length(condition);

  r_obj* cases = KEEP(r_alloc_list(2));
  r_list_poke(cases, 0, condition);
  r_list_poke(cases, 1, r_lgl_invert(condition));

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

  r_obj* out = vec_case_when(
    cases,
    values,
    missing,
    LIST_UNCHOP_UNMATCHED_default,
    ptype,
    new_optional_r_ssize(size),
    vec_args.empty,
    vec_args.empty,
    p_missing_arg,
    call
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
  struct vctrs_arg* p_true_arg,
  struct vctrs_arg* p_false_arg,
  struct vctrs_arg* p_missing_arg,
  struct r_lazy call,
  bool has_missing
) {
  int n_prot = 0;

  const r_ssize condition_size = r_length(condition);
  const r_ssize true_size = r_length(true_);
  const r_ssize false_size = r_length(false_);
  const r_ssize missing_size = r_length(missing);

  // `true`, `false`, and `missing` must all be the size of `condition`
  if (true_size != 1 && true_size != condition_size) {
    vec_check_recycle(true_, condition_size, p_true_arg, call);
  }
  if (false_size != 1 && false_size != condition_size) {
    vec_check_recycle(false_, condition_size, p_false_arg, call);
  }
  if (has_missing && missing_size != 1 && missing_size != condition_size) {
    vec_check_recycle(missing, condition_size, p_missing_arg, call);
  }

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
      call
    ),
    &n_prot
  );

  false_ = KEEP_N(
    vec_cast(
      false_,
      ptype,
      p_false_arg,
      vec_args.empty,
      call
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
        call
      ),
      &n_prot
    );
  }

  r_obj* out;

  switch (r_typeof(ptype)) {
    case R_TYPE_logical: {
      out = lgl_if_else(
        condition,
        true_,
        false_,
        missing,
        condition_size,
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
      break;
    }
    case R_TYPE_integer: {
      out = int_if_else(
        condition,
        true_,
        false_,
        missing,
        condition_size,
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
      break;
    }
    case R_TYPE_double: {
      out = dbl_if_else(
        condition,
        true_,
        false_,
        missing,
        condition_size,
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
      break;
    }
    case R_TYPE_complex: {
      out = cpl_if_else(
        condition,
        true_,
        false_,
        missing,
        condition_size,
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
      break;
    }
    case R_TYPE_character: {
      out = chr_if_else(
        condition,
        true_,
        false_,
        missing,
        condition_size,
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
      break;
    }
    case R_TYPE_list: {
      out = list_if_else(
        condition,
        true_,
        false_,
        missing,
        condition_size,
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
      break;
    }
    default: {
      r_stop_unreachable();
    }
  }

  FREE(n_prot);
  return out;
}

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

// `r_chr_poke()` takes the majority of the time in the named case
#define ATOMIC_IF_ELSE_NAMES_LOOP(                                                         \
  ELT_TRUE,                                                                                \
  ELT_FALSE,                                                                               \
  ELT_MISSING                                                                              \
) do {                                                                                     \
  r_obj* names = r_alloc_character(size);                                                  \
  r_attrib_poke_names(out, names);                                                         \
                                                                                           \
  for (r_ssize i = 0; i < size; ++i) {                                                     \
    const int cnd = v_condition[i];                                                        \
    r_chr_poke(names, i, (cnd == 1) ? ELT_TRUE : (cnd == 0) ? ELT_FALSE : ELT_MISSING);    \
  }                                                                                        \
} while (0)

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
  const bool true_name_recycles = has_true_names ? true_recycles : true;                    \
  const bool false_name_recycles = has_false_names ? false_recycles : true;                 \
  const bool missing_name_recycles = has_missing_names ? missing_recycles : true;           \
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
    if (true_name_recycles) {                                                               \
      if (false_name_recycles) {                                                            \
        if (missing_name_recycles) {                                                        \
          ATOMIC_IF_ELSE_NAMES_LOOP(v_true_names[0], v_false_names[0], v_missing_names[0]); \
        } else {                                                                            \
          ATOMIC_IF_ELSE_NAMES_LOOP(v_true_names[0], v_false_names[0], v_missing_names[i]); \
        }                                                                                   \
      } else {                                                                              \
        if (missing_name_recycles) {                                                        \
          ATOMIC_IF_ELSE_NAMES_LOOP(v_true_names[0], v_false_names[i], v_missing_names[0]); \
        } else {                                                                            \
          ATOMIC_IF_ELSE_NAMES_LOOP(v_true_names[0], v_false_names[i], v_missing_names[i]); \
        }                                                                                   \
      }                                                                                     \
    } else {                                                                                \
      if (false_name_recycles) {                                                            \
        if (missing_name_recycles) {                                                        \
          ATOMIC_IF_ELSE_NAMES_LOOP(v_true_names[i], v_false_names[0], v_missing_names[0]); \
        } else {                                                                            \
          ATOMIC_IF_ELSE_NAMES_LOOP(v_true_names[i], v_false_names[0], v_missing_names[i]); \
        }                                                                                   \
      } else {                                                                              \
        if (missing_name_recycles) {                                                        \
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
r_obj* lgl_if_else(
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
  ATOMIC_IF_ELSE(
    int,
    int* v_out = r_lgl_begin(out),
    v_out[i] = elt,
    r_lgl_cbegin,
    r_alloc_logical,
    lgl_missing
  );
}

static
r_obj* int_if_else(
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
  ATOMIC_IF_ELSE(
    int,
    int* v_out = r_int_begin(out),
    v_out[i] = elt,
    r_int_cbegin,
    r_alloc_integer,
    int_missing
  );
}

static
r_obj* dbl_if_else(
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
  ATOMIC_IF_ELSE(
    double,
    double* v_out = r_dbl_begin(out),
    v_out[i] = elt,
    r_dbl_cbegin,
    r_alloc_double,
    dbl_missing
  );
}

static
r_obj* cpl_if_else(
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
  ATOMIC_IF_ELSE(
    r_complex,
    r_complex* v_out = r_cpl_begin(out),
    v_out[i] = elt,
    r_cpl_cbegin,
    r_alloc_complex,
    cpl_missing
  );
}

static
r_obj* chr_if_else(
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
  ATOMIC_IF_ELSE(
    r_obj*,
    NULL,
    r_chr_poke(out, i, elt),
    r_chr_cbegin,
    r_alloc_character,
    chr_missing
  );
}

static
r_obj* list_if_else(
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
  ATOMIC_IF_ELSE(
    r_obj*,
    NULL,
    r_list_poke(out, i, elt),
    r_list_cbegin,
    r_alloc_list,
    list_missing
  );
}

#undef ATOMIC_IF_ELSE_LOOP
#undef ATOMIC_IF_ELSE_NAMES_LOOP
#undef ATOMIC_IF_ELSE

static
bool ptype_is_atomic(r_obj* ptype) {
  if (r_is_object(ptype)) {
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
  struct r_lazy call
) {
  if (ptype != r_null) {
    // Performs scalar checks and whatnot
    return vec_ptype(ptype, vec_args.ptype, call);
  }

  int n_prot = 0;

  // Common type of `true` and `false`
  int _;
  ptype = KEEP_N(
    vec_ptype2_params(
      true_,
      false_,
      p_true_arg,
      p_false_arg,
      call,
      &_
    ),
    &n_prot
  );

  // Mix in `missing` if needed
  if (has_missing) {
    ptype = KEEP_N(
      vec_ptype2_params(
        missing,
        ptype,
        p_missing_arg,
        vec_args.empty,
        call,
        &_
      ),
      &n_prot
    );
  }

  FREE(n_prot);
  return ptype;
}

static
void check_logical(r_obj* x, struct vctrs_arg* p_x_arg, struct r_lazy call) {
  if (r_typeof(x) != R_TYPE_logical) {
    r_abort_lazy_call(
      call,
      "%s must be a logical vector, not %s.",
      vec_arg_format(p_x_arg),
      r_obj_type_friendly(x)
    );
  }

  if (has_dim(x)) {
    r_abort_lazy_call(
      call,
      "%s can't be an array.",
      vec_arg_format(p_x_arg)
    );
  }
}

void vctrs_init_if_else(r_obj* ns) {
  lgl_missing = r_alloc_logical(1);
  r_preserve_global(lgl_missing);
  r_lgl_poke(lgl_missing, 0, r_globals.na_lgl);

  int_missing = r_alloc_integer(1);
  r_preserve_global(int_missing);
  r_int_poke(int_missing, 0, r_globals.na_int);

  dbl_missing = r_alloc_double(1);
  r_preserve_global(dbl_missing);
  r_dbl_poke(dbl_missing, 0, r_globals.na_dbl);

  cpl_missing = r_alloc_complex(1);
  r_preserve_global(cpl_missing);
  r_cpl_poke(cpl_missing, 0, r_globals.na_cpl);

  chr_missing = r_alloc_character(1);
  r_preserve_global(chr_missing);
  r_chr_poke(chr_missing, 0, r_globals.na_str);

  list_missing = r_alloc_list(1);
  r_preserve_global(list_missing);
  r_list_poke(list_missing, 0, r_null);
}
