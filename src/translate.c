#include "translate.h"
#include "vctrs.h"
#include "utils.h"

// For testing
// [[ register() ]]
SEXP vctrs_normalize_encoding(SEXP x) {
  return vec_normalize_encoding(x);
}

static inline SEXP obj_normalize_encoding(SEXP x);

/*
 * Recursively normalize encodings of character vectors.
 *
 * A CHARSXP is considered normalized if:
 * - It is the NA_STRING
 * - It is ASCII, which means the encoding will be unmarked
 * - It is marked as UTF-8
 *
 * Attributes are translated as well.
 *
 * ASCII strings will never get marked with an encoding when they go
 * through `Rf_mkCharLenCE()`, but they will get marked as ASCII. Since
 * UTF-8 is fully compatible with ASCII and ASCII is by far the most common
 * case, we let ASCII strings through without translating them.
 *
 * This converts vectors that are completely marked as Latin-1 to UTF-8. In
 * theory we could leave these as Latin-1, and comparing within
 * a single vector would be fine, since the encoding would be consistent.
 * However, this makes comparing between vectors difficult because we then
 * have to normalize the vectors relative to each other's encodings.
 * Consistently converting to UTF-8 avoids this issue altogether.
 *
 * Vectors with "bytes" encodings are not supported, as they cannot be
 * converted to UTF-8 by `Rf_translateCharUTF8()`.
 *
 * [[ include("translate.h") ]]
 */
SEXP vec_normalize_encoding(SEXP x) {
  return obj_normalize_encoding(x);
}

// -----------------------------------------------------------------------------

static SEXP chr_normalize_encoding(SEXP x);
static SEXP list_normalize_encoding(SEXP x);
static SEXP obj_attrib_normalize_encoding(SEXP x);

static inline SEXP obj_normalize_encoding(SEXP x) {
  x = PROTECT(obj_attrib_normalize_encoding(x));

  switch (TYPEOF(x)) {
  case STRSXP: x = chr_normalize_encoding(x); break;
  case VECSXP: x = list_normalize_encoding(x); break;
  default: break;
  }

  UNPROTECT(1);
  return x;
}

// -----------------------------------------------------------------------------

static inline r_ssize chr_find_normalize_start(SEXP x, r_ssize size);

static SEXP chr_normalize_encoding(SEXP x) {
  r_ssize size = r_length(x);
  r_ssize start = chr_find_normalize_start(x, size);

  if (size == start) {
    return x;
  }

  x = PROTECT(r_clone_referenced(x));
  const SEXP* p_x = STRING_PTR_RO(x);

  const void* vmax = vmaxget();

  for (r_ssize i = start; i < size; ++i) {
    const SEXP elt = p_x[i];

    if (string_is_normalized(elt)) {
      continue;
    }

    SET_STRING_ELT(x, i, string_normalize(elt));
  }

  vmaxset(vmax);
  UNPROTECT(1);
  return x;
}

static inline r_ssize chr_find_normalize_start(SEXP x, r_ssize size) {
  const SEXP* p_x = STRING_PTR_RO(x);

  for (r_ssize i = 0; i < size; ++i) {
    const SEXP elt = p_x[i];

    if (string_is_normalized(elt)) {
      continue;
    }

    return i;
  }

  return size;
}

// -----------------------------------------------------------------------------

static SEXP list_normalize_encoding(SEXP x) {
  int nprot = 0;

  r_ssize size = r_length(x);
  const SEXP* p_x = VECTOR_PTR_RO(x);

  for (r_ssize i = 0; i < size; ++i) {
    SEXP elt_old = p_x[i];

    SEXP elt_new = obj_normalize_encoding(elt_old);
    if (elt_old == elt_new) {
      continue;
    }
    PROTECT(elt_new);

    if (MAYBE_REFERENCED(x)) {
      x = PROTECT(r_clone_referenced(x));
      ++nprot;
      p_x = VECTOR_PTR_RO(x);
    }

    SET_VECTOR_ELT(x, i, elt_new);
    UNPROTECT(1);
  }

  UNPROTECT(nprot);
  return x;
}

// -----------------------------------------------------------------------------

static SEXP attrib_normalize_encoding(SEXP x);

static SEXP obj_attrib_normalize_encoding(SEXP x) {
  SEXP attrib_old = r_attrib(x);

  if (attrib_old == r_null) {
    return x;
  }

  SEXP attrib_new = attrib_normalize_encoding(attrib_old);
  if (attrib_new == attrib_old) {
    return x;
  }
  PROTECT(attrib_new);

  x = PROTECT(r_clone_referenced(x));
  r_poke_attrib(x, attrib_new);

  UNPROTECT(2);
  return x;
}

static SEXP attrib_normalize_encoding(SEXP x) {
  int nprot = 0;
  r_ssize loc = 0;

  for (SEXP node = x; node != r_null; node = r_node_cdr(node), ++loc) {
    SEXP elt_old = r_node_car(node);

    SEXP elt_new = obj_normalize_encoding(elt_old);
    if (elt_old == elt_new) {
      continue;
    }
    PROTECT(elt_new);

    if (MAYBE_REFERENCED(x)) {
      x = PROTECT(r_clone_referenced(x));
      ++nprot;
      node = x;

      // Restore original positioning post-clone
      for (r_ssize i = 0; i < loc; ++i) {
        node = r_node_cdr(node);
      }
    }

    r_node_poke_car(node, elt_new);
    UNPROTECT(1);
  }

  UNPROTECT(nprot);
  return x;
}
