#include "translate.h"

// -----------------------------------------------------------------------------

static r_ssize chr_find_normalize_start(SEXP x, r_ssize size);
static r_ssize list_find_normalize_start(SEXP x, r_ssize size);

static SEXP chr_normalize_encoding(SEXP x, r_ssize size, r_ssize start);
static SEXP list_normalize_encoding(SEXP x, r_ssize size, r_ssize start);

/*
 * Recursively normalize encodings of character vectors.
 *
 * This can be called on any vector, but is generally called on a proxy vector.
 *
 * Note that attributes are currently not translated. This means that it is
 * often important to call this function on the proxy, rather than the original
 * vector. For example, a list-rcrd with a vectorized character attribute must
 * be proxied to have the attribute promoted to a data frame column first before
 * calling `vec_normalize_encoding()`.
 *
 * A CHARSXP is considered normalized if:
 * - It is the NA_STRING
 * - It is ASCII, which means the encoding will be unmarked
 * - It is marked as UTF-8
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
  switch (TYPEOF(x)) {
  case STRSXP: {
    r_ssize size = r_length(x);
    r_ssize start = chr_find_normalize_start(x, size);

    if (size == start) {
      return x;
    } else {
      return chr_normalize_encoding(x, size, start);
    }
  }
  case VECSXP: {
    r_ssize size = r_length(x);
    r_ssize start = list_find_normalize_start(x, size);

    if (size == start) {
      return x;
    } else {
      return list_normalize_encoding(x, size, start);
    }
  }
  default: {
    return x;
  }
  }
}

// -----------------------------------------------------------------------------

static SEXP chr_normalize_encoding(SEXP x, r_ssize size, r_ssize start) {
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

static r_ssize chr_find_normalize_start(SEXP x, r_ssize size) {
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

static SEXP list_normalize_encoding(SEXP x, r_ssize size, r_ssize start) {
  x = PROTECT(r_clone_referenced(x));
  const SEXP* p_x = VECTOR_PTR_RO(x);

  for (r_ssize i = start; i < size; ++i) {
    SEXP elt = p_x[i];

    switch (TYPEOF(elt)) {
    case STRSXP: {
      r_ssize elt_size = r_length(elt);
      r_ssize elt_start = chr_find_normalize_start(elt, elt_size);

      if (elt_size == elt_start) {
        break;
      }

      elt = chr_normalize_encoding(elt, elt_size, elt_start);
      SET_VECTOR_ELT(x, i, elt);
      break;
    }
    case VECSXP: {
      r_ssize elt_size = r_length(elt);
      r_ssize elt_start = list_find_normalize_start(elt, elt_size);

      if (elt_size == elt_start) {
        break;
      }

      elt = list_normalize_encoding(elt, elt_size, elt_start);
      SET_VECTOR_ELT(x, i, elt);
      break;
    }
    default:
      continue;
    }
  }

  UNPROTECT(1);
  return x;
}

static inline bool elt_all_normalized(SEXP x);

static r_ssize list_find_normalize_start(SEXP x, r_ssize size) {
  const SEXP* p_x = VECTOR_PTR_RO(x);

  for (r_ssize i = 0; i < size; ++i) {
    const SEXP elt = p_x[i];

    if (elt_all_normalized(elt)) {
      continue;
    }

    return i;
  }

  return size;
}

static inline bool elt_all_normalized(SEXP x) {
  switch (TYPEOF(x)) {
  case STRSXP: {
    r_ssize size = r_length(x);
    r_ssize start = chr_find_normalize_start(x, size);
    return size == start;
  }
  case VECSXP: {
    r_ssize size = r_length(x);
    r_ssize start = list_find_normalize_start(x, size);
    return size == start;
  }
  default: {
    return true;
  }
  }
}

// -----------------------------------------------------------------------------

// For testing
// [[ register() ]]
SEXP vctrs_normalize_encoding(SEXP x) {
  return vec_normalize_encoding(x);
}
