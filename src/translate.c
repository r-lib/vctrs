#include "translate.h"

// -----------------------------------------------------------------------------

static r_ssize chr_find_normalize_start(SEXP x, r_ssize size);
static r_ssize list_find_normalize_start(SEXP x, r_ssize size);

static SEXP chr_normalize_encoding(SEXP x, r_ssize size, r_ssize start);
static SEXP list_normalize_encoding(SEXP x, r_ssize size, r_ssize start);

/*
 * Recursively normalize encodings of character vectors.
 *
 * A CHARSXP is considered normalized if:
 * - It is the NA_STRING
 * - It is ASCII, with any encoding (UTF-8, Latin-1, native)
 * - It is UTF-8
 *
 * ASCII strings are the same regardless of the underlying encoding. This
 * allows us to avoid a large amount of overhead with the most common case
 * of having a native/unknown encoding. As long as the string is ASCII, we
 * won't ever need to translate it, even if we don't know that it is UTF-8.
 *
 * This converts vectors that are completely Latin-1 (but also not just ASCII)
 * to UTF-8. In theory we could leave these as Latin-1, and comparing within
 * a single vector would be fine, since the encoding would be consistent.
 * However, this makes comparing between vectors difficult because we then
 * have to normalize the vectors relative to each other's encodings.
 * Consistently converting to UTF-8 avoids this issue altogether.
 *
 * Vectors with "bytes" encodings are not supported, as they cannot be
 * converted to UTF-8 by `Rf_translateCharUTF8()`.
 *
 * [[ include("vctrs.h") ]]
 */
SEXP proxy_normalize_encoding(SEXP proxy) {
  switch (TYPEOF(proxy)) {
  case STRSXP: {
    r_ssize size = r_length(proxy);
    r_ssize start = chr_find_normalize_start(proxy, size);

    if (size == start) {
      return proxy;
    } else {
      return chr_normalize_encoding(proxy, size, start);
    }
  }
  case VECSXP: {
    r_ssize size = r_length(proxy);
    r_ssize start = list_find_normalize_start(proxy, size);

    if (size == start) {
      return proxy;
    } else {
      return list_normalize_encoding(proxy, size, start);
    }
  }
  default: {
    return proxy;
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
SEXP vctrs_proxy_normalize_encoding(SEXP proxy) {
  return proxy_normalize_encoding(proxy);
}
