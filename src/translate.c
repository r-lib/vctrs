#include "vctrs.h"

// -----------------------------------------------------------------------------

static bool chr_all_normalized(SEXP x, r_ssize size);
static bool list_all_normalized(SEXP x, r_ssize size);

static SEXP chr_normalize_encoding(SEXP x, r_ssize size);
static SEXP list_normalize_encoding(SEXP x, r_ssize size);

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

    if (chr_all_normalized(proxy, size)) {
      return proxy;
    } else {
      return chr_normalize_encoding(proxy, size);
    }
  }
  case VECSXP: {
    r_ssize size = r_length(proxy);

    if (list_all_normalized(proxy, size)) {
      return proxy;
    } else {
      return list_normalize_encoding(proxy, size);
    }
  }
  default: {
    return proxy;
  }
  }
}

// -----------------------------------------------------------------------------

static inline SEXP char_normalize(SEXP x);
static inline bool char_is_normalized(SEXP x);

static SEXP chr_normalize_encoding(SEXP x, r_ssize size) {
  x = PROTECT(r_clone_referenced(x));
  const SEXP* p_x = STRING_PTR_RO(x);

  const void* vmax = vmaxget();

  for (r_ssize i = 0; i < size; ++i) {
    const SEXP elt = p_x[i];

    if (char_is_normalized(elt)) {
      continue;
    }

    SET_STRING_ELT(x, i, char_normalize(elt));
  }

  vmaxset(vmax);
  UNPROTECT(1);
  return x;
}

static bool chr_all_normalized(SEXP x, r_ssize size) {
  const SEXP* p_x = STRING_PTR_RO(x);

  for (r_ssize i = 0; i < size; ++i) {
    const SEXP elt = p_x[i];

    if (char_is_normalized(elt)) {
      continue;
    }

    return false;
  }

  return true;
}

// -----------------------------------------------------------------------------

static SEXP list_normalize_encoding(SEXP x, r_ssize size) {
  x = PROTECT(r_clone_referenced(x));
  const SEXP* p_x = VECTOR_PTR_RO(x);

  for (r_ssize i = 0; i < size; ++i) {
    const SEXP elt = p_x[i];

    switch (TYPEOF(elt)) {
    case STRSXP: {
      r_ssize size = r_length(elt);

      if (chr_all_normalized(elt, size)) {
        break;
      }

      SET_VECTOR_ELT(x, i, chr_normalize_encoding(elt, size));
      break;
    }
    case VECSXP: {
      r_ssize size = r_length(elt);

      if (list_all_normalized(elt, size)) {
        break;
      }

      SET_VECTOR_ELT(x, i, list_normalize_encoding(elt, size));
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

static bool list_all_normalized(SEXP x, r_ssize size) {
  const SEXP* p_x = VECTOR_PTR_RO(x);

  for (r_ssize i = 0; i < size; ++i) {
    const SEXP elt = p_x[i];

    if (elt_all_normalized(elt)) {
      continue;
    }

    return false;
  }

  return true;
}

static inline bool elt_all_normalized(SEXP x) {
  switch (TYPEOF(x)) {
  case STRSXP: return chr_all_normalized(x, r_length(x));
  case VECSXP: return list_all_normalized(x, r_length(x));
  default: return true;
  }
}

// -----------------------------------------------------------------------------

static inline bool char_is_ascii_or_utf8(SEXP x);


static inline SEXP char_normalize(SEXP x) {
  return Rf_mkCharCE(Rf_translateCharUTF8(x), CE_UTF8);
}
static inline bool char_is_normalized(SEXP x) {
  return char_is_ascii_or_utf8(x) || (x == NA_STRING);
}


static inline bool levels_is_ascii(int levels);
static inline bool levels_is_utf8(int levels);

// The first 128 values are ASCII, and are the same regardless of the encoding.
// Otherwise we enforce UTF-8.
static inline bool char_is_ascii_or_utf8(SEXP x) {
  const int levels = LEVELS(x);
  return levels_is_ascii(levels) || levels_is_utf8(levels);
}

static inline bool levels_is_ascii(int levels) {
  return levels & 8;
}
static inline bool levels_is_utf8(int levels) {
  return levels & 64;
}

// -----------------------------------------------------------------------------

// For testing
// [[ register() ]]
SEXP vctrs_normalize_encoding(SEXP x) {
  SEXP proxy = PROTECT(vec_proxy_equal(x));
  SEXP out = proxy_normalize_encoding(proxy);
  UNPROTECT(1);
  return out;
}
