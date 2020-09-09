#ifndef VCTRS_TRANSLATE_H
#define VCTRS_TRANSLATE_H

#include "vctrs.h"

// -----------------------------------------------------------------------------
// Proxy vector translation

SEXP proxy_normalize_encoding(SEXP proxy);

// -----------------------------------------------------------------------------
// Low-level string translation

#define MASK_ASCII 8
#define MASK_UTF8 64

// The first 128 values are ASCII, and are the same regardless of the encoding.
// Otherwise we enforce UTF-8.
static inline bool string_is_ascii_or_utf8(SEXP x) {
  const int levels = LEVELS(x);
  return (levels & MASK_ASCII) || (levels & MASK_UTF8);
}

#undef MASK_ASCII
#undef MASK_UTF8

static inline SEXP string_normalize(SEXP x) {
  return Rf_mkCharCE(Rf_translateCharUTF8(x), CE_UTF8);
}

static inline bool string_is_normalized(SEXP x) {
  return string_is_ascii_or_utf8(x) || (x == NA_STRING);
}

// -----------------------------------------------------------------------------
#endif
