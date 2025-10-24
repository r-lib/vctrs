#ifndef VCTRS_TRANSLATE_H
#define VCTRS_TRANSLATE_H

#include "vctrs-core.h"

SEXP vec_normalize_encoding(SEXP x);

// String encoding normalization
//
// In R 4.5.0 we got `Rf_charIsUTF8()`, but we cannot use it.
//
// It returns:
// - `true` if `IS_ASCII()`, i.e. has `SET_ASCII()` bit (also always marked `CE_NATIVE`)
// - `true` if `IS_UTF8()`, i.e. has `SET_UTF8()` bit (also always marked `CE_UTF8`)
// - `true` if `CE_NATIVE` (we call this "unmarked") but `utf8locale = true`
//
// The 3rd condition is problematic for us. For CHARSXP hashing purposes,
// the following are different CHARSXPs:
//
// - `°C` that is marked `CE_UTF8`, and has `SET_UTF8()` bit set
// - `°C` that is marked `CE_NATIVE`, but `utf8locale = true`
//
// Meaning `vec_match("°C", "°C")` would return `NA` with these.
//
// The 2nd is possible to create with `iconv(mark = FALSE)`, i.e.
// `iconv("\u00B0C", from = Encoding("\u00B0C"), to = "", mark = FALSE)`
//
// We need the 2nd to be normalized and marked as `CE_UTF8`, but
// `Rf_charIsUTF8()` can't help us with that because it returns `true`.
//
// Instead, we do a more granular check of:
// - `true` if `Rf_charIsASCII()`, i.e. if `IS_ASCII()`
// - `true` if `Rf_getCharCE() == CE_UTF8`, i.e. if `IS_UTF8()` since if a string
//   has `CE_UTF8` it also has the `SET_UTF8()` bit set
//
// This forces the `°C` marked as `CE_NATIVE` with `utf8locale = true` to still
// be forced through `Rf_translateCharUTF8()` (which does nothing due to
// `utf8locale = true`) and into `Rf_mkCharCE(, CE_UTF8)`, which marks it with
// `CE_UTF8` so now we can `vec_match()` against it.
static inline bool string_is_ascii_or_utf8(r_obj* x) {
#if (R_VERSION >= R_Version(4, 5, 0))
  return Rf_charIsASCII(x) || (Rf_getCharCE(x) == CE_UTF8) || (x == r_globals.na_str);
#else
  const int mask_ascii = 8;
  const int mask_utf8 = 64;
  const int levels = LEVELS(x);
  return (levels & mask_ascii) || (levels & mask_utf8) || (x == r_globals.na_str);
#endif
}

static inline r_obj* string_as_utf8(r_obj* x) {
  return Rf_mkCharCE(Rf_translateCharUTF8(x), CE_UTF8);
}

#endif
