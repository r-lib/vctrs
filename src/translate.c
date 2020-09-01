#include "vctrs.h"
#include "ptype2.h"
#include "utils.h"

// -----------------------------------------------------------------------------
// Helpers for determining if UTF-8 translation is required for character
// vectors

// UTF-8 translation will be successful in these cases:
// - (utf8 + latin1), (unknown + utf8), (unknown + latin1)
// UTF-8 translation will fail purposefully in these cases:
// - (bytes + utf8), (bytes + latin1), (bytes + unknown)
// UTF-8 translation is not attempted in these cases:
// - (utf8 + utf8), (latin1 + latin1), (unknown + unknown), (bytes + bytes)

static bool chr_translation_required_impl(const SEXP* p_x, R_len_t n, cetype_t reference) {
  for (R_len_t i = 0; i < n; ++i) {
    if (Rf_getCharCE(p_x[i]) != reference) {
      return true;
    }
  }

  return false;
}

static bool chr_translation_required(SEXP x, R_len_t n) {
  if (n == 0) {
    return false;
  }

  const SEXP* p_x = STRING_PTR_RO(x);
  cetype_t reference = Rf_getCharCE(*p_x);

  return chr_translation_required_impl(p_x, n, reference);
}

// Check if `x` or `y` need to be translated to UTF-8, relative to each other
static bool chr_translation_required2(SEXP x, R_len_t x_n, SEXP y, R_len_t y_n) {
  const SEXP* p_x;
  const SEXP* p_y;

  bool x_empty = x_n == 0;
  bool y_empty = y_n == 0;

  if (x_empty && y_empty) {
    return false;
  }

  if (x_empty) {
    p_y = STRING_PTR_RO(y);
    return chr_translation_required_impl(p_y, y_n, Rf_getCharCE(*p_y));
  }

  if (y_empty) {
    p_x = STRING_PTR_RO(x);
    return chr_translation_required_impl(p_x, x_n, Rf_getCharCE(*p_x));
  }

  p_x = STRING_PTR_RO(x);
  cetype_t reference = Rf_getCharCE(*p_x);

  if (chr_translation_required_impl(p_x, x_n, reference)) {
    return true;
  }

  p_y = STRING_PTR_RO(y);

  if (chr_translation_required_impl(p_y, y_n, reference)) {
    return true;
  }

  return false;
}

// -----------------------------------------------------------------------------
// Utilities to check if any character elements of a list have a
// "known" encoding (UTF-8 or Latin1). This implies that we have to convert
// all character elements of the list to UTF-8. Only `list_any_known_encoding()`
// is ever called directly.

static bool chr_any_known_encoding(SEXP x, R_len_t n);
static bool list_any_known_encoding(SEXP x, R_len_t n);
static bool df_any_known_encoding(SEXP x, R_len_t n);

static bool obj_any_known_encoding(SEXP x, R_len_t n) {
  switch (TYPEOF(x)) {
  case STRSXP: {
    return chr_any_known_encoding(x, n);
  }
  case VECSXP: {
    if (is_data_frame(x)) {
      return df_any_known_encoding(x, n);
    } else {
      return list_any_known_encoding(x, n);
    }
  }
  default: {
    return false;
  }
  }
}

// For usage on list elements. They have unknown n, and might be scalars.
static bool elt_any_known_encoding(SEXP x) {
  switch (TYPEOF(x)) {
  case STRSXP: {
    return chr_any_known_encoding(x, Rf_length(x));
  }
  case VECSXP: {
    if (is_data_frame(x)) {
      return df_any_known_encoding(x, vec_size(x));
    } else {
      return list_any_known_encoding(x, Rf_length(x));
    }
  }
  default: {
    return false;
  }
  }
}

static bool chr_any_known_encoding(SEXP x, R_len_t n) {
  if (n == 0) {
    return false;
  }

  const SEXP* p_x = STRING_PTR_RO(x);

  for (R_len_t i = 0; i < n; ++i) {
    if (Rf_getCharCE(p_x[i]) != CE_NATIVE) {
      return true;
    }
  }

  return false;
}

static bool list_any_known_encoding(SEXP x, R_len_t n) {
  for (R_len_t i = 0; i < n; ++i) {
    if (elt_any_known_encoding(VECTOR_ELT(x, i))) {
      return true;
    }
  }

  return false;
}

// Data frames have a separate path from lists here purely for
// performance reasons. We know the size of each column, and can
// pass that information through.
static bool df_any_known_encoding(SEXP x, R_len_t n) {
  R_len_t n_col = Rf_length(x);

  for (R_len_t i = 0; i < n_col; ++i) {
    if (obj_any_known_encoding(VECTOR_ELT(x, i), n)) {
      return true;
    }
  }

  return false;
}

// -----------------------------------------------------------------------------
// Utilities to translate all character vector elements of an object to UTF-8.
// This does not check if a translation is required.

static SEXP chr_translate_encoding(SEXP x, R_len_t n);
static SEXP list_translate_encoding(SEXP x, R_len_t n);
static SEXP df_translate_encoding(SEXP x, R_len_t n);

static SEXP obj_translate_encoding(SEXP x, R_len_t n) {
  switch (TYPEOF(x)) {
  case STRSXP: {
    return chr_translate_encoding(x, n);
  }
  case VECSXP: {
    if (is_data_frame(x)) {
      return df_translate_encoding(x, n);
    } else {
      return list_translate_encoding(x, n);
    }
  }
  default: {
    return x;
  }
  }
}

// For usage on list elements. They have unknown size, and might be scalars.
static SEXP elt_translate_encoding(SEXP x) {
  switch (TYPEOF(x)) {
  case STRSXP: {
    return chr_translate_encoding(x, Rf_length(x));
  }
  case VECSXP: {
    if (is_data_frame(x)) {
      return df_translate_encoding(x, vec_size(x));
    } else {
      return list_translate_encoding(x, Rf_length(x));
    }
  }
  default: {
    return x;
  }
  }
}

static SEXP chr_translate_encoding(SEXP x, R_len_t n) {
  if (n == 0) {
    return x;
  }

  const SEXP* p_x = STRING_PTR_RO(x);

  SEXP out = PROTECT(r_clone_referenced(x));

  const void *vmax = vmaxget();

  for (R_len_t i = 0; i < n; ++i) {
    SEXP chr = p_x[i];

    if (Rf_getCharCE(chr) == CE_UTF8) {
      SET_STRING_ELT(out, i, chr);
      continue;
    }

    SET_STRING_ELT(out, i, Rf_mkCharCE(Rf_translateCharUTF8(chr), CE_UTF8));
  }

  vmaxset(vmax);
  UNPROTECT(1);
  return out;
}

static SEXP list_translate_encoding(SEXP x, R_len_t n) {
  x = PROTECT(r_clone_referenced(x));

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = VECTOR_ELT(x, i);
    SET_VECTOR_ELT(x, i, elt_translate_encoding(elt));
  }

  UNPROTECT(1);
  return x;
}

static SEXP df_translate_encoding(SEXP x, R_len_t n) {
  R_len_t n_col = Rf_length(x);

  x = PROTECT(r_clone_referenced(x));

  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    SET_VECTOR_ELT(x, i, obj_translate_encoding(col, n));
  }

  UNPROTECT(1);
  return x;
}

// -----------------------------------------------------------------------------
// Utilities for translating encodings within one vector, if required.

// - If `x` is a character vector requiring translation, translate it.
// - If `x` is a list where any element has a "known" encoding, force a
//   translation of every element in the list.
// - If `x` is a data frame, translate the columns one by one, independently.

// Notes:
// - Assumes that `x` has been proxied recursively.

static SEXP chr_maybe_translate_encoding(SEXP x, R_len_t n);
static SEXP list_maybe_translate_encoding(SEXP x, R_len_t n);
static SEXP df_maybe_translate_encoding(SEXP x, R_len_t n);

// [[ include("vctrs.h") ]]
SEXP obj_maybe_translate_encoding(SEXP x, R_len_t n) {
  switch (TYPEOF(x)) {
  case STRSXP: {
    return chr_maybe_translate_encoding(x, n);
  }
  case VECSXP: {
    if (is_data_frame(x)) {
      return df_maybe_translate_encoding(x, n);
    } else {
      return list_maybe_translate_encoding(x, n);
    }
  }
  default: {
    return x;
  }
  }
}

static SEXP chr_maybe_translate_encoding(SEXP x, R_len_t n) {
  return chr_translation_required(x, n) ? chr_translate_encoding(x, n) : x;
}

static SEXP list_maybe_translate_encoding(SEXP x, R_len_t n) {
  return list_any_known_encoding(x, n) ? list_translate_encoding(x, n) : x;
}

static SEXP df_maybe_translate_encoding(SEXP x, R_len_t n) {
  R_len_t n_col = Rf_length(x);

  x = PROTECT(r_clone_referenced(x));

  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP elt = VECTOR_ELT(x, i);
    SET_VECTOR_ELT(x, i, obj_maybe_translate_encoding(elt, n));
  }

  UNPROTECT(1);
  return x;
}

// -----------------------------------------------------------------------------
// Utilities for translating encodings of `x` and `y` relative to each other,
// if required.

static SEXP translate_none(SEXP x, SEXP y);
static SEXP chr_maybe_translate_encoding2(SEXP x, R_len_t x_n, SEXP y, R_len_t y_n);
static SEXP list_maybe_translate_encoding2(SEXP x, R_len_t x_n, SEXP y, R_len_t y_n);
static SEXP df_maybe_translate_encoding2(SEXP x, R_len_t x_n, SEXP y, R_len_t y_n);

// Notes:
// - Assumes that `x` and `y` are the same type from calling `vec_cast()`.
// - Assumes that `x` and `y` have been recursively proxied.
// - Does not assume that `x` and `y` are the same size.
// - Returns a list holding `x` and `y` translated to their common encoding.

// [[ include("vctrs.h") ]]
SEXP obj_maybe_translate_encoding2(SEXP x, R_len_t x_n, SEXP y, R_len_t y_n) {
  switch (TYPEOF(x)) {
  case STRSXP: {
    return chr_maybe_translate_encoding2(x, x_n, y, y_n);
  }
  case VECSXP: {
    if (is_data_frame(x)) {
      return df_maybe_translate_encoding2(x, x_n, y, y_n);
    } else {
      return list_maybe_translate_encoding2(x, x_n, y, y_n);
    }
  }
  default: {
    return translate_none(x, y);
  }
  }
}

static SEXP translate_none(SEXP x, SEXP y) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  SET_VECTOR_ELT(out, 0, x);
  SET_VECTOR_ELT(out, 1, y);

  UNPROTECT(1);
  return out;
}

static SEXP chr_maybe_translate_encoding2(SEXP x, R_len_t x_n, SEXP y, R_len_t y_n) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  if (chr_translation_required2(x, x_n, y, y_n)) {
    SET_VECTOR_ELT(out, 0, chr_translate_encoding(x, x_n));
    SET_VECTOR_ELT(out, 1, chr_translate_encoding(y, y_n));
  } else {
    SET_VECTOR_ELT(out, 0, x);
    SET_VECTOR_ELT(out, 1, y);
  }

  UNPROTECT(1);
  return out;
}

static SEXP list_maybe_translate_encoding2(SEXP x, R_len_t x_n, SEXP y, R_len_t y_n) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  if (list_any_known_encoding(x, x_n) || list_any_known_encoding(y, y_n)) {
    SET_VECTOR_ELT(out, 0, list_translate_encoding(x, x_n));
    SET_VECTOR_ELT(out, 1, list_translate_encoding(y, y_n));
  } else {
    SET_VECTOR_ELT(out, 0, x);
    SET_VECTOR_ELT(out, 1, y);
  }

  UNPROTECT(1);
  return out;
}

static SEXP df_maybe_translate_encoding2(SEXP x, R_len_t x_n, SEXP y, R_len_t y_n) {
  R_len_t n_col = Rf_length(x);

  x = PROTECT(r_clone_referenced(x));
  y = PROTECT(r_clone_referenced(y));

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP x_elt = VECTOR_ELT(x, i);
    SEXP y_elt = VECTOR_ELT(y, i);

    SEXP translated = PROTECT(obj_maybe_translate_encoding2(x_elt, x_n, y_elt, y_n));

    SET_VECTOR_ELT(x, i, VECTOR_ELT(translated, 0));
    SET_VECTOR_ELT(y, i, VECTOR_ELT(translated, 1));

    UNPROTECT(1);
  }

  SET_VECTOR_ELT(out, 0, x);
  SET_VECTOR_ELT(out, 1, y);

  UNPROTECT(3);
  return out;
}

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP vctrs_maybe_translate_encoding(SEXP x) {
  SEXP out = PROTECT(obj_maybe_translate_encoding(x, vec_size(x)));

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP vctrs_maybe_translate_encoding2(SEXP x, SEXP y) {
  int _;

  SEXP type = PROTECT(vec_ptype2(x, y, args_empty, args_empty, &_));

  x = PROTECT(vec_cast(x, type, args_empty, args_empty));
  y = PROTECT(vec_cast(y, type, args_empty, args_empty));

  SEXP out = obj_maybe_translate_encoding2(x, vec_size(x), y, vec_size(y));

  UNPROTECT(3);
  return out;
}

