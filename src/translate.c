#include "vctrs.h"
#include "utils.h"

// -----------------------------------------------------------------------------

// UTF-8 translation will be successful in these cases:
// - (utf8 + latin1), (unknown + utf8), (unknown + latin1)
// UTF-8 translation will fail purposefully in these cases:
// - (bytes + utf8), (bytes + latin1), (bytes + unknown)
// UTF-8 translation is not attempted in these cases:
// - (utf8 + utf8), (latin1 + latin1), (unknown + unknown), (bytes + bytes)

static bool translation_required_chr_impl(const SEXP* x, R_len_t size, int reference) {
  for (R_len_t i = 0; i < size; ++i, ++x) {
    if (CHAR_ENC_TYPE(*x) != reference) {
      return true;
    }
  }

  return false;
}

// [[ include("vctrs.h") ]]
bool translation_required_chr(SEXP x, R_len_t size) {
  const SEXP* xp = STRING_PTR_RO(x);
  int reference = CHAR_ENC_TYPE(*xp);

  return translation_required_chr_impl(xp, size, reference);
}

// Check if `x` or `y` need to be translated to UTF-8, relative to each other
static bool translation_required_chr2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  const SEXP* p_x = STRING_PTR_RO(x);
  int reference = CHAR_ENC_TYPE(*p_x);

  if (translation_required_chr_impl(p_x, x_size, reference)) {
    return true;
  }

  const SEXP* p_y = STRING_PTR_RO(y);

  if (translation_required_chr_impl(p_y, y_size, reference)) {
    return true;
  }

  return false;
}

// -----------------------------------------------------------------------------
// Utilities required for checking if any character elements of a list have a
// "known" encoding. This implies that we have to convert all character
// elements of the list to UTF-8. This function is solely used by
// `translate_encoding_list2()`.

static bool any_known_encoding_chr(SEXP x, R_len_t size);
static bool any_known_encoding_list(SEXP x, R_len_t size);
static bool any_known_encoding_df(SEXP x, R_len_t size);

static bool any_known_encoding(SEXP x, R_len_t size) {
  switch (vec_typeof(x)) {
  case vctrs_type_character: return any_known_encoding_chr(x, size);
  case vctrs_type_list: return any_known_encoding_list(x, size);
  case vctrs_type_dataframe: return any_known_encoding_df(x, size);
  default: return true;
  }
}

static bool any_known_encoding_chr(SEXP x, R_len_t size) {
  const SEXP* p_x = STRING_PTR_RO(x);

  for (int i = 0; i < size; ++i, ++p_x) {
    if (CHAR_ENC_TYPE(*p_x) != 0) {
      return true;
    }
  }

  return false;
}

static bool any_known_encoding_list(SEXP x, R_len_t size) {
  SEXP elt;

  for (int i = 0; i < size; ++i) {
    elt = VECTOR_ELT(x, i);

    if (any_known_encoding(elt, vec_size(elt))) {
      return true;
    }
  }

  return false;
}

static bool any_known_encoding_df(SEXP x, R_len_t size) {
  int n_col = Rf_length(x);

  for (int i = 0; i < n_col; ++i) {
    if (any_known_encoding(VECTOR_ELT(x, i), size)) {
      return true;
    }
  }

  return false;
}

// -----------------------------------------------------------------------------
// Utilities required for translating the character vector elements of a list
// to UTF-8. This function is solely used by `translate_encoding_list2()`.

static SEXP translate_encoding_chr(SEXP x, R_len_t size);
static SEXP translate_encoding_list(SEXP x, R_len_t size);
static SEXP translate_encoding_df(SEXP x, R_len_t size);

static SEXP translate_encoding(SEXP x, R_len_t size) {
  switch (vec_typeof(x)) {
  case vctrs_type_character: return translate_encoding_chr(x, size);
  case vctrs_type_list: return translate_encoding_list(x, size);
  case vctrs_type_dataframe: return translate_encoding_df(x, size);
  default: return x;
  }
}

static SEXP translate_encoding_chr(SEXP x, R_len_t size) {
  const SEXP* p_x = STRING_PTR_RO(x);

  SEXP out = PROTECT(Rf_allocVector(STRSXP, size));
  SEXP* p_out = STRING_PTR(out);

  SEXP chr;
  const void *vmax = vmaxget();

  for (int i = 0; i < size; ++i, ++p_x, ++p_out) {
    chr = *p_x;

    if (CHAR_IS_UTF8(chr)) {
      *p_out = chr;
      continue;
    }

    *p_out = Rf_mkCharCE(Rf_translateCharUTF8(chr), CE_UTF8);
  }

  vmaxset(vmax);
  UNPROTECT(1);
  return out;
}

static SEXP translate_encoding_list(SEXP x, R_len_t size) {
  SEXP x_elt;
  x = PROTECT(r_maybe_duplicate(x));

  for (int i = 0; i < size; ++i) {
    x_elt = VECTOR_ELT(x, i);
    SET_VECTOR_ELT(x, i, translate_encoding(x_elt, vec_size(x_elt)));
  }

  UNPROTECT(1);
  return x;
}

static SEXP translate_encoding_df(SEXP x, R_len_t size) {
  SEXP x_col;
  x = PROTECT(r_maybe_duplicate(x));

  int n_col = Rf_length(x);

  for (int i = 0; i < n_col; ++i) {
    x_col = VECTOR_ELT(x, i);
    SET_VECTOR_ELT(x, i, translate_encoding(x_col, size));
  }

  UNPROTECT(1);
  return x;
}

// -----------------------------------------------------------------------------

static SEXP translate_none(SEXP x, SEXP y);
static SEXP translate_encoding_chr2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size);
static SEXP translate_encoding_list2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size);
static SEXP translate_encoding_df2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size);

// Notes:
// - Assumes that `x` and `y` are the same type from calling `vec_cast()`.
// - Does not assume that `x` and `y` are the same size.
// - Returns a list holding `x` and `y` translated to their common encoding.

// [[ include("vctrs.h") ]]
SEXP translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  switch (vec_typeof(x)) {
  case vctrs_type_character: return translate_encoding_chr2(x, x_size, y, y_size);
  case vctrs_type_list: return translate_encoding_list2(x, x_size, y, y_size);
  case vctrs_type_dataframe: return translate_encoding_df2(x, x_size, y, y_size);
  default: return translate_none(x, y);
  }
}

static SEXP translate_none(SEXP x, SEXP y) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  SET_VECTOR_ELT(out, 0, x);
  SET_VECTOR_ELT(out, 1, y);

  UNPROTECT(1);
  return out;
}

static SEXP translate_encoding_chr2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  if (translation_required_chr2(x, x_size, y, y_size)) {
    SET_VECTOR_ELT(out, 0, translate_encoding_chr(x, x_size));
    SET_VECTOR_ELT(out, 1, translate_encoding_chr(y, y_size));
  } else {
    SET_VECTOR_ELT(out, 0, x);
    SET_VECTOR_ELT(out, 1, y);
  }

  UNPROTECT(1);
  return out;
}

static SEXP translate_encoding_list2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  if (any_known_encoding(x, x_size) || any_known_encoding(y, y_size)) {
    SET_VECTOR_ELT(out, 0, translate_encoding_list(x, x_size));
    SET_VECTOR_ELT(out, 1, translate_encoding_list(y, y_size));
  } else {
    SET_VECTOR_ELT(out, 0, x);
    SET_VECTOR_ELT(out, 1, y);
  }

  UNPROTECT(1);
  return out;
}

static SEXP translate_encoding_df2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  SEXP x_i;
  SEXP y_i;
  SEXP translated;

  x = PROTECT(r_maybe_duplicate(x));
  y = PROTECT(r_maybe_duplicate(y));

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  int n_col = Rf_length(x);

  for (int i = 0; i < n_col; ++i) {
    x_i = VECTOR_ELT(x, i);
    y_i = VECTOR_ELT(y, i);

    translated = PROTECT(translate_encoding2(x_i, x_size, y_i, y_size));

    SET_VECTOR_ELT(x, i, VECTOR_ELT(translated, 0));
    SET_VECTOR_ELT(y, i, VECTOR_ELT(translated, 1));

    UNPROTECT(1);
  }

  SET_VECTOR_ELT(out, 0, x);
  SET_VECTOR_ELT(out, 1, y);

  UNPROTECT(3);
  return out;
}
