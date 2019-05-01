#include "vctrs.h"
#include "dictionary.h"
#include "utils.h"

// [[ register() ]]
SEXP vec_names(SEXP x) {
  if (OBJECT(x) && Rf_inherits(x, "data.frame")) {
    return R_NilValue;
  }

  if (vec_dim(x) == 1) {
    if (OBJECT(x)) {
      return vctrs_dispatch1(syms_names, fns_names, syms_x, x);
    } else {
      return r_names(x);
    }
  }

  SEXP dimnames = PROTECT(Rf_getAttrib(x, R_DimNamesSymbol));
  if (dimnames == R_NilValue || Rf_length(dimnames) < 1) {
    UNPROTECT(1);
    return R_NilValue;
  }

  SEXP out = VECTOR_ELT(dimnames, 0);
  UNPROTECT(1);
  return out;
}

static struct sexp_cow as_minimal_names(struct sexp_cow cow_names) {
  SEXP names = cow_names.obj;

  if (TYPEOF(names) != STRSXP) {
    Rf_errorcall(R_NilValue, "`names` must be a character vector");
  }

  R_len_t i = 0;
  R_len_t n = Rf_length(names);
  SEXP* ptr = STRING_PTR(names);

  for (; i < n; ++i, ++ptr) {
    SEXP elt = *ptr;
    if (elt == NA_STRING) {
      break;
    }
  }
  if (i == n) {
    return cow_names;
  }

  cow_names = r_maybe_copy(cow_names);
  names = cow_names.obj;

  for (; i < n; ++i, ++ptr) {
    SEXP elt = *ptr;
    if (elt == NA_STRING) {
      SET_STRING_ELT(names, i, strings_empty);
    }
  }

  return cow_names;
}

SEXP vctrs_as_minimal_names(SEXP names) {
  struct sexp_cow cow_names = PROTECT_COW(names);
  cow_names = as_minimal_names(cow_names);

  UNPROTECT(1);
  return cow_names.obj;
}

SEXP vctrs_minimal_names(SEXP x) {
  SEXP names = PROTECT(vec_names(x));

  if (names == R_NilValue) {
    UNPROTECT(1);
    return Rf_allocVector(STRSXP, vec_size(x));
  }

  struct sexp_cow cow_names = PROTECT_COW(names);
  cow_names = as_minimal_names(cow_names);

  UNPROTECT(2);
  return cow_names.obj;
}


void stop_large_name();
bool is_dotdotint(const char* name);
ptrdiff_t suffix_pos(const char* name);

static struct sexp_cow as_unique_names(struct sexp_cow cow_names) {
  SEXP names = cow_names.obj;

  if (TYPEOF(names) != STRSXP) {
    Rf_errorcall(R_NilValue, "`names` must be a character vector");
  }

  dictionary d;
  dict_init(&d, names);
  SEXP dups = PROTECT(Rf_allocVector(INTSXP, d.size));
  int* dups_ptr = INTEGER(dups);

  R_len_t i = 0;
  R_len_t n = Rf_length(names);
  SEXP* ptr = STRING_PTR(names);

  // First quick pass to detect if any repairs are needed. See second
  // part of the loop for the meaning of each branch.
  for (; i < n; ++i, ++ptr) {
    SEXP elt = *ptr;

    if (elt == NA_STRING || elt == strings_dots || elt == strings_empty || is_dotdotint(CHAR(elt))) {
      break;
    }
    if (suffix_pos(CHAR(elt)) >= 0) {
      break;
    }

    int32_t k = dict_find(&d, names, i);

    if (d.key[k] == DICT_EMPTY) {
      dict_put(&d, k, i);
      dups_ptr[k] = 1;
    } else {
      break;
    }
  }

  // Return early when no repairs are needed
  if (i == n) {
    UNPROTECT(1);
    return cow_names;
  }


  cow_names = r_maybe_copy(cow_names);
  names = cow_names.obj;
  ptr = STRING_PTR(names) + i;

  for (; i < n; ++i, ++ptr) {
    SEXP elt = *ptr;

    // Set `NA` and dots values to "" so they get replaced by `...n` later on
    if (elt == NA_STRING || elt == strings_dots || is_dotdotint(CHAR(elt))) {
      elt = strings_empty;
      SET_STRING_ELT(names, i, elt);
      SET_STRING_ELT(d.x, i, elt);
    }

    // Strip `...n` suffixes
    const char* nm = CHAR(elt);
    int pos = suffix_pos(nm);
    if (pos >= 0) {
      R_CheckStack2(pos + 1);
      char buf[pos + 1];
      memcpy(buf, nm, pos);
      buf[pos] = '\0';

      elt = Rf_mkChar(buf);
      SET_STRING_ELT(names, i, elt);
      SET_STRING_ELT(d.x, i, elt);
    }

    // Duplicates need a `...n` suffix
    int32_t k = dict_find(&d, names, i);

    if (d.key[k] == DICT_EMPTY) {
      dict_put(&d, k, i);
      dups_ptr[k] = 0;

      // Ensure "" is seen as a duplicate so it always gets a suffix
      if (elt == strings_empty) {
        dups_ptr[k]++;
      }
    }
    dups_ptr[k]++;
  }

  // Append all duplicates with a suffix
  char buf[100] = "";

  for (i = 0, ptr = STRING_PTR(names); i < n; ++i, ++ptr) {
    int32_t k = dict_find(&d, names, i);
    if (dups_ptr[k] != 1) {
      const char* name = CHAR(*ptr);

      int remaining = 100;
      int size = strlen(name);
      if (size >= 100) {
        stop_large_name();
      }

      memcpy(buf, name, size + 1);
      remaining -= size;

      int needed = snprintf(buf + size, remaining, "...%d", i + 1);
      if (needed >= remaining) {
        stop_large_name();
      }

      SET_STRING_ELT(names, i, Rf_mkChar(buf));
    }
  }

  UNPROTECT(1);
  return cow_names;
}

SEXP vctrs_as_unique_names(SEXP names) {
  struct sexp_cow cow_names = PROTECT_COW(names);
  cow_names = as_unique_names(cow_names);

  UNPROTECT(1);
  return cow_names.obj;
}

bool is_dotdotint(const char* name) {
  int n = strlen(name);

  if (n < 3) {
    return false;
  }
  if (name[0] != '.' || name[1] != '.') {
    return false;
  }

  if (name[2] == '.') {
    name += 3;
  } else {
    name += 2;
  }

  return (bool) strtol(name, NULL, 10);
}

static bool is_digit(const char c) {
  switch (c) {
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    return true;
  default:
    return false;
  }
}

ptrdiff_t suffix_pos(const char* name) {
  int n = strlen(name);

  const char* suffix_end = NULL;
  int in_dots = 0;
  bool in_digits = false;

  for (const char* ptr = name + n - 1; ptr >= name; --ptr) {
    char c = *ptr;

    if (in_digits) {
      if (c == '.') {
        in_digits = false;
        in_dots = 1;
        continue;
      }

      if (is_digit(c)) {
        continue;
      }

      goto done;
    }

    switch (in_dots) {
    case 0:
      if (is_digit(c)) {
        in_digits = true;
        continue;
      }
      goto done;
    case 1:
    case 2:
      if (c == '.') {
        ++in_dots;
        continue;
      }
      goto done;
    case 3:
      suffix_end = ptr + 1;
      if (is_digit(c)) {
        in_dots = 0;
        in_digits = true;
        continue;
      }
      goto done;

    default:
      Rf_error("Internal error: Unexpected state in `suffix_pos()`");
    }}

 done:
  if (suffix_end) {
    return suffix_end - name;
  } else {
    return -1;
  }
}

void stop_large_name() {
  Rf_errorcall(R_NilValue, "Can't tidy up name because it is too large");
}


static SEXP names_iota(R_len_t n);

SEXP vctrs_unique_names(SEXP x) {
  SEXP names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));

  if (names == R_NilValue) {
    UNPROTECT(1);
    return(names_iota(vec_size(x)));
  }

  names = vctrs_as_unique_names(names);

  UNPROTECT(1);
  return names;
}


// 3 leading '.' + 1 trailing '\0' + 24 characters
#define TOTAL_BUF_SIZE 28
#define FREE_BUF_SIZE 25

static SEXP names_iota(R_len_t n) {
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, n));

  char buf[TOTAL_BUF_SIZE] = "...";
  char* beg = buf + 3;

  for (R_len_t i = 0; i < n; ++i) {
    int written = snprintf(beg, FREE_BUF_SIZE, "%d", i + 1);

    if (written >= FREE_BUF_SIZE) {
      Rf_errorcall(R_NilValue, "Can't write repaired names as there are too many.");
    }

    SET_STRING_ELT(nms, i, Rf_mkChar(buf));
  }

  UNPROTECT(1);
  return nms;
}

#undef TOTAL_BUF_SIZE
#undef FREE_BUF_SIZE
