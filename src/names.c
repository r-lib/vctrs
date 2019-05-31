#include "vctrs.h"
#include "utils.h"

static void describe_repair(SEXP old, SEXP new);


// [[ register(); include("vctrs.h") ]]
SEXP vec_names(SEXP x) {
  if (OBJECT(x) && Rf_inherits(x, "data.frame")) {
    return R_NilValue;
  }

  if (vec_dims(x) == 1) {
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

SEXP vctrs_as_minimal_names(SEXP names) {
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
    return names;
  }

  names = PROTECT(r_maybe_duplicate(names));

  for (; i < n; ++i, ++ptr) {
    SEXP elt = *ptr;
    if (elt == NA_STRING) {
      SET_STRING_ELT(names, i, strings_empty);
    }
  }

  UNPROTECT(1);
  return names;
}

SEXP vctrs_minimal_names(SEXP x) {
  SEXP names = PROTECT(vec_names(x));

  if (names == R_NilValue) {
    names = Rf_allocVector(STRSXP, vec_size(x));
  } else {
    names = vctrs_as_minimal_names(names);
  }

  UNPROTECT(1);
  return names;
}


// From dictionary.c
SEXP vctrs_duplicated(SEXP x);

static void stop_large_name();
static bool is_dotdotint(const char* name);
static ptrdiff_t suffix_pos(const char* name);
static bool needs_suffix(SEXP str);

static SEXP as_unique_names(SEXP names) {
  if (TYPEOF(names) != STRSXP) {
    Rf_errorcall(R_NilValue, "`names` must be a character vector");
  }

  R_len_t i = 0;
  R_len_t n = Rf_length(names);
  SEXP* ptr = STRING_PTR(names);

  SEXP dups = PROTECT(vctrs_duplicated(names));
  int* dups_ptr = LOGICAL(dups);

  // First quick pass to detect if any repairs are needed. See second
  // part of the loop for the meaning of each branch.
  for (; i < n; ++i, ++ptr, ++dups_ptr) {
    SEXP elt = *ptr;

    if (needs_suffix(elt) || suffix_pos(CHAR(elt)) >= 0 || *dups_ptr) {
      break;
    }
  }
  UNPROTECT(1);

  if (i == n) {
    return names;
  }

  names = PROTECT(r_maybe_duplicate(names));
  ptr = STRING_PTR(names);

  for (; i < n; ++i, ++ptr) {
    SEXP elt = *ptr;

    // Set `NA` and dots values to "" so they get replaced by `...n`
    // later on
    if (needs_suffix(elt)) {
      elt = strings_empty;
      SET_STRING_ELT(names, i, elt);
      continue;
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
      continue;
    }
  }

  // Append all duplicates with a suffix
  char buf[100] = "";
  ptr = STRING_PTR(names);

  dups = PROTECT(vctrs_duplicated(names));
  dups_ptr = LOGICAL(dups);

  for (R_len_t i = 0; i < n; ++i, ++ptr) {
    SEXP elt = *ptr;

    if (elt != strings_empty && !dups_ptr[i]) {
      continue;
    }

    const char* name = CHAR(elt);

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

  UNPROTECT(2);
  return names;
}

SEXP vctrs_as_unique_names(SEXP names, SEXP quiet) {
  SEXP out = PROTECT(as_unique_names(names));

  if (!LOGICAL(quiet)[0]) {
    describe_repair(names, out);
  }

  UNPROTECT(1);
  return out;
}

static bool is_dotdotint(const char* name) {
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

static ptrdiff_t suffix_pos(const char* name) {
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

static void stop_large_name() {
  Rf_errorcall(R_NilValue, "Can't tidy up name because it is too large");
}

static bool needs_suffix(SEXP str) {
  return
    str == NA_STRING ||
    str == strings_dots ||
    str == strings_empty ||
    is_dotdotint(CHAR(str));
}


static SEXP names_iota(R_len_t n);

SEXP vec_unique_names(SEXP x, bool quiet) {
  SEXP names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));

  SEXP out;
  if (names == R_NilValue) {
    out = PROTECT(names_iota(vec_size(x)));
  } else {
    out = PROTECT(as_unique_names(names));
  }

  if (!quiet) {
    describe_repair(names, out);
  }

  UNPROTECT(2);
  return(out);
}

SEXP vctrs_unique_names(SEXP x, SEXP quiet) {
  return vec_unique_names(x, LOGICAL(quiet)[0]);
}


// 3 leading '.' + 1 trailing '\0' + 24 characters
#define TOTAL_BUF_SIZE 28

static SEXP names_iota(R_len_t n) {
  char buf[TOTAL_BUF_SIZE];
  SEXP nms = r_chr_iota(n, buf, TOTAL_BUF_SIZE, "...");

  if (nms == R_NilValue) {
    Rf_errorcall(R_NilValue, "Too many names to repair.");
  }

  return nms;
}

#undef TOTAL_BUF_SIZE
#undef FREE_BUF_SIZE


static void describe_repair(SEXP old, SEXP new) {
  SEXP call = PROTECT(Rf_lang3(Rf_install("describe_repair"), old, new));
  Rf_eval(call, vctrs_ns_env);
  UNPROTECT(1);
}


static SEXP outer_names_cat(const char* outer, SEXP names);
static SEXP outer_names_seq(const char* outer, R_len_t n);

// [[ register() ]]
SEXP vctrs_outer_names(SEXP names, SEXP outer, SEXP n) {
  if (names != R_NilValue && TYPEOF(names) != STRSXP) {
    Rf_error("Internal error: `names` must be `NULL` or a string");
  }
  if (!r_is_number(n)) {
    Rf_error("Internal error: `n` must be a single integer");
  }

  return outer_names(names, outer, r_int_get(n, 0));
}

static SEXP str_as_chr(SEXP x) {
  if (TYPEOF(x) == STRSXP) {
    return x;
  } else {
    SEXP out = Rf_allocVector(STRSXP, 1);
    SET_STRING_ELT(out, 0, x);
    return out;
  }
}

// [[ include("utils.h") ]]
SEXP outer_names(SEXP names, SEXP outer, R_len_t n) {
  if (outer == R_NilValue) {
    return names;
  }

  SEXP outer_str;
  switch (TYPEOF(outer)) {
  case STRSXP:
    if (Rf_length(outer) != 1) {
      goto bad_outer;
    }
    outer_str = STRING_ELT(outer, 0);
    break;
  case CHARSXP:
    outer_str = outer;
    break;
  default:
  bad_outer:
    Rf_error("Internal error: `outer` must be a string");
  }

  if (outer_str == strings_empty || outer_str == NA_STRING) {
    return names;
  }

  if (r_is_empty_names(names)) {
    if (n == 1) {
      return str_as_chr(outer);
    } else {
      return outer_names_seq(CHAR(outer_str), n);
    }
  } else {
    return outer_names_cat(CHAR(outer_str), names);
  }
}

static SEXP outer_names_cat(const char* outer, SEXP names) {
  names = PROTECT(Rf_shallow_duplicate(names));
  R_len_t n = Rf_length(names);

  int outer_len = strlen(outer);
  int names_len = r_chr_max_len(names);

  int total_len = outer_len + names_len + strlen("..") + 1;

  R_CheckStack2(total_len);
  char buf[total_len];
  buf[total_len - 1] = '\0';
  char* bufp = buf;

  memcpy(bufp, outer, outer_len); bufp += outer_len;
  *bufp = '.'; bufp += 1;
  *bufp = '.'; bufp += 1;

  SEXP* p = STRING_PTR(names);

  for (R_len_t i = 0; i < n; ++i, ++p) {
    const char* inner = CHAR(*p);
    int inner_n = strlen(inner);

    memcpy(bufp, inner, inner_n);
    bufp[inner_n] = '\0';

    SET_STRING_ELT(names, i, r_str(buf));
  }

  UNPROTECT(1);
  return names;
}

static SEXP outer_names_seq(const char* outer, R_len_t n) {
  int total_len = 24 + strlen(outer) + 1;

  R_CheckStack2(total_len);
  char buf[total_len];

  return r_chr_iota(n, buf, total_len, outer);
}


// Initialised at load time
SEXP syms_set_rownames = NULL;
SEXP fns_set_rownames = NULL;

// [[ include("utils.h") ]]
SEXP set_rownames(SEXP x, SEXP names) {
  return vctrs_dispatch2(syms_set_rownames, fns_set_rownames,
                         syms_x, x,
                         syms_names, names);
}


void vctrs_init_names(SEXP ns) {
  syms_set_rownames = Rf_install("set_rownames");
  fns_set_rownames = r_env_get(ns, syms_set_rownames);
}
