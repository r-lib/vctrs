#include "vctrs.h"
#include "utils.h"
#include "equal.h"

static bool parse_direction(SEXP x);
static SEXP vec_fill(SEXP x, bool down, bool leading);

// [[ register() ]]
SEXP vctrs_fill(SEXP x, SEXP direction, SEXP leading) {
  bool c_down = parse_direction(direction);

  if (!r_is_bool(leading)) {
    r_abort("`leading` must be a single `TRUE` or `FALSE`.");
  }
  bool c_leading = r_lgl_get(leading, 0);

  return vec_fill(x, c_down, c_leading);
}

static void vec_fill_down(const int* p_na, r_ssize size, bool leading, int* p_loc);
static void vec_fill_up(const int* p_na, r_ssize size, bool leading, int* p_loc);

static
SEXP vec_fill(SEXP x, bool down, bool leading) {
  r_ssize size = vec_size(x);

  SEXP na = PROTECT(vec_equal_na(x));
  const int* p_na = LOGICAL_RO(na);

  SEXP loc = PROTECT(r_new_integer(size));
  int* p_loc = INTEGER(loc);

  if (down) {
    vec_fill_down(p_na, size, leading, p_loc);
  } else {
    vec_fill_up(p_na, size, leading, p_loc);
  }

  SEXP out = vec_slice_impl(x, loc);

  UNPROTECT(2);
  return out;
}

static
void vec_fill_down(const int* p_na, r_ssize size, bool leading, int* p_loc) {
  r_ssize loc = 0;

  if (leading) {
    // Increment `loc` to the first non-missing value
    for (r_ssize i = loc; i < size; ++i) {
      if (!p_na[i]) {
        loc = i;
        break;
      }
    }

    // Back-fill with first non-missing value
    for (r_ssize i = 0; i < loc; ++i) {
      p_loc[i] = loc + 1;
    }
  }

  for (r_ssize i = loc; i < size; ++i) {
    if (!p_na[i]) {
      loc = i;
    }

    p_loc[i] = loc + 1;
  }
}

static
void vec_fill_up(const int* p_na, r_ssize size, bool leading, int* p_loc) {
  r_ssize loc = size - 1;

  if (leading) {
    // Decrement `loc` to the last non-missing value
    for (r_ssize i = loc; i >= 0; --i) {
      if (!p_na[i]) {
        loc = i;
        break;
      }
    }

    // Forward-fill with last non-missing value
    for (r_ssize i = loc; i < size; ++i) {
      p_loc[i] = loc + 1;
    }
  }

  for (r_ssize i = loc; i >= 0; --i) {
    if (!p_na[i]) {
      loc = i;
    }

    p_loc[i] = loc + 1;
  }
}

// -----------------------------------------------------------------------------

static void stop_bad_direction();

static
bool parse_direction(SEXP x) {
  if (TYPEOF(x) != STRSXP || Rf_length(x) == 0) {
    stop_bad_direction();
  }

  const char* str = CHAR(STRING_ELT(x, 0));

  if (!strcmp(str, "down")) return true;
  if (!strcmp(str, "up")) return false;

  stop_bad_direction();
  never_reached("parse_direction");
}

static
void stop_bad_direction() {
  Rf_errorcall(R_NilValue, "`direction` must be either \"down\" or \"up\".");
}
