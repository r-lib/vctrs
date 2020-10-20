#include "vctrs.h"
#include "utils.h"

enum direction {
  DIRECTION_down,
  DIRECTION_up,
  DIRECTION_downup,
  DIRECTION_updown
};

static enum direction parse_direction(SEXP x);
static SEXP vec_fill(SEXP x, const enum direction direction);

// [[ register() ]]
SEXP vctrs_fill(SEXP x, SEXP direction) {
  enum direction c_direction = parse_direction(direction);
  return vec_fill(x, c_direction);
}

static void vec_fill_down(const int* p_na, r_ssize size, bool reverse, int* p_loc);
static void vec_fill_up(const int* p_na, r_ssize size, bool reverse, int* p_loc);

static
SEXP vec_fill(SEXP x, const enum direction direction) {
  r_ssize size = vec_size(x);

  SEXP na = PROTECT(vec_equal_na(x));
  const int* p_na = LOGICAL_RO(na);

  SEXP loc = PROTECT(r_new_integer(size));
  int* p_loc = INTEGER(loc);

  // Initialize with sequential locations
  for (r_ssize i = 0; i < size; ++i) {
    p_loc[i] = i + 1;
  }

  switch (direction) {
  case DIRECTION_down: vec_fill_down(p_na, size, false, p_loc); break;
  case DIRECTION_downup: vec_fill_down(p_na, size, true, p_loc); break;
  case DIRECTION_up: vec_fill_up(p_na, size, false, p_loc); break;
  case DIRECTION_updown: vec_fill_up(p_na, size, true, p_loc); break;
  }

  SEXP out = vec_slice_impl(x, loc);

  UNPROTECT(2);
  return out;
}

static
void vec_fill_down(const int* p_na, r_ssize size, bool reverse, int* p_loc) {
  r_ssize loc = 0;

  if (reverse) {
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
    if (p_na[i]) {
      p_loc[i] = loc + 1;
    } else {
      loc = i;
    }
  }
}

static
void vec_fill_up(const int* p_na, r_ssize size, bool reverse, int* p_loc) {
  r_ssize loc = size - 1;

  if (reverse) {
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
    if (p_na[i]) {
      p_loc[i] = loc + 1;
    } else {
      loc = i;
    }
  }
}

// -----------------------------------------------------------------------------

static void stop_bad_direction();

static
enum direction parse_direction(SEXP x) {
  if (TYPEOF(x) != STRSXP || Rf_length(x) == 0) {
    stop_bad_direction();
  }

  const char* str = CHAR(STRING_ELT(x, 0));

  if (!strcmp(str, "down")) return DIRECTION_down;
  if (!strcmp(str, "up")) return DIRECTION_up;
  if (!strcmp(str, "downup")) return DIRECTION_downup;
  if (!strcmp(str, "updown")) return DIRECTION_updown;

  stop_bad_direction();
  never_reached("parse_direction");
}

static
void stop_bad_direction() {
  Rf_errorcall(R_NilValue, "`direction` must be one of \"down\", \"up\", \"downup\", or \"updown\".");
}
