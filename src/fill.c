#include "vctrs.h"
#include "utils.h"
#include "equal.h"

#define INFINITE_GAP -1

static bool parse_direction(SEXP x);
static int parse_max_gap(SEXP x);
static SEXP vec_fill_missing(SEXP x, bool down, bool leading, int max_gap);

// [[ register() ]]
SEXP vctrs_fill_missing(SEXP x, SEXP direction, SEXP leading, SEXP max_gap) {
  bool c_down = parse_direction(direction);

  if (!r_is_bool(leading)) {
    r_abort("`leading` must be a single `TRUE` or `FALSE`.");
  }
  bool c_leading = r_lgl_get(leading, 0);

  int c_max_gap = parse_max_gap(max_gap);

  return vec_fill_missing(x, c_down, c_leading, c_max_gap);
}

static void vec_fill_missing_down(const int* p_na, r_ssize size, bool leading, int* p_loc);
static void vec_fill_missing_down_with_max_gap(const int* p_na, r_ssize size, bool leading, int max_gap, int* p_loc);
static void vec_fill_missing_up(const int* p_na, r_ssize size, bool leading, int* p_loc);
static void vec_fill_missing_up_with_max_gap(const int* p_na, r_ssize size, bool leading, int max_gap, int* p_loc);

static
SEXP vec_fill_missing(SEXP x, bool down, bool leading, int max_gap) {
  r_ssize size = vec_size(x);

  SEXP na = PROTECT(vec_equal_na(x));
  const int* p_na = LOGICAL_RO(na);

  SEXP loc = PROTECT(r_new_integer(size));
  int* p_loc = INTEGER(loc);

  const bool has_max_gap = max_gap != INFINITE_GAP;

  if (down) {
    if (has_max_gap) {
      vec_fill_missing_down_with_max_gap(p_na, size, leading, max_gap, p_loc);
    } else {
      vec_fill_missing_down(p_na, size, leading, p_loc);
    }
  } else {
    if (has_max_gap) {
      vec_fill_missing_up_with_max_gap(p_na, size, leading, max_gap, p_loc);
    } else {
      vec_fill_missing_up(p_na, size, leading, p_loc);
    }
  }

  SEXP out = vec_slice_impl(x, loc);

  UNPROTECT(2);
  return out;
}

static
void vec_fill_missing_down(const int* p_na, r_ssize size, bool leading, int* p_loc) {
  r_ssize loc = 0;

  if (leading) {
    // Increment `loc` to the first non-missing value
    for (r_ssize i = 0; i < size; ++i) {
      if (!p_na[i]) {
        loc = i;
        break;
      }
    }

    // Back-fill with first non-missing value
    for (r_ssize i = loc - 1; i >= 0; --i) {
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
void vec_fill_missing_down_with_max_gap(const int* p_na, r_ssize size, bool leading, int max_gap, int* p_loc) {
  r_ssize loc = 0;

  if (leading) {
    // Increment `loc` to the first non-missing value
    for (r_ssize i = 0; i < size; ++i) {
      if (!p_na[i]) {
        loc = i;
        break;
      }
    }

    // Back-fill with first non-missing value with a max_gap
    r_ssize gap = 0;

    for (r_ssize i = loc - 1; i >= 0; --i) {
      if (gap == max_gap) {
        p_loc[i] = i + 1;
      } else {
        p_loc[i] = loc + 1;
        ++gap;
      }
    }
  }

  r_ssize gap = 0;

  for (r_ssize i = loc; i < size; ++i) {
    if (!p_na[i]) {
      loc = i;
      gap = 0;
      p_loc[i] = i + 1;
      continue;
    }

    if (gap == max_gap) {
      p_loc[i] = i + 1;
    } else {
      p_loc[i] = loc + 1;
      ++gap;
    }
  }
}

static
void vec_fill_missing_up(const int* p_na, r_ssize size, bool leading, int* p_loc) {
  r_ssize loc = size - 1;

  if (leading) {
    // Decrement `loc` to the last non-missing value
    for (r_ssize i = size - 1; i >= 0; --i) {
      if (!p_na[i]) {
        loc = i;
        break;
      }
    }

    // Forward-fill with last non-missing value
    for (r_ssize i = loc + 1; i < size; ++i) {
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

static
void vec_fill_missing_up_with_max_gap(const int* p_na, r_ssize size, bool leading, int max_gap, int* p_loc) {
  r_ssize loc = size - 1;

  if (leading) {
    // Decrement `loc` to the last non-missing value
    for (r_ssize i = size - 1; i >= 0; --i) {
      if (!p_na[i]) {
        loc = i;
        break;
      }
    }

    // Forward-fill with last non-missing value with a max_gap
    r_ssize gap = 0;

    for (r_ssize i = loc + 1; i < size; ++i) {
      if (gap == max_gap) {
        p_loc[i] = i + 1;
      } else {
        p_loc[i] = loc + 1;
        ++gap;
      }
    }
  }

  r_ssize gap = 0;

  for (r_ssize i = loc; i >= 0; --i) {
    if (!p_na[i]) {
      loc = i;
      gap = 0;
      p_loc[i] = i + 1;
      continue;
    }

    if (gap == max_gap) {
      p_loc[i] = i + 1;
    } else {
      p_loc[i] = loc + 1;
      ++gap;
    }
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
  r_abort("`direction` must be either \"down\" or \"up\".");
}

static
int parse_max_gap(SEXP x) {
  if (x == R_NilValue) {
    return INFINITE_GAP;
  }

  x = PROTECT(vec_cast(x, vctrs_shared_empty_int, args_max_gap, args_empty));

  if (!r_is_positive_number(x)) {
    r_abort("`max_gap` must be `NULL` or a single positive integer.");
  }

  int out = r_int_get(x, 0);

  UNPROTECT(1);
  return out;
}
