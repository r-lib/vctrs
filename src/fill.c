#include <rlang.h>
#include "vctrs.h"
#include "utils.h"
#include "equal.h"

#define INFINITE_FILL -1

static void parse_direction(SEXP x, bool* p_down, bool* p_leading);
static int parse_max_fill(SEXP x);
static SEXP vec_fill_missing(SEXP x, bool down, bool leading, int max_fill);

// [[ register() ]]
SEXP vctrs_fill_missing(SEXP x, SEXP direction, SEXP max_fill) {
  bool down;
  bool leading;
  parse_direction(direction, &down, &leading);

  int c_max_fill = parse_max_fill(max_fill);

  return vec_fill_missing(x, down, leading, c_max_fill);
}

static void vec_fill_missing_down(const int* p_na, r_ssize size, bool leading, int* p_loc);
static void vec_fill_missing_down_with_max_fill(const int* p_na, r_ssize size, bool leading, int max_fill, int* p_loc);
static void vec_fill_missing_up(const int* p_na, r_ssize size, bool leading, int* p_loc);
static void vec_fill_missing_up_with_max_fill(const int* p_na, r_ssize size, bool leading, int max_fill, int* p_loc);

static
SEXP vec_fill_missing(SEXP x, bool down, bool leading, int max_fill) {
  r_ssize size = vec_size(x);

  SEXP na = PROTECT(vec_equal_na(x));
  const int* p_na = LOGICAL_RO(na);

  SEXP loc = PROTECT(r_new_integer(size));
  int* p_loc = INTEGER(loc);

  const bool has_max_fill = max_fill != INFINITE_FILL;

  if (down) {
    if (has_max_fill) {
      vec_fill_missing_down_with_max_fill(p_na, size, leading, max_fill, p_loc);
    } else {
      vec_fill_missing_down(p_na, size, leading, p_loc);
    }
  } else {
    if (has_max_fill) {
      vec_fill_missing_up_with_max_fill(p_na, size, leading, max_fill, p_loc);
    } else {
      vec_fill_missing_up(p_na, size, leading, p_loc);
    }
  }

  SEXP out = vec_slice_unsafe(x, loc);

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
void vec_fill_missing_down_with_max_fill(const int* p_na, r_ssize size, bool leading, int max_fill, int* p_loc) {
  r_ssize loc = 0;

  if (leading) {
    // Increment `loc` to the first non-missing value
    for (r_ssize i = 0; i < size; ++i) {
      if (!p_na[i]) {
        loc = i;
        break;
      }
    }

    // Back-fill with first non-missing value with a max_fill
    r_ssize n_fill = 0;

    for (r_ssize i = loc - 1; i >= 0; --i) {
      if (n_fill == max_fill) {
        p_loc[i] = i + 1;
      } else {
        p_loc[i] = loc + 1;
        ++n_fill;
      }
    }
  }

  r_ssize n_fill = 0;

  for (r_ssize i = loc; i < size; ++i) {
    if (!p_na[i]) {
      loc = i;
      n_fill = 0;
      p_loc[i] = i + 1;
      continue;
    }

    if (n_fill == max_fill) {
      p_loc[i] = i + 1;
    } else {
      p_loc[i] = loc + 1;
      ++n_fill;
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
void vec_fill_missing_up_with_max_fill(const int* p_na, r_ssize size, bool leading, int max_fill, int* p_loc) {
  r_ssize loc = size - 1;

  if (leading) {
    // Decrement `loc` to the last non-missing value
    for (r_ssize i = size - 1; i >= 0; --i) {
      if (!p_na[i]) {
        loc = i;
        break;
      }
    }

    // Forward-fill with last non-missing value with a max_fill
    r_ssize n_fill = 0;

    for (r_ssize i = loc + 1; i < size; ++i) {
      if (n_fill == max_fill) {
        p_loc[i] = i + 1;
      } else {
        p_loc[i] = loc + 1;
        ++n_fill;
      }
    }
  }

  r_ssize n_fill = 0;

  for (r_ssize i = loc; i >= 0; --i) {
    if (!p_na[i]) {
      loc = i;
      n_fill = 0;
      p_loc[i] = i + 1;
      continue;
    }

    if (n_fill == max_fill) {
      p_loc[i] = i + 1;
    } else {
      p_loc[i] = loc + 1;
      ++n_fill;
    }
  }
}

// -----------------------------------------------------------------------------

static void stop_bad_direction();

static
void parse_direction(SEXP x, bool* p_down, bool* p_leading) {
  if (TYPEOF(x) != STRSXP || Rf_length(x) == 0) {
    stop_bad_direction();
  }

  const char* str = CHAR(STRING_ELT(x, 0));

  if (!strcmp(str, "down")) {
    *p_down = true;
    *p_leading = false;
    return;
  }
  if (!strcmp(str, "up")) {
    *p_down = false;
    *p_leading = false;
    return;
  }
  if (!strcmp(str, "downup")) {
    *p_down = true;
    *p_leading = true;
    return;
  }
  if (!strcmp(str, "updown")) {
    *p_down = false;
    *p_leading = true;
    return;
  }

  stop_bad_direction();
  never_reached("parse_direction");
}

static
void stop_bad_direction() {
  r_abort("`direction` must be one of \"down\", \"up\", \"downup\", or \"updown\".");
}

static
int parse_max_fill(r_obj* x) {
  if (x == R_NilValue) {
    return INFINITE_FILL;
  }

  x = KEEP(vec_cast(x,
                    vctrs_shared_empty_int,
                    args_max_fill,
                    args_empty,
                    r_lazy_null));

  if (!r_is_positive_number(x)) {
    r_abort("`max_fill` must be `NULL` or a single positive integer.");
  }

  int out = r_int_get(x, 0);

  FREE(1);
  return out;
}
