#include "vctrs.h"
#include "utils.h"


// [[ register(external = TRUE) ]]
SEXP vctrs_size_common(SEXP call, SEXP op, SEXP args, SEXP env) {
  args = CDR(args);

  SEXP size = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  if (size != R_NilValue) {
    size_validate(size, ".size");
    UNPROTECT(1);
    return size;
  }

  SEXP absent = PROTECT(Rf_eval(CAR(args), env));
  if (absent != R_NilValue && (TYPEOF(absent) != INTSXP || Rf_length(absent) != 1)) {
    Rf_errorcall(R_NilValue, "`.absent` must be a single integer.");
  }

  SEXP xs = PROTECT(rlang_env_dots_list(env));
  R_len_t common = vec_size_common(xs, -1);

  SEXP out;
  if (common < 0) {
    if (absent == R_NilValue) {
      Rf_errorcall(R_NilValue, "`...` is empty, and no `.absent` value was supplied.");
    }
    out = absent;
  } else {
    out = r_int(common);
  }

  UNPROTECT(3);
  return out;
}


static SEXP vctrs_size2_common(SEXP x, SEXP y, struct counters* counters);

// [[ include("vctrs.h") ]]
R_len_t vec_size_common(SEXP xs, R_len_t absent) {
  SEXP common = PROTECT(reduce(R_NilValue, args_empty, xs, &vctrs_size2_common));
  R_len_t out;

  if (common == R_NilValue) {
    out = absent;
  } else {
    out = vec_size(common);
  }

  UNPROTECT(1);
  return out;
}

static SEXP vctrs_size2_common(SEXP x, SEXP y, struct counters* counters) {
  if (x == R_NilValue) {
    counters_shift(counters);
    return y;
  }
  if (y == R_NilValue) {
    return x;
  }

  R_len_t nx = vec_size(x);
  R_len_t ny = vec_size(y);

  if (nx == ny) {
    return x;
  }
  if (nx == 1) {
    counters_shift(counters);
    return y;
  }
  if (ny == 1) {
    return x;
  }

  stop_incompatible_size(x, y, nx, ny, counters->curr_arg, counters->next_arg);
}

