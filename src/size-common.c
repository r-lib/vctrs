#include "vctrs.h"


// [[ register(external = TRUE) ]]
SEXP vctrs_size_common(SEXP ffi_call, SEXP op, SEXP args, SEXP env) {
  // TODO! arg
  // TODO! call
  struct r_lazy call = r_lazy_null;

  args = CDR(args);

  SEXP size = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);
  if (size != R_NilValue) {
    R_len_t out = check_size(size, vec_args.dot_size, call);
    UNPROTECT(1);
    return r_int(out);
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


static
SEXP vctrs_size2_common(SEXP x, SEXP y, struct counters* counters, void* data);

r_ssize vec_size_common(r_obj* xs, r_ssize absent) {
  r_obj* common = KEEP(reduce(R_NilValue, args_empty, NULL, xs, &vctrs_size2_common, NULL));
  r_ssize out;

  if (common == r_null) {
    out = absent;
  } else {
    out = vec_size(common);
  }

  FREE(1);
  return out;
}

static SEXP vctrs_size2_common(SEXP x, SEXP y, struct counters* counters, void* data) {
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

// [[ register(external = TRUE) ]]
SEXP vctrs_recycle_common(SEXP ffi_call, SEXP op, SEXP args, SEXP env) {
  // TODO! arg
  // TODO! call
  struct r_lazy call = r_lazy_null;

  args = CDR(args);

  SEXP size = PROTECT(Rf_eval(CAR(args), env)); args = CDR(args);

  R_len_t common;

  SEXP xs = PROTECT(rlang_env_dots_list(env));

  if (size != R_NilValue) {
    common = check_size(size, vec_args.dot_size, call);
  } else {
    common = vec_size_common(xs, -1);
  }

  SEXP out = PROTECT(vec_recycle_common(xs, common));

  UNPROTECT(3);
  return out;
}

r_obj* vec_recycle_common(r_obj* xs, r_ssize size) {
  if (size < 0) {
    return xs;
  }

  xs = KEEP(r_clone_referenced(xs));
  r_ssize n = vec_size(xs);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* elt = r_list_get(xs, i);
    r_list_poke(xs, i, vec_recycle(elt, size, args_empty));
  }

  FREE(1);
  return xs;
}
