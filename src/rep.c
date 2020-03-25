#include "vctrs.h"
#include "utils.h"
#include <inttypes.h> // For PRId64

// Initialised at load time
static struct vctrs_arg args_times_;
static struct vctrs_arg args_x_;
static struct vctrs_arg* const args_times = &args_times_;
static struct vctrs_arg* const args_x = &args_x_;

static inline void stop_rep_size_oob(int64_t size);
static inline void stop_rep_times_size();
static inline void stop_rep_times_negative();
static inline void stop_rep_times_missing();
static inline void stop_rep_each_times_negative(R_len_t i);
static inline void stop_rep_each_times_missing(R_len_t i);

// -----------------------------------------------------------------------------

static SEXP vec_rep(SEXP x, R_len_t times);

// [[ register() ]]
SEXP vctrs_rep(SEXP x, SEXP times) {
  times = PROTECT(vec_cast(times, vctrs_shared_empty_int, args_times, args_empty));

  if (vec_size(times) != 1) {
    stop_rep_times_size();
  }

  const R_len_t times_ = r_int_get(times, 0);

  SEXP out = vec_rep(x, times_);

  UNPROTECT(1);
  return out;
}

static SEXP vec_rep(SEXP x, R_len_t times) {
  if (times < 0) {
    if (times == NA_INTEGER) {
      stop_rep_times_missing();
    } else {
      stop_rep_times_negative();
    }
  }

  const R_len_t x_size = vec_size(x);

  // TODO: Use more robust overflow check if we add support for long vectors
  const int64_t temp_size = (int64_t) x_size * times;
  if (temp_size > INT_MAX) {
    stop_rep_size_oob(temp_size);
  }

  const R_len_t size = (R_len_t) temp_size;

  SEXP subscript = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_subscript = INTEGER(subscript);

  R_len_t k = 0;

  for (R_len_t i = 0; i < times; ++i) {
    for (R_len_t j = 1; j <= x_size; ++j, ++k) {
      p_subscript[k] = j;
    }
  }

  SEXP out = vec_slice_impl(x, subscript);

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP vec_rep_each(SEXP x, SEXP times);

// [[ register() ]]
SEXP vctrs_rep_each(SEXP x, SEXP times) {
  return vec_rep_each(x, times);
}

static SEXP vec_rep_each_uniform(SEXP x, R_len_t times);
static SEXP vec_rep_each_impl(SEXP x, SEXP times, const R_len_t times_size);

static SEXP vec_rep_each(SEXP x, SEXP times) {
  times = PROTECT(vec_cast(times, vctrs_shared_empty_int, args_times, args_empty));

  const R_len_t times_size = vec_size(times);

  SEXP out;

  if (times_size == 1) {
    const R_len_t times_ = r_int_get(times, 0);
    out = vec_rep_each_uniform(x, times_);
  } else {
    out = vec_rep_each_impl(x, times, times_size);
  }

  UNPROTECT(1);
  return out;
}

static SEXP vec_rep_each_uniform(SEXP x, R_len_t times) {
  if (times < 0) {
    if (times == NA_INTEGER) {
      stop_rep_each_times_missing(1);
    } else {
      stop_rep_each_times_negative(1);
    }
  }

  const R_len_t x_size = vec_size(x);

  // TODO: Use more robust overflow check if we add support for long vectors
  const int64_t temp_size = (int64_t) x_size * times;
  if (temp_size > INT_MAX) {
    stop_rep_size_oob(temp_size);
  }

  const R_len_t size = (R_len_t) temp_size;

  SEXP subscript = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_subscript = INTEGER(subscript);

  R_len_t k = 0;

  for (R_len_t i = 1; i <= x_size; ++i) {
    for (R_len_t j = 0; j < times; ++j, ++k) {
      p_subscript[k] = i;
    }
  }

  SEXP out = vec_slice_impl(x, subscript);

  UNPROTECT(1);
  return out;
}

static SEXP vec_rep_each_impl(SEXP x, SEXP times, const R_len_t times_size) {
  const R_len_t x_size = vec_size(x);

  if (x_size != times_size) {
    stop_recycle_incompatible_size(times_size, x_size, args_times);
  }

  const int* p_times = INTEGER_RO(times);

  // TODO: Use more robust overflow check if we add support for long vectors
  int64_t temp_size = 0;
  for (R_len_t i = 0; i < times_size; ++i) {
    const int elt_times = p_times[i];

    if (elt_times < 0) {
      if (elt_times == NA_INTEGER) {
        stop_rep_each_times_missing(i + 1);
      } else {
        stop_rep_each_times_negative(i + 1);
      }
    }

    temp_size += elt_times;
  }

  if (temp_size > INT_MAX) {
    stop_rep_size_oob(temp_size);
  }

  const R_len_t size = (R_len_t) temp_size;

  SEXP subscript = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_subscript = INTEGER(subscript);

  R_len_t k = 0;

  for (R_len_t i = 1; i <= x_size; ++i) {
    const int elt_times = p_times[i - 1];

    for (R_len_t j = 0; j < elt_times; ++j, ++k) {
      p_subscript[k] = i;
    }
  }

  SEXP out = vec_slice_impl(x, subscript);

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

static inline void stop_rep_size_oob(int64_t size) {
  Rf_errorcall(R_NilValue, "Requested output size must be less than %i, not %" PRId64 ".", INT_MAX, size);
}

static inline void stop_rep_times_size() {
  Rf_errorcall(R_NilValue, "`times` must be a single number.");
}

static inline void stop_rep_times_negative() {
  Rf_errorcall(R_NilValue, "`times` must be a positive number.");
}

static inline void stop_rep_times_missing() {
  Rf_errorcall(R_NilValue, "`times` can't be missing.");
}

static inline void stop_rep_each_times_negative(R_len_t i) {
  Rf_errorcall(R_NilValue, "`times` must be a vector of positive numbers. Location %i is negative.", i);
}

static inline void stop_rep_each_times_missing(R_len_t i) {
  Rf_errorcall(R_NilValue, "`times` can't be missing. Location %i is missing.", i);
}

// -----------------------------------------------------------------------------

void vctrs_init_rep(SEXP ns) {
  args_times_ = new_wrapper_arg(NULL, "times");
  args_x_ = new_wrapper_arg(NULL, "x");
}
