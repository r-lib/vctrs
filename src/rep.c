#include "vctrs.h"
#include "utils.h"

// Initialised at load time
static struct vctrs_arg args_times_;
static struct vctrs_arg* const args_times = &args_times_;

static inline void stop_rep_times_size();

static inline void check_rep_times(int times);
static inline void check_rep_each_times(int times, R_len_t i);

static inline bool multiply_would_overflow(R_len_t x, R_len_t y);
static inline bool plus_would_overflow(R_len_t x, R_len_t y);
static inline void stop_rep_size_oob();

// -----------------------------------------------------------------------------

static SEXP vec_rep(SEXP x, int times);

// [[ register() ]]
SEXP vctrs_rep(SEXP x, SEXP times) {
  times = PROTECT(vec_cast(times, vctrs_shared_empty_int, args_times, args_empty));

  if (vec_size(times) != 1) {
    stop_rep_times_size();
  }

  const int times_ = r_int_get(times, 0);

  SEXP out = vec_rep(x, times_);

  UNPROTECT(1);
  return out;
}

static SEXP vec_rep(SEXP x, int times) {
  check_rep_times(times);

  const R_len_t times_ = (R_len_t) times;
  const R_len_t x_size = vec_size(x);

  if (multiply_would_overflow(x_size, times_)) {
    stop_rep_size_oob();
  };

  const R_len_t size = x_size * times_;

  SEXP subscript = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_subscript = INTEGER(subscript);

  R_len_t k = 0;

  for (R_len_t i = 0; i < times_; ++i) {
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

static SEXP vec_rep_each_uniform(SEXP x, int times);
static SEXP vec_rep_each_impl(SEXP x, SEXP times, const R_len_t times_size);

static SEXP vec_rep_each(SEXP x, SEXP times) {
  times = PROTECT(vec_cast(times, vctrs_shared_empty_int, args_times, args_empty));

  const R_len_t times_size = vec_size(times);

  SEXP out;

  if (times_size == 1) {
    const int times_ = r_int_get(times, 0);
    out = vec_rep_each_uniform(x, times_);
  } else {
    out = vec_rep_each_impl(x, times, times_size);
  }

  UNPROTECT(1);
  return out;
}

static SEXP vec_rep_each_uniform(SEXP x, int times) {
  check_rep_each_times(times, 1);

  const R_len_t times_ = (R_len_t) times;
  const R_len_t x_size = vec_size(x);

  if (multiply_would_overflow(x_size, times_)) {
    stop_rep_size_oob();
  };

  const R_len_t size = x_size * times_;

  SEXP subscript = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_subscript = INTEGER(subscript);

  R_len_t k = 0;

  for (R_len_t i = 1; i <= x_size; ++i) {
    for (R_len_t j = 0; j < times_; ++j, ++k) {
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

  R_len_t size = 0;
  for (R_len_t i = 0; i < times_size; ++i) {
    const int elt_times = p_times[i];

    check_rep_each_times(elt_times, i + 1);

    const R_len_t elt_times_ = (R_len_t) elt_times;

    if (plus_would_overflow(size, elt_times_)) {
      stop_rep_size_oob();
    }

    size += elt_times_;
  }

  SEXP subscript = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_subscript = INTEGER(subscript);

  R_len_t k = 0;

  for (R_len_t i = 1; i <= x_size; ++i) {
    const R_len_t elt_times = (R_len_t) p_times[i - 1];

    for (R_len_t j = 0; j < elt_times; ++j, ++k) {
      p_subscript[k] = i;
    }
  }

  SEXP out = vec_slice_impl(x, subscript);

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

// TODO: Modify for long vectors with `R_XLEN_T_MAX` and `R_xlen_t`.

static inline bool times_is_oob(int times) {
  return times > R_LEN_T_MAX;
}

// Only useful for positive or zero inputs
static inline bool multiply_would_overflow(R_len_t x, R_len_t y) {
  return (double) x * y > R_LEN_T_MAX;
}

// Only useful for positive or zero inputs
static inline bool plus_would_overflow(R_len_t x, R_len_t y) {
  return x > R_LEN_T_MAX - y;
}

// -----------------------------------------------------------------------------

static inline void stop_rep_times_negative();
static inline void stop_rep_times_missing();
static inline void stop_rep_times_oob(int times);

static inline void check_rep_times(int times) {
  if (times < 0) {
    if (times == NA_INTEGER) {
      stop_rep_times_missing();
    } else {
      stop_rep_times_negative();
    }
  } else if (times_is_oob(times)) {
    stop_rep_times_oob(times);
  }
}

static inline void stop_rep_times_negative() {
  Rf_errorcall(R_NilValue, "`times` must be a positive number.");
}

static inline void stop_rep_times_missing() {
  Rf_errorcall(R_NilValue, "`times` can't be missing.");
}

// Not currently thrown since `R_len_t == int`, but might be once
// long vectors are supported
static inline void stop_rep_times_oob(int times) {
  Rf_errorcall(
    R_NilValue,
    "`times` must be less than %i, not %i.",
    R_LEN_T_MAX,
    times
  );
}

// -----------------------------------------------------------------------------

static inline void stop_rep_each_times_negative(R_len_t i);
static inline void stop_rep_each_times_missing(R_len_t i);
static inline void stop_rep_each_times_oob(int times, R_len_t i);

static inline void check_rep_each_times(int times, R_len_t i) {
  if (times < 0) {
    if (times == NA_INTEGER) {
      stop_rep_each_times_missing(i);
    } else {
      stop_rep_each_times_negative(i);
    }
  } else if (times_is_oob(times)) {
    stop_rep_each_times_oob(times, i);
  }
}

static inline void stop_rep_each_times_negative(R_len_t i) {
  Rf_errorcall(R_NilValue, "`times` must be a vector of positive numbers. Location %i is negative.", i);
}

static inline void stop_rep_each_times_missing(R_len_t i) {
  Rf_errorcall(R_NilValue, "`times` can't be missing. Location %i is missing.", i);
}

// Not currently thrown since `R_len_t == int`, but might be once
// long vectors are supported
static inline void stop_rep_each_times_oob(int times, R_len_t i) {
  Rf_errorcall(
    R_NilValue,
    "`times` must be less than %i, not %i. ",
    "Location %i is too large.",
    R_LEN_T_MAX,
    times,
    i
  );
}

// -----------------------------------------------------------------------------

static inline void stop_rep_size_oob() {
  Rf_errorcall(
    R_NilValue,
    "Long vectors are not yet supported. "
    "Requested output size must be less than %i.",
    R_LEN_T_MAX
  );
}

static inline void stop_rep_times_size() {
  Rf_errorcall(R_NilValue, "`times` must be a single number.");
}

// -----------------------------------------------------------------------------

void vctrs_init_rep(SEXP ns) {
  args_times_ = new_wrapper_arg(NULL, "times");
}
