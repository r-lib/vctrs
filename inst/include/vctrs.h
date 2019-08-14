#ifndef VCTRS_H
#define VCTRS_H

#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdbool.h>

static R_INLINE R_len_t vec_size(SEXP x) {
  static R_len_t (*fn)(SEXP) = NULL;

  if (fn == NULL) {
    fn = (R_len_t (*)(SEXP)) R_GetCCallable("vctrs", "vec_size");
  }

  return fn(x);
}

static R_INLINE SEXP vec_proxy(SEXP x) {
  static SEXP (*fn)(SEXP) = NULL;

  if (fn == NULL) {
    fn = (SEXP (*)(SEXP)) R_GetCCallable("vctrs", "vec_proxy");
  }

  return fn(x);
}

static R_INLINE SEXP vec_restore(SEXP x, SEXP to, SEXP n) {
  static SEXP (*fn)(SEXP, SEXP, SEXP) = NULL;

  if (fn == NULL) {
    fn = (SEXP (*)(SEXP, SEXP, SEXP)) R_GetCCallable("vctrs", "vec_restore");
  }

  return fn(x, to, n);
}

static R_INLINE SEXP vctrs_cast(SEXP x, SEXP to, SEXP x_arg_, SEXP to_arg_) {
  static SEXP (*fn)(SEXP, SEXP, SEXP, SEXP) = NULL;

  if (fn == NULL) {
    fn = (SEXP (*)(SEXP, SEXP, SEXP, SEXP)) R_GetCCallable("vctrs", "vctrs_cast");
  }

  return fn(x, to, x_arg_, to_arg_);
}

static R_INLINE SEXP vec_init(SEXP x, R_len_t n) {
  static SEXP (*fn)(SEXP, R_len_t) = NULL;

  if (fn == NULL) {
    fn = (SEXP (*)(SEXP, R_len_t)) R_GetCCallable("vctrs", "vec_init");
  }

  return fn(x, n);
}

static R_INLINE SEXP vec_assign_impl(SEXP x, SEXP i, SEXP value, bool clone) {
  static SEXP (*fn)(SEXP, SEXP, SEXP, bool) = NULL;

  if (fn == NULL) {
    fn = (SEXP (*)(SEXP, SEXP, SEXP, bool)) R_GetCCallable("vctrs", "vec_assign_impl");
  }

  return fn(x, i, value, clone);
}

static R_INLINE SEXP vec_slice_impl(SEXP x, SEXP index) {
  static SEXP (*fn)(SEXP, SEXP) = NULL;

  if (fn == NULL) {
    fn = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("vctrs", "vec_slice_impl");
  }

  return fn(x, index);
}

static R_INLINE SEXP vec_names(SEXP x) {
  static SEXP (*fn)(SEXP) = NULL;

  if (fn == NULL) {
    fn = (SEXP (*)(SEXP)) R_GetCCallable("vctrs", "vec_names");
  }

  return fn(x);
}

static R_INLINE SEXP vec_set_names(SEXP x, SEXP names) {
  static SEXP (*fn)(SEXP, SEXP) = NULL;

  if (fn == NULL) {
    fn = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("vctrs", "vec_set_names");
  }

  return fn(x, names);
}

#endif
