#' Find the common type for a pair of vectors
#'
#' @description
#'
#' `vec_ptype2()` defines the coercion hierarchy for a set of related
#' vector types. Along with [vec_cast()], this generic forms the
#' foundation of type coercions in vctrs.
#'
#' `vec_ptype2()` is relevant when you are implementing vctrs methods
#' for your class, but it should not usually be called directly. If
#' you need to find the common type of a set of inputs, call
#' [vec_ptype_common()] instead. This function supports multiple
#' inputs and [finalises][vec_ptype_finalise] the common type.
#'
#' @includeRmd man/faq/developer/links-coercion.Rmd
#'
#' @inheritParams ellipsis::dots_empty
#' @param x,y Vector types.
#' @param x_arg,y_arg Argument names for `x` and `y`. These are used
#'   in error messages to inform the user about the locations of
#'   incompatible types (see [stop_incompatible_type()]).
#'
#' @seealso [stop_incompatible_type()] when you determine from the
#'   attributes that an input can't be cast to the target type.
#' @export
vec_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  return(.Call(vctrs_ptype2, x, y, x_arg, y_arg))
  UseMethod("vec_ptype2")
}
vec_ptype2_dispatch_s3 <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2")
}

#' Default cast and ptype2 methods
#'
#' @description
#'
#' These functions are automatically called when no [vec_ptype2()] or
#' [vec_cast()] method is implemented for a pair of types.
#'
#' * They apply special handling if one of the inputs is of type
#'   `AsIs` or `sfc`.
#'
#' * They attempt a number of fallbacks in cases where it would be too
#'   inconvenient to be strict:
#'
#'   - If the class and attributes are the same they are considered
#'     compatible. `vec_default_cast()` returns `x` in this case.
#'
#'   - In case of incompatible data frame classes, they fall back to
#'     `data.frame`. If an incompatible subclass of tibble is
#'     involved, they fall back to `tbl_df`.
#'
#' * Otherwise, an error is thrown with [stop_incompatible_type()] or
#'   [stop_incompatible_cast()].
#'
#' @keywords internal
#' @export
vec_default_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  if (is_asis(x)) {
    return(vec_ptype2_asis_left(x, y, x_arg = x_arg, y_arg = y_arg))
  }
  if (is_asis(y)) {
    return(vec_ptype2_asis_right(x, y, x_arg = x_arg, y_arg = y_arg))
  }

  # Compatibility for sfc lists (#989)
  if (inherits(x, "sfc") || inherits(y, "sfc")) {
    return(UseMethod("vec_ptype2"))
  }

  # If both data frames, first find common type of columns before the
  # same-type fallback
  if (df_needs_normalisation(x, y)) {
    out <- vec_ptype2_df_fallback_normalise(x, y)
    x <- out$x
    y <- out$y
  }

  if (is_same_type(x, y)) {
    return(vec_ptype(x, x_arg = x_arg))
  }

  internal <- match_ptype2_params(...)
  df_fallback <- internal$df_fallback

  if (has_df_fallback(df_fallback)) {
    if (is_df_subclass(x) && is.data.frame(y)) {
      return(vec_ptype2_df_fallback(x, y, df_fallback))
    }
    if (is_df_subclass(y) && is.data.frame(x)) {
      return(vec_ptype2_df_fallback(x, y, df_fallback))
    }
  }

  # The from-dispatch parameter is set only when called from our S3
  # dispatch mechanism, when no method is found to dispatch to. It
  # indicates whether the error message should provide advice about
  # diverging attributes.
  stop_incompatible_type(
    x,
    y,
    x_arg = x_arg,
    y_arg = y_arg,
    `vctrs:::from_dispatch` = internal$from_dispatch
  )
}

match_ptype2_params <- function(...) {
  list(
    from_dispatch = match_from_dispatch(...),
    df_fallback = match_df_fallback(...)
  )
}
match_df_fallback <- function(..., `vctrs:::df_fallback` = FALSE) {
  `vctrs:::df_fallback`
}
match_from_dispatch <- function(..., `vctrs:::from_dispatch` = FALSE) {
  `vctrs:::from_dispatch`
}

vec_ptype2_params <- function(x,
                              y,
                              ...,
                              df_fallback = DF_FALLBACK_DEFAULT,
                              x_arg = "",
                              y_arg = "") {
  .Call(vctrs_ptype2_params, x, y, x_arg, y_arg, df_fallback)
}

vec_ptype2_no_fallback <- function(x, y, ..., x_arg = "", y_arg = "") {
  .Call(vctrs_ptype2_params, x, y, x_arg, y_arg, DF_FALLBACK_NONE)
}


# Kept in sync with ptype2.h
DF_FALLBACK_DEFAULT <- 0L
DF_FALLBACK_NONE <- 1L
DF_FALLBACK_WARN <- 2L
DF_FALLBACK_WARN_MAYBE <- 3L
DF_FALLBACK_QUIET <- 255L

df_fallback <- function(df_fallback) {
  if (df_fallback) df_fallback else DF_FALLBACK_WARN_MAYBE
}
has_df_fallback <- function(df_fallback) {
  df_fallback(df_fallback) > DF_FALLBACK_NONE
}
needs_fallback_warning <- function(df_fallback) {
  df_fallback <- df_fallback(df_fallback)

  if (df_fallback == DF_FALLBACK_WARN_MAYBE) {
    is_true(peek_option("vctrs:::warn_on_fallback"))
  } else {
    df_fallback < DF_FALLBACK_QUIET
  }
}
with_fallback_warning <- function(expr) {
  with_options(expr, `vctrs:::warn_on_fallback` = TRUE)
}
with_fallback_quiet <- function(expr) {
  with_options(expr, `vctrs:::warn_on_fallback` = FALSE)
}

vec_typeof2 <- function(x, y) {
  .Call(vctrs_typeof2, x, y)
}

vec_typeof2_s3 <- function(x, y) {
  .Call(vctrs_typeof2_s3, x, y)
}

# https://github.com/r-lib/vctrs/issues/571
vec_is_coercible <- function(x, y, ..., x_arg = "", y_arg = "", df_fallback = FALSE) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  .Call(vctrs_is_coercible, x, y, x_arg, y_arg, df_fallback = df_fallback)
}

vec_is_subtype <- function(x, super, ..., x_arg = "", super_arg = "") {
  tryCatch(
    vctrs_error_incompatible_type = function(...) FALSE,
    {
      common <- vctrs::vec_ptype2(x, super, ..., x_arg = x_arg, y_arg = super_arg)
      vec_is(common, super)
    }
  )
}

vec_implements_ptype2 <- function(x) {
  .Call(vctrs_implements_ptype2, x)
}
