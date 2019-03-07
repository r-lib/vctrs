#' Vector kind of an object
#'
#' @section Dispatch:
#'
#' This function is internally generic:
#'
#' * Bare atomic vectors are `atomic`.
#' * Bare lists are `recursive`.
#' * Data frames are both `atomic` and `recursive`.
#'
#' All other cases can be overriden by implementing an S3 method for
#' the `vec_kind()` generic. The defaults are:
#'
#' * S3 atomic vectors are `atomic`.
#' * S3 lists are `atom`. This means objects like model fit are not
#'   treated as recursive.
#'
#' @param x An object.
#' @return One of `"atom"`, `"atomic"`, `"recursive"`.
#'
#'   Can also be `c("atomic", "recursive")`. For instance, data frames
#'   are both atomic (vector of rows) and recursive (list of columns).
#'
#' @export
vec_kind <- function(x) {
  kind <- switch(vec_typeof(x),
    logical = ,
    integer = ,
    double = ,
    complex = ,
    character = ,
    raw =
      "atomic",
    list =
      "recursive",
    dataframe =
      c("atomic", "recursive"),
    s3 =
      # Dispatch via another closure instead of calling `UseMethod()`
      # directly to avoid the non-local exit. This way we can validate
      # the return value.
      eval_bare(call2(vec_kind_dispatch, x), caller_env()),
    abort("Internal error: Unexpected type in `vec_kind()`")
  )

  if (length(kind) > 1 && !identical(kind, c("atomic", "recursive"))) {
    kind <- collapse_backtick(kind)
    abort(glue::glue("Internal error: Unexpected vector kind combination { kind }"))
  }
  if (!any(kind %in% c("atom", "atomic", "recursive"))) {
    kind <- collapse_backtick(kind)
    abort(glue::glue("Internal error: Unexpected vector kind { kind }"))
  }

  kind
}
vec_kind_dispatch <- function(x) {
  UseMethod("vec_kind")
}

#' @export
vec_kind.default <- function(x) {
  # All S3 objects except those wrapping an atomic vectors are treated
  # as atoms by default
  if (is_atomic(x)) {
    "atomic"
  } else {
    "atom"
  }
}

vec_typeof <- function(x) {
  .Call(vctrs_typeof, x)
}
