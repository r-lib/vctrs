#' Assert an argument has known prototype and/or size
#'
#' @description
#'
#' * `vec_is()` is a predicate that checks if its input conforms to a
#'   prototype and/or a size.
#'
#' * `vec_assert()` throws an error when the input doesn't conform.
#'
#' @section Error types:
#'
#' * If the prototype doesn't match, an error of class
#'   `"vctrs_error_assert_ptype"` is raised.
#'
#' * If the prototype doesn't match, an error of class
#' `"vctrs_error_assert_size"` is raised.
#'
#' Both errors inherit from `"vctrs_error_assert"`.
#'
#' @param x A vector argument to check.
#' @param ptype Prototype to compare against.
#' @param size Size to compare against
#' @param arg Name of argument being checked. This is used in error
#'   messages. The label of the expression passed as `x` is taken as
#'   default.
#'
#' @return `vec_is()` returns `TRUE` or `FALSE`. `vec_assert()` either
#'   throws a typed error (see section on error types) or returns `x`,
#'   invisibly.
#' @export
vec_assert <- function(x, ptype = NULL, size = NULL, arg = NULL) {
  arg <- arg %||% as_label(substitute(x))

  if (!vec_is_vector(x)) {
    msg <- paste0("`", arg, "` must be a vector, not a scalar")
    abort(msg, "vctrs_error_assert_scalar", actual = x)
  }

  if (!is_null(ptype)) {
    ptype <- vec_type(ptype)
    x_type <- vec_type(x)
    if (!identical(x_type, ptype)) {
      ptype <- vec_type(ptype)
      x_type <- vec_type(x)
      msg <- paste0("`", arg, "` must be <", vec_ptype_abbr(ptype), ">, not <", vec_ptype_abbr(x_type), ">.")
      abort(
        msg,
        .subclass = c("vctrs_error_assert_ptype", "vctrs_error_assert"),
        required = ptype,
        actual = x_type
      )
    }
  }

  if (!is_null(size)) {
    size <- vec_recycle(vec_cast(size, integer()), 1L)
    x_size <- vec_size(x)
    if (!identical(x_size, size)) {
      msg <- paste0("`", arg, "` must have size ", size, ", not size ", x_size, ".")
      abort(
        msg,
        .subclass = c("vctrs_error_assert_size", "vctrs_error_assert"),
        required = size,
        actual = x_size
      )
    }
  }

  invisible(x)
}
#' @rdname vec_assert
#' @export
vec_is <- function(x, ptype = NULL, size = NULL) {
  if (!vec_is_vector(x)) {
    return(FALSE)
  }

  if (!is_null(ptype)) {
    ptype <- vec_type(ptype)
    x_type <- vec_type(x)
    if (!identical(x_type, ptype)) {
      return(FALSE)
    }
  }

  if (!is_null(size)) {
    size <- vec_recycle(vec_cast(size, integer()), 1L)
    x_size <- vec_size(x)
    if (!identical(x_size, size)) {
      return(FALSE)
    }
  }

  TRUE
}

#' Is an object a vector?
#'
#' @description
#'
#' A vector is a collection of values. In practical terms, a vector is
#' something that can be a column of a data frame. The opposite of a
#' vector is a scalar. Such an object cannot be [sliced][vec_slice],
#' nor can it be a data frame column.
#'
#' @section Dispatch:
#'
#' This function is internally generic:
#'
#' * Atomic vectors are always vectors.
#' * Bare lists are vectors.
#' * Data frames are always vectors.
#' * Non-vector base types are always scalars.
#'
#' Only lists can be overriden by implementing an S3 method for
#' `vec_is_vector()`.
#'
#' @param x An object.
#'
#' @examples
#' # Atomic vectors are always vectors:
#' vec_is_vector(1:2)
#' vec_is_vector(structure(1:2, class = "foo"))
#'
#' # Bare lists and data frames are vectors:
#' vec_is_vector(list(1, 2))
#' vec_is_vector(mtcars)
#'
#' # S3 lists need to explicitly implement `vec_is_vector()` to be
#' # treated as lists. They are treated as scalars by default:
#' fit <- stats::lm(disp ~ drat, mtcars)
#' vec_is_vector(fit)
#'
#' # Other base types are never vectors:
#' vec_is_vector(quote(foo(bar)))
#' vec_is_vector(~foo)
#' vec_is_vector(function() NULL)
#' @export
vec_is_vector <- function(x) {
  switch(vec_typeof(x),
    null =
      return(FALSE),
    logical = ,
    integer = ,
    double = ,
    complex = ,
    character = ,
    raw = ,
    list = ,
    dataframe =
      return(TRUE),
    s3 =
      if (inherits(x, "vctrs_vctr")) {
        return(TRUE)
      } else {
        UseMethod("vec_is_vector")
      },
    scalar =
      return(FALSE)
  )

  stop_unimplemented(vec_typeof(x), "vec_is_vector")
}
#' @export
vec_is_vector.default <- function(x) {
  # All S3 objects except those wrapping atomic vectors are treated as
  # scalars by default
  is_atomic(x)
}

stop_unimplemented <- function(type, fn) {
  abort("Internal error: Unimplemented vctrs type `{ type }` in `{ fn }`")
}

vec_typeof <- function(x) {
  .Call(vctrs_typeof, x)
}
