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
    msg <- glue::glue("`{ arg }` must be a vector, not { friendly_type_of(x) }")
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
#' A vector is a collection of values like a logical vector, a factor,
#' or a date vector. Vectors can be [sliced][vec_slice] and can be
#' assigned as data frame columns. The opposite of a vector is a
#' scalar. For example, a function is not a vector, and neither is a
#' linear model. An environment is a collection of values but it
#' cannot be a data frame column and so is not a vector.
#'
#' Compared to base R our definition is both more generic and
#' constrained. In base R, anything that is an atomic vector or a list
#' is a vector, even a model fit object ([base::is.vector()] returns
#' `TRUE` for these objects). For the most part, functions calls
#' implement the vector interface and can generally be treated as
#' vectors. In vctrs, all of these objects are scalars.
#'
#' @section Dispatch:
#'
#' This function is internally generic:
#'
#' * Atomic vectors are always vectors, regardless of their class.
#' * Bare lists are vectors.
#' * Data frames are always vectors.
#' * Other objects like functions and calls are always scalar.
#' * S3 objects built on top of lists are scalar by default.
#'
#' The behaviour of `vec_is_vector()` can be overridden for S3 objects
#' built on top of lists by providing a method. If you build your
#' class on top of [`vctrs_vctr`][new_vctr] (which we recommend), you
#' don't need to override it.
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
  .Call(vctrs_is_vector, x)
}
vec_is_vector_dispatch <- function(x) {
  UseMethod("vec_is_vector")
}
#' @export
vec_is_vector.default <- function(x) {
  # All S3 objects except those wrapping atomic vectors are treated as
  # scalars by default
  is_atomic(x)
}

vec_typeof <- function(x) {
  .Call(vctrs_typeof, x)
}
