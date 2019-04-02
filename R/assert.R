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
#' * If the size doesn't match, an error of class
#' `"vctrs_error_assert_size"` is raised.
#'
#' Both errors inherit from `"vctrs_error_assert"`.
#'
#' @param x A vector argument to check.
#' @param ptype Prototype to compare against. If the prototype has a
#'   class, its [vec_type()] is compared to that of `x` with
#'   `identical()`. Otherwise, its [typeof()] is compared to that of
#'   `x` with `==`.
#' @param size Size to compare against
#' @param arg Name of argument being checked. This is used in error
#'   messages. The label of the expression passed as `x` is taken as
#'   default.
#'
#' @return `vec_is()` returns `TRUE` or `FALSE`. `vec_assert()` either
#'   throws a typed error (see section on error types) or returns `x`,
#'   invisibly.
#' @export
vec_assert <- function(x, ptype = NULL, size = NULL, arg = as_label(substitute(x))) {
  if (!vec_is_vector(x)) {
    msg <- glue::glue("`{ arg }` must be a vector, not { friendly_type_of(x) }")
    abort(msg, "vctrs_error_assert_scalar", actual = x)
  }

  if (!is_null(ptype)) {
    ptype <- vec_type(ptype)
    x_type <- vec_type(x)
    if (!is_same_type(x_type, ptype)) {
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
    if (!is_same_type(x_type, ptype)) {
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

# Bare prototypes act as partial types. Only the SEXPTYPE is checked.
is_same_type <- function(x, ptype) {
  if (!is.object(x) || !is.object(ptype)) {
    typeof(x) == typeof(ptype)
  } else {
    identical(x, ptype)
  }
}
