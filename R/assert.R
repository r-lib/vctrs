#' Assert an argument has known prototype and/or size
#'
#' @description
#'
#' * `vec_is()` is a predicate that checks if its input is a vector that
#'   conforms to a prototype and/or a size.
#'
#' * `vec_assert()` throws an error when the input is not a vector or
#'   doesn't conform.
#'
#' @section Scalars and vectors:
#'
#' Informally, a vector is a collection that makes sense to use as
#' column in a data frame. An object is a vector if one of the
#' following conditions hold:
#'
#' - A [vec_proxy()] method is implemented for the class of the
#'   object.
#'
#' - The [base type][typeof] of the object is atomic: `"logical"`,
#'   `"integer"`, `"double"`, `"complex"`, `"character"`, `"raw"`
#'
#' - The object is a [data.frame].
#'
#' - The base type is `"list"`, and one of:
#'     - The object is a bare `"list"` without a `"class"` attribute.
#'     - The object explicitly inherits from `"list"`. That is, the
#'       `"class"` attribute contains `"list"` and `inherits(x,
#'       "list")` is `TRUE`.
#'
#' Otherwise an object is treated as scalar and cannot be used as a
#' vector. In particular:
#'
#' - `NULL` is not a vector.
#' - S3 lists like `lm` objects are treated as scalars by default.
#' - Objects of type [expression] are not treated as vectors.
#' - Support for S4 vectors is currently limited to objects that
#'   inherit from an atomic type.
#' - Subclasses of [data.frame] that *append* their class to the `"class"`
#'   attribute are not treated as vectors. If you inherit from an S3 class,
#'   always prepend your class to the `"class"` attribute for correct dispatch.
#'
#' @section Error types:
#'
#' `vec_is()` never throws.
#' `vec_assert()` throws the following errors:
#'
#' * If the input is not a vector, an error of class
#'   `"vctrs_error_scalar_type"` is raised.
#'
#' * If the prototype doesn't match, an error of class
#'   `"vctrs_error_assert_ptype"` is raised.
#'
#' * If the size doesn't match, an error of class
#' `"vctrs_error_assert_size"` is raised.
#'
#' Both errors inherit from `"vctrs_error_assert"`.
#'
#' @inheritParams rlang::args_error_context
#'
#' @param x A vector argument to check.
#' @param ptype Prototype to compare against. If the prototype has a
#'   class, its [vec_ptype()] is compared to that of `x` with
#'   `identical()`. Otherwise, its [typeof()] is compared to that of
#'   `x` with `==`.
#' @param size A single integer size against which to compare.
#' @param arg Name of argument being checked. This is used in error
#'   messages. The label of the expression passed as `x` is taken as
#'   default.
#'
#' @return `vec_is()` returns `TRUE` or `FALSE`. `vec_assert()` either
#'   throws a typed error (see section on error types) or returns `x`,
#'   invisibly.
#' @export
vec_assert <- function(x,
                       ptype = NULL,
                       size = NULL,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!vec_is_vector(x)) {
    stop_scalar_type(x, arg, call = call)
  }

  if (!is_null(ptype)) {
    ptype <- vec_ptype(ptype)
    x_type <- vec_ptype_finalise(vec_ptype(x))
    if (!is_same_type(x_type, ptype)) {
      msg <- vec_assert_type_explain(x_type, ptype, arg)
      abort(
        msg,
        class = c("vctrs_error_assert_ptype", "vctrs_error_assert"),
        required = ptype,
        actual = x_type,
        call = call
      )
    }
  }

  if (!is_null(size)) {
    size <- vec_cast(size, integer(), x_arg = "size")

    n_size <- length(size)
    if (n_size != 1L) {
      abort(glue::glue("`size` must be length 1, not length {n_size}."))
    }

    x_size <- vec_size(x)
    if (!identical(x_size, size)) {
      stop_assert_size(
        x_size,
        size,
        arg,
        call = call
      )
    }
  }

  invisible(x)
}

# Also thrown from C
stop_assert_size <- function(actual,
                             required,
                             arg,
                             call = caller_env()) {
  if (!nzchar(arg)) {
    arg <- "Input"
  } else {
    arg <- glue::backtick(arg)
  }

  message <- glue::glue("{arg} must have size {required}, not size {actual}.")

  stop_assert(
    message,
    class = "vctrs_error_assert_size",
    actual = actual,
    required = required,
    call = call
  )
}

stop_assert <- function(message = NULL,
                        class = NULL,
                        ...,
                        call = caller_env()) {
  stop_vctrs(
    message,
    class = c(class, "vctrs_error_assert"),
    ...,
    call = call
  )
}

#' @rdname vec_assert
#' @export
vec_is <- function(x, ptype = NULL, size = NULL) {
  if (!vec_is_vector(x)) {
    return(FALSE)
  }

  if (!is_null(ptype)) {
    ptype <- vec_ptype(ptype)
    x_type <- vec_ptype_finalise(vec_ptype(x))
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

#' Is object a vector?
#' @noRd
#'
#' @description
#'
#' Returns `TRUE` if:
#'
#' * `x` is an atomic, whether it has a class or not.
#' * `x` is a bare list without class.
#' * `x` implements [vec_proxy()].
#'
#' S3 lists are thus treated as scalars unless they implement a proxy.
vec_is_vector <- function(x) {
  .Call(vctrs_is_vector, x)
}

#' Is the object a list?
#'
#' @description
#' `vec_is_list()` tests if `x` is considered a list in the vctrs sense. It
#' returns `TRUE` if:
#'
#' * `x` is a bare list with no class.
#' * `x` is a list explicitly inheriting from `"list"`.
#'
#' `vec_is_list_of_vectors()` checks in addition that all elements of
#' `x` are vectors.
#'
#' `vec_check_list()` and `vec_check_list_of_vectors()` throw a type
#' error if the input is not a list as defined by `vec_is_list()` and
#' `vec_is_list_of_vectors()` respectively.
#'
#' @inheritParams rlang::args_error_context
#' @param x An object.
#'
#' @details
#' Notably, data frames and S3 record style classes like POSIXlt are not
#' considered lists.
#'
#' @export
#' @examples
#' vec_is_list(list())
#' vec_is_list(list_of(1))
#'
#' vec_is_list(data.frame())
vec_is_list <- function(x) {
  .Call(vctrs_is_list, x)
}
#' @rdname vec_is_list
#' @export
vec_is_list_of_vectors <- function(x) {
  .Call(ffi_is_list_of_vectors, x)
}

#' @rdname vec_is_list
#' @export
vec_check_list <- function(x,
                           arg = caller_arg(x),
                           call = caller_env()) {
  .Call(ffi_check_list, x, environment())
}
#' @rdname vec_is_list
#' @export
vec_check_list_of_vectors <- function(x,
                                      arg = caller_arg(x),
                                      call = caller_env()) {
  invisible(.Call(ffi_check_list_of_vectors, x, environment()))
}
stop_non_list_type <- function(x, arg, call) {
  cli::cli_abort(
    "{.arg {arg}} must be a list, not {friendly_type_of(x)}.",
    call = call
  )
}

is_same_type <- function(x, ptype) {
  if (is_partial(ptype)) {
    env <- environment()
    ptype <- tryCatch(
      vctrs_error_incompatible_type = function(...) return_from(env, FALSE),
      vec_ptype_common(x, ptype)
    )
  }

  x <- vec_slice(x, integer())
  ptype <- vec_slice(ptype, integer())

  # FIXME: Remove row names for matrices and arrays, and handle empty
  # but existing dimnames
  x <- vec_set_names(x, NULL)
  ptype <- vec_set_names(ptype, NULL)

  identical(x, ptype)
}

vec_assert_type_explain <- function(x, type, arg) {
  arg <- str_backtick(arg)
  x <- paste0("<", vec_ptype_full(x), ">")
  type <- paste0("<", vec_ptype_full(type), ">")

  intro <- paste0(arg, " must be a vector with type")
  intro <- layout_type(intro, type)

  outro <- paste0("Instead, it has type")
  outro <- layout_type(outro, x)

  paste_line(
    !!!intro,
    if (str_is_multiline(intro)) "",
    !!!outro
  )
}

layout_type <- function(start, type) {
  if (str_is_multiline(type)) {
    paste_line(
      paste0(start, ":"),
      "",
      paste0("  ", indent(type, 2))
    )
  } else {
    paste0(start, " ", type, ".")
  }
}
