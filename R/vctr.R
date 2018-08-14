#' vctr S3 class
#'
#' This abstract class provides a set of useful default methods that makes it
#' considerably easier to get started with a new S3 vector class.
#'
#' @section Recommended workflow:
#' 1. Start by creating a low-level constructor. It should be called
#'    `new_myclass()`, and should check the types (but not the values)
#'    of its inputs.
#'
#' 1. Depending on the class create either a helper `myclass()`, a coercer
#'    `as_myclass()`, or both. A helper should construct valid values then
#'    pass on to constructor. A coercer should either check values are correct,
#'    or rely on the helper. Avoid defining validation code in multiple
#'    places.
#'
#' 1. Define a useful `format.myclass()` method. This will give default
#'    `print()` and `as.character()` methods that should be adequate for
#'    most classes. Be warned: a good format method may be as much work
#'    as the rest of the class put together!
#'
#' 1. If attributes depend on the data, you'll need to provide a
#'   `reconstruct()` method that recomputes the values. If attributes are
#'   paramters of the type, provide a `vec_type_string()` method that displays
#'   them.
#'
#' The vctrs super class provides `as.data.frame()`, `[[`, `[`, `as.list()`,
#' and `rep()` methods; it is unlikely that you should need to redefine for
#' you own class. `[[<-` and `[<-` use `vec_cast()` to cast `value` to the
#' same type as `x`; they will work once you have fleshed out your `vec_type2()`
#' and `vec_cast()` methods.
#'
#' @param x Foundation of class. Must be a vector
#' @param ... Name-value pairs defining attributes
#' @param class Name of subclass.
#' @export
#' @keywords internal
new_vctr <- function(.data, ..., class) {
  if (!is_vector(.data)) {
    stop("`.data` must be a vector type", call. = FALSE)
  }

  structure(.data, ..., class = c(class, "vctr"))
}

# Printing ----------------------------------------------------------------

#' @export
print.vctr <- function(x, ...) {
  if (length(x) == 0) {
    cat_line("<", class(x)[[1]], "[0]>")
  } else {
    cat_line("<", class(x)[[1]], ">")
    print(format(x), quote = FALSE)
  }
  invisible(x)
}

#' @export
format.vctr <- function(x, ...) {
  stop(glue::glue("`format.{class(x)[[1]]}()` method not implemented"), call. = FALSE)
}

#' @export
as.character.vctr <- function(x, ...) {
  format(x, ...)
}

# Subsetting --------------------------------------------------------------

#' @export
vec_reconstruct.vctr <- function(new, old) {
  # safe because we prohibit names and dims
  attributes(new) <- attributes(old)
  new
}

#' @export
`[.vctr` <- function(x, i) {
  vec_reconstruct(NextMethod(), x)
}

#' @export
`[[.vctr` <- function(x, i) {
  vec_reconstruct(NextMethod(), x)
}

#' @export
as.list.vctr <- function(x, ...) {
  lapply(seq_along(x), function(i) x[[i]])
}

#' @export
rep.vctr <- function(x, ...) {
  vec_reconstruct(NextMethod(), x)
}

# Replacement -------------------------------------------------------------

#' @export
`[[<-.vctrs` <- function(x, i, value) {
  value <- vec_cast(value, x)
  NextMethod()
}

#' @export
`[<-.vctrs` <- function(x, i, value) {
  value <- vec_cast(value, x)
  NextMethod()
}

# Protection --------------------------------------------------------------

#' @export
`names<-.vctr` <- function(x, value) {
  stop("Must not set names() of ", class(x)[[1]], " vector", call. = FALSE)
}

#' @export
`dim<-.vctr` <- function(x, value) {
  stop("Must not set dim() of ", class(x)[[1]], " vector", call. = FALSE)
}

#' @export
`dimnames<-.vctr` <- function(x, value) {
  stop("Must not set dimnames() of ", class(x)[[1]], " vector", call. = FALSE)
}

# Data frame --------------------------------------------------------------

#' @export
as.data.frame.vctr <- function(x,
                               row.names = NULL,
                               optional = FALSE,
                               ...,
                               nm = paste(deparse(substitute(x), width.cutoff = 500L), collapse = " ")
                               ) {

  force(nm)
  cols <- list(x)
  if (!optional) {
    names(cols) <- nm
  }

  structure(
    cols,
    class = "data.frame",
    row.names = .set_row_names(length(x))
  )
}

# Helpers -----------------------------------------------------------------

# TODO: how does this interact with sloop::reconstruct?
vec_reconstruct <- function(new, old) {
  UseMethod("vec_reconstruct", old)
}

# This simple class is used for testing as defining methods inside
# a test does not work
new_hidden <- function(x) new_vctr(x, class = "hidden")
#' @export
format.hidden <- function(x, ...) rep("xxx", length(x))
