# print -------------------------------------------------------------------

#' `print() and `str()` generics.
#'
#' These are contructed to be more easily extensible since you can override
#' the `_header()`, `_data()` or `_footer()` components individually. The
#' default methods are built on top of `format()`.
#'
#' @param x A vector
#' @param ... Additional arguments passed on to methods. See [print()] and
#'   [str()] for commonly used options
#' @keywords internal
#' @export
vec_print <- function(x, ...) {
  vec_print_header(x, ...)
  vec_print_data(x, ...)
  vec_print_footer(x, ...)
  invisible(x)
}

#' @export
#' @rdname vec_print
vec_print_header <- function(x, ...) {
  UseMethod("vec_print_header")
}

#' @export
vec_print_header.default <- function(x, ...) {
  cat_line("<", vec_ptype_full(x), "[", length(x), "]>")
  invisible(x)
}

#' @export
#' @rdname vec_print
vec_print_data <- function(x, ...) {
  UseMethod("vec_print_data")
}

#' @export
vec_print_data.default <- function(x, ...) {
  if (length(x) == 0)
    return()

  out <- stats::setNames(format(x), names(x))
  print(out, quote = FALSE)

  invisible(x)
}

#' @export
#' @rdname vec_print
vec_print_footer <- function(x, ...) {
  UseMethod("vec_print_footer")
}

#' @export
vec_print_footer.default <- function(x, ...) {
  invisible(x)
}


# str ---------------------------------------------------------------------

#' @export
#' @rdname vec_print
vec_str <- function(x, ...) {
  vec_str_header(x, ...)
  vec_str_data(x, ...)
  vec_str_footer(x, ...)
}

#' @export
#' @rdname vec_print
vec_str_header <- function(x, ...) {
  UseMethod("vec_str_header")
}

#' @export
vec_str_header.default <- function(x, ...) {
  invisible(x)
}

#' @export
#' @rdname vec_print
vec_str_data <- function(x, ...) {
  UseMethod("vec_str_data")
}

#' @export
vec_str_data.default <- function(x, ...) {
  if (is.list(x)) {
    vec_str_recursive(x, ...)
  } else {
    vec_str_leaf(x, ...)
  }
}

vec_str_recursive <- function(x, ...,
                              indent.str = "",
                              nest.lev = 0) {

  if (nest.lev != 0L)
    cat(" ")
  cat_line(glue::glue("{vec_ptype_abbr(x)} [1:{vec_obs(x)}] "))

  utils::str(
    vec_data(x),
    no.list = TRUE,
    ...,
    nest.lev = nest.lev + 1L,
    indent.str = indent.str
  )
}

vec_str_leaf <- function(x, ...,
                         indent.str = "",
                         width = getOption("width")) {
  width <- width - nchar(indent.str) - 2

  # Avoid spending too much time formatting elements that won't see
  length <- ceiling(width / 2)
  if (length(x) > length) {
    out <- x[1:length]
  } else {
    out <- x
  }

  title <- glue::glue(" {vec_ptype_abbr(x)} [1:{length(x)}] ")
  cat_line(inline_list(title, format(out), width = width))

  invisible(x)
}

#' @export
#' @rdname vec_print
vec_str_footer <- function(x, ...) {
  UseMethod("vec_str_footer")
}

#' @export
vec_str_footer.default <- function(x, ...,
                                   indent.str = "",
                                   nest.lev = 0) {
  attr <- attributes(x)
  attr[["class"]] <- NULL
  attr[["names"]] <- NULL

  if (length(attr) == 0)
    return(invisible(x))

  if (!is.list(x)) {
    indent.str <- paste0(" ", indent.str)
  }

  utils::str(
    attr,
    no.list = TRUE,
    ...,
    comp.str = "@ ",
    nest.lev = nest.lev + 1L,
    indent.str = indent.str
  )

  invisible(x)
}

