# print -------------------------------------------------------------------

#' `print()` and `str()` generics.
#'
#' These are constructed to be more easily extensible since you can override
#' the `_header()`, `_data()` or `_footer()` components individually. The
#' default methods are built on top of `format()`.
#'
#' @param x A vector
#' @param ... Additional arguments passed on to methods. See [print()] and
#'   [str()] for commonly used options
#' @keywords internal
#' @export
obj_print <- function(x, ...) {
  obj_print_header(x, ...)
  obj_print_data(x, ...)
  obj_print_footer(x, ...)
  invisible(x)
}

#' @export
#' @rdname obj_print
obj_print_header <- function(x, ...) {
  UseMethod("obj_print_header")
}

#' @export
obj_print_header.default <- function(x, ...) {
  cat_line("<", vec_ptype_full(x), "[", vec_size(x), "]>")
  invisible(x)
}

#' @export
#' @rdname obj_print
obj_print_data <- function(x, ...) {
  UseMethod("obj_print_data")
}

#' @export
obj_print_data.default <- function(x, ...) {
  if (length(x) == 0) {
    return(invisible(x))
  }

  out <- stats::setNames(format(x), names(x))
  print(out, quote = FALSE)

  invisible(x)
}

#' @export
#' @rdname obj_print
obj_print_footer <- function(x, ...) {
  UseMethod("obj_print_footer")
}

#' @export
obj_print_footer.default <- function(x, ...) {
  invisible(x)
}


# str ---------------------------------------------------------------------

#' @export
#' @rdname obj_print
obj_str <- function(x, ...) {
  obj_str_header(x, ...)
  obj_str_data(x, ...)
  obj_str_footer(x, ...)
}

#' @export
#' @rdname obj_print
obj_str_header <- function(x, ...) {
  UseMethod("obj_str_header")
}

#' @export
obj_str_header.default <- function(x, ...) {
  invisible(x)
}

#' @export
#' @rdname obj_print
obj_str_data <- function(x, ...) {
  UseMethod("obj_str_data")
}

#' @export
obj_str_data.default <- function(x, ...) {
  if (is.list(x)) {
    obj_str_recursive(x, ...)
  } else {
    obj_str_leaf(x, ...)
  }
}

obj_str_recursive <- function(x, ..., indent.str = "", nest.lev = 0) {
  if (nest.lev != 0L) {
    cat(" ")
  }
  cat_line(glue::glue("{vec_ptype_abbr(x)} [1:{vec_size(x)}] "))

  utils::str(
    vec_data(x),
    no.list = TRUE,
    ...,
    nest.lev = nest.lev + 1L,
    indent.str = indent.str
  )
}

obj_str_leaf <- function(x, ..., indent.str = "", width = getOption("width")) {
  width <- width - nchar(indent.str) - 2

  # Avoid spending too much time formatting elements that won't see
  length <- ceiling(width / 2)
  if (length(x) > length) {
    out <- x[seq2(1, length)]
  } else {
    out <- x
  }

  title <- glue::glue(" {vec_ptype_abbr(x)} [1:{length(x)}] ")
  cat_line(inline_list(title, format(out), width = width))

  invisible(x)
}

#' @export
#' @rdname obj_print
obj_str_footer <- function(x, ...) {
  UseMethod("obj_str_footer")
}

#' @export
obj_str_footer.default <- function(x, ..., indent.str = "", nest.lev = 0) {
  attr <- attributes(x)
  attr[["class"]] <- NULL
  attr[["names"]] <- NULL

  if (length(attr) == 0) {
    return(invisible(x))
  }

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
