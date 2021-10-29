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
#' @param max The maximum number of items to print, defaults to
#'   `getOption("print.max")`.
#' @keywords internal
#' @export
obj_print <- function(x, ..., max = NULL) {
  max <- local_max_print(max)
  obj_print_header_dispatch(x, ..., max = max)
  obj_print_data_dispatch(x, ..., max = max)
  obj_print_footer_dispatch(x, ..., max = max)
  invisible(x)
}

#' @export
#' @rdname obj_print
obj_print_header <- function(x, ..., max = NULL) {
  max <- local_max_print(max)
  return(obj_print_header_dispatch(x, ..., max = max))
  UseMethod("obj_print_header")
}
obj_print_header_dispatch <- function(x, ..., max) {
  UseMethod("obj_print_header")
}

#' @export
obj_print_header.default <- function(x, ...) {
  cat_line("<", vec_ptype_full(x), "[", vec_size(x), "]>")
  invisible(x)
}

#' @export
#' @rdname obj_print
obj_print_data <- function(x, ..., max = NULL) {
  max <- local_max_print(max)
  return(obj_print_data_dispatch(x, ..., max = max))
  UseMethod("obj_print_data")
}
obj_print_data_dispatch <- function(x, ..., max) {
  UseMethod("obj_print_data")
}

#' @export
obj_print_data.default <- function(x, ..., max) {
  if (!vec_is(x)) {
    print(x, quote = FALSE)
    return(invisible(x))
  }

  if (vec_size(x) > max) {
    x_max <- vec_slice(x, seq_len(max))
  } else {
    x_max <- x
  }

  if (vec_size(x_max) == 0) {
    return(invisible(x))
  }

  out <- stats::setNames(format(x_max), names(x_max))
  print(out, quote = FALSE)

  invisible(x)
}

#' @export
#' @rdname obj_print
obj_print_footer <- function(x, ..., max = NULL) {
  max <- local_max_print(max)
  return(obj_print_footer_dispatch(x, ..., max = max))
  UseMethod("obj_print_footer")
}
obj_print_footer_dispatch <- function(x, ..., max) {
  UseMethod("obj_print_footer")
}

#' @export
obj_print_footer.default <- function(x, ..., max) {
  if (!vec_is(x)) {
    return(invisible(x))
  }

  delta <- vec_size(x) - max
  if (delta > 0) {
    max_print <- attr(max, "max_print")
    if (is.null(max_print)) {
      max_print <- getOption("max.print")
    }

    cat_line("... and ", big_mark(delta), " more")
    if (max < max_print) {
      cat_line("Set `max` to a larger value to show all items.")
    } else {
      cat_line("Set `options(max.print = )` to a larger value to show all items.")
    }
  }
  invisible(x)
}

local_max_print <- function(max, frame = parent.frame()) {
  max_print <- getOption("max.print")
  if (is.null(max)) {
    max <- max_print
  }

  stopifnot(is_integerish(max, 1L, finite = TRUE), max >= 0)
  if (max > max_print) {
    # Avoid truncation in case we're forwarding to print()
    local_options(max.print = max, .frame = frame)
  }

  structure(max, max_print = max_print)
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

obj_str_recursive <- function(x, ...,
                              indent.str = "",
                              nest.lev = 0) {

  if (nest.lev != 0L)
    cat(" ")
  cat_line(glue::glue("{vec_ptype_abbr(x)} [1:{vec_size(x)}] "))

  utils::str(
    vec_data(x),
    no.list = TRUE,
    ...,
    nest.lev = nest.lev + 1L,
    indent.str = indent.str
  )
}

obj_str_leaf <- function(x, ...,
                         indent.str = "",
                         width = getOption("width")) {
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
obj_str_footer.default <- function(x, ...,
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
