#' Find the prototype of a set of vectors
#'
#' `vec_type()` finds the prototype of a single vector.
#' `vec_type_common()` finds the common type of multiple vectors.
#' `vec_ptype()` nicely prints the common type of any number of
#' inputs, and is designed for interative exploration.
#'
#' `vec_type_common()` first finds the prototype of each input, then
#' finds the common type using [vec_type2()] and [Reduce()].
#'
#' @section Prototype:
#' A prototype is [size](vec_size) 0 vector containing attributes, but no
#' data. Generally, this is just `vec_slice(x, 0L)`, but some inputs
#' require special handling.
#'
#' For example, the prototype of logical vectors that only contain missing
#' values is the special [unspecified] type, which can be coerced to any
#' other 1d type. This allows bare `NA`s to represent missing values for
#' any 1d vector type.
#'
#' @param ...,x Vectors inputs
#' @param .ptype If `NULL`, the default, the output type is determined by
#'   computing the common type across all elements of `...`.
#'
#'   Alternatively, you can supply `.ptype` to give the output known type.
#'   If `getOption("vctrs.no_guessing")` is `TRUE` you must supply this value:
#'   this is a convenient way to make production code demand fixed types.
#' @return `vec_type()` and `vec_type_common()` return a prototype
#'   (a size-0 vector)
#' @export
#' @examples
#' # Unknown types ------------------------------------------
#' vec_ptype()
#' vec_ptype(NA)
#' vec_ptype(NULL)
#'
#' # Vectors ------------------------------------------------
#' vec_ptype(1:10)
#' vec_ptype(letters)
#' vec_ptype(TRUE)
#'
#' vec_ptype(Sys.Date())
#' vec_ptype(Sys.time())
#' vec_ptype(factor("a"))
#' vec_ptype(ordered("a"))
#'
#' # Matrices -----------------------------------------------
#' # The prototype of a matrix includes the number of columns
#' vec_ptype(array(1, dim = c(1, 2)))
#' vec_ptype(array("x", dim = c(1, 2)))
#'
#' # Data frames --------------------------------------------
#' # The prototype of a data frame includes the prototype of
#' # every column
#' vec_ptype(iris)
#'
#' # The prototype of multiple data frames includes the prototype
#' # of every column that in any data frame
#' vec_ptype(
#'   data.frame(x = TRUE),
#'   data.frame(y = 2),
#'   data.frame(z = "a")
#' )
vec_type <- function(x) {
  UseMethod("vec_type")
}

#' @export
vec_type.default <- function(x) {
  if (is_vector(x)) {
    vec_slice(x, 0L)
  } else {
    stop("`x` is not a vector", call. = FALSE)
  }
}

#' @export
vec_type.NULL <- function(x) {
  NULL
}

#' @export
vec_type.logical <- function(x) {
  if (is_unspecified(x)) {
    unspecified()
  } else {
    vec_slice(x, 0L)
  }
}

#' @export
vec_type.data.frame <- function(x) {
  cols <- map(x, vec_type)
  vec_restore(cols, x)
}

#' @export
#' @rdname vec_type
vec_type_common <- function(..., .ptype = NULL) {
  if (!is_partial(.ptype)) {
    return(vec_type(.ptype))
  }

  if (isTRUE(getOption("vctrs.no_guessing"))) {
    stop("strict mode is activated; you must supply complete .ptype", call. = FALSE)
  }

  args <- compact(list2(.ptype, ...))
  if (length(args) == 0) {
    ptype <- NULL
  } else if (length(args) == 1) {
    ptype <- vec_type(args[[1]])
  } else {
    ptypes <- map(args, vec_type)
    ptype <- reduce(ptypes, vec_type2)
  }

  vec_type_finalise(ptype)
}

#' @export
#' @rdname vec_type
vec_ptype <- function(...) {
  args <- compact(list2(...))
  n <- length(args)
  if (n == 0) {
    cat_line("Prototype: NULL")
  } else if (n == 1) {
    cat_line("Prototype: ", vec_ptype_full(args[[1]]))
  } else {
    in_types <- map(args, vec_type)
    out_types <- vector("list", length(in_types))
    out_types[[1]] <- in_types[[1]]
    for (i in seq2(2, n)) {
      out_types[[i]] <- vec_type2(out_types[[i - 1]], in_types[[i]])
    }

    in_full <- paste0("<", map_chr(in_types, vec_ptype_full), ">")
    out_full <- paste0("<", map_chr(out_types, vec_ptype_full), ">")

    out <- cbind(
      n = paste0(seq(0, n - 1), ". "),
      lhs = c("", out_full[-n]),
      comma = " , ",
      rhs = in_full,
      equals = " = ",
      res = c(in_full[[1]], out_full[-1])
    )
    out <- t(apply(out, 1, pad_height))
    out <- apply(out, 2, pad_width)
    # apply(out, 1:2, block_size)

    out[, "lhs"] <- parens(out[, "lhs"])
    out[, "rhs"] <- parens(out[, "rhs"], FALSE)

    lines <- strsplit(out, "\n")
    dim(lines) <- dim(out)

    steps <- apply(lines, 1, function(x) do.call(cbind, x))
    if (is.list(steps)) {
      step_lines <- unlist(lapply(steps, function(x) apply(x, 1, paste0, collapse = "")))
    } else {
      step_lines <- apply(steps, 2, paste0, collapse = "")
    }

    cat_line("Prototype: ", out_full[[n]])
    cat_line(step_lines)
  }

  invisible()
}

block_size <- function(x) {
  lines <- strsplit(x, "\n")[[1]]
  width <- unique(map_int(lines, nchar))
  paste0("[", length(lines), ", ", if (length(width) > 1) "?" else width, "]")
}

blank <- function(x) {
  paste0(rep(" ", nchar(x)), collapse = "")
}

parens <- function(x, left = TRUE) {
  x_lines <- strsplit(x, "\n")
  x_lines <- map(x_lines, paren, left = left)
  map_chr(x_lines, paste0, collapse = "\n")
}

paren <- function(x, left = TRUE) {
  if (length(x) <= 1) {
    if (left) {
      paste0("( ", x)
    } else {
      paste0(x, " )")
    }
  } else {
    if (left) {
      paste0(c("\u250c ", rep("\u2502 ", length(x) - 2), "\u2514 "), x)
    } else {
      paste0(format(x), c(" \u2510", rep(" \u2502", length(x) - 2), " \u2518"))
    }
  }
}

pad_height <- function(x) {
  pad <- function(x, n) c(x, rep("", n - length(x)))

  lines <- strsplit(x, "\n")
  height <- max(map_int(lines, length))
  lines <- map(lines, pad, height)
  map_chr(lines, paste0, "\n", collapse = "")
}

pad_width <- function(x) {
  lines <- strsplit(x, "\n", fixed = TRUE)

  # fix up strsplit bug
  n <- map_int(lines, length)
  lines[n == 0] <- ""

  width <- max(unlist(map(lines, nchar)))
  lines <- map(lines, format, width = width)
  map_chr(lines, paste, collapse = "\n")
}
