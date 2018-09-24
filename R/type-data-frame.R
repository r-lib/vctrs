new_data_frame <- function(x = list(), n = NULL, ..., class = character()) {
  stopifnot(is.list(x))
  n <- n %||% df_length(x)
  stopifnot(is.integer(n), length(n) == 1L)

  # names() should always be a character vector, but we can't enforce that
  # because as.data.frame() returns a data frame with NULL names to indicate
  # that outer names should be used
  if (length(x) == 0) {
    names(x) <- character()
  }

  structure(
    x,
    ...,
    class = c(class, "data.frame"),
    row.names = .set_row_names(n)
  )
}

# Light weight constructor used for tests - avoids having to repeatedly do
# stringsAsFactors = FALSE etc. Should not be used in internal code as is
# not a real helper as it lacks value checks.
data_frame <- function(...) {
  cols <- list(...)
  new_data_frame(cols)
}

#' @export
vec_restore.data.frame <- function(x, to) {
  # Copy attribute, preserving existing names & recreating rownames
  attr_to <- attributes(to)
  attr_to[["names"]] <- names(x)
  attr_to[["row.names"]] <- .set_row_names(df_length(x))
  attributes(x) <- attr_to

  x
}

#' @export
vec_ptype_full.data.frame <- function(x) {
  if (length(x) == 0) {
    return(paste0(class(x)[[1]], "<>"))
  } else if (length(x) == 1) {
    return(paste0(class(x)[[1]], "<", names(x), ":", vec_ptype_full(x[[1]]), ">"))
  }

  # Needs to handle recursion with indenting
  types <- map_chr(x, vec_ptype_full)
  needs_indent <- grepl("\n", types)
  types[needs_indent] <- map(types[needs_indent], function(x) indent(paste0("\n", x), 4))

  names <- paste0("  ", format(names(x)))

  paste0(
    class(x)[[1]], "<\n",
    paste0(names, ": ", types, collapse = "\n"),
    "\n>"
  )
}

#' @export
vec_ptype_abbr.data.frame <- function(x) {
  paste0("df", vec_ptype_shape(x))
}

# Coercion ----------------------------------------------------------------

#' @rdname vec_type2
#' @export vec_type2.data.frame
#' @method vec_type2 data.frame
#' @export
vec_type2.data.frame <- function(x, y) UseMethod("vec_type2.data.frame", y)
#' @method vec_type2.data.frame data.frame
#' @export
vec_type2.data.frame.data.frame <- function(x, y) {
  df <- df_col_type2(x, y)

  new_data_frame(df, n = 0L)
}
#' @method vec_type2.data.frame default
#' @export
vec_type2.data.frame.default <- function(x, y) stop_incompatible_type(x, y)

# Cast --------------------------------------------------------------------

#' @rdname vec_cast
#' @export vec_cast.data.frame
#' @method vec_cast data.frame
#' @export
vec_cast.data.frame <- function(x, to) {
  UseMethod("vec_cast.data.frame")
}
#' @export
#' @method vec_cast.data.frame NULL
vec_cast.data.frame.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.data.frame data.frame
vec_cast.data.frame.data.frame <- function(x, to) {
  df <- df_col_cast(x, to)
  vec_restore(df, to)
}
#' @export
#' @method vec_cast.data.frame default
vec_cast.data.frame.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

# Helpers -----------------------------------------------------------------

df_length <- function(x) {
  if (length(x) > 0) {
    length(x[[1]])
  } else {
    0L
  }
}

df_col_type2 <- function(x, y) {
  x <- vec_data(x)
  y <- vec_data(y)
  names <- set_partition(names(x), names(y))

  # Find types
  if (length(names$both) > 0) {
    common_types <- map2(x[names$both], y[names$both], vec_type2)
  } else {
    common_types <- list()
  }
  only_x_types <- map(x[names$only_x], vec_subset, 0L)
  only_y_types <- map(y[names$only_y], vec_subset, 0L)

  # Combine and restore order
  out <- c(common_types, only_x_types, only_y_types)
  out[c(names(x), names$only_y)]
}

df_col_cast <- function(x, to) {
  n <- vec_length(x)
  x <- vec_data(x)

  # Coerce common columns
  common <- intersect(names(x), names(to))
  x[common] <- map2(x[common], to[common], vec_cast)

  # Add new columns
  from_type <- setdiff(names(to), names(x))
  x[from_type] <- map(to[from_type], vec_na, n = n)

  # Warn about dropped columns
  dropped <- setdiff(names(x), names(to))
  if (length(dropped) > 0 ) {
    warn_lossy_cast(
      x, to,
      details = inline_list("Dropped variables: ", dropped, quote = "`")
    )
  }

  x[names(to)]
}
