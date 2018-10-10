#' Data frame class
#'
#' A `data.frame` [data.frame()] is a list with "row.names" attribute. Each
#' element of the list must be named, and of the same length. These functions
#' help the base data.frame classes fit in to the vctrs type system by
#' providing constructors, coercion functions, and casting functions.
#'
#' @param x A named list of equal-length vectors. The lengths are not
#'   checked; it is responsibility of the caller to make sure they are
#'   equal.
#' @param n Number of rows. If `NULL`, will be computed from the length of
#'   the first element of `x`.
#' @param ...,class Additional arguments for creating subclasses.
#' @export
#' @keywords internal
#' @examples
#' new_data_frame(list(x = 1:10, y = 10:1))
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

#' @rdname new_data_frame
#' @export vec_type2.data.frame
#' @method vec_type2 data.frame
#' @export
vec_type2.data.frame <- function(x, y) UseMethod("vec_type2.data.frame", y)
#' @method vec_type2.data.frame data.frame
#' @export
vec_type2.data.frame.data.frame <- function(x, y) df_col_type2(x, y)
#' @method vec_type2.data.frame default
#' @export
vec_type2.data.frame.default    <- function(x, y) stop_incompatible_type(x, y)

# Cast --------------------------------------------------------------------

#' @rdname new_data_frame
#' @export vec_cast.data.frame
#' @method vec_cast data.frame
#' @export
vec_cast.data.frame <- function(x, to) {
  UseMethod("vec_cast.data.frame")
}
#' @export
#' @method vec_cast.data.frame data.frame
vec_cast.data.frame.data.frame <- function(x, to) df_col_cast(x, to)
#' @export
#' @method vec_cast.data.frame default
vec_cast.data.frame.default    <- function(x, to) stop_incompatible_cast(x, to)

# Helpers -----------------------------------------------------------------

df_length <- function(x) {
  if (length(x) > 0) {
    length(x[[1]])
  } else {
    0L
  }
}

df_col_type2 <- function(x, y) {
  # Avoid expensive [.data.frame
  x_raw <- vec_data(vec_slice(x, 0L))
  y_raw <- vec_data(vec_slice(y, 0L))

  # Find types
  names <- set_partition(names(x), names(y))
  if (length(names$both) > 0) {
    common_types <- map2(x_raw[names$both], y_raw[names$both], vec_type2)
  } else {
    common_types <- list()
  }
  only_x_types <- x_raw[names$only_x]
  only_y_types <- y_raw[names$only_y]

  # Combine and restore order and type
  out <- c(common_types, only_x_types, only_y_types)
  out <- out[c(names(x), names$only_y)]
  vec_restore(out, x)
}

df_col_cast <- function(x, to) {
  # Avoid expensive [.data.frame method
  out <- vec_data(x)

  # Coerce common columns
  common <- intersect(names(x), names(to))
  out[common] <- map2(out[common], to[common], vec_cast)

  # Add new columns
  from_type <- setdiff(names(to), names(x))
  out[from_type] <- map(to[from_type], vec_na, n = vec_size(x))

  # Drop extra columns
  out <- out[names(to)]
  extra <- setdiff(names(x), names(to))
  if (length(extra) > 0 ) {
    warn_lossy_cast(
      x, to,
      details = inline_list("Dropped variables: ", extra, quote = "`")
    )
  }

  vec_restore(out, to)
}
