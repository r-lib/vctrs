#' Combine multiple data frames into a single data frames
#'
#' This pair of functions binds together data frames (and vectors), either
#' row-wise or column-wise. Row-binding creates a data frame with common type
#' across all arguments. Column-binding creates a data frame with common length
#' across all arguments.
#'
#' @param ... Data frames or vectors.
#'
#'   `vec_rbind()` ignores names. `vec_cbind()` preserves outer names,
#'   combining with inner names if also present.
#'
#'   `NULL` inputs are silently ignored. Empty (e.g. zero row) inputs
#'   will not appear in the output, but will affect the derived `.type`.
#' @inheritParams vec_c
#' @return A data frame, or subclass of data frame.
#'
#'   If `...` is a mix of different data frame subclases, `vec_type2()`
#'   will be used to determine the output type. For `vec_rbind()`, this
#'   will determine the type of the container and the type of each column;
#'   for `vec_cbind()` it only determines the type of the output container.
#'   If there are no non-`NULL` inputs, the result will be `data.frame()`.
#' @seealso [vec_c()] for combining 1d vectors.
#' @examples
#' # row binding -----------------------------------------
#'
#' # common columns are coerced to common type
#' vec_rbind(
#'   data.frame(x = 1),
#'   data.frame(x = FALSE)
#' )
#'
#' # unique columns are filled with NAs
#' vec_rbind(
#'   data.frame(x = 1),
#'   data.frame(y = "x")
#' )
#'
#' # null inputs are ignored
#' vec_rbind(
#'   data.frame(x = 1),
#'   NULL,
#'   data.frame(x = 2)
#' )
#'
#' # bare vectors are treated as rows
#' vec_rbind(
#'   c(x = 1, y = 2),
#'   c(x = 3)
#' )
#'
#' # default names will be supplied if arguments are not named
#' vec_rbind(
#'   1:2,
#'   1:3,
#'   1:4
#' )
#'
#' # column binding --------------------------------------
#'
#' # each input is recycled to have common length
#' vec_cbind(
#'   data.frame(x = 1),
#'   data.frame(y = 1:3)
#' )
#'
#' # bare vectors are treated as columns
#' vec_cbind(
#'   data.frame(x = 1),
#'   y = letters[1:3]
#' )
#'
#' # outer names are combined with inner names
#' vec_cbind(
#'   x = data.frame(a = 1, b = 2),
#'   y = 1
#' )
#'
#' # duplicate names are flagged
#' vec_cbind(x = 1, x = 2)
#'
#' @name vec_bind
NULL

#' @export
#' @rdname vec_bind
vec_rbind <- function(..., .type = NULL) {
  args <- list2(...)
  tbls <- map(args, as_df_row)
  type <- find_type(tbls, .type = .type)

  if (is.null(type))
    return(data.frame())

  ns <- map_int(tbls, vec_length)
  out <- vec_rep(type, sum(ns))
  rownames(out) <- NULL

  pos <- 1
  for (i in seq_along(ns)) {
    n <- ns[[i]]
    if (n == 0L)
      next

    out[pos:(pos + n - 1), ] <- vec_cast(tbls[[i]], to = type)
    pos <- pos + n
  }

  out
}

#' @export
#' @rdname vec_bind
#' @param .nrow If, `NULL`, the default, will determing the number of
#'   rows in `vec_cbind()` output by using the standard recycling rules.
#'
#'   Alternatively, specify the desired number of rows, and any inputs
#'   of length 1 will be recycled appropriately.
vec_cbind <- function(..., .type = NULL, .nrow = NULL) {
  args <- list2(...)

  # container type: common type of all (data frame) inputs
  # compute early so we can fail fast
  tbl_empty <- map(args, function(x) {
    if (is.data.frame(x))
      x[0]
  })
  out <- find_type(tbl_empty, .type = .type[0]) %||% data.frame()

  # container size: common length of all inputs
  nrow <- find_nrow(args, .nrow = .nrow)
  args <- map(args, recycle, n = nrow)

  # convert input to columns and prepare output containers
  tbls <- map2(args, names2(args), as_df_col)
  arg_cols <- map_int(tbls, length)
  ncol <- sum(arg_cols)

  cols <- vec_rep(list(), ncol)
  names <- vec_rep(character(), ncol)

  pos <- 1
  for (i in seq_along(arg_cols)) {
    n <- arg_cols[[i]]
    if (n == 0L)
      next

    cols[pos:(pos + n - 1)] <- tbls[[i]]
    names[pos:(pos + n - 1)] <- names(tbls[[i]]) %||% rep("", n)
    pos <- pos + n
  }

  # Need to document these assumptions, or better, move into
  # a generic
  attr(out, "row.names") <- .set_row_names(nrow)
  out[seq_along(cols)] <- cols
  names(out) <- tibble::tidy_names(names)

  out
}

find_nrow <- function(x, .nrow = NULL) {
  if (!is.null(.nrow)) {
    .nrow
  } else {
    lengths <- map_int(x, vec_length)
    Reduce(recycle_length, lengths) %||% 0L
  }
}

recycle <- function(x, n) {
  nx <- vec_length(x)

  if (is.null(x) || nx == n) {
    x
  } else if (nx == 1L || n == 0L) {
    vec_rep(x, n)
  } else {
    stop("Can't recycle vector of length ", nx, " to length ", n, call. = FALSE)
  }
}

# as_df --------------------------------------------------------------

as_df_row <- function(x) UseMethod("as_df_row")

#' @export
as_df_row.data.frame <- function(x) x

#' @export
as_df_row.NULL <- function(x) x

#' @export
as_df_row.default <- function(x) {
  if (vec_dims(x) == 1L) {
    x <- as.list(x)
    x <- tibble::set_tidy_names(x)
    new_data_frame(x, 1)
  } else {
    as.data.frame(x)
  }
}

as_df_col <- function(x, outer_name) UseMethod("as_df_col")

#' @export
as_df_col.data.frame <- function(x, outer_name = NULL) {
  names(x) <- outer_names(x, outer_name)
  x
}

#' @export
as_df_col.NULL <- function(x, outer_name = NULL) x

#' @export
as_df_col.default <- function(x, outer_name = NULL) {
  if (vec_dims(x) == 1L) {
    x <- stats::setNames(list(x), outer_name)
    new_data_frame(x, vec_length(x[[1]]))
  } else {
    colnames(x) <- outer_names(x, outer_name)
    as.data.frame(x)
  }
}
