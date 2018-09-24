#' Combine many data frames into one data frame
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
#'   will not appear in the output, but will affect the derived `.ptype`.
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
#' # common columns are coerced to common class
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
vec_rbind <- function(..., .ptype = NULL) {
  args <- list2(...)
  tbls <- map(args, as_df_row)
  ptype <- vec_ptype(!!!tbls, .ptype = .ptype)[[1]]

  if (is_unknown(ptype))
    return(data_frame())

  ns <- map_int(tbls, vec_obs)
  # Use list so we can rely on efficient internal [[<-
  out <- vec_data(vec_rep(ptype, sum(ns)))

  pos <- 1
  for (i in seq_along(ns)) {
    n <- ns[[i]]
    if (n == 0L)
      next

    tbl_i <- vec_data(vec_cast(tbls[[i]], to = ptype))
    for (j in seq_along(out)) {
      out[[j]][pos:(pos + n - 1)] <- tbl_i[[j]]
    }
    pos <- pos + n
  }

  vec_restore(out, ptype)
}

#' @export
#' @rdname vec_bind
#' @param .nrow If, `NULL`, the default, will determing the number of
#'   rows in `vec_cbind()` output by using the standard recycling rules.
#'
#'   Alternatively, specify the desired number of rows, and any inputs
#'   of length 1 will be recycled appropriately.
vec_cbind <- function(..., .ptype = NULL, .nrow = NULL) {
  args <- list2(...)

  # container type: common type of all (data frame) inputs
  # compute early so we can fail fast
  tbl_empty <- map(args, function(x) {
    if (is.data.frame(x))
      x[0]
  })
  out <- vec_ptype(!!!tbl_empty, .ptype = .ptype[0])[[1]]
  if (is_unknown(out)) {
    out <- data_frame()
  }

  is_null <- map_lgl(args, is_null)
  args <- args[!is_null]

  # container size: common length of all inputs
  nrow <- find_nrow(args, .nrow = .nrow)
  args <- map(args, recycle, n = nrow)

  # convert input to columns and prepare output containers
  tbls <- map2(args, names2(args), as_df_col)

  ps <- map_int(tbls, length)
  cols <- vec_rep(list(), sum(ps))
  names <- vec_rep(character(), sum(ps))

  col <- 1
  for (j in seq_along(tbls)) {
    p <- ps[[j]]
    if (p == 0L)
      next

    cols[col:(col + p - 1)] <- tbls[[j]]
    names[col:(col + p - 1)] <- names(tbls[[j]]) %||% rep("", p)
    col <- col + p
  }

  # Need to document these assumptions, or better, move into
  # a generic
  attr(out, "row.names") <- .set_row_names(nrow)
  out[seq_along(cols)] <- cols
  if (is_installed("tibble")) {
    names <- tibble::tidy_names(names)
  }
  names(out) <- names

  out
}

find_nrow <- function(x, .nrow = NULL) {
  if (!is.null(.nrow)) {
    .nrow
  } else {
    lengths <- map_int(x, vec_obs)
    Reduce(recycle_length, lengths) %||% 0L
  }
}

recycle <- function(x, n) {
  nx <- vec_obs(x)

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
    if (is_installed("tibble"))
      x <- tibble::set_tidy_names(x)
    new_data_frame(x, n = 1L)
  } else {
    as.data.frame(x)
  }
}

as_df_col <- function(x, outer_name) UseMethod("as_df_col")

#' @export
as_df_col.data.frame <- function(x, outer_name = NULL) {
  names(x) <- outer_names(outer_name, names(x), length(x))
  x
}

#' @export
as_df_col.default <- function(x, outer_name = NULL) {
  if (vec_dims(x) == 1L) {
    x <- stats::setNames(list(x), outer_name)
    new_data_frame(x)
  } else {
    colnames(x) <- outer_names(outer_name, colnames(x), ncol(x))
    as.data.frame(x)
  }
}
