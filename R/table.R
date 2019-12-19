#' Generics for tabular vectors
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("experimental")}
#'
#' These generics make it possible to work with tables, i.e. vectors
#' of dimension 2 like data frames and matrices. The main motivation
#' of this API is to support colwise operations such as slicing
#' columns. Operating across columns requires slightly different
#' semantics and concepts than rowwise operations:
#'
#' - A tabular prototype is a zero-rows, zero-cols table. In order for
#'   your tabular class to be compatible with vctrs, it needs to
#'   support such empty instances.
#'
#' - The vector type of a data frame includes its columns (their names
#'   and their types). The tabular type of a data frame doesn't.
#'
#' The details and theory of tabular operations are in development and
#' likely to change in the future.
#'
#' @param x A tabular vector.
#'
#' @export
tbl_is <- function(x) {
  .Call(vctrs_tbl_is, x)
}
#' @rdname tbl_is
#' @export
tbl_assert <- function(x) {
  .Call(vctrs_tbl_assert, x)
}

#' @rdname tbl_is
#' @inheritParams vec_slice
tbl_slice <- function(x, i) {
  .Call(vctrs_tbl_slice, x, i)
}
#' @rdname tbl_is
tbl_ptype <- function(x) {
  .Call(vctrs_tbl_ptype, x)
}
