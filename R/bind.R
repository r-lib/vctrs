#' Combine many data frames into one data frame
#'
#' This pair of functions binds together data frames (and vectors), either
#' row-wise or column-wise. Row-binding creates a data frame with common type
#' across all arguments. Column-binding creates a data frame with common length
#' across all arguments.
#'
#' @section Invariants:
#'
#' All inputs are first converted to a data frame. The conversion for
#' 1d vectors depends on the direction of binding:
#'
#' * For `vec_rbind()`, each element of the vector becomes a column in
#'   a single row.
#' * For `vec_cbind()`, each element of the vector becomes a row in a
#'   single column.
#'
#' Once the inputs have all become data frames, the following
#' invariants are observed for row-binding:
#'
#' * `vec_size(vec_rbind(x, y)) == vec_size(x) + vec_size(y)`
#' * `vec_type(vec_rbind(x, y)) = vec_type_common(x, y)`
#'
#' Note that if an input is an empty vector, it is first converted to
#' a 1-row data frame with 0 columns. Despite being empty, its
#' effective size for the total number of rows is 1.
#'
#' For column-binding, the following invariants apply:
#'
#' * `vec_size(vec_cbind(x, y)) == vec_size_common(x, y)`
#' * `vec_type(vec_cbind(x, y)) == vec_cbind(vec_type(x), vec_type(x))`
#' @param ... Data frames or vectors.
#'
#'   `vec_rbind()` ignores names. `vec_cbind()` preserves outer names,
#'   combining with inner names if also present.
#'
#'   `NULL` inputs are silently ignored. Empty (e.g. zero row) inputs
#'   will not appear in the output, but will affect the derived `.ptype`.
#' @param .name_repair One of `"unique"`, `"universal"`, or
#'   `"check_unique"`. See [vec_as_names()] for the meaning of these
#'   options.
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
vec_rbind <- function(...,
                      .ptype = NULL,
                      .name_repair = c("unique", "universal", "check_unique")) {
  .External2(vctrs_rbind, .ptype, .name_repair)
}
vec_rbind <- fn_inline_formals(vec_rbind, ".name_repair")

#' @export
#' @rdname vec_bind
#' @param .size If, `NULL`, the default, will determine the number of
#'   rows in `vec_cbind()` output by using the standard recycling rules.
#'
#'   Alternatively, specify the desired number of rows, and any inputs
#'   of length 1 will be recycled appropriately.
vec_cbind <- function(...,
                      .ptype = NULL,
                      .size = NULL,
                      .name_repair = c("unique", "universal", "check_unique")) {
  .External2(vctrs_cbind, .ptype, .size, .name_repair)
}
vec_cbind <- fn_inline_formals(vec_cbind, ".name_repair")

as_df_row <- function(x, quiet = FALSE) {
  .Call(vctrs_as_df_row, x, quiet)
}
as_df_col <- function(x, outer_name) {
  .Call(vctrs_as_df_col, x, outer_name)
}
