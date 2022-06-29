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
#' * `vec_ptype(vec_rbind(x, y)) = vec_ptype_common(x, y)`
#'
#' Note that if an input is an empty vector, it is first converted to
#' a 1-row data frame with 0 columns. Despite being empty, its
#' effective size for the total number of rows is 1.
#'
#' For column-binding, the following invariants apply:
#'
#' * `vec_size(vec_cbind(x, y)) == vec_size_common(x, y)`
#' * `vec_ptype(vec_cbind(x, y)) == vec_cbind(vec_ptype(x), vec_ptype(x))`
#' @param ... Data frames or vectors.
#'
#'   When the inputs are named:
#'   * `vec_rbind()` assigns names to row names unless `.names_to` is
#'     supplied. In that case the names are assigned in the column
#'     defined by `.names_to`.
#'   * `vec_cbind()` creates packed data frame columns with named
#'      inputs.
#'
#'   `NULL` inputs are silently ignored. Empty (e.g. zero row) inputs
#'   will not appear in the output, but will affect the derived `.ptype`.
#' @param .names_to
#' This controls what to do with input names supplied in `...`.
#'   * By default, input names are [zapped][rlang::zap].
#'
#'   * If a string, specifies a column where the input names will be
#'     copied. These names are often useful to identify rows with
#'     their original input. If a column name is supplied and `...` is
#'     not named, an integer column is used instead.
#'
#'   * If `NULL`, the input names are used as row names.
#' @param .name_repair One of `"unique"`, `"universal"`, or
#'   `"check_unique"`. See [vec_as_names()] for the meaning of these
#'   options.
#'
#'   With `vec_rbind()`, the repair function is applied to all inputs
#'   separately. This is because `vec_rbind()` needs to align their
#'   columns before binding the rows, and thus needs all inputs to
#'   have unique names. On the other hand, `vec_cbind()` applies the
#'   repair function after all inputs have been concatenated together
#'   in a final data frame. Hence `vec_cbind()` allows the more
#'   permissive minimal names repair.
#' @inheritParams vec_c
#' @return A data frame, or subclass of data frame.
#'
#'   If `...` is a mix of different data frame subclasses, `vec_ptype2()`
#'   will be used to determine the output type. For `vec_rbind()`, this
#'   will determine the type of the container and the type of each column;
#'   for `vec_cbind()` it only determines the type of the output container.
#'   If there are no non-`NULL` inputs, the result will be `data.frame()`.
#'
#' @section Dependencies:
#'
#' ## vctrs dependencies
#'
#' - [vec_cast_common()]
#' - [vec_proxy()]
#' - [vec_init()]
#' - [vec_assign()]
#' - [vec_restore()]
#'
#'
#' ## base dependencies of `vec_rbind()`
#'
#' - [base::c()]
#'
#' If columns to combine inherit from a common class,
#' `vec_rbind()` falls back to `base::c()` if there exists a `c()`
#' method implemented for this class hierarchy.
#'
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
#' # if you supply a named data frame, it is packed in a single column
#' data <- vec_cbind(
#'   x = data.frame(a = 1, b = 2),
#'   y = 1
#' )
#' data
#'
#' # Packed data frames are nested in a single column. This makes it
#' # possible to access it through a single name:
#' data$x
#'
#' # since the base print method is suboptimal with packed data
#' # frames, it is recommended to use tibble to work with these:
#' if (rlang::is_installed("tibble")) {
#'   vec_cbind(x = tibble::tibble(a = 1, b = 2), y = 1)
#' }
#'
#' # duplicate names are flagged
#' vec_cbind(x = 1, x = 2)
#'
#' @name vec_bind
NULL

#' @export
#' @param .name_spec A name specification (as documented in [vec_c()])
#'   for combining the outer inputs names in `...` and the inner row
#'   names of the inputs. This only has an effect when `.names_to` is
#'   set to `NULL`, which causes the input names to be assigned as row
#'   names.
#' @rdname vec_bind
vec_rbind <- function(...,
                      .ptype = NULL,
                      .names_to = rlang::zap(),
                      .name_repair = c("unique", "universal", "check_unique"),
                      .name_spec = NULL) {
  .External2(ffi_rbind, .ptype, .names_to, .name_repair, .name_spec)
}
vec_rbind <- fn_inline_formals(vec_rbind, ".name_repair")

#' @export
#' @rdname vec_bind
#' @param .size If, `NULL`, the default, will determine the number of rows in
#'   `vec_cbind()` output by using the standard [recycling
#'   rules][vector_recycling_rules].
#'
#'   Alternatively, specify the desired number of rows, and any inputs of length
#'   1 will be recycled appropriately.
vec_cbind <- function(...,
                      .ptype = NULL,
                      .size = NULL,
                      .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  .External2(ffi_cbind, .ptype, .size, .name_repair)
}
vec_cbind <- fn_inline_formals(vec_cbind, ".name_repair")

as_df_row <- function(x, quiet = FALSE) {
  .Call(ffi_as_df_row, x, quiet, environment())
}
as_df_col <- function(x, outer_name) {
  .Call(ffi_as_df_col, x, outer_name, environment())
}

#' Frame prototype
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' This is an experimental generic that returns zero-columns variants
#' of a data frame. It is needed for [vec_cbind()], to work around the
#' lack of colwise primitives in vctrs. Expect changes.
#'
#' @param x A data frame.
#' @inheritParams rlang::args_dots_empty
#'
#' @keywords internal
#' @export
vec_cbind_frame_ptype <- function(x, ...) {
  UseMethod("vec_cbind_frame_ptype")
}
#' @export
vec_cbind_frame_ptype.default <- function(x, ...) {
  x[0]
}

#' @export
vec_cbind_frame_ptype.sf <- function(x, ...) {
  data.frame()
}
