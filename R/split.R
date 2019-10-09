#' Split a vector into groups
#'
#' This is a generalisation of [split()] that can split by any type of vector,
#' not just factors. Instead of returning the keys in the character names,
#' the are returned in a separate parallel vector.
#'
#' @param x Vector to divide into groups.
#' @param by Vector whose unique values defines the groups.
#' @return A data frame with two columns and size equal to
#'   `vec_size(vec_unique(by))`. The `key` column has the same type as
#'   `by`, and the `val` column has type `list_of<vec_ptype(x)>`.
#'
#'   Note for complex types, the default `data.frame` print method will be
#'   suboptimal, and you will want to coerce into a tibble to better
#'   understand the output.
#' @export
#' @examples
#' vec_split(mtcars$cyl, mtcars$vs)
#' vec_split(mtcars$cyl, mtcars[c("vs", "am")])
#'
#' if (require("tibble")) {
#'   as_tibble(vec_split(mtcars$cyl, mtcars[c("vs", "am")]))
#'   as_tibble(vec_split(mtcars, mtcars[c("vs", "am")]))
#' }
vec_split <- function(x, by) {
  if (vec_size(x) != vec_size(by)) {
    abort("`x` and `by` must have same size")
  }

  out <- vec_split_id(by)

  x_split <- map(out$id, vec_slice, x = x)
  out$val <- new_list_of(x_split, vec_ptype(x))

  out$id <- NULL

  out
}

#' Locate unique groups in a vector
#'
#' This locates unique groups in `x` and returns both the unique values and
#' the locations of every appearance of each value. It is used to power
#' [vec_split()].
#'
#' @param x A vector to locate unique groups for.
#' @return A data frame with two columns and size equal to
#'   `vec_size(vec_unique(x))`. The `key` column has the same type as `x`, and
#'   the `id` column is a `list_of<integer>`.
#'
#'   Note for complex types, the default `data.frame` print method will be
#'   suboptimal, and you will want to coerce into a tibble to better understand
#'   the output.
#' @seealso [vec_split]
#' @export
#' @examples
#' vec_split_id(mtcars$vs)
#' vec_split_id(mtcars[c("vs", "am")])
#'
#' if (require("tibble")) {
#'   as_tibble(vec_split_id(mtcars[c("vs", "am")]))
#' }
vec_split_id <- function(x) {
  .Call(vctrs_split_id, x)
}

# Used internally by `vec_rbind()`, but exported for testing
vec_split_along <- function(x, indices = NULL) {
  .Call(vctrs_split_along, x, indices)
}
