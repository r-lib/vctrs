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
#'   `by`, and the `val` column is a list containing elements of type
#'   `vec_ptype(x)`.
#'
#'   Note for complex types, the default `data.frame` print method will be
#'   suboptimal, and you will want to coerce into a tibble to better
#'   understand the output.
#' @export
#'
#' @section Dependencies:
#' - [vec_group_loc()]
#' - [vec_chop()]
#'
#' @examples
#' vec_split(mtcars$cyl, mtcars$vs)
#' vec_split(mtcars$cyl, mtcars[c("vs", "am")])
#'
#' if (require("tibble")) {
#'   as_tibble(vec_split(mtcars$cyl, mtcars[c("vs", "am")]))
#'   as_tibble(vec_split(mtcars, mtcars[c("vs", "am")]))
#' }
vec_split <- function(x, by) {
  .Call(vctrs_split, x, by)
}
