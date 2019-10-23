#' Identify groups
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("experimental")}
#'
#' * `vec_group_id()` returns an identifier for the group that each element of
#'   `x` falls in, constructed in the order that they appear. The number of
#'   groups is also returned as an attribute, `n`.
#'
#' * `vec_group_rle()` locates groups in `x` and returns them run length
#'   encoded in the order that they appear. The return value is a rcrd object
#'   with fields for the `group` identifiers and the run `length` of the
#'   corresponding group. The number of groups is also returned as an
#'   attribute, `n`.
#'
#' @param x A vector
#' @return
#'   * `vec_group_id()`: An integer vector with the same size as `x`.
#'   * `vec_group_rle()`: A `vctrs_group_rle` rcrd object with two integer
#'     vector fields: `group` and `length`.
#' @name vec_group
#' @examples
#' purrr <- c("p", "u", "r", "r", "r")
#' vec_group_id(purrr)
#' vec_group_rle(purrr)
#'
#' groups <- mtcars[c("vs", "am")]
#' vec_group_id(groups)
#'
#' group_rle <- vec_group_rle(groups)
#' group_rle
#'
#' # Access fields with `field()`
#' field(group_rle, "group")
#' field(group_rle, "length")
#'
#' # `vec_group_id()` is equivalent to
#' vec_match(groups, vec_unique(groups))
NULL

#' @rdname vec_group
#' @export
vec_group_id <- function(x) {
  .Call(vctrs_group_id, x)
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
#' vec_group_pos(mtcars$vs)
#' vec_group_pos(mtcars[c("vs", "am")])
#'
#' if (require("tibble")) {
#'   as_tibble(vec_group_pos(mtcars[c("vs", "am")]))
#' }
vec_group_pos <- function(x) {
  .Call(vctrs_group_pos, x)
}

#' @rdname vec_group
#' @export
vec_group_rle <- function(x) {
  .Call(vctrs_group_rle, x)
}

#' @export
format.vctrs_group_rle <- function(x, ...) {
  group <- field(x, "group")
  length <- field(x, "length")
  paste0(group, "x", length)
}

#' @export
obj_print_header.vctrs_group_rle <- function(x, ...) {
  size <- vec_size(x)
  n <- attr(x, "n")
  cat_line("<", vec_ptype_full(x), "[", size, "][n = ", n, "]>")
  invisible(x)
}

# For testing
new_group_rle <- function(group, length, n) {
  vec_assert(group, integer())
  vec_assert(length, integer())
  vec_assert(n, integer(), 1L)

  if (vec_size(group) != vec_size(length)) {
    abort("`group` and `length` must have the same size.")
  }

  new_rcrd(list(group = group, length = length), n = n, class = "vctrs_group_rle")
}
