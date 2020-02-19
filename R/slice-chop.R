#' Repeatedly slice a vector
#'
#' `vec_chop()` provides an efficient method to repeatedly slice a vector. It
#' captures the pattern of `map(indices, vec_slice, x = x)`.
#'
#' @param x A vector
#' @param indices A list of index values to slice `x` with, or `NULL`. Each
#'   element of the list must be an integer, character or logical vector that
#'   would be valid as an index in [vec_slice()]. If `NULL`, `x` is split into
#'   its individual elements, equivalent to using an `indices` of
#'   `as.list(vec_seq_along(x))`.
#' @return A list of size `vec_size(indices)` or, if `indices == NULL`,
#'   `vec_size(x)`.
#' @export
#' @examples
#' vec_chop(1:5)
#' vec_chop(1:5, list(1, 1:2))
#' vec_chop(mtcars, list(1:3, 4:6))
vec_chop <- function(x, indices = NULL) {
  .Call(vctrs_chop, x, indices)
}

#' Combine a list of vectors
#'
#' `vec_unchop()` combines `x`, a list of vectors, into a single vector, placing
#' elements in the output according to the locations specified by `indices`. It
#' is similar to [vec_c()], but gives greater control over how the elements
#' are combined, and does not respect outer names on the list.
#'
#' @inheritParams vec_chop
#' @inheritParams vec_c
#' @export
vec_unchop <- function(x,
                       indices = NULL,
                       ptype = NULL,
                       name_repair = c("minimal", "unique", "check_unique", "universal")) {
  .Call(vctrs_unchop, x, indices, ptype, name_repair)
}

# Exposed for testing  (`starts` is 0-based)
vec_chop_seq <- function(x, starts, sizes, increasings = TRUE) {
  args <- vec_recycle_common(starts, sizes, increasings)
  .Call(vctrs_chop_seq, x, args[[1]], args[[2]], args[[3]])
}

