#' Chopping
#'
#' @description
#' `vec_chop()` provides an efficient method to repeatedly slice a vector. It
#' captures the pattern of `map(indices, vec_slice, x = x)`. When no indices
#' are supplied, it is generally equivalent to [as.list()].
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x A vector
#'
#' @param indices A list of positive integer vectors to slice `x` with, or
#'   `NULL`. Can't be used if `sizes` is already specified. If both `indices`
#'   and `sizes` are `NULL`, `x` is split into its individual elements,
#'   equivalent to using an `indices` of `as.list(vec_seq_along(x))`.
#'
#' @param sizes An integer vector of non-negative sizes representing sequential
#'   indices to slice `x` with, or `NULL`. Can't be used if `indices` is already
#'   specified.
#'
#'   For example, `sizes = c(2, 4)` is equivalent to `indices = list(1:2, 3:6)`,
#'   but is typically faster.
#'
#'   `sum(sizes)` must be equal to `vec_size(x)`, i.e. `sizes` must completely
#'   partition `x`, but an individual size is allowed to be `0`.
#'
#' @returns
#' A list where each element has the same type as `x`. The size of the list is
#' equal to `vec_size(indices)`, `vec_size(sizes)`, or `vec_size(x)` depending
#' on whether or not `indices` or `sizes` is provided.
#'
#' @section Dependencies:
#' - [vec_slice()]
#'
#' @export
#' @examples
#' vec_chop(1:5)
#'
#' # These two are equivalent
#' vec_chop(1:5, indices = list(1:2, 3:5))
#' vec_chop(1:5, sizes = c(2, 3))
#'
#' # Can also be used on data frames
#' vec_chop(mtcars, indices = list(1:3, 4:6))
#'
#' # If you know your input is sorted and you'd like to split on the groups,
#' # `vec_run_sizes()` can be efficiently combined with `sizes`
#' df <- data_frame(
#'   g = c(2, 5, 5, 6, 6, 6, 6, 8, 9, 9),
#'   x = 1:10
#' )
#' vec_chop(df, sizes = vec_run_sizes(df$g))
#'
#' # If you have a list of homogeneous vectors, sometimes it can be useful to
#' # combine, apply a function to the flattened vector, and chop according
#' # to the original indices. This can be done efficiently with `list_sizes()`.
#' x <- list(c(1, 2, 1), c(3, 1), 5, double())
#' x_flat <- vec_c(!!!x)
#' x_flat <- x_flat + max(x_flat)
#' vec_chop(x_flat, sizes = list_sizes(x))
vec_chop <- function(x, ..., indices = NULL, sizes = NULL) {
  if (!missing(...)) {
    indices <- check_dots_chop(..., indices = indices)
  }
  .Call(ffi_vec_chop, x, indices, sizes)
}

check_dots_chop <- function(..., indices = NULL, call = caller_env()) {
  if (!is_null(indices)) {
    # Definitely can't supply both `indices` and `...`
    check_dots_empty0(..., call = call)
  }

  if (dots_n(...) != 1L) {
    # Backwards compatible case doesn't allow for length >1 `...`.
    # This must be an error case.
    check_dots_empty0(..., call = call)
  }

  # TODO: Soft-deprecate this after dplyr/tidyr have updated all `vec_chop()`
  # calls to be explicit about `indices =`

  # Assume this is an old style `vec_chop(x, indices)` call, before we
  # added the `...`
  indices <- list(...)[[1L]]

  indices
}

# Exposed for testing  (`starts` is 0-based)
vec_chop_seq <- function(x, starts, sizes, increasings = TRUE) {
  args <- vec_recycle_common(starts, sizes, increasings)
  .Call(ffi_vec_chop_seq, x, args[[1]], args[[2]], args[[3]])
}
