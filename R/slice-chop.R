#' Chopping
#'
#' @description
#' - `vec_chop()` provides an efficient method to repeatedly slice a vector. It
#'   captures the pattern of `map(indices, vec_slice, x = x)`. When no indices
#'   are supplied, it is generally equivalent to [as.list()].
#'
#' - `list_unchop()` combines a list of vectors into a single vector, placing
#'   elements in the output according to the locations specified by `indices`.
#'   It is similar to [vec_c()], but gives greater control over how the elements
#'   are combined. When no indices are supplied, it is identical to `vec_c()`,
#'   but typically a little faster.
#'
#' If `indices` selects every value in `x` exactly once, in any order, then
#' `list_unchop()` is the inverse of `vec_chop()` and the following invariant
#' holds:
#'
#' ```
#' list_unchop(vec_chop(x, indices = indices), indices = indices) == x
#' ```
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams vec_c
#'
#' @param x A vector
#' @param indices For `vec_chop()`, a list of positive integer vectors to
#'   slice `x` with, or `NULL`. Can't be used if `sizes` is already specified.
#'   If both `indices` and `sizes` are `NULL`, `x` is split into its individual
#'   elements, equivalent to using an `indices` of `as.list(vec_seq_along(x))`.
#'
#'   For `list_unchop()`, a list of positive integer vectors specifying the
#'   locations to place elements of `x` in. Each element of `x` is recycled to
#'   the size of the corresponding index vector. The size of `indices` must
#'   match the size of `x`. If `NULL`, `x` is combined in the order it is
#'   provided in, which is equivalent to using [vec_c()].
#' @param sizes An integer vector of non-negative sizes representing sequential
#'   indices to slice `x` with, or `NULL`. Can't be used if `indices` is already
#'   specified.
#'
#'   For example, `sizes = c(2, 4)` is equivalent to `indices = list(1:2, 3:6)`,
#'   but is typically faster.
#'
#'   `sum(sizes)` must be equal to `vec_size(x)`, i.e. `sizes` must completely
#'   partition `x`, but an individual size is allowed to be `0`.
#' @param ptype If `NULL`, the default, the output type is determined by
#'   computing the common type across all elements of `x`. Alternatively, you
#'   can supply `ptype` to give the output a known type.
#' @return
#' - `vec_chop()`: A list where each element has the same type as `x`. The size
#'   of the list is equal to `vec_size(indices)`, `vec_size(sizes)`, or
#'   `vec_size(x)` depending on whether or not `indices` or `sizes` is provided.
#'
#' - `list_unchop()`: A vector of type `vec_ptype_common(!!!x)`, or `ptype`, if
#'   specified. The size is computed as `vec_size_common(!!!indices)` unless
#'   the indices are `NULL`, in which case the size is `vec_size_common(!!!x)`.
#'
#' @section Dependencies of `vec_chop()`:
#' - [vec_slice()]
#'
#' @section Dependencies of `list_unchop()`:
#' - [vec_c()]
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
#' # If `indices` selects every value in `x` exactly once,
#' # in any order, then `list_unchop()` inverts `vec_chop()`
#' x <- c("a", "b", "c", "d")
#' indices <- list(2, c(3, 1), 4)
#' vec_chop(x, indices = indices)
#' list_unchop(vec_chop(x, indices = indices), indices = indices)
#'
#' # When unchopping, size 1 elements of `x` are recycled
#' # to the size of the corresponding index
#' list_unchop(list(1, 2:3), indices = list(c(1, 3, 5), c(2, 4)))
#'
#' # Names are retained, and outer names can be combined with inner
#' # names through the use of a `name_spec`
#' lst <- list(x = c(a = 1, b = 2), y = 1)
#' list_unchop(lst, indices = list(c(3, 2), c(1, 4)), name_spec = "{outer}_{inner}")
#'
#' # An alternative implementation of `ave()` can be constructed using
#' # `vec_chop()` and `list_unchop()` in combination with `vec_group_loc()`
#' ave2 <- function(.x, .by, .f, ...) {
#'   indices <- vec_group_loc(.by)$loc
#'   chopped <- vec_chop(.x, indices = indices)
#'   out <- lapply(chopped, .f, ...)
#'   list_unchop(out, indices = indices)
#' }
#'
#' breaks <- warpbreaks$breaks
#' wool <- warpbreaks$wool
#'
#' ave2(breaks, wool, mean)
#'
#' identical(
#'   ave2(breaks, wool, mean),
#'   ave(breaks, wool, FUN = mean)
#' )
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
#' # unchop, apply a function to the flattened vector, and then rechop according
#' # to the original indices. This can be done efficiently with `list_sizes()`.
#' x <- list(c(1, 2, 1), c(3, 1), 5, double())
#' x_flat <- list_unchop(x)
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

#' @rdname vec_chop
#' @export
list_unchop <- function(x,
                        ...,
                        indices = NULL,
                        ptype = NULL,
                        name_spec = NULL,
                        name_repair = c("minimal", "unique", "check_unique", "universal", "unique_quiet", "universal_quiet"),
                        error_arg = "x",
                        error_call = current_env()) {
  check_dots_empty0(...)
  .Call(ffi_list_unchop, x, indices, ptype, name_spec, name_repair, environment())
}

# Exposed for testing  (`starts` is 0-based)
vec_chop_seq <- function(x, starts, sizes, increasings = TRUE) {
  args <- vec_recycle_common(starts, sizes, increasings)
  .Call(ffi_vec_chop_seq, x, args[[1]], args[[2]], args[[3]])
}
