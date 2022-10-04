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
#' list_unchop(vec_chop(x, indices), indices) == x
#' ```
#'
#' @inheritParams vec_c
#' @param x A vector
#' @param indices For `vec_chop()`, a list of positive integer vectors to
#'   slice `x` with, or `NULL`. If `NULL`, `x` is split into its individual
#'   elements, equivalent to using an `indices` of `as.list(vec_seq_along(x))`.
#'
#'   For `list_unchop()`, a list of positive integer vectors specifying the
#'   locations to place elements of `x` in. Each element of `x` is recycled to
#'   the size of the corresponding index vector. The size of `indices` must
#'   match the size of `x`. If `NULL`, `x` is combined in the order it is
#'   provided in, which is equivalent to using [vec_c()].
#' @param ptype If `NULL`, the default, the output type is determined by
#'   computing the common type across all elements of `x`. Alternatively, you
#'   can supply `ptype` to give the output a known type.
#' @return
#' - `vec_chop()`: A list of size `vec_size(indices)` or, if `indices == NULL`,
#'   `vec_size(x)`.
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
#' vec_chop(1:5, list(1, 1:2))
#' vec_chop(mtcars, list(1:3, 4:6))
#'
#' # If `indices` selects every value in `x` exactly once,
#' # in any order, then `list_unchop()` inverts `vec_chop()`
#' x <- c("a", "b", "c", "d")
#' indices <- list(2, c(3, 1), 4)
#' vec_chop(x, indices)
#' list_unchop(vec_chop(x, indices), indices)
#'
#' # When unchopping, size 1 elements of `x` are recycled
#' # to the size of the corresponding index
#' list_unchop(list(1, 2:3), list(c(1, 3, 5), c(2, 4)))
#'
#' # Names are retained, and outer names can be combined with inner
#' # names through the use of a `name_spec`
#' lst <- list(x = c(a = 1, b = 2), y = 1)
#' list_unchop(lst, list(c(3, 2), c(1, 4)), name_spec = "{outer}_{inner}")
#'
#' # An alternative implementation of `ave()` can be constructed using
#' # `vec_chop()` and `list_unchop()` in combination with `vec_group_loc()`
#' ave2 <- function(.x, .by, .f, ...) {
#'   indices <- vec_group_loc(.by)$loc
#'   chopped <- vec_chop(.x, indices)
#'   out <- lapply(chopped, .f, ...)
#'   list_unchop(out, indices)
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
vec_chop <- function(x, indices = NULL) {
  .Call(vctrs_chop, x, indices)
}

#' @rdname vec_chop
#' @export
list_unchop <- function(x,
                        indices = NULL,
                        ptype = NULL,
                        name_spec = NULL,
                        name_repair = c("minimal", "unique", "check_unique", "universal", "unique_quiet", "universal_quiet"),
                        error_arg = "x",
                        error_call = current_env()) {
  .Call(ffi_list_unchop, x, indices, ptype, name_spec, name_repair, environment())
}

# Exposed for testing  (`starts` is 0-based)
vec_chop_seq <- function(x, starts, sizes, increasings = TRUE) {
  args <- vec_recycle_common(starts, sizes, increasings)
  .Call(vctrs_chop_seq, x, args[[1]], args[[2]], args[[3]])
}
