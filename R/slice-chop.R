#' Chopping
#'
#' @description
#' - `vec_chop()` provides an efficient method to repeatedly slice a vector. It
#' captures the pattern of `map(indices, vec_slice, x = x)`.
#'
#' - `vec_unchop()` combines a list of vectors into a single vector, placing
#' elements in the output according to the locations specified by `indices`. It
#' is similar to [vec_c()], but gives greater control over how the elements
#' are combined, and does not respect outer names on the list.
#'
#' @inheritParams vec_c
#' @param x A vector
#' @param indices For `vec_chop()`, a list of index values to slice `x` with,
#'   or `NULL`. Each element of the list must be an integer, character or
#'   logical vector that would be valid as an index in [vec_slice()]. If `NULL`,
#'   `x` is split into its individual elements, equivalent to using an
#'   `indices` of `as.list(vec_seq_along(x))`.
#'
#'   For `vec_unchop()`, a list of integer vectors specifying the locations to
#'   place elements of `x` in. Each element of `x` is recycled to the size
#'   of the corresponding index vector. The size of `indices` must match the
#'   size of `x`. If `NULL`, `x` is combined in the order it is provided in.
#' @param ptype If `NULL`, the default, the output type is determined by
#'   computing the common type across all elements of `x`.
#'
#'   Alternatively, you can supply `ptype` to give the output known type.
#'   If `getOption("vctrs.no_guessing")` is `TRUE` you must supply this value:
#'   this is a convenient way to make production code demand fixed types.
#' @return
#' - `vec_chop()`: A list of size `vec_size(indices)` or, if `indices == NULL`,
#'   `vec_size(x)`.
#'
#' - `vec_unchop()`: A vector of type `vec_ptype_common(!!!x)`, or `ptype`, if
#'   specified. The size is computed as `vec_size_common(!!!indices)` unless
#'   the indices are `NULL`, in which case the size is `vec_size_common(!!!x)`.
#' @export
#' @examples
#' vec_chop(1:5)
#' vec_chop(1:5, list(1, 1:2))
#' vec_chop(mtcars, list(1:3, 4:6))
#'
#' # If `indices` uses the full sequence along `x` (in any order),
#' # then `vec_unchop()` inverts `vec_chop()`
#' x <- c("a", "b", "c", "d")
#' indices <- list(2, c(3, 1), 4)
#' vec_chop(x, indices)
#' vec_unchop(vec_chop(x, indices), indices)
#'
#' # When unchopping, size 1 elements of `x` are recycled
#' # to the size of the corresponding index
#' vec_unchop(list(1, 2:3), list(c(1, 3, 5), c(2, 4)))
#'
#' # An alternative implementation of `ave()` can be constructed using
#' # `vec_chop()` and `vec_unchop()` in combination with `vec_group_loc()`
#' attach(warpbreaks)
#'
#' ave2 <- function(.x, .by, .f, ...) {
#'   indices <- vec_group_loc(.by)$loc
#'   chopped <- vec_chop(.x, indices)
#'   out <- lapply(chopped, .f, ...)
#'   vec_unchop(out, indices)
#' }
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

