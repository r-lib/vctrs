#' Combine a list of vectors
#'
#' @description
#' While `list_unchop()` is not deprecated, we now recommend that you use
#' either:
#'
#' - `list_combine(x, indices = indices, size = size)` over
#'   `list_unchop(x, indices = indices)`
#'
#' - `vec_c(!!!x)` over `list_unchop(x)`
#'
#' `list_unchop()` combines a list of vectors into a single vector, placing
#' elements in the output according to the locations specified by `indices`. It
#' is similar to [vec_c()], but gives greater control over how the elements are
#' combined.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams vec_c
#'
#' @param x A list
#'
#' @param indices A list of positive integer vectors specifying the
#'   locations to place elements of `x` in. Each element of `x` is recycled to
#'   the size of the corresponding index vector. The size of `indices` must
#'   match the size of `x`. If `NULL`, `x` is combined in the order it is
#'   provided in, which is equivalent to using [vec_c()].
#'
#' @param ptype If `NULL`, the default, the output type is determined by
#'   computing the common type across all elements of `x`. Alternatively, you
#'   can supply `ptype` to give the output a known type.
#'
#' @returns
#' A vector of type `vec_ptype_common(!!!x)`, or `ptype`, if specified. The size
#' is computed as `vec_size_common(!!!indices)` unless the indices are `NULL`,
#' in which case the size is `vec_size_common(!!!x)`.
#'
#' @section Dependencies:
#' - [vec_c()]
#'
#' @keywords internal
#'
#' @export
#' @examples
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
#' # If you have a list of homogeneous vectors, sometimes it can be useful to
#' # unchop, apply a function to the flattened vector, and then rechop according
#' # to the original indices. This can be done efficiently with `list_sizes()`.
#' x <- list(c(1, 2, 1), c(3, 1), 5, double())
#' x_flat <- list_unchop(x)
#' x_flat <- x_flat + max(x_flat)
#' vec_chop(x_flat, sizes = list_sizes(x))
list_unchop <- function(
  x,
  ...,
  indices = NULL,
  ptype = NULL,
  name_spec = NULL,
  name_repair = c(
    "minimal",
    "unique",
    "check_unique",
    "universal",
    "unique_quiet",
    "universal_quiet"
  ),
  error_arg = "x",
  error_call = current_env()
) {
  check_dots_empty0(...)
  .Call(
    ffi_list_unchop,
    x,
    indices,
    ptype,
    name_spec,
    name_repair,
    environment()
  )
}
