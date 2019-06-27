#' Vector recycling
#'
#' `vec_recycle(x, size)` recycles a single vector to given size.
#' `vec_recycle_common(...)` recycles multiple vectors to their common size.
#' All functions obey the vctrs recycling rules, described below, and will
#' throw an error if recycling is not possible. See [vec_size()] for the
#' precise definition of size.
#'
#' @section Recycling rules:
#' The common size of two vectors defines the recycling rules, and can be
#' summarise with the following table:
#'
#' \figure{sizes-recycling.png}
#'
#' (Note `NULL`s are handled specially; they are treated like empty
#' arguments and hence don't affect the size)
#'
#' This is a stricter set of rules than base R, which will usually
#' return output of length `max(nx, ny)`, warning if the length of the longer
#' vector is not an integer multiple of the length of the shorter.
#'
#' We say that two vectors have __compatible size__ if they can be
#' recycled to be the same length.
#'
#' @param x,... Vectors to recycle.
#' @param size,.size Desired output size. If omitted in `vec_recycle_common()`
#'   will use the common size from [vec_size_common()].
#' @export
#' @examples
#' # Inputs with 1 observation are recycled
#' vec_recycle_common(1:5, 5)
#' vec_recycle_common(integer(), 5)
#' \dontrun{
#' vec_recycle_common(1:5, 1:2)
#' }
#'
#' # Data frames and matrices are recycled along their rows
#' vec_recycle_common(data.frame(x = 1), 1:5)
#' vec_recycle_common(array(1:2, c(1, 2)), 1:5)
#' vec_recycle_common(array(1:3, c(1, 3, 1)), 1:5)
vec_recycle <- function(x, size) {
  .Call(vctrs_recycle, x, size)
}

#' @export
#' @rdname vec_recycle
vec_recycle_common <- function(..., .size = NULL) {
  args <- list2(...)
  size <- vec_size_common(!!!args, .size = .size, .absent = na_int)

  if (identical(size, na_int)) {
    args
  } else {
    map(args, vec_recycle, size = size)
  }
}
