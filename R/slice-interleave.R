#' Interleave many vectors into one vector
#'
#' @description
#' `vec_interleave()` combines multiple vectors together, much like [vec_c()],
#' but does so in such a way that the elements of each vector are interleaved
#' together.
#'
#' It is a more efficient equivalent to the following usage of `vec_c()`:
#'
#' ```
#' vec_interleave(x, y) == vec_c(x[1], y[1], x[2], y[2], ..., x[n], y[n])
#' ```
#'
#' @section Dependencies:
#'
#' ## vctrs dependencies
#'
#' - [list_unchop()]
#'
#' @inheritParams vec_c
#'
#' @param ... Vectors to interleave. These will be
#'   [recycled][vector_recycling_rules] to a common size.
#'
#' @export
#' @examples
#' # The most common case is to interleave two vectors
#' vec_interleave(1:3, 4:6)
#'
#' # But you aren't restricted to just two
#' vec_interleave(1:3, 4:6, 7:9, 10:12)
#'
#' # You can also interleave data frames
#' x <- data_frame(x = 1:2, y = c("a", "b"))
#' y <- data_frame(x = 3:4, y = c("c", "d"))
#'
#' vec_interleave(x, y)
vec_interleave <- function(...,
                           .ptype = NULL,
                           .name_spec = NULL,
                           .name_repair = c("minimal", "unique", "check_unique", "universal")) {
  args <- list2(...)

  # TODO: Use `vec_drop_missing()`
  # `NULL`s must be dropped up front to generate appropriate indices
  missing <- vec_equal_na(args)
  if (any(missing)) {
    args <- vec_slice(args, !missing)
  }

  n <- length(args)
  size <- vec_size_common(!!!args)

  indices <- vec_interleave_indices(n, size)

  list_unchop(
    x = args,
    indices = indices,
    ptype = .ptype,
    name_spec = .name_spec,
    name_repair = .name_repair
  )
}

vec_interleave_indices <- function(n, size) {
  .Call(ffi_interleave_indices, n, size)
}
