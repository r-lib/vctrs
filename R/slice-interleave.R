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
#' - [list_combine()]
#'
#' @inheritParams vec_c
#'
#' @param ... Vectors to interleave.
#'
#' @param .size The expected size of each vector. If not provided, computed
#'   automatically by [vec_size_common()]. Each vector will be
#'   [recycled][theory-faq-recycling] to this size.
#'
#' @param .ptype The expected type of each vector. If not provided, computed
#'   automatically by [vec_ptype_common()]. Each vector will be
#'   [cast][theory-faq-coercion] to this type.
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
#'
#' # `.size` can be used to recycle size 1 elements before interleaving
#' vec_interleave(1, 2, .size = 3)
#'
#' # `.ptype` can be used to enforce a particular type
#' typeof(vec_interleave(1, 2, .ptype = integer()))
vec_interleave <- function(
  ...,
  .size = NULL,
  .ptype = NULL,
  .name_spec = NULL,
  .name_repair = c(
    "minimal",
    "unique",
    "check_unique",
    "universal",
    "unique_quiet",
    "universal_quiet"
  ),
  .error_call = current_env()
) {
  list_interleave(
    x = list2(...),
    size = .size,
    ptype = .ptype,
    name_spec = .name_spec,
    name_repair = .name_repair,
    x_arg = "",
    error_call = .error_call
  )
}

# It's sometimes more convenient to supply a list, plus you get access to
# `x_arg` for better error messages than you get from `vec_interleave(!!!x)`.
# We could consider exporting this alongside `vec_interleave()`.
list_interleave <- function(
  x,
  ...,
  size = NULL,
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
  x_arg = caller_arg(x),
  error_call = current_env()
) {
  check_dots_empty0(...)
  .Call(
    ffi_list_interleave,
    x,
    size,
    ptype,
    name_spec,
    name_repair,
    environment()
  )
}
