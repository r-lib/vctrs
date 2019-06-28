#' Number of observations
#'
#' @description
#'
#' `vec_size(x)` returns the size of a vector. `vec_is_empty()`
#' returns `TRUE` if the size is zero, `FALSE` otherwise.
#'
#' The size is distinct from the [length()] of a vector because it
#' generalises to the "number of observations" for 2d structures,
#' i.e. it's the number of rows in matrix or a data frame.  This
#' definition has the important property that every column of a data
#' frame (even data frame and matrix columns) have the same size.
#' `vec_size_common(...)` returns the common size of multiple vectors.
#'
#' @seealso [vec_slice()] for a variation of `[` compatible with `vec_size()`,
#'   and [vec_recycle()] to recycle vectors to common length.
#' @section Invariants:
#' * `vec_size(dataframe)` == `vec_size(dataframe[[i]])`
#' * `vec_size(matrix)` == `vec_size(matrix[, i, drop = FALSE])`
#' * `vec_size(vec_c(x, y))` == `vec_size(x)` + `vec_size(y)`
#'
#' @param x,... Vector inputs or `NULL`.
#' @param .size If `NULL`, the default, the output size is determined by
#'   recycling the lengths of all elements of `...`. Alternatively, you can
#'   supply `.size` to force a known size.
#' @param .absent The size used when no input is provided, or when all input
#' is `NULL`. If left as `NULL` when no input is supplied, an error is thrown.
#' @return An integer (or double for long vectors).
#'
#'   `vec_size_common()` returns `.absent` if all inputs are `NULL` or
#'   absent, `0L` by default.
#'
#'
#' @details
#'
#' There is no vctrs helper that retrieves the number of columns: as this
#' is a property of the [type][vec_ptype_show()].
#'
#' `vec_size()` is equivalent to `NROW()` but has a name that is easier to
#' pronounce, and throws an error when passed non-vector inputs.
#'
#'
#' @section The size of NULL:
#'
#' The size of `NULL` is hard-coded to `0L` in `vec_size()`.
#' `vec_size_common()` returns `.absent` when all inputs are `NULL`
#' (if only some inputs are `NULL`, they are simply ignored).
#'
#' A default size of 0 makes sense because sizes are most often
#' queried in order to compute a total size while assembling a
#' collection of vectors. Since we treat `NULL` as an absent input by
#' principle, we return the identity of sizes under addition to
#' reflect that an absent input doesn't take up any size.
#'
#' Note that other defaults might make sense under different
#' circumstances. For instance, a default size of 1 makes sense for
#' finding the common size because 1 is the identity of the recycling
#' rules.
#'
#'
#' @export
#' @examples
#' vec_size(1:100)
#' vec_size(mtcars)
#' vec_size(array(dim = c(3, 5, 10)))
#'
#' vec_size_common(1:10, 1:10)
#' vec_size_common(1:10, 1)
#' vec_size_common(integer(), 1)
vec_size <- function(x) {
  .Call(vctrs_size, x)
}

#' @export
#' @rdname vec_size
vec_size_common <- function(..., .size = NULL, .absent = 0L) {
  .External2(vctrs_size_common, .size, .absent)
}

#' @rdname vec_size
#' @export
vec_is_empty <- function(x) {
  vec_size(x) == 0L
}

#' Default value for empty vectors
#'
#' Use this inline operator when you need to provide a default value for
#' empty (as defined by [vec_is_empty()]) vectors.
#'
#' @param x A vector
#' @param y Value to use to `x` is empty. To preserve type-stability, should
#'   be the same type as `x`.
#' @rdname op-empty-default
#' @export
#' @examples
#' 1:10 %0% 5
#' integer() %0% 5
`%0%` <- function(x, y) {
  if (vec_is_empty(x)) y else x
}


# sequences -------------------------------------------------------------------

#' Useful sequences
#'
#' `vec_seq_along()` is equivalent to [seq_along()] but uses size, not length.
#' `vec_init_along()` creates a vector of missing values with size matching
#' an existing object.
#'
#' @param x,y Vectors
#' @return
#' * `vec_seq_along()` an integer vector with the same size as `x`.
#' * `vec_init_along()` a vector with the same type as `x` and the same size
#'   as `y`.
#' @export
#' @examples
#' vec_seq_along(mtcars)
#' vec_init_along(head(mtcars))
vec_seq_along <- function(x) {
  seq_len(vec_size(x))
}

#' @export
#' @rdname vec_seq_along
vec_init_along <- function(x, y = x) {
  vec_slice(x, rep_len(NA_integer_, vec_size(y)))
}

#' Expand the length of a vector
#'
#' This is a special case of [rep()] for the special case of integer `times`
#' and `each` values, and works along size, rather than length.
#'
#' @param x A vector.
#' @param each Number of times to repeat each element of `x`.
#' @param times Number of times to repeat the whole vector of `x`.
#' @return A vector the same type as `x` with size `vec_size(x) * times * each`.
#' @export
#' @examples
#' # each repeats within
#' vec_repeat(1:3, each = 2)
#' # times repeats whole thing
#' vec_repeat(1:3, times = 2)
#'
#' df <- data.frame(x = 1:2, y = 1:2)
#' # rep() repeats columns of data frame, and returns list:
#' rep(df, each = 2)
#' # vec_repeat() repeats rows, and returns same data.frame
#' vec_repeat(df, 2)
vec_repeat <- function(x, each = 1L, times = 1L) {
  vec_assert(each, size = 1L)
  vec_assert(times, size = 1L)

  idx <- rep(vec_seq_along(x), times = times, each = each)
  vec_slice(x, idx)
}
