#' Number of observations
#'
#' `vec_size(x)` returns the size of a vector. This is distinct from the
#' [length()] of a vector because it generalises to the "number of observations"
#' for 2d structures, i.e. it's the number of rows in matrix or a data frame.
#' This definition has the important property that every column of a data frame
#' (even data frame and matrix columns) have the same size.
#' `vec_size_common(...)` returns the common size of multiple vectors.
#'
#' There is no vctrs helper that retrieves the number of columns: as this
#' is a property of the [type][vec_ptype()].
#'
#' `vec_size()` is equivalent to `NROW()` but has a name that is easier to
#' pronounce, and throws an error when passed non-vector inputs.
#'
#' @seealso [vec_slice()] for a variation of `[` compatible with `vec_size()`,
#'   and [vec_recycle()] to recycle vectors to common length.
#' @section Invariants:
#' * `vec_size(dataframe)` == `vec_size(dataframe[[i]])`
#' * `vec_size(matrix)` == `vec_size(matrix[, i, drop = FALSE])`
#' * `vec_size(vec_c(x, y))` == `vec_size(x)` + `vec_size(y)`
#'
#' @param x,... Vector inputs
#' @param .size If `NULL`, the default, the output size is determined by
#'   recycling the lengths of all elements of `...`. Alternatively, you can
#'   supply `.size` to force a known size.
#' @return An integer (or double for long vectors). Will throw an error
#'   if `x` is not a vector.
#'
#'   `vec_size_common()` will return `0` if all inputs are `NULL` or absent.
#' @export
#' @examples
#' vec_size(1:100)
#' vec_size(mtcars)
#' vec_size(array(dim = c(3, 5, 10)))
#'
#' vec_size(NULL)
#' # Because vec_size(vec_c(NULL, x)) ==
#' #   vec_size(NULL) + vec_size(x) ==
#' #   vec_size(x)
#'
#' vec_size_common(1:10, 1:10)
#' vec_size_common(1:10, 1)
#' vec_size_common(1:10, integer())
vec_size <- function(x) {
  .Call(vctrs_size, x)
}

#' @export
#' @rdname vec_size
vec_size_common <- function(..., .size = NULL) {
  if (!is.null(.size)) {
    return(.size)
  }

  args <- compact(list2(...))
  if (length(args) == 0) {
    return(0L)
  }

  nobs <- map_int(args, vec_size)
  reduce(nobs, vec_size2)
}

vec_size2 <- function(nx, ny) {
  if (nx == ny) {
    nx
  } else if (nx == 0L || ny == 0L) {
    0L
  } else if (nx == 1L) {
    ny
  } else if (ny == 1L) {
    nx
  } else {
    abort(paste0("Incompatible lengths: ", nx, ", ", ny, "."))
  }
}


# sequences -------------------------------------------------------------------

#' Useful sequences
#'
#' `vec_seq_along()` is equivalent to [seq_along()] but uses size, not length.
#' `vec_na_along()` creates a vector of missing values with size matching
#' an existing object.
#'
#' @param x,y Vectors
#' @return
#' * `vec_seq_along()` an integer vector with the same size as `x`.
#' * `vec_na_along()` a vector with the same type as `x` and the same size
#'   as `y`.
#' @export
#' @examples
#' vec_seq_along(mtcars)
#' vec_na_along(head(mtcars))
vec_seq_along <- function(x) {
  seq_len(vec_size(x))
}

#' @export
#' @rdname vec_seq_along
vec_na_along <- function(x, y = x) {
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
