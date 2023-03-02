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
#' `list_sizes()` returns an integer vector containing the size of each element
#' of a list. It is nearly equivalent to, but faster than,
#' `map_int(x, vec_size)`, with the exception that `list_sizes()` will
#' error on non-list inputs, as defined by [obj_is_list()]. `list_sizes()` is
#' to `vec_size()` as [lengths()] is to [length()].
#'
#' @seealso [vec_slice()] for a variation of `[` compatible with `vec_size()`,
#'   and [vec_recycle()] to [recycle][vector_recycling_rules] vectors to common
#'   length.
#' @section Invariants:
#' * `vec_size(dataframe)` == `vec_size(dataframe[[i]])`
#' * `vec_size(matrix)` == `vec_size(matrix[, i, drop = FALSE])`
#' * `vec_size(vec_c(x, y))` == `vec_size(x)` + `vec_size(y)`
#'
#' @inheritParams rlang::args_error_context
#'
#' @param x,... Vector inputs or `NULL`.
#' @param .size If `NULL`, the default, the output size is determined by
#'   recycling the lengths of all elements of `...`. Alternatively, you can
#'   supply `.size` to force a known size; in this case, `x` and `...` are
#'   ignored.
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
#' @section Dependencies:
#' - [vec_proxy()]
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
#'
#' list_sizes(list("a", 1:5, letters))
vec_size <- function(x) {
  .Call(ffi_size, x, environment())
}

#' @export
#' @rdname vec_size
vec_size_common <- function(...,
                            .size = NULL,
                            .absent = 0L,
                            .arg = "",
                            .call = caller_env()) {
  .External2(ffi_size_common, .size, .absent)
}

#' @rdname vec_size
#' @export
list_sizes <- function(x) {
  .Call(ffi_list_sizes, x, environment())
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
#' @param y Value to use if `x` is empty. To preserve type-stability, should
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

vec_as_short_length <- function(n,
                                arg = caller_arg(n),
                                call = caller_env()) {
  .Call(ffi_as_short_length, n, environment())
}
