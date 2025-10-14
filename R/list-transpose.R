#' Transpose a list of vectors
#'
#' @description
#' `list_transpose()` takes a list of vectors, transposes it, and returns a new
#' list of vectors.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#'
#' @param x A list.
#'
#' @param size The expected size of each element of `x`. If not provided,
#'   computed automatically by [vec_size_common()].
#'
#' @param ptype The expected type of each element of `x`. If not provided,
#'   computed automatically by [vec_ptype_common()].
#'
#' @param x_arg Argument name used in error messages.
#'
#' @returns
#' A list of vectors with the following invariants:
#'
#' For the list:
#'
#' - `vec_ptype(list_transpose(x)) == vec_ptype(x)`
#' - `vec_size(list_transpose(x)) == (size || vec_size_common(!!!x))`
#'
#' For the list elements:
#'
#' - `vec_ptype(list_transpose(x)[[i]]) == (ptype || vec_ptype_common(!!!x))`
#' - `vec_size(list_transpose(x)[[i]]) == vec_size(x)`
#'
#' @export
#' @examples
#' # Input:
#' # - List size 3
#' # - Element size 2
#' # Output:
#' # - List size 2
#' # - Element size 3
#' list_transpose(list(1:2, 3:4, 5:6))
#'
#' # With data frames
#' x <- data_frame(a = 1:2, b = letters[1:2])
#' y <- data_frame(a = 3:4, b = letters[3:4])
#' list_transpose(list(x, y))
#'
#' # Size 1 elements are recycled to the common size before transposing
#' list_transpose(list(1, 2:4))
#'
#' # With all size 1 elements, you can use `size` if you want to force a known
#' # common size other than size 1
#' list_transpose(list(1, 2), size = 3)
#'
#' # With size 0 elements, the invariants are a bit tricky!
#' # This must return a size 0 list, but then you lose expected
#' # type (integer) and size (2) information about the elements.
#' # Losing that information makes it difficult to reverse the
#' # transposition.
#' #
#' # Input:
#' # - List size 2
#' # - Element size 0
#' # Output:
#' # - List size 0
#' # - Element size 2
#' x <- list(integer(), integer())
#' out <- list_transpose(x)
#' out
#'
#' # Note how transposing a second time doesn't recover the original list
#' list_transpose(out)
#'
#' # To work around this, provide the lost `size` and `ptype` manually
#' list_transpose(out, size = vec_size(x), ptype = vec_ptype_common(!!!x))
#'
#' # If you'd like to pad with a missing value rather than recycling or
#' # erroring, you might do something like this, which left-pads
#' x <- list(1, 2:5, 6:7)
#' try(list_transpose(x))
#'
#' sizes <- list_sizes(x)
#' size <- max(sizes)
#' index <- which(sizes != size)
#' x[index] <- lapply(
#'   index,
#'   function(i) vec_c(rep(NA, times = size - sizes[[i]]), x[[i]])
#' )
#' list_transpose(x)
#'
#' # `NULL` values aren't allowed in `list_transpose()`. If you'd like `NULL`s
#' # to be treated as size 1 missing values, replace them with `NA` first.
#' x <- list(1:3, NULL, 5:7, NULL)
#' try(list_transpose(x))
#'
#' x <- vec_assign(x, vec_detect_missing(x), list(NA))
#' list_transpose(x)
list_transpose <- function(
  x,
  ...,
  size = NULL,
  ptype = NULL,
  x_arg = caller_arg(x),
  error_call = current_env()
) {
  check_dots_empty0(...)

  # Disallow `NULL` entirely. These would break `vec_size()` invariants of
  # `list_transpose()` if we simply drop them via `list_interleave()`.
  #
  # For example:
  #
  # ```
  # list_transpose(list(1:4, NULL, 5:8))
  # ```
  #
  # Input:
  # - List size 3
  # - Element size 4
  # Output:
  # - List size 4
  # - Element size 3
  #
  # But if we drop `NULL` you'd get:
  # - List size 4
  # - Element size 2
  #
  # A reasonable thing for users to do would be to replace `NULL` with `NA`
  # ahead of time. This is similar to `keep_empty` in some tidyr functions.
  # But we force the caller to make that decision, and it's a fairly easy
  # replacement to make with `vec_detect_missing()` and `vec_assign()`.
  #
  # ```
  # list_transpose(list(1:4, NA, 5:8))
  # ```
  allow_null <- FALSE

  obj_check_list(x, arg = x_arg, call = error_call)
  list_check_all_vectors(
    x,
    allow_null = allow_null,
    arg = x_arg,
    call = error_call
  )

  flat <- list_interleave(
    x,
    size = size,
    ptype = ptype,
    name_spec = "inner",
    x_arg = x_arg,
    error_call = error_call
  )

  x_size <- vec_size(x)

  if (is_null(size)) {
    # Identical to `elt_size <- vec_size_common(!!!x)`, but faster.
    # Utilizes known info about the `list_interleave()` return value.
    if (x_size == 0L) {
      elt_size <- 0L
    } else {
      elt_size <- vec_size(flat) / x_size
    }
  } else {
    elt_size <- size
  }

  sizes <- vec_rep(x_size, times = elt_size)

  # Chop the one big vector into transposed pieces of size `x_size`
  out <- vec_chop(flat, sizes = sizes)

  out
}
