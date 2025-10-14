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
#' @param null A value to replace `NULL` elements with before transposing.
#'
#'   If specified:
#'
#'   - `null` must be size 1.
#'
#'   - `null` will participate in common type determination alongside the
#'     elements of `x`.
#'
#'   If not specified, an error will be thrown if any `NULL` values are
#'   detected.
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
#' - `vec_ptype(list_transpose(x)) == <list>`
#' - `vec_size(list_transpose(x)) == vec_size_common(!!!x, .size = size)`
#'
#' For the list elements:
#'
#' - `vec_ptype(list_transpose(x)[[i]]) == vec_ptype_common(!!!x, .ptype = ptype)`
#' - `vec_size(list_transpose(x)[[i]]) == vec_size(x)`
#'
#' If `NULL` elements are present in `x`, then an error is thrown unless `null`
#' is provided, in which case any `NULL` elements are treated as size 1 for the
#' common size computation.
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
#' # `NULL` values aren't allowed in `list_transpose()`
#' x <- list(1:3, NULL, 5:7, NULL)
#' try(list_transpose(x))
#'
#' # Use `null` to replace `NULL` values before transposing
#' list_transpose(x, null = NA)
#' list_transpose(x, null = 0L)
list_transpose <- function(
  x,
  ...,
  null = NULL,
  size = NULL,
  ptype = NULL,
  x_arg = caller_arg(x),
  error_call = current_env()
) {
  check_dots_empty0(...)

  obj_check_list(x, arg = x_arg, call = error_call)

  if (is.object(x)) {
    # The list input type should not affect the transposition process in any
    # way. In particular, supplying a list subclass that doesn't have a
    # `vec_cast.subclass.list` method shouldn't prevent the insertion of
    # `list(null)` before the transposition. The fact that we must insert
    # `list(null)` should be considered an internal detail.
    x <- unclass(x)
  }

  # We disallow `NULL` elements. These would break `vec_size()` invariants of
  # `list_transpose()` if we simply drop them via `list_interleave()`.
  #
  # Either `list_check_all_vectors()` errors, or the user supplied `null` which
  # will replace `NULL`s with size 1 vectors before we `list_interleave()`.
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
  # Users should instead use `null` to replace `NULL` elements with something
  # else, like `NA`. This is similar to `purrr::list_transpose(default =)` and
  # `keep_empty` in some tidyr functions.
  #
  # ```
  # list_transpose(list(1:4, NULL, 5:8), null = NA)
  # ```
  allow_null <- !is_null(null)

  list_check_all_vectors(
    x,
    allow_null = allow_null,
    arg = x_arg,
    call = error_call
  )

  ptype <- list_transpose_ptype_common(
    x,
    null,
    ptype,
    x_arg,
    error_call
  )

  if (!is_null(null)) {
    # Do `null` checks regardless of usage
    null <- vec_cast(
      x = null,
      to = ptype,
      x_arg = "null",
      to_arg = "",
      call = error_call
    )
    vec_check_size(
      null,
      size = 1L,
      arg = "null",
      call = error_call
    )

    if (vec_any_missing(x)) {
      null <- list(null)
      x <- vec_assign(x, vec_detect_missing(x), null)
    }
  }

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

# Computes the `ptype` incorporating both `x` and `null`
#
# Like `ptype_finalize()` in `vec_recode_values()` and `vec_if_else()`
list_transpose_ptype_common <- function(
  x,
  null,
  ptype,
  x_arg,
  error_call
) {
  if (!is_null(ptype)) {
    # Validate and return user specified `ptype`
    ptype <- vec_ptype(ptype, x_arg = "ptype", call = error_call)
    return(vec_ptype_finalise(ptype))
  }

  # Compute from `x`
  ptype <- vec_ptype_common(!!!x, .arg = x_arg, .call = error_call)

  if (!is_null(null)) {
    # Layer in `null`
    ptype <- vec_ptype2(
      x = null,
      y = ptype,
      x_arg = "null",
      y_arg = "",
      call = error_call
    )
  }

  ptype
}
