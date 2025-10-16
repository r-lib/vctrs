#' Transpose a list of vectors
#'
#' @description
#' `list_transpose()` takes a list of vectors, transposes it, and returns a new
#' list of vectors.
#'
#' To predict the output from `list_transpose()`, swap the size of the list
#' with the size of the list elements. For example:
#'
#' - Input: List of size 2, elements of size 3
#' - Output: List of size 3, elements of size 2
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#'
#' @param x A list of vectors.
#'
#'   - Each vector will be [recycled][theory-faq-recycling] to the common size
#'     before transposing.
#'
#'   - Each vector will be [cast][theory-faq-coercion] to the common type before
#'     transposing.
#'
#' @param null A value to replace `NULL` elements with before transposing.
#'
#'   If left unspecified, any `NULL` elements in `x` result in an error.
#'
#'   If specified:
#'
#'   - Will be [recycled][theory-faq-recycling] to the common size of `x` before
#'     transposing.
#'
#'   - Will participate in determining the common type, and will be
#'     [cast][theory-faq-coercion] to that type before transposing.
#'
#'   Note that `null` can alter the output type, but cannot alter the output
#'   size.
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
#' - `vec_size(list_transpose(x)) == vec_size_common(!!!x)`
#'
#' For the list elements:
#'
#' - `vec_ptype(list_transpose(x)[[i]]) == vec_ptype_common(!!!x)`
#' - `vec_size(list_transpose(x)[[i]]) == vec_size(x)`
#'
#' @export
#' @examples
#' # I: List size 3, Element size 2
#' # O: List size 2, Element size 3
#' list_transpose(list(1:2, 3:4, 5:6))
#'
#' # With data frames
#' x <- data_frame(a = 1:2, b = letters[1:2])
#' y <- data_frame(a = 3:4, b = letters[3:4])
#' list_transpose(list(x, y))
#'
#' # Size 1 elements are recycled
#' list_transpose(list(1, 2:3, 4))
#'
#' # With size 0 elements, the invariants are a bit tricky!
#' # This must return a size 0 list, but then you lose expected
#' # type (integer) and size (2) information about the elements.
#' # Losing that information makes it difficult to reverse the
#' # transposition.
#' #
#' # I: List size 2, Element size 0
#' # O: List size 0, Element size 2
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
#' # If you'd like to pad with a missing value rather than erroring,
#' # you might do something like this, which left-pads
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
#' # Replace them with `null`
#' list_transpose(x, null = NA)
#' list_transpose(x, null = -(1:3))
#'
#' # Note that using `null` is not fully identical to swapping any `NULL`s
#' # with their replacement value ahead of the `list_transpose()` call.
#' # Most of the time `null` works as you'd expect, but some confusion can occur
#' # when you have a length >1 `null` and `x` is an empty list, a list of
#' # `NULL`s, or a list with only size 1 elements. The main thing to remember is
#' # that `null` is not allowed to change the output size, which makes it more
#' # predictable to program with, but sometimes requires you to provide more
#' # information through `size`.
#'
#' # This is an error, because the common size from the list is 0,
#' # and you can't recycle `null` to that size.
#' try(list_transpose(list(), null = 3:4))
#' try(list_transpose(list(NULL), null = 3:4))
#'
#' # This is also an error, because the common size from the list is 1,
#' # and you can't recycle `null` to that size either.
#' try(list_transpose(list(1, 2), null = 3:4))
#'
#' # If you're programming with `list_transpose()` and you're supplying a
#' # length >1 `null` value like this, then that implies you know the
#' # expected element size (otherwise you wouldn't have been able to create
#' # the `null` value). Supply that `size` to override the inferred common size,
#' # and then things work as expected:
#'
#' # I: List size 0, Element size 2
#' # O: List size 2, Element size 0
#' list_transpose(list(), null = 3:4, size = 2)
#'
#' # I: List size 1, Element size 2
#' # O: List size 2, Element size 1
#' list_transpose(list(NULL), null = 3:4, size = 2)
#'
#' # I: List size 2, Element size 2
#' # O: List size 2, Element size 2
#' list_transpose(list(1, 2), null = 3:4, size = 2)
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

  # Disallow `NULL` elements if the user isn't replacing them with something
  list_check_all_vectors(
    x,
    allow_null = !is_null(null),
    arg = x_arg,
    call = error_call
  )

  # `size` only comes from `x` and `size`.
  # `null` is recycled to this size but doesn't contribute to it!
  size <- vec_size_common(
    !!!x,
    .size = size,
    .arg = x_arg,
    .call = error_call
  )

  # `ptype` comes from `x`, `null`, and `ptype`
  ptype <- list_transpose_ptype_common(
    x,
    null,
    ptype,
    x_arg,
    error_call
  )

  if (is.object(x)) {
    # The list input type should not affect the transposition process in any
    # way. In particular, supplying a list subclass that doesn't have a
    # `vec_cast.subclass.list` method shouldn't prevent the insertion of
    # `list(null)` before the transposition. The fact that we must insert
    # `list(null)` should be considered an internal detail.
    x <- unclass(x)
  }

  if (!is_null(null)) {
    # Always perform `null` checks
    null <- vec_cast(
      x = null,
      to = ptype,
      x_arg = "null",
      to_arg = "",
      call = error_call
    )

    null <- vec_recycle(
      x = null,
      size = size,
      x_arg = "null",
      call = error_call
    )
    # TODO!: vec_check_recyclable()

    if (vec_any_missing(x)) {
      null <- list(null)
      x <- vec_assign(x, vec_detect_missing(x), null)
    }
  }

  x_size <- vec_size(x)
  sizes <- vec_rep(x_size, times = size)

  out <- list_interleave(
    x,
    size = size,
    ptype = ptype,
    name_spec = "inner",
    x_arg = x_arg,
    error_call = error_call
  )

  # Chop the one big vector into transposed pieces of size `x_size`
  out <- vec_chop(out, sizes = sizes)

  out
}

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
