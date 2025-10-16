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
#'   size. See the examples for consequences of this.
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
#' # ---------------------------------------------------------------------------
#' # Using `size` and `ptype`
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
#' # ---------------------------------------------------------------------------
#' # Padding
#'
#' # If you'd like to pad with a missing value rather than erroring,
#' # you might do something like this, which left-pads
#' x <- list(1, 2:5, 6:7)
#' try(list_transpose(x))
#'
#' sizes <- list_sizes(x)
#' size <- max(sizes)
#' index <- which(sizes != size)
#'
#' x[index] <- lapply(
#'   index,
#'   function(i) vec_c(rep(NA, times = size - sizes[[i]]), x[[i]])
#' )
#' x
#'
#' list_transpose(x)
#'
#' # ---------------------------------------------------------------------------
#' # `NULL` handling
#'
#' # `NULL` values aren't allowed in `list_transpose()`
#' x <- list(1:3, NULL, 5:7, NULL)
#' try(list_transpose(x))
#'
#' # Replace them with `null`
#' list_transpose(x, null = NA)
#' list_transpose(x, null = -(1:3))
#'
#' # When you don't know the list element size up front, but you still want
#' # to replace `NULL`s with something, use a size 1 `null` value which will
#' # get recycled to the element size after it has been computed
#' list_transpose(list(), null = NA)
#' list_transpose(list(1, NULL, 3), null = NA)
#' list_transpose(list(1, NULL, 3:4), null = NA)
#'
#' # When you do know the list element size up front, it's best to also provide
#' # that information as `size`, as this helps direct the recycling process
#' # for `null`, particularly in the cases of an empty list, a list of `NULL`s,
#' # or a list of size 1 elements. You typically know the list element size if
#' # you are providing a `null` of size != 1, because otherwise you wouldn't
#' # have been able to make `null` in the first place!
#' size <- 2L
#' null <- 3:4
#'
#' # `size` overrides the inferred element size of 0
#' #
#' # I: List size 0, Element size 0
#' # O: List size 0, Element size 0
#' try(list_transpose(list(), null = null))
#' # I: List size 0, Element size 2
#' # O: List size 2, Element size 0
#' list_transpose(list(), null = null, size = size)
#'
#' # Same idea here
#' #
#' # I: List size 1, Element size 0
#' # O: List size 0, Element size 1
#' try(list_transpose(list(NULL), null = null))
#' # I: List size 1, Element size 2
#' # O: List size 2, Element size 1
#' list_transpose(list(NULL), null = null, size = size)
#'
#' # `size` overrides the inferred element size of 1
#' #
#' # I: List size 2, Element size 1
#' # O: List size 1, Element size 2
#' try(list_transpose(list(1, 2), null = null))
#' # I: List size 2, Element size 2
#' # O: List size 2, Element size 2
#' list_transpose(list(1, 2), null = null, size = size)
#'
#' # The reason that `list_transpose()` recycles `null` to the common size
#' # rather than letting `null` participate in common size determination is
#' # due to this example. When supplying a size 1 `null`, most of the time
#' # you don't know the element size, and you just want `null` to recycle to
#' # whatever the required size will be. If `null` participated in determining
#' # the common size, the output of this would be `list(logical())` rather than
#' # `list()` because the element size would be computed as 1. Since a size 1
#' # `null` is much more common than a size !=1 `null`, we've optimized for this
#' # case at the cost of needing to specify `size` explicitly in some scenarios.
#' list_transpose(list(), null = NA)
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
