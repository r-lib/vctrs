#' Transpose a list of vectors
#'
#' @description
#' `list_transpose()` takes a list of vectors of equal size, transposes it, and
#' returns a new list of vectors of equal size.
#'
#' To predict the output from `list_transpose()`, swap the size of the list
#' with the size of the list elements. For example:
#'
#' - Input: List of size 2, elements of size 3
#' - Output: List of size 3, elements of size 2
#'
#' @details
#' In an ideal world, this function would transpose a data frame. Data frames
#' have a few desirable properties:
#'
#' - Each column is the same size
#' - `NULL` columns are not allowed
#'
#' The downside is that data frames must have names. When transposing, names
#' are meaningless, both on the input and output.
#'
#' The compromise struck here is to allow a list, which doesn't require names,
#' but to enforce that each element of the list must be the same size and can't
#' be `NULL`.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#'
#' @param x A list of vectors.
#'
#'   - Each vector must be the same size.
#'
#'   - Each vector will be [cast][vctrs::theory-faq-coercion] to the common type
#'     before transposing.
#'
#'   - `NULL` elements are not allowed.
#'
#' @param size The expected size of each element of `x`. If not provided,
#'   computed as `vec_size(x[[1L]])`, or `0L` if `x` is empty.
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
#' - `vec_size(list_transpose(x)) == size %||% vec_size(x[[1L]]) %||% 0L`
#'
#' For the list elements:
#'
#' - `vec_ptype(list_transpose(x)[[i]]) == vec_ptype_common(!!!x, .ptype = ptype)`
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
#' # Note that this function doesn't recycle elements. This is purposeful,
#' # it is meant for transposing "rectangular lists", which are lists with
#' # elements of equal size.
#' x <- list(1, 2:3)
#' try(list_transpose(x))
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
#' # `NULL` values aren't allowed in `list_transpose()` because theoretically
#' # this function is meant to transpose a data frame, which can't have `NULL`
#' # columns.
#' x <- list(1:3, NULL, 5:7, NULL)
#' try(list_transpose(x))
#'
#' # Either drop the `NULL` values or replace them with something else
#' list_transpose(list_drop_empty(x))
#'
#' na <- list(vec_rep(NA, vec_size_common(!!!x)))
#' x <- vec_assign(x, vec_detect_missing(x), na)
#' list_transpose(x)
list_transpose <- function(
  x,
  ...,
  size = NULL,
  ptype = NULL,
  x_arg = caller_arg(x),
  error_call = current_env()
) {
  # NOTES:
  #
  # # Recycling
  #
  # Resist the urge to provide support for recycling in this function. It is
  # designed to transpose a "df-list" or "rectangular list" returned by
  # `df_list()`, which already contains vectors of equal size. We treat any
  # differing sizes as a user error.
  #
  # This also leaves open the possibility of providing automatic support for
  # padding via `too_short = c("error", "left", "right"), pad = NULL` where
  # `pad` could be a scalar value to pad with when choosing `"left"` or
  # `"right"`.
  #
  # # `NULL` handling
  #
  # We spent a lot of effort trying to rationalize `NULL` handling, but
  # ultimately decided to treat this function as if it takes a "df-list",
  # which don't allow `NULL` columns.
  #
  # Simply dropping `NULL`s is not a good option. That would break `vec_size()`
  # invariants of `list_transpose()`. For example:
  #
  # ```
  # I: List size 3, Element size 4
  # O: List size 4, Element size 3 (in theory)
  # O: List size 4, Element size 2 (if you drop `NULL`s)
  # list_transpose(list(1:4, NULL, 5:8))
  # ```
  #
  # Adding a `null` argument that accepts a vector to replace `NULL`s with is
  # extremely tricky to get right due to the number of edge cases related to
  # empty inputs. In particular, deciding whether `null` should participate in
  # size determination or not is very confusing.
  #
  # ```
  # # If `null` participates in size determination
  # # I: List size 0, Element size 2 (infer from `null`?)
  # # O: List size 2, Element size 0
  # list_transpose(list(), null = 1:2)
  # # list(integer(), integer())
  # # Very weird output result, expecting `list()`
  #
  # # If `null` must be the input element size, then this errors
  # list_transpose(list(), null = 1:2)
  # # Error: Can't recycle `null` (2) to size 0.
  # # But then this works
  # list_transpose(list(1:2), null = 1:2)
  # # You'd have to do this
  # list_transpose(list(), null = 1:2, size = 2)
  # ```
  #
  # ```
  # # If `null` participates in size determination
  # # I: List size 1, Element size 2 (infer from `null`?)
  # # O: List size 2, Element size 1
  # list_transpose(list(NULL), null = 1:2)
  # # list(1L, 2L)
  #
  # # If `null` must be the input element size, then this errors
  # list_transpose(list(NULL), null = 1:2)
  # # Error: Can't recycle `null` (2) to size 0.
  # # Again, you'd have to do this
  # list_transpose(list(NULL), null = 1:2, size = 2)
  #
  # # This is weird because it feels like it should be identical to replacing
  # # the `NULL`s up front, i.e.
  # list_transpose(list(1:2))
  # ```
  #
  # And then you also have to deal with common type. Does `null` contribute to
  # the common type or is it cast to the input element type? What happens when
  # there are no inputs or only `NULL`s and the type is `unspecified`?

  check_dots_empty0(...)

  obj_check_list(x, arg = x_arg, call = error_call)

  # Disallow `NULL` elements - i.e. pretend the user supplied a data frame,
  # which can't have `NULL` columns
  list_check_all_vectors(
    x,
    allow_null = FALSE,
    arg = x_arg,
    call = error_call
  )

  # Finalize `size`
  if (is_null(size)) {
    if (vec_size(x) == 0L) {
      size <- 0L
    } else {
      size <- vec_size(x[[1L]])
    }
  }

  # Check that all elements are the same size, with no recycling - i.e. pretend
  # the user supplied a data frame, where all columns are the same size
  list_check_all_size(
    x,
    size = size,
    arg = x_arg,
    call = error_call
  )

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
