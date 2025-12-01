#' Transpose a list of vectors
#'
#' @description
#' `list_transpose()` takes a list of vectors, transposes it, and
#' returns a new list of vectors. To perform a transpose, three pieces of
#' information are required:
#'
#' - The _list size_. This is always known, and is `vec_size(x)`.
#'
#' - The _element size_. This is computed as `vec_size_common(!!!x)`. If `x`
#'   does not contain any vector inputs, then `size` must be provided, otherwise
#'   an error is thrown.
#'
#' - The _element type_. This is computed as `vec_ptype_common(!!!x)`. If `x`
#'   does not contain any vector inputs, then `ptype` must be provided,
#'   otherwise an error is thrown.
#'
#' Supplying both `size` and `ptype` is recommended when programming with
#' `list_transpose()`, as this removes all ambiguity and ensures an error is
#' never thrown.
#'
#' To predict the output from `list_transpose()`, swap the list size with the
#' element size. For example:
#'
#' - Input: List size 2, Element size 3, Element type integer
#' - Output: List size 3, Element size 2, Element type integer
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
#'   - Will be [cast][theory-faq-coercion] to the common type of `x` before
#'     transposing.
#'
#' @param size The expected size of each element of `x`. If not provided,
#'   computed automatically by [vec_size_common()].
#'
#'   If no vector inputs are provided, the common size cannot be determined
#'   automatically and `size` must be provided, otherwise an error will be
#'   thrown.
#'
#' @param ptype The expected type of each element of `x`. If not provided,
#'   computed automatically by [vec_ptype_common()].
#'
#'   If no vector inputs are provided, the common type cannot be determined
#'   automatically and `ptype` must be provided, otherwise an error will be
#'   thrown.
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
#' # I: List size 3, Element size 2, Element type integer
#' # O: List size 2, Element size 3, Element type integer
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
#' # `NULL` handling
#'
#' # `NULL` values aren't allowed in `list_transpose()`
#' x <- list(1:3, NULL, 5:7, NULL)
#' try(list_transpose(x))
#'
#' # Replace them with `null`, which will be recycled to the common size,
#' # and cast to the common type
#' list_transpose(x, null = NA)
#' list_transpose(x, null = -(1:3))
#'
#' # ---------------------------------------------------------------------------
#' # Unknown size and type
#'
#' # `list_transpose()` refuses to guess the element size and element type when
#' # no vector inputs have been provided.
#'
#' # I: List size 0, Element size ?, Element type ?
#' # O: List size ?, Element size 0, Element type ?
#' try(list_transpose(list()))
#'
#' # I: List size 0, Element size 2, Element type ?
#' # O: List size 2, Element size 0, Element type ?
#' try(list_transpose(list(), size = 2L))
#'
#' # Explicitly supplying both `size` and `ptype` removes all ambiguity
#'
#' # I: List size 0, Element size 2, Element type integer
#' # O: List size 2, Element size 0, Element type integer
#' list_transpose(list(), size = 2L, ptype = integer())
#'
#' # `NULL` does not contribute to the element size or element type, so a list
#' # of `NULL` also has an unknown element size and element type and will fail
#' # unless `size` and `ptype` are provided.
#'
#' # I: List size 2, Element size ?, Element type ?
#' # O: List size ?, Element size 2, Element type ?
#' try(list_transpose(list(NULL, NULL), null = NA))
#'
#' # I: List size 2, Element size 3, Element type double
#' # O: List size 3, Element size 2, Element type double
#' list_transpose(list(NULL, NULL), null = NA, size = 3, ptype = double())
#'
#' # ---------------------------------------------------------------------------
#' # Size 0 elements
#'
#' # With size 0 elements, the invariants are a bit tricky! This must return a
#' # size 0 list, but then you lose expected type (integer) and size (2)
#' # information about the elements. Losing that information makes it difficult
#' # to reverse the transposition.
#'
#' # I: List size 2, Element size 0, Element type integer
#' # O: List size 0, Element size 2, Element type integer (but 0 elements)
#' x <- list(integer(), integer())
#' out <- list_transpose(x)
#' out
#'
#' # Note how transposing a second time fails because it can't infer the element
#' # size or element type.
#'
#' # I: List size 0, Element size ?, Element type ?
#' # O: List size ?, Element size 0, Element type ?
#' try(list_transpose(out))
#'
#' # To work around this, provide the expected `size` and `ptype` manually
#'
#' # I: List size 0, Element size 2, Element type integer
#' # O: List size 2, Element size 0, Element type integer
#' list_transpose(out, size = 2L, ptype = integer())
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

  # `size` and `ptype` only come from `x`.
  # `null` is recycled to the common size and cast to the common type.
  size <- vec_size_common(
    !!!x,
    .size = size,
    .absent = -1L,
    .arg = x_arg,
    .call = error_call
  )
  ptype <- vec_ptype_common(
    !!!x,
    .ptype = ptype,
    .arg = x_arg,
    .call = error_call
  )

  # Refuse to guess if no vector inputs were provided.
  # Best practice is to always provide both when programming with this function.
  if (size == -1L) {
    abort(
      "Can't automatically infer the element size. Please supply `size` directly.",
      call = error_call
    )
  }
  if (is_null(ptype)) {
    abort(
      "Can't automatically infer the element type. Please supply `ptype` directly.",
      call = error_call
    )
  }

  # The list input type should not affect the transposition process in any
  # way. In particular, supplying a list subclass that doesn't have a
  # `vec_cast.subclass.list` method shouldn't prevent the insertion of
  # `list(null)` before the transposition. The fact that we must insert
  # `list(null)` should be considered an internal detail. We must assign
  # to `x_data` not `x`, because `x_arg` needs to refer to the original `x`
  # if we hit an error down in `list_interleave()`.
  x_data <- vec_data(x)

  if (!is_null(null)) {
    # Always perform `null` checks
    obj_check_vector(null, arg = "null", call = error_call)

    null <- vec_cast(
      x = null,
      to = ptype,
      x_arg = "null",
      to_arg = "",
      call = error_call
    )

    vec_check_recyclable(
      x = null,
      size = size,
      arg = "null",
      call = error_call
    )

    if (vec_any_missing(x_data)) {
      null <- list(null)
      x_data <- vec_assign(x_data, vec_detect_missing(x_data), null)
    }
  }

  x_size <- vec_size(x_data)
  sizes <- vec_rep(x_size, times = size)

  out <- list_interleave(
    x_data,
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
