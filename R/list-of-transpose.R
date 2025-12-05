#' Transpose a list of homogenous vectors
#'
#' @description
#' `list_of_transpose()` takes a list of homogenous vectors, transposes it, and
#' returns a new list of homogenous vectors. To perform a transpose, three
#' pieces of information are required:
#'
#' - The _list size_, from [`vec_size(x)`][vec_size()].
#'
#' - The _element size_, from [`list_of_size(x)`][list_of_size()].
#'
#' - The _element type_, from [`list_of_ptype(x)`][list_of_ptype()].
#'
#' Because all three of these are required, this function only works on fully
#' specified [list_of()]s, with both `size` and `ptype` specified.
#'
#' To predict the output from `list_of_transpose()`, swap the list size with the
#' element size. For example:
#'
#' - Input: `list_of<integer[3]>[2]`
#' - Output: `list_of<integer[2]>[3]`
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#'
#' @param x A [list_of][list_of()] with both `size` and `ptype` specified.
#'
#' @param null A value to replace `NULL` elements with before transposing.
#'
#'   If left unspecified, any `NULL` elements in `x` result in an error.
#'
#'   If specified:
#'
#'   - Will be [recycled][theory-faq-recycling] to
#'     [`list_of_size(x)`][list_of_size()] before transposing.
#'
#'   - Will be [cast][theory-faq-coercion] to
#'     [`list_of_ptype(x)`][list_of_ptype()] before transposing.
#'
#' @param x_arg Argument name used in error messages.
#'
#' @returns
#' A `list_of` of size `list_of_size(x)`, with an element size of `vec_size(x)`
#' and an element type of `list_of_ptype(x)`.
#'
#' @export
#' @examples
#' # A form of `list_of()` that infers both ptype and size
#' list_of2 <- function(...) {
#'   list_of(..., .ptype = NULL, .size = NULL)
#' }
#'
#' # I: list_of<integer[2]>[3]
#' # O: list_of<integer[3]>[2]
#' list_of_transpose(list_of2(1:2, 3:4, 5:6))
#'
#' # With data frames
#' x <- data_frame(a = 1:2, b = letters[1:2])
#' y <- data_frame(a = 3:4, b = letters[3:4])
#' list_of_transpose(list_of2(x, y))
#'
#' # Size 1 elements are recycled
#' list_of_transpose(list_of2(1, 2:3, 4))
#'
#' # ---------------------------------------------------------------------------
#' # `NULL` handling
#'
#' # `NULL` values aren't allowed in `list_of_transpose()`
#' x <- list_of2(1:3, NULL, 5:7, NULL)
#' try(list_of_transpose(x))
#'
#' # Replace them with `null`, which will be recycled to the element size,
#' # and cast to the element type
#' list_of_transpose(x, null = NA)
#' list_of_transpose(x, null = -(1:3))
#'
#' # ---------------------------------------------------------------------------
#' # Reversibility
#'
#' # Because `list_of_transpose()` takes and returns fully specified list-ofs,
#' # it is fully reversible, even in the edge cases.
#' x <- list_of2(integer(), integer())
#'
#' # This returns a list of size 0
#' # I: list_of<integer[0]>[2]
#' # O: list_of<integer[2]>[0]
#' out <- list_of_transpose(x)
#' out
#'
#' # Even though there are no elements, we know the element size and type,
#' # so we can transpose a second time to recover `x`. This would not be
#' # possible if this function returned a bare `list()`, which would result
#' # in lost information.
#' # I: list_of<integer[2]>[0]
#' # O: list_of<integer[0]>[2]
#' list_of_transpose(out)
#'
#' # ---------------------------------------------------------------------------
#' # Padding
#'
#' # If you'd like to pad with a missing value rather than erroring,
#' # you might do something like this, which left-pads before conversion
#' # to list-of.
#' x <- list(1, 2:5, 6:7)
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
#' x <- as_list_of(x, .ptype = NULL, .size = NULL)
#'
#' list_of_transpose(x)
list_of_transpose <- function(
  x,
  ...,
  null = NULL,
  x_arg = caller_arg(x),
  error_call = current_env()
) {
  check_dots_empty0(...)

  check_list_of(x, arg = x_arg, call = error_call)

  size <- list_of_size0(x)
  ptype <- list_of_ptype0(x)

  if (is_null(size)) {
    abort(
      c(
        cli::format_inline(
          "{arg_backtick(x_arg)} must be a fully specified `<list_of>`."
        ),
        i = "`size` is not specified."
      ),
      call = error_call
    )
  }
  if (is_null(ptype)) {
    abort(
      c(
        cli::format_inline(
          "{arg_backtick(x_arg)} must be a fully specified `<list_of>`."
        ),
        i = "`ptype` is not specified."
      ),
      call = error_call
    )
  }

  x_data <- vec_data(x)

  if (is_null(null)) {
    if (vec_any_missing(x_data)) {
      abort(
        c(
          cli::format_inline(
            "{arg_backtick(x_arg)} can't contain `NULL` values."
          ),
          i = "Specify `null` to replace them."
        ),
        call = error_call
      )
    }
  } else {
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

  new_list_of0(out, ptype = ptype, size = x_size)
}
