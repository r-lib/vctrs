#' Construct a list of homogenous vectors
#'
#' @description
#' A `list_of` is a restricted version of a list that adds constraints on the
#' list elements.
#'
#' - `list_of(.ptype = )` restricts the _type_ of each element.
#'
#'   - `.ptype = <type>` asserts that each element has type `<type>`.
#'
#'   - `.ptype = NULL` infers the type from the original set of elements, or
#'     errors if no vector inputs were provided.
#'
#'   - `.ptype = rlang::zap()` doesn't restrict the type.
#'
#' - `list_of(.size = )` restricts the _size_ of each element.
#'
#'   - `.size = <size>` asserts that each element has size `<size>`.
#'
#'   - `.size = NULL` infers the size from the original set of elements, or
#'     errors if no vector inputs were provided.
#'
#'   - `.size = rlang::zap()` doesn't restrict the size.
#'
#' The default behavior infers the element type and doesn't restrict the size.
#'
#' Both `.ptype` and `.size` may be specified to restrict both the size and
#' type of the list elements. You cannot set both of these to `rlang::zap()`,
#' as that would be the same as a bare `list()` with no restrictions.
#'
#' Modifying a `list_of` with `$<-`, `[<-`, and `[[<-` preserves the constraints
#' by coercing and recycling all input items.
#'
#' @param ... For `list_of()`, vectors to include in the list.
#'
#'   For other methods, these dots must be empty.
#'
#' @param x For `as_list_of()`, a vector to be coerced to list_of.
#'
#'   For `is_list_of()`, an object to test.
#'
#' @param y,to Arguments to `vec_ptype2()` and `vec_cast()`.
#'
#' @param .ptype The type to restrict each list element to. One of:
#'
#'   - A prototype like `integer()` or `double()`.
#'
#'   - `NULL`, to infer the type from `...`. If no vector inputs are provided,
#'     an error is thrown.
#'
#'   - [rlang::zap()] to avoid placing any restrictions on the type.
#'
#' @param .size The size to restrict each list element to. One of:
#'
#'   - A scalar integer size.
#'
#'   - `NULL`, to infer the size from `...`. If no vector inputs are provided,
#'     an error is thrown.
#'
#'   - [rlang::zap()] to avoid placing any restrictions on the size.
#'
#' @export
#' @examples
#' # Restrict the type, but not the size
#' x <- list_of(1:3, 5:6, 10:15)
#' x
#'
#' if (requireNamespace("tibble", quietly = TRUE)) {
#'   # As a column in a tibble
#'   tibble::tibble(x = x)
#' }
#'
#' # Coercion happens during assignment
#' x[1] <- list(4)
#' typeof(x[[1]])
#'
#' try(x[1] <- list(4.5))
#'
#' # Restrict the size, but not the type
#' x <- list_of(1, 2:3, .ptype = rlang::zap(), .size = 2)
#' x
#'
#' # Recycling happens during assignment
#' x[1] <- list(4)
#' x
#'
#' try(x[1] <- list(3:6))
#'
#' # Restricting both size and type
#' x <- list_of(1L, 2:3, .ptype = integer(), .size = 2)
#' x
#'
#' # Setting an element to `NULL`
#' x[2] <- list(NULL)
#' x
#'
#' # Note that using `NULL` shortens the list, like a base R list
#' x[2] <- NULL
#' x
#'
#' # Combining a list_of with a list results in a list
#' vec_c(list_of(1), list(2, "x"))
#'
#' # Combining a list_of with another list_of tries to find a common element
#' # type and common element size, but will remove the constraint if that
#' # fails
#' x <- list_of(1, .ptype = double())
#' y <- list_of(c("a", "b"), .ptype = character(), .size = 2)
#' z <- list_of(c("c", "d", "e"), .ptype = character(), .size = 3)
#'
#' # Falls back to a list
#' vec_c(x, y)
#'
#' # Falls back to a `list_of<character>` with no size restriction
#' vec_c(y, z)
list_of <- function(..., .ptype = NULL, .size = zap()) {
  args <- list2(...)
  list_as_list_of(args, ptype = .ptype, size = .size)
}

#' @export
#' @rdname list_of
as_list_of <- function(x, ...) {
  UseMethod("as_list_of")
}

#' @export
as_list_of.vctrs_list_of <- function(x, ...) {
  x
}

#' @export
as_list_of.list <- function(x, ..., .ptype = NULL, .size = zap()) {
  list_as_list_of(x, ptype = .ptype, size = .size)
}

#' Create list_of subclass
#'
#' @param x A list
#' @param ptype The prototype which every element of `x` belongs to. If `NULL`,
#'   the prototype is not specified.
#' @param size The size which every element of `x` has. If `NULL`, the size is
#'   not specified.
#' @param ... Additional attributes used by subclass
#' @param class Optional subclass name
#' @keywords internal
#' @export
new_list_of <- function(
  x = list(),
  ptype = logical(),
  size = NULL,
  ...,
  class = character()
) {
  obj_check_list(x)

  has_ptype <- !is_null(ptype)
  has_size <- !is_null(size)

  if (!has_ptype && !has_size) {
    abort("Must specify at least one of `ptype` or `size`.")
  }

  if (has_ptype) {
    ptype <- vec_ptype(ptype, x_arg = "ptype")
    ptype <- vec_ptype_finalise(ptype)
  }

  if (has_size) {
    check_number_whole(size, min = 0)
    size <- vec_cast(size, integer())
  }

  new_list_of0(x = x, ptype = ptype, size = size, ..., class = class)
}

new_list_of0 <- function(x, ptype, size, ..., class = character()) {
  new_vctr(
    x,
    ...,
    ptype = ptype,
    size = size,
    class = c(class, "vctrs_list_of")
  )
}

list_of_unstructure <- function(x) {
  attr(x, "ptype") <- NULL
  attr(x, "size") <- NULL
  attr(x, "class") <- NULL
  x
}

list_of_ptype <- function(x) {
  attr(x, "ptype", exact = TRUE)
}

list_of_size <- function(x) {
  attr(x, "size", exact = TRUE)
}

#' @export
#' @rdname list_of
is_list_of <- function(x) {
  inherits(x, "vctrs_list_of")
}

check_list_of <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
  if (is_list_of(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a `<list_of>`",
    ...,
    arg = arg,
    call = call
  )
}

#' @export
vec_proxy.vctrs_list_of <- function(x, ...) {
  unclass(x)
}

# Formatting --------------------------------------------------------------

#' @export
obj_print_data.vctrs_list_of <- function(x, ...) {
  if (length(x) == 0) {
    return()
  }

  print(vec_data(x))
}

#' @export
format.vctrs_list_of <- function(x, ...) {
  format.default(x)
}

#' @export
vec_ptype_full.vctrs_list_of <- function(x, ...) {
  size <- list_of_size(x)
  if (is_null(size)) {
    size <- ""
  } else {
    size <- paste0("[", size, "]")
  }

  ptype <- list_of_ptype(x)
  if (is_null(ptype)) {
    ptype <- "any"
  } else {
    ptype <- vec_ptype_full(ptype)
  }

  ptype <- paste0(ptype, size)

  if (grepl("\n", ptype)) {
    ptype <- paste0(indent(paste0("\n", ptype), 2), "\n")
  }

  paste0("list_of<", ptype, ">")
}

#' @export
vec_ptype_abbr.vctrs_list_of <- function(x, ...) {
  size <- list_of_size(x)
  if (is_null(size)) {
    size <- ""
  } else {
    size <- paste0("[", size, "]")
  }

  ptype <- list_of_ptype(x)
  if (is_null(ptype)) {
    ptype <- "any"
  } else {
    ptype <- vec_ptype_abbr(ptype)
  }

  ptype <- paste0(ptype, size)

  paste0("list<", ptype, ">")
}

# vctr methods ------------------------------------------------------------

#' @export
as.list.vctrs_list_of <- function(x, ...) {
  list_of_unstructure(x)
}

#' @export
as.character.vctrs_list_of <- function(x, ...) {
  # For compatibility with the RStudio Viewer. See tidyverse/tidyr#654.
  map_chr(x, function(elt) paste0("<", vec_ptype_abbr(elt), ">"))
}

#' @export
`[[.vctrs_list_of` <- function(x, i, ...) {
  .Call(vctrs_list_get, x, i)
}

#' @export
`$.vctrs_list_of` <- function(x, i, ...) {
  .Call(vctrs_list_get, x, i)
}

#' @export
`[<-.vctrs_list_of` <- function(x, i, value) {
  if (is_null(value)) {
    return(NextMethod())
  }

  if (!obj_is_list(value)) {
    # Ideally the user provides a list, but if `value` is not a list, we chop
    # it. This matches list semantics where this works:
    #
    # ```
    # x <- list(1, 2, 3)
    # x[1:2] <- c(4, 5)
    # ```
    value <- vec_chop(value)
  }

  ptype <- list_of_ptype(x)
  if (!is_null(ptype)) {
    value <- map(value, vec_cast, to = ptype)
  }

  size <- list_of_size(x)
  if (!is_null(size)) {
    value <- map(value, vec_recycle, size = size)
  }

  value <- new_list_of0(value, ptype = ptype, size = size)

  NextMethod()
}

#' @export
`[[<-.vctrs_list_of` <- function(x, i, value) {
  if (is_null(value)) {
    return(NextMethod())
  }

  ptype <- list_of_ptype(x)
  if (!is_null(ptype)) {
    value <- vec_cast(value, ptype)
  }

  size <- list_of_size(x)
  if (!is_null(size)) {
    value <- vec_recycle(value, size)
  }

  NextMethod()
}

#' @export
`$<-.vctrs_list_of` <- function(x, i, value) {
  if (is_null(value)) {
    return(NextMethod())
  }

  ptype <- list_of_ptype(x)
  if (!is_null(ptype)) {
    value <- vec_cast(value, ptype)
  }

  size <- list_of_size(x)
  if (!is_null(size)) {
    value <- vec_recycle(value, size)
  }

  NextMethod()
}

# Type system -------------------------------------------------------------

#' @rdname list_of
#' @inheritParams vec_ptype2
#' @export vec_ptype2.vctrs_list_of
#' @method vec_ptype2 vctrs_list_of
#' @export
vec_ptype2.vctrs_list_of <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2.vctrs_list_of")
}

#' @method vec_ptype2.vctrs_list_of vctrs_list_of
#' @export
vec_ptype2.vctrs_list_of.vctrs_list_of <- function(
  x,
  y,
  ...,
  x_arg = "",
  y_arg = ""
) {
  x_ptype <- list_of_ptype(x)
  y_ptype <- list_of_ptype(y)

  x_size <- list_of_size(x)
  y_size <- list_of_size(y)

  if (identical(x_ptype, y_ptype) && identical(x_size, y_size)) {
    return(x)
  }

  # Common type always goes towards more lenient type
  #
  # Element type:
  # - If either `x_ptype` or `y_ptype` are `NULL`, fall back to `NULL`
  # - If both `x_ptype` and `y_ptype` are specified, try common type but fall
  #   back to `NULL`
  #
  # Element size:
  # - If either `x_size` or `y_size` are `NULL`, fall back to `NULL`
  # - If both `x_ptype` and `y_ptype` are specified, try common size but fall
  #   back to `NULL`
  #
  # If both `ptype` and `size` are `NULL` after this, return bare `list()`,
  # otherwise return `list_of()` with appropriate restrictions. Note that with
  # this set up we may fail a ptype2 determination but pass a size2
  # determination and still return a list-of.
  if (is_null(x_ptype) || is_null(y_ptype)) {
    ptype <- NULL
  } else {
    ptype <- tryCatch(
      vec_ptype2(x_ptype, y_ptype),
      vctrs_error_incompatible_type = function(cnd) NULL
    )
  }

  if (is_null(x_size) || is_null(y_size)) {
    size <- NULL
  } else {
    # No `vec_size2()`. This uses ALTREP to be efficient.
    size <- tryCatch(
      vec_size_common(seq_len(x_size), seq_len(y_size)),
      vctrs_error_incompatible_size = function(cnd) NULL
    )
  }

  if (is_null(ptype) && is_null(size)) {
    list()
  } else {
    new_list_of0(x = list(), ptype = ptype, size = size)
  }
}

#' @export
vec_ptype2.list.vctrs_list_of <- function(x, y, ...) {
  list()
}
#' @export
vec_ptype2.vctrs_list_of.list <- function(x, y, ...) {
  list()
}

#' @rdname list_of
#' @export vec_cast.vctrs_list_of
#' @method vec_cast vctrs_list_of
#' @export
vec_cast.vctrs_list_of <- function(x, to, ...) {
  UseMethod("vec_cast.vctrs_list_of")
}

#' @export
#' @method vec_cast.vctrs_list_of vctrs_list_of
vec_cast.vctrs_list_of.vctrs_list_of <- function(
  x,
  to,
  ...,
  call = caller_env()
) {
  x_ptype <- list_of_ptype(x)
  to_ptype <- list_of_ptype(to)

  x_size <- list_of_size(x)
  to_size <- list_of_size(to)

  if (identical(x_ptype, to_ptype) && identical(x_size, to_size)) {
    # FIXME: Suboptimal check for "same type", but should be good enough for the
    # common case of unchopping a list of identically generated list-ofs (#875).
    # Would be fixed by https://github.com/r-lib/vctrs/issues/1688.
    return(x)
  }

  x <- list_of_unstructure(x)

  ptype <- to_ptype %||% zap()
  size <- to_size %||% zap()

  list_as_list_of(
    x = x,
    ptype = ptype,
    size = size,
    error_call = call
  )
}

#' @export
vec_cast.list.vctrs_list_of <- function(x, to, ...) {
  list_of_unstructure(x)
}
#' @export
vec_cast.vctrs_list_of.list <- function(x, to, ..., call = caller_env()) {
  ptype <- list_of_ptype(to) %||% zap()
  size <- list_of_size(to) %||% zap()
  list_as_list_of(x, ptype = ptype, size = size, error_call = call)
}

# Helpers -----------------------------------------------------------------

list_as_list_of <- function(x, ptype, size, error_call = caller_env()) {
  free_ptype <- is_zap(ptype)
  free_size <- is_zap(size)

  if (free_ptype && free_size) {
    abort("Can't set both `ptype` and `size` to `<zap>`.", call = error_call)
  }

  if (free_ptype) {
    # Not locked
    ptype <- NULL
  } else {
    # Lock to a type or die trying
    ptype <- vec_ptype_common(
      !!!x,
      .ptype = ptype,
      .call = error_call
    )
    if (is_null(ptype)) {
      abort("Can't find common type for elements of `x`.", call = error_call)
    }
    x <- vec_cast_common(!!!x, .to = ptype, .call = error_call)
  }

  if (free_size) {
    # Not locked
    size <- NULL
  } else {
    # Lock to a size or die trying
    size <- vec_size_common(
      !!!x,
      .size = size,
      .absent = -1L,
      .call = error_call
    )
    if (size == -1L) {
      abort("Can't find common size for elements of `x`.", call = error_call)
    }
    x <- vec_recycle_common(!!!x, .size = size, .call = error_call)
  }

  new_list_of0(x = x, ptype = ptype, size = size)
}
