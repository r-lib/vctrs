#' `list_of` S3 class for homogenous lists
#'
#' A `list_of` object is a list where each element has the same type.
#' Modifying the list with `$`, `[`, and `[[` preserves the constraint
#' by coercing all input items.
#'
#' Unlike regular lists, setting a list element to `NULL` using `[[`
#' does not remove it.
#'
#' @inheritParams vec_c
#' @param x For `as_list_of()`, a vector to be coerced to list_of.
#' @param y,to Arguments to `vec_ptype2()` and `vec_cast()`.
#' @export
#' @examples
#' x <- list_of(1:3, 5:6, 10:15)
#' if (requireNamespace("tibble", quietly = TRUE)) {
#'   tibble::tibble(x = x)
#' }
#'
#' vec_c(list_of(1, 2), list_of(FALSE, TRUE))
list_of <- function(..., .ptype = NULL) {
  args <- list2(...)
  list_as_list_of(args, ptype = .ptype)
}

#' @export
#' @rdname list_of
as_list_of <- function(x, ...) {
  UseMethod("as_list_of")
}

#' @export
as_list_of.vctrs_list_of <- function(x, ..., .ptype = NULL) {
  if (!is.null(.ptype)) {
    x <- unclass(x)
    list_as_list_of(x, ptype = .ptype)
  } else {
    x
  }
}

#' @export
as_list_of.list <- function(x, ..., .ptype = NULL) {
  list_as_list_of(x, ptype = .ptype)
}

#' Create list_of subclass
#'
#' @param x A list
#' @param ptype The prototype which every element of `x` belongs to
#' @param ... Additional attributes used by subclass
#' @param class Optional subclass name
#' @keywords internal
#' @export
new_list_of <- function(
  x = list(),
  ptype = logical(),
  ...,
  class = character()
) {
  if (!obj_is_list(x)) {
    abort("`x` must be a list.")
  }

  if (vec_size(ptype) != 0L) {
    abort("`ptype` must have size 0.")
  }

  new_list_of0(x = x, ptype = ptype, ..., class = class)
}

new_list_of0 <- function(x, ptype, ..., class = character()) {
  new_vctr(x, ..., ptype = ptype, class = c(class, "vctrs_list_of"))
}

list_of_unstructure <- function(x) {
  attr(x, "ptype") <- NULL
  attr(x, "class") <- NULL
  x
}

#' @export
#' @rdname list_of
is_list_of <- function(x) {
  inherits(x, "vctrs_list_of")
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
  param <- vec_ptype_full(attr(x, "ptype"))
  if (grepl("\n", param)) {
    param <- paste0(indent(paste0("\n", param), 2), "\n")
  }

  paste0("list_of<", param, ">")
}

#' @export
vec_ptype_abbr.vctrs_list_of <- function(x, ...) {
  paste0("list<", vec_ptype_abbr(attr(x, "ptype")), ">")
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
  wrapped_type <- attr(x, "ptype")
  value <- map(value, vec_cast, to = wrapped_type)
  value <- new_list_of0(value, ptype = wrapped_type)
  NextMethod()
}
#' @export
`[[<-.vctrs_list_of` <- function(x, i, value) {
  if (is.null(value)) {
    # Setting to NULL via [[ shortens the list! Example:
    # `[[<-`(list(1), 1, NULL)
    x[i] <- list(value)
    return(x)
  }

  value <- vec_cast(value, attr(x, "ptype"))
  NextMethod()
}

#' @export
`$<-.vctrs_list_of` <- function(x, i, value) {
  value <- vec_cast(value, attr(x, "ptype"))
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
  x_ptype <- attr(x, "ptype", exact = TRUE)
  y_ptype <- attr(y, "ptype", exact = TRUE)
  if (identical(x_ptype, y_ptype)) {
    return(x)
  }

  tryCatch(
    expr = {
      ptype <- vec_ptype2(x_ptype, y_ptype, x_arg = x_arg, y_arg = y_arg)
      new_list_of0(x = list(), ptype = ptype)
    },
    vctrs_error_incompatible_type = function(cnd) {
      list()
    }
  )
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
  x_ptype <- attr(x, "ptype", exact = TRUE)
  to_ptype <- attr(to, "ptype", exact = TRUE)

  if (identical(x_ptype, to_ptype)) {
    # FIXME: Suboptimal check for "same type", but should be good enough for the
    # common case of unchopping a list of identically generated list-ofs (#875).
    # Would be fixed by https://github.com/r-lib/vctrs/issues/1688.
    x
  } else {
    x <- unclass(x)
    list_as_list_of(x, ptype = to_ptype, error_call = call)
  }
}

#' @export
vec_cast.list.vctrs_list_of <- function(x, to, ...) {
  list_of_unstructure(x)
}
#' @export
vec_cast.vctrs_list_of.list <- function(x, to, ..., call = caller_env()) {
  list_as_list_of(
    x,
    attr(to, "ptype"),
    error_call = call
  )
}

# Helpers -----------------------------------------------------------------

list_as_list_of <- function(x, ptype = NULL, error_call = caller_env()) {
  ptype <- vec_ptype_common(!!!x, .ptype = ptype, .call = error_call)

  if (is.null(ptype)) {
    abort("Can't find common type for elements of `x`.", call = error_call)
  }

  x <- vec_cast_common(!!!x, .to = ptype, .call = error_call)

  new_list_of0(x = x, ptype = ptype)
}
