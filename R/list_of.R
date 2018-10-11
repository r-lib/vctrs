#' `list_of` S3 class for homogenous lists
#'
#' A `list_of` object is a list where each element has the same type.
#' Modifying the list with `$`, `[`, and `[[` preserves the constraint
#' by coercing all input items.
#'
#' @inheritParams vec_c
#' @param x For `as_list_of()`, a vector to be coerced to list_of.
#' @param y,to Arguments to `vec_type2()` and `vec_cast()`.
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

  ptype <- vec_type_common(!!!args, .ptype = .ptype)
  if (is.null(ptype)) {
    stop("Could not find common type for elements of `x`", call. = FALSE)
  }

  x <- map(args, vec_cast, to = ptype)
  new_list_of(x, ptype)
}

#' @export
#' @rdname list_of
as_list_of <- function(x, ...) {
  UseMethod("as_list_of")
}

#' @export
as_list_of.vctrs_list_of <- function(x, .ptype = NULL, ...) {
  if (!is.null(.ptype)) {
    list_of(!!!x, .ptype = .ptype)
  } else {
    x
  }
}

#' @export
as_list_of.list <- function(x, ..., .ptype = NULL) {
  list_of(!!!x, .ptype = .ptype)
}

#' Create list_of subclass
#'
#' @param x A list
#' @param ptype The prototype which every element of `x` belongs to
#' @param ... Additional attributes used by subclass
#' @param class Optional subclass name
#' @keywords internal
#' @export
new_list_of <- function(x = list(), ptype = logical(), ..., class = character()) {
  stopifnot(is.list(x))
  stopifnot(vec_size(ptype) == 0)

  new_vctr(x, ..., ptype = ptype, class = c(class, "vctrs_list_of"))
}

#' @export
#' @rdname list_of
is_list_of <- function(x) {
  inherits(x, "vctrs_list_of")
}


# Formatting --------------------------------------------------------------

#' @export
obj_print_data.vctrs_list_of <- function(x, ...) {
  if (length(x) == 0)
    return()

  print(vec_data(x))
}

#' @export
format.vctrs_list_of <- function(x, ...) {
  format.default(x)
}

#' @export
vec_ptype_full.vctrs_list_of <- function(x) {
  param <- vec_ptype_full(attr(x, "ptype"))
  if (grepl("\n", param)) {
    param <- paste0(indent(paste0("\n", param), 2), "\n")
  }

  paste0("list_of<", param, ">")
}

#' @export
vec_ptype_abbr.vctrs_list_of <- function(x) {
  paste0("list<", vec_ptype_abbr(attr(x, "ptype")), ">")
}

# vctr methods ------------------------------------------------------------

#' @export
as.list.vctrs_list_of <- function(x, ...) {
  attr(x, "ptype") <- NULL
  attr(x, "class") <- NULL
  x
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
`[[<-.vctrs_list_of` <- function(x, i, value) {
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
#' @export vec_type2.vctrs_list_of
#' @method vec_type2 vctrs_list_of
#' @export
vec_type2.vctrs_list_of <- function(x, y) UseMethod("vec_type2.vctrs_list_of", y)
#' @method vec_type2.vctrs_list_of vctrs_list_of
#' @export
vec_type2.vctrs_list_of.vctrs_list_of <- function(x, y) {
  type <- vec_type2(attr(x, "ptype"), attr(y, "ptype"))
  new_list_of(list(), type)
}
#' @method vec_type2.vctrs_list_of default
#' @export
vec_type2.vctrs_list_of.default  <- function(x, y) {
  stop_incompatible_type(x, y)
}

#' @rdname list_of
#' @export vec_cast.vctrs_list_of
#' @method vec_cast vctrs_list_of
#' @export
vec_cast.vctrs_list_of <- function(x, to) {
  UseMethod("vec_cast.vctrs_list_of")
}
#' @export
#' @method vec_cast.vctrs_list_of list
vec_cast.vctrs_list_of.list <- function(x, to) {
  as_list_of(x, .ptype = attr(to, "ptype"))
}

#' @export
#' @method vec_cast.list vctrs_list_of
vec_cast.list.vctrs_list_of <- function(x, to) {
  warn_lossy_cast(x, to)
  shape_broadcast(as.list(x), to)
}

#' @export
#' @method vec_cast.vctrs_list_of vctrs_list_of
vec_cast.vctrs_list_of.vctrs_list_of <- vec_cast.vctrs_list_of.list

#' @export
#' @method vec_cast.vctrs_list_of default
vec_cast.vctrs_list_of.default <- function(x, to) {
  x <- vec_cast(x, attr(to, "ptype"))
  out <- lapply(seq_along(x), function(i) x[[i]])

  miss <- is.na(x)
  out[miss] <- rep(list(NULL), sum(miss))

  new_list_of(out, ptype = attr(to, "ptype"))
}
