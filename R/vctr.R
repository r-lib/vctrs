#' vctr (vectr) S3 class
#'
#' This abstract class provides a set of useful default methods that makes it
#' considerably easier to get started with a new S3 vector class.
#'
#' @section Recommended workflow:
#' 1. Start by creating a low-level constructor. It should be called
#'    `new_myclass()`, and should check the types (but not the values)
#'    of its inputs.
#'
#' 1. Depending on the class create either a helper `myclass()`, a coercer
#'    `as_myclass()`, or both. A helper should construct valid values then
#'    pass on to constructor. A coercer should either check values are correct,
#'    or rely on the helper. Avoid defining validation code in multiple
#'    places.
#'
#' 1. Define a useful `format.myclass()` method. This will give default
#'    `print()` and `as.character()` methods that should be adequate for
#'    most classes. Be warned: a good format method may be as much work
#'    as the rest of the class put together!
#'
#' 1. Next provide [vec_type2()] and [vec_cast()]. First focus on the
#'    casts between your class and its underlying the base type.
#'    Next think about base types that should be coercible or castable.
#'    See `vignette("s3-vector")` for details.
#'
#' 1. If your function behaves similarly to numbers or booleans, or has
#'    specialised comparison methods, read [vec_grp] to learn about the vctrs
#'    group generics: these allow you to implement many methods at once.
#'
#' Implementing these methods gets you many methods for free:
#'
#' * `[[` and `[` use `NextMethod()` dispatch to the underlying base function,
#'    reconstructing attributes with `vec_cast()`. `rep()` works similarly.
#'    Override if one or more attributes have a one-to-one relationship to
#'    the underlying data.
#'
#' * `[[<-` and `[<-` cast the RHS to the LHS, then call `NextMethod()`.
#'   Override these methods if any attributes depend on the data.
#'
#' * `as.list.vctr()` calls `[[` repeatedly, `as.character.vctr()` calls
#'   `format()`.
#'
#' * `as.data.frame.vctr()` uses a standard technique to wrap a vector
#'   in a data frame. You should never need to override this method.
#'
#' * `dims<-.vctr()`, and `dimnames<-.vctr()` all throw errors as generally
#'   custom vector classes do not need to support dimensions.
#'
#' @param x Foundation of class. Must be a vector
#' @param ... Name-value pairs defining attributes
#' @param class Name of subclass.
#' @export
#' @keywords internal
#' @aliases vctr
new_vctr <- function(.data, ..., class = character()) {
  if (!is_vector(.data)) {
    stop("`.data` must be a vector type", call. = FALSE)
  }
  check_attr(.data)

  structure(.data, ..., class = c(class, "vctr"))
}

check_attr <- function(.data) {
  attr <- attributes(.data)
  if (is.null(attr))
    return()

  if (!identical(names(attr), "names")) {
    stop("`.data` must not have attributes apart from names", call. = FALSE)
  }

  if (!names_all_or_nothing(attr[[1]])) {
    stop("If any elements of `.data` are named, all must be named", call. = FALSE)
  }
}

names_all_or_nothing <- function(names) {
  if (is.null(names)) {
    TRUE
  } else {
    all(names != "" & !is.na(names))
  }
}

#' @method vec_cast vctr
#' @export
vec_cast.vctr <- function(x, to) UseMethod("vec_cast.vctr")

#' @method vec_cast.vctr NULL
#' @export
vec_cast.vctr.NULL <- function(x, to) x

#' @method vec_cast.vctr default
#' @export
vec_cast.vctr.default <- function(x, to) {
  if (is.object(x)) {
    if (identical(attributes(x), attributes(to))) {
      return(x)
    } else {
      stop_incompatible_cast(x, to)
    }
  }

  if (typeof(x) != typeof(to)) {
    stop_incompatible_cast(x, to)
  }

  # Copy every attribute, preserving names
  attr_to <- attributes(to)
  attr_to[["names"]] <- names(x)
  attributes(x) <- attr_to

  x
}

# Printing ----------------------------------------------------------------

#' @export
print.vctr <- function(x, ...) {
  cat_line("<", vec_ptype_full(x), "[", length(x), "]>")
  if (length(x) > 0) {
    out <- stats::setNames(format(x), names(x))
    print(out, quote = FALSE)
  }
  invisible(x)
}

# manually registered in zzz.R
pillar_shaft.vctr <- function(x, ...) {
  align <- if (is_character(x)) "left" else "right"
  pillar::new_pillar_shaft_simple(format(x), align = align)
}

# manually registered in zzz.R
type_sum.vctr <- function(x) {
  vec_ptype_abbr(x)
}

stop_unimplemented <- function(x, method) {
  msg <- glue::glue("`{method}.{class(x)[[1]]}()` not implemented")
  abort(
    "error_unimplemented",
    message = msg,
    x = x,
    method = method
  )
}

#' @export
format.vctr <- function(x, ...) {
  stop_unimplemented(x, "format")
}

#' @export
str.vctr <- function(object, ..., indent.str = "", width = getOption("width")) {
  width <- width - nchar(indent.str) - 2
  # Avoid spending too much time formatting elements that won't see
  length <- ceiling(width / 2)
  if (length(object) > length) {
    x <- object[1:length]
  } else {
    x <- object
  }

  title <- glue::glue(" {vec_ptype_abbr(object)} [1:{length(object)}] ")
  cat_line(inline_list(title, format(x), width = width))
}

# Subsetting --------------------------------------------------------------

#' @export
`[.vctr` <- function(x, i,...) {
  vec_cast(NextMethod(), x)
}

#' @export
`[[.vctr` <- function(x, i, ...) {
  vec_cast(NextMethod(), x)
}

#' @export
`$.vctr` <- function(x, i) {
  vec_cast(NextMethod(), x)
}

#' @export
rep.vctr <- function(x, ...) {
  vec_cast(NextMethod(), x)
}

#' @export
`length<-.vctr` <- function(x, value) {
  vec_cast(NextMethod(), x)
}

# Replacement -------------------------------------------------------------

#' @export
`[[<-.vctr` <- function(x, i, value) {
  value <- vec_cast(value, x)
  NextMethod()
}

#' @export
`$<-.vctr` <- function(x, i, value) {
  if (!is.list(x)) {
    # Default behaviour is to cast LHS to a list
    stop("$ operator is invalid for atomic vectors", call. = FALSE)
  }
  value <- vec_cast(value, x)
  NextMethod()
}

#' @export
`[<-.vctr` <- function(x, i, value) {
  value <- vec_cast(value, x)
  NextMethod()
}

# Coercion ----------------------------------------------------------------

#' @export
as.logical.vctr <- function(x, ...) {
  vec_cast(x, logical())
}

#' @export
as.integer.vctr <- function(x, ...) {
  vec_cast(x, integer())
}

#' @export
as.double.vctr <- function(x, ...) {
  vec_cast(x, double())
}

#' @export
as.character.vctr <- function(x, ...) {
  vec_cast(x, character())
}

#' @export
as.list.vctr <- function(x, ...) {
  lapply(seq_along(x), function(i) x[[i]])
}

# Group generics ----------------------------------------------------------

#' @export
Ops.vctr <- function(e1, e2) {
  if (missing(e2)) {
    if (.Generic == "!") {
      return(vec_grp_logical(.Generic, e1))
    } else {
      return(vec_grp_unary(.Generic, e1))
    }
  }

  if (length(e2) == 1) {
    # Optimisation if RHS is a scalar
    ptype <- e1
  } else {
    ptype <- vec_ptype(e1, e2)[[1]]
  }
  e1 <- vec_cast(e1, ptype)
  e2 <- vec_cast(e2, ptype)

  if (.Generic %in% c("+", "-", "*", "/", "^", "%%", "%/%")) {
    vec_grp_numeric(.Generic, e1, e2)
  } else if (.Generic %in% c("&", "|", "!")) {
    vec_grp_logical(.Generic, e1, e2)
  } else {
    vec_grp_compare(.Generic, e1, e2)
  }
}

#' @export
Summary.vctr <- function(..., na.rm = FALSE) {
  vec_grp_summary(.Generic, vec_c(...), na.rm = na.rm)
}

#' @export
mean.vctr <- function(x, ..., na.rm = FALSE) {
  vec_cast(NextMethod(), x)
}

#' @importFrom stats median
#' @export
median.vctr <- function(x, ..., na.rm = FALSE) {
  vec_cast(NextMethod(), x)
}

#' @export
Math.vctr <- function(x, ..., na.rm = FALSE) {
  vec_cast(NextMethod(), x)
}

#' @export
c.vctr <- function(...) {
  vec_c(...)
}


# Order and equality ------------------------------------------------------

#' @export
vec_proxy_order.vctr <- function(x) {
  if (is.list(x)) {
    # no natural ordering for lists, so just preserve
    seq_along(x)
  } else {
    vec_data(x)
  }
}

#' @export
vec_proxy_equality.default <- function(x) {
  vec_data(x)
}

#' @export
xtfrm.vctr <- function(x) {
  proxy <- vec_proxy_order(x)

  # order(order(x)) ~= rank(x)
  if (is.data.frame(proxy)) {
    order(do.call(base::order, proxy))
  } else if (is_integer(proxy) || is_double(proxy)) {
    proxy
  } else if (is_character(proxy)) {
    order(order(proxy))
  } else {
    stop("Invalid value returned by `vec_proxy_equality()`.", call. = FALSE)
  }
}

#' @export
unique.vctr <- function(x, incomparables = FALSE, ...) {
  vec_unique(x)
}

#' @export
duplicated.vctr <- function(x, incomparables = FALSE, ...) {
  vec_duplicate_id(x) != seq_along(x)
}

#' @export
anyDuplicated.vctr <- function(x, incomparables = FALSE, ...) {
  vec_duplicate_any(x)
}

# Protection --------------------------------------------------------------

stop_unsupported <- function(x, operation) {
  msg <- glue::glue("Must not {operation} {vec_ptype_full(x)} vector")
  abort(
    "error_unsupported",
    message = msg,
    x = x,
    operation = operation
  )
}

#' @export
`dim<-.vctr` <- function(x, value) {
  stop_unsupported(x, "set dim() on")
}

#' @export
`dimnames<-.vctr` <- function(x, value) {
  stop_unsupported(x, "set dimnames() on ")
}

#' @export
`levels<-.vctr` <- function(x, value) {
  stop_unsupported(x, "set levels() on")
}

#' @export
`t.vctr` <- function(x) {
  stop_unsupported(x, "transpose")
}

#' @export
`names<-.vctr` <- function(x, value) {
  if (length(value) != 0 && length(value) != length(x)) {
    stop("`names()` must be the same length as x", call. = FALSE)
  }
  if (!names_all_or_nothing(value)) {
    stop("If any elements are named, all elements must be named", call. = FALSE)
  }
  NextMethod()
}

# Data frame --------------------------------------------------------------

#' @export
as.data.frame.vctr <- function(x,
                               row.names = NULL,
                               optional = FALSE,
                               ...,
                               nm = paste(deparse(substitute(x), width.cutoff = 500L), collapse = " ")
                               ) {

  force(nm)
  cols <- list(x)
  if (!optional) {
    names(cols) <- nm
  }

  structure(
    cols,
    class = "data.frame",
    row.names = .set_row_names(vec_length(x))
  )
}

# Helpers -----------------------------------------------------------------

# This simple class is used for testing as defining methods inside
# a test does not work (because the lexical scope is lost)
# nocov start
new_hidden <- function(x = double()) {
  stopifnot(is.numeric(x))
  new_vctr(vec_cast(x, double()), class = "hidden")
}
format.hidden <- function(x, ...) rep("xxx", length(x))

vec_type2.hidden          <- function(x, y) UseMethod("vec_type2.hidden")
vec_type2.hidden.default  <- function(x, y) stop_incompatible_type(x, y)
vec_type2.hidden.hidden   <- function(x, y) new_hidden()
vec_type2.hidden.NULL     <- function(x, y) new_hidden()
vec_type2.NULL.hidden     <- function(x, y) new_hidden()
vec_type2.hidden.double   <- function(x, y) new_hidden()
vec_type2.double.hidden   <- function(x, y) new_hidden()
vec_type2.hidden.logical  <- function(x, y) new_hidden()
vec_type2.logical.hidden  <- function(x, y) new_hidden()

vec_cast.hidden           <- function(x, to) UseMethod("vec_cast.hidden")
vec_cast.hidden.default   <- function(x, to) stop_incompatible_cast(x, to)
vec_cast.hidden.hidden    <- function(x, to) x
vec_cast.hidden.NULL      <- function(x, to) x
vec_cast.NULL.hidden      <- function(x, to) x
vec_cast.hidden.double    <- function(x, to) new_hidden(vec_data(x))
vec_cast.double.hidden    <- function(x, to) vec_data(x)
vec_cast.hidden.logical   <- function(x, to) new_hidden(as.double(x))
vec_cast.logical.hidden   <- function(x, to) as.logical(vec_data(x))
# nocov end
