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
#' * `as.list.vctrs_vctr()` calls `[[` repeatedly, `as.character.vctrs_vctr()` calls
#'   `format()`.
#'
#' * `as.data.frame.vctrs_vctr()` uses a standard technique to wrap a vector
#'   in a data frame. You should never need to override this method.
#'
#' * `dims<-.vctrs_vctr()`, and `dimnames<-.vctrs_vctr()` all throw errors as generally
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

  structure(.data, ..., class = c(class, "vctrs_vctr"))
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

#' @export
vec_restore.vctrs_vctr <- function(x, to) {
  # Copy every attribute, but preserve existing names
  attr_to <- attributes(to)
  attr_to[["names"]] <- names(x)
  attributes(x) <- attr_to

  x
}

#' @method vec_cast vctrs_vctr
#' @export
vec_cast.vctrs_vctr <- function(x, to) UseMethod("vec_cast.vctrs_vctr")

#' @method vec_cast.vctrs_vctr NULL
#' @export
vec_cast.vctrs_vctr.NULL <- function(x, to) x

#' @method vec_cast.vctrs_vctr default
#' @export
vec_cast.vctrs_vctr.default <- function(x, to) {
  if (is.object(x)) {
    if (identical(attributes(x), attributes(to))) {
      return(x)
    } else {
      stop_incompatible_cast(x, to)
    }
  }

  vec_restore(x, to)
}

#' @export
c.vctrs_vctr <- function(...) {
  vec_c(...)
}

# Printing ----------------------------------------------------------------

#' @export
print.vctrs_vctr <- function(x, ...) {
  vec_print_header(x, ...)
  vec_print_data(x, ...)
  vec_print_footer(x, ...)
  invisible(x)
}

#' @export
#' @rdname new_vctr
vec_print_header <- function(x, ...) {
  UseMethod("vec_print_header")
}

#' @export
vec_print_header.vctrs_vctr <- function(x, ...) {
  cat_line("<", vec_ptype_full(x), "[", length(x), "]>")
}

#' @export
#' @rdname new_vctr
vec_print_data <- function(x, ...) {
  UseMethod("vec_print_data")
}

#' @export
vec_print_data.vctrs_vctr <- function(x, ...) {
  if (length(x) == 0)
    return()

  out <- stats::setNames(format(x), names(x))
  print(out, quote = FALSE)
}

#' @export
#' @rdname new_vctr
vec_print_footer <- function(x, ...) {
  UseMethod("vec_print_footer")
}

#' @export
vec_print_footer.vctrs_vctr <- function(x, ...) {
}


# manually registered in zzz.R
pillar_shaft.vctrs_vctr <- function(x, ...) {
  align <- if (is_character(x)) "left" else "right"
  pillar::new_pillar_shaft_simple(format(x), align = align)
}

# manually registered in zzz.R
type_sum.vctrs_vctr <- function(x) {
  vec_ptype_abbr(x)
}

#' @export
format.vctrs_vctr <- function(x, ...) {
  format(vec_data(x, ...))
}

#' @export
str.vctrs_vctr <- function(object, ..., indent.str = "", width = getOption("width")) {
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
`[.vctrs_vctr` <- function(x, i,...) {
  vec_restore(NextMethod(), x)
}

#' @export
`[[.vctrs_vctr` <- function(x, i, ...) {
  vec_restore(NextMethod(), x)
}

#' @export
`$.vctrs_vctr` <- function(x, i) {
  vec_restore(NextMethod(), x)
}

#' @export
rep.vctrs_vctr <- function(x, ...) {
  vec_restore(NextMethod(), x)
}

#' @export
`length<-.vctrs_vctr` <- function(x, value) {
  vec_restore(NextMethod(), x)
}

# Replacement -------------------------------------------------------------

#' @export
`[[<-.vctrs_vctr` <- function(x, i, value) {
  value <- vec_cast(value, x)
  NextMethod()
}

#' @export
`$<-.vctrs_vctr` <- function(x, i, value) {
  if (!is.list(x)) {
    # Default behaviour is to cast LHS to a list
    stop("$ operator is invalid for atomic vectors", call. = FALSE)
  }
  value <- vec_cast(value, x)
  NextMethod()
}

#' @export
`[<-.vctrs_vctr` <- function(x, i, value) {
  value <- vec_cast(value, x)
  NextMethod()
}

# Coercion ----------------------------------------------------------------

#' @export
as.logical.vctrs_vctr <- function(x, ...) {
  vec_cast(x, logical())
}

#' @export
as.integer.vctrs_vctr <- function(x, ...) {
  vec_cast(x, integer())
}

#' @export
as.double.vctrs_vctr <- function(x, ...) {
  vec_cast(x, double())
}

#' @export
as.character.vctrs_vctr <- function(x, ...) {
  vec_cast(x, character())
}

#' @export
as.list.vctrs_vctr <- function(x, ...) {
  vec_cast(x, list())
}

#' @export
as.Date.vctrs_vctr <- function(x, ...) {
  vec_cast(x, date())
}

#' @export
as.POSIXct.vctrs_vctr <- function(x, tz = "", ...) {
  vec_cast(x, new_datetime(tzone = tz))
}

# Equality ----------------------------------------------------------------

#' @export
`==.vctrs_vctr` <- function(e1, e2) {
  vec_equal(e1, e2)
}

#' @export
`!=.vctrs_vctr` <- function(e1, e2) {
  !vec_equal(e1, e2)
}

#' @export
is.na.vcrs_vctr <- function(x) {
  vec_equal_na(x)
}

#' @export
anyNA.vctrs_vctr <- if (getRversion() >= "3.2") {
  function(x, recursive = FALSE) {
    any(is.na(x))
  }
} else {
  function(x) {
    any(is.na(x))
  }
}

#' @export
unique.vctrs_vctr <- function(x, incomparables = FALSE, ...) {
  vec_unique(x)
}

#' @export
duplicated.vctrs_vctr <- function(x, incomparables = FALSE, ...) {
  vec_duplicate_id(x) != seq_along(x)
}

#' @export
anyDuplicated.vctrs_vctr <- function(x, incomparables = FALSE, ...) {
  vec_duplicate_any(x)
}

# Comparison ----------------------------------------------------------------

#' @export
`<=.vctrs_vctr` <- function(e1, e2) {
  vec_compare(e1, e2) <= 0
}

#' @export
`<.vctrs_vctr` <- function(e1, e2) {
  vec_compare(e1, e2) < 0
}

#' @export
`>=.vctrs_vctr` <- function(e1, e2) {
  vec_compare(e1, e2) >= 0
}

#' @export
`>.vctrs_vctr` <- function(e1, e2) {
  vec_compare(e1, e2) > 0
}

#' @export
xtfrm.vctrs_vctr <- function(x) {
  proxy <- vec_proxy_compare(x)

  # order(order(x)) ~= rank(x)
  if (is.data.frame(proxy)) {
    order(do.call(base::order, proxy))
  } else if (is_integer(proxy) || is_double(proxy)) {
    proxy
  } else if (is_character(proxy) || is_logical(proxy)) {
    order(order(proxy))
  } else if (is_list(proxy)) {
    stop("Lists are not comparible", call. = FALSE)
  } else {
    stop("Invalid type returned by `vec_proxy_compare()`.", call. = FALSE)
  }
}

#' @importFrom stats median
#' @export
median.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
  stop_unimplemented(x, "median")
}

#' @importFrom stats quantile
#' @export
quantile.vctrs_vctr <- function(x, ..., type = 1, na.rm = FALSE) {
  stop_unimplemented(x, "quantile")
}

#' @export
min.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
  # TODO: implement to do vec_arg_min()
  rank <- xtfrm(x)
  idx <- if (isTRUE(na.rm)) which.max(rank) else which(rank == min(rank))
  x[[idx[[1]]]]
}

#' @export
max.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
  # TODO: implement to do vec_arg_max()
  rank <- xtfrm(x)
  idx <- if (isTRUE(na.rm)) which.max(rank) else which(rank == max(rank))
  x[[idx[[1]]]]
}


# Logical -----------------------------------------------------------------

#' @export
`!.vctrs_vctr` <- function(x) {
  vec_restore_logical(!vec_proxy_logical(x), x)
}

#' @export
`&.vctrs_vctr` <- function(e1, e2) {
  args <- vec_binary(e1, e2, vec_proxy_logical)
  out <- args[[1]] & args[[2]]
  vec_restore_logical(out, args[[1]])
}

#' @export
`|.vctrs_vctr` <- function(e1, e2) {
  args <- vec_binary(e1, e2, vec_proxy_logical)
  out <- args[[1]] | args[[2]]
  vec_restore_logical(out, args[[1]])
}

# Numeric -----------------------------------------------------------------

#' @export
Math.vctrs_vctr <- function(x, ...) {
  generic <- getExportedValue("base", .Generic)
  out <- generic(vec_proxy_numeric(x), ...)
  vec_restore_numeric(out, x)
}

#' @export
Summary.vctrs_vctr <- function(..., na.rm = FALSE) {
  x <- vec_c(...)

  generic <- getExportedValue("base", .Generic)
  out <- generic(vec_proxy_numeric(x), na.rm = na.rm)
  vec_restore_numeric(out, x)
}

#' @export
mean.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
  out <- mean(vec_proxy_numeric(x), ..., na.rm = na.rm)
  vec_restore_numeric(out, x)
}

#' @export
is.finite.vctrs_rcrd <- function(x) {
  is.finite(vec_proxy_numeric(x))
}

#' @export
is.infinite.vctrs_rcrd <- function(x) {
  is.infinite(vec_proxy_numeric(x))
}

#' @export
is.nan.vctrs_rcrd <- function(x) {
  is.nan(vec_proxy_numeric(x))
}

# Protection --------------------------------------------------------------

stop_unsupported <- function(x, operation) {
  msg <- glue::glue("Can not {operation} {vec_ptype_full(x)} vector")
  abort(
    "error_unsupported",
    message = msg,
    x = x,
    operation = operation
  )
}

#' @export
`dim<-.vctrs_vctr` <- function(x, value) {
  stop_unsupported(x, "set dim() on")
}

#' @export
`dimnames<-.vctrs_vctr` <- function(x, value) {
  stop_unsupported(x, "set dimnames() on ")
}

#' @export
`levels<-.vctrs_vctr` <- function(x, value) {
  stop_unsupported(x, "set levels() on")
}

#' @export
`t.vctrs_vctr` <- function(x) {
  stop_unsupported(x, "transpose")
}

#' @export
`names<-.vctrs_vctr` <- function(x, value) {
  if (length(value) != 0 && length(value) != length(x)) {
    stop("`names()` must be the same length as x", call. = FALSE)
  }
  if (!names_all_or_nothing(value)) {
    stop("If any elements are named, all elements must be named", call. = FALSE)
  }
  NextMethod()
}

#' @export
`is.na<-.vctrs_vctr` <- function(x, value) {
  stop_unsupported(x, "set missing values")
}

# Data frame --------------------------------------------------------------

#' @export
as.data.frame.vctrs_vctr <- function(x,
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

vec_restore_numeric.hidden <- function(x, to) new_hidden(x)

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
