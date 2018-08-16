#' vctr S3 class
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
#'    See `vignette("extending-vctrs")` for details.
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
#' * `names<-.vctr()`, `dims<-.vctr()`, and `dimnames<-.vctr()` all through
#'   errors as generally custom vector classes do not need names or dimensions.
#'
#' @param x Foundation of class. Must be a vector
#' @param ... Name-value pairs defining attributes
#' @param class Name of subclass.
#' @export
#' @keywords internal
new_vctr <- function(.data, ..., class) {
  if (!is_vector(.data)) {
    stop("`.data` must be a vector type", call. = FALSE)
  }

  structure(.data, ..., class = c(class, "vctr"))
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

  attributes(x) <- attributes(to)
  x
}

# Printing ----------------------------------------------------------------

#' @export
print.vctr <- function(x, ...) {
  if (length(x) == 0) {
    cat_line("<", class(x)[[1]], "[0]>")
  } else {
    cat_line("<", class(x)[[1]], ">")
    print(format(x), quote = FALSE)
  }
  invisible(x)
}

#' @export
format.vctr <- function(x, ...) {
  stop(glue::glue("`format.{class(x)[[1]]}()` method not implemented"), call. = FALSE)
}

#' @export
as.character.vctr <- function(x, ...) {
  format(x, ...)
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
as.list.vctr <- function(x, ...) {
  lapply(seq_along(x), function(i) x[[i]])
}

#' @export
rep.vctr <- function(x, ...) {
  vec_cast(NextMethod(), x)
}

# Replacement -------------------------------------------------------------

#' @export
`[[<-.vctr` <- function(x, i, value) {
  value <- vec_cast(value, x)
  NextMethod()
}

#' @export
`[<-.vctr` <- function(x, i, value) {
  value <- vec_cast(value, x)
  NextMethod()
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

# Protection --------------------------------------------------------------

#' @export
`names<-.vctr` <- function(x, value) {
  if (is.null(value)) {
    x
  } else {
    stop("Must not set names() of ", class(x)[[1]], " vector", call. = FALSE)
  }
}

#' @export
`dim<-.vctr` <- function(x, value) {
  stop("Must not set dim() of ", class(x)[[1]], " vector", call. = FALSE)
}

#' @export
`dimnames<-.vctr` <- function(x, value) {
  stop("Must not set dimnames() of ", class(x)[[1]], " vector", call. = FALSE)
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
  new_vctr(as.double(x), class = "hidden")
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
vec_cast.hidden.double    <- function(x, to) new_hidden(as.double(x)) # strip attr
vec_cast.double.hidden    <- function(x, to) as.double(x)
vec_cast.hidden.logical   <- function(x, to) new_hidden(as.double(x)) # strip attr
vec_cast.logical.hidden   <- function(x, to) as.logical(x)
# nocov end
