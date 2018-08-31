# Constructor and basic methods  ---------------------------------------------

#' record S3 class
#'
#' The record class extends [vctr]. A record is composed of 1 or more [field]s,
#' which must be vectors of the same length. Is designed specifically for
#' classes that can naturally be decomposed into multiple vectors of the same
#' length, like [POSIXlt], but where the organisation should be considered
#' an implementation detail invisible to the user (unlike a [data.frame]).
#'
#' @param fields A list. It must possess the following properties:
#'   * no attributes (apart from names)
#'   * syntactic names
#'   * length 1 or greater
#'   * elements are vectors
#'   * elements have equal length
#' @param ... Additional attributes
#' @param class Name of subclass.
#' @export
#' @aliases ses record
#' @keywords internal
new_record <- function(fields, ..., class = character()) {
  check_fields(fields)
  structure(fields, ..., class = c(class, "record", "vctr"))
}

check_fields <- function(fields) {
  if (!is.list(fields) || length(fields) == 0) {
    stop("`fields` must be a list of length 1 or greater", call. = FALSE)
  }

  if (!unique_field_names(names(fields))) {
    stop("`fields` must have unique names", call. = FALSE)
  }

  if (!identical(names(attributes(fields)), "names")) {
    stop("`fields` must have no attributes (apart from names)", call. = FALSE)
  }

  is_vector <- map_lgl(fields, is_vector)
  if (!all(is_vector)) {
    stop("Every field must be a vector", call. = FALSE)
  }

  lengths <- map_int(fields, length)
  if (!all_equal(lengths)) {
    stop("Every field must be the same length", call. = FALSE)
  }

  invisible(fields)
}

#' @export
length.record <- function(x) {
  length(field(x, 1L))
}

#' @export
names.record <- function(x) {
  NULL
}

#' @export
str.record <- function(object, ..., indent.str = "", width = getOption("width")) {
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

#' @method vec_cast record
#' @export
vec_cast.record <- function(x, to) UseMethod("vec_cast.record")

#' @method vec_cast.record NULL
#' @export
vec_cast.record.NULL <- function(x, to) x

#' @method vec_cast.record list
#' @export
vec_cast.record.list <- function(x, to) {
  # if not a record, need to check valid

  if (!setequal(fields(x), fields(to))) {
    diff <- set_partition(fields(x), fields(to))
    if (length(diff$only_x) > 0) {
      warn_lossy_cast(
        x, to,
        details = inline_list("Extra names: ", diff$only_x, quote = "`")
      )
    }

    if (length(diff$only_y) > 0) {
      stop_incompatible_cast(
        x, to,
        details = inline_list("Missing names: ", diff$only_y, quote = "`")
      )
    }
  }

  attributes(x) <- attributes(to)
  x
}

# Subsetting --------------------------------------------------------------

#' @export
`[.record` <- function(x, i,...) {
  out <- lapply(vec_data(x), `[`, i, ...)
  vec_cast(out, x)
}

#' @export
`[[.record` <- function(x, i, ...) {
  out <- lapply(vec_data(x), `[[`, i, ...)
  vec_cast(out, x)
}

#' @export
rep.record <- function(x, ...) {
  out <- lapply(vec_data(x), rep, ...)
  vec_cast(out, x)
}

#' @export
`length<-.record` <- function(x, value) {
  out <- lapply(vec_data(x), `length<-`, value)
  vec_cast(out, x)
}

# Replacement -------------------------------------------------------------

#' @export
`[[<-.record` <- function(x, i, value) {
  value <- vec_cast(value, x)
  out <- map2(vec_data(x), vec_data(value), function(x, value) {
    x[[i]] <- value
    x
  })
  vec_cast(out, x)
}

#' @export
`[<-.record` <- function(x, i, value) {
  value <- vec_cast(value, x)
  out <- map2(vec_data(x), vec_data(value), function(x, value) {
    x[i] <- value
    x
  })
  vec_cast(out, x)
}


# Unimplemented -----------------------------------------------------------

#' @export
mean.record <- function(x, ..., na.rm = FALSE) {
  stop_unimplemented(x, "mean")
}

#' @importFrom stats median
#' @export
median.record <- function(x, ..., na.rm = FALSE) {
  stop_unimplemented(x, "median")
}

#' @export
Math.record <- function(x, ..., na.rm = FALSE) {
  stop_unimplemented(x, .Generic)
}

#' @export
anyNA.record <- function(x, recursive = FALSE) {
  stop_unimplemented(x, .Method)
}

#' @export
is.finite.record <- function(x) {
  stop_unimplemented(x, .Method)
}

#' @export
is.finite.record <- function(x) {
  stop_unimplemented(x, .Method)
}

#' @export
is.na.record <- function(x) {
  stop_unimplemented(x, .Method)
}

#' @export
is.nan.record <- function(x) {
  stop_unimplemented(x, .Method)
}

# Helpers -----------------------------------------------------------------

unique_field_names <- function(x) {
  if (length(x) == 0) {
    return(FALSE)
  }

  if (any(is.na(x) | x == "")) {
    return(FALSE)
  }

  !anyDuplicated(x)
}


# Test class ---------------------------------------------------------------

# This simple class is used for testing as defining methods inside
# a test does not work (because the lexical scope is lost)
# nocov start

#' @examples
#' r <- rational(1, 1:10)
#'
#' r[0]
#' r[5:1]
#' r[[1]]
#'
#' -r
#' abs(r)
#' r == r
rational <- function(n = integer(), d = integer()) {
  fields <- vec_recycle(
    n = vec_cast(n, integer()),
    d = vec_cast(d, integer())
  )
  new_record(fields, class = "rational")
}

format.rational <- function(x, ...) {
  paste0(field(x, "n"), "/", field(x, "d"))
}

vec_type2.rational <- function(x, y)  UseMethod("vec_type2.rational", y)
vec_type2.rational.unknown <- function(x, y) rational()
vec_type2.rational.integer <- function(x, y) rational()
vec_type2.integer.rational <- function(x, y) rational()
vec_type2.rational.rational <- function(x, y) rational()
vec_type2.rational.default <- function(x, y) stop_incompatible_type(x, y)

vec_cast.rational <- function(x, to) UseMethod("vec_cast.rational")
vec_cast.rational.integer <- function(x, to) rational(x, 1)
vec_cast.rational.list <- function(x, to) rational(x$n, x$d)
vec_cast.rational.rational <- function(x, to) x

# nocov end
