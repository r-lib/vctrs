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

#' Tools for accessing the fields of a record.
#'
#' A [record] behaves like a vector, so `length()`, `names()`, and `$` can
#' not provide access to the fields of the underlying list. These helpers do:
#' `fields()` is equivalent to `names()`; `n_fields()` is equivalent to
#' `length()`; `field()` is equivalent to `$`.
#'
#' @param x A record
#' @keywords internal
#' @export
fields <- function(x) {
  attr(x, "names")
}

#' @export
#' @rdname fields
n_fields <- function(x) {
  length(vec_data(x))
}

#' @export
#' @rdname fields
field <- function(x, i) {
  .subset2(x, i)
}

#' @export
length.record <- function(x) {
  length(field(x, 1L))
}

#' @export
names.record <- function(x) {
  NULL
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
      fields <- glue::glue_collapse(diff$only_x, sep = ", ", width = 80)
      details <- paste0("Extra names: ", feilds)
      warn_lossy_cast(x, to, details = details)
    }

    if (length(diff$only_y) > 0) {
      details <- paste0("Missing names: ", paste0(diff$only_y, collapse = ", "))
      stop_incompatible_cast(x, to, details)
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
`[[<-.vctr` <- function(x, i, value) {
  value <- vec_cast(value, x)
  out <- map2(vec_data(x), vec_data(value), function(x, value, i) x[i] <- value)
  vec_cast(out, x)
}

#' @export
`[<-.vctr` <- function(x, i, value) {
  value <- vec_cast(value, x)
  out <- map2(vec_data(x), vec_data(value), function(x, value, i) x[[i]] <- value)
  vec_cast(out, x)
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

  new_rational(fields$n, fields$d)
}

new_rational <- function(n, d) {
  new_record(list(n = n, d = d), class = "rational")
}

format.rational <- function(x, ...) {
  paste0(.subset2(x, "n"), "/", .subset2(x, "d"))
}

vec_grp_unary.rational <- function(generic, x) {
  new_rational(
    vec_generic_call(generic, .subset2(x, "n")),
    .subset2(x, "d")
  )
}

# nocov end
