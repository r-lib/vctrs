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

record_reconstruct <- function(x, to) {
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

  # check types

  attributes(x) <- attributes(to)
  x
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

#' @method vec_cast record
#' @export
vec_cast.record <- function(x, to) UseMethod("vec_cast.record")

#' @method vec_cast.record NULL
#' @export
vec_cast.record.NULL <- function(x, to) x

#' @method vec_cast.record list
#' @export
vec_cast.record.list <- function(x, to) {
  vec_list_cast(x, to)
}

#' @method vec_cast.record default
#' @export
vec_cast.record.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

# Subsetting --------------------------------------------------------------

#' @export
`[.record` <- function(x, i,...) {
  out <- lapply(vec_data(x), `[`, i, ...)
  record_reconstruct(out, x)
}

#' @export
`[[.record` <- function(x, i, ...) {
  out <- lapply(vec_data(x), `[[`, i, ...)
  record_reconstruct(out, x)
}

#' @export
`$.record` <- function(x, i, ...) {
  stop_unsupported(x, "subsetting with $")
}

#' @export
rep.record <- function(x, ...) {
  out <- lapply(vec_data(x), rep, ...)
  record_reconstruct(out, x)
}

#' @export
`length<-.record` <- function(x, value) {
  out <- lapply(vec_data(x), `length<-`, value)
  record_reconstruct(out, x)
}

#' @export
as.list.record <- function(x, ...) {
  lapply(seq_along(x), function(i) x[[i]])
}

# Replacement -------------------------------------------------------------

#' @export
`[[<-.record` <- function(x, i, value) {
  value <- vec_cast(value, x)
  out <- map2(vec_data(x), vec_data(value), function(x, value) {
    x[[i]] <- value
    x
  })
  record_reconstruct(out, x)
}

#' @export
`$<-.record` <- function(x, i, value) {
  stop_unsupported(x, "subset assignment with $")
}

#' @export
`[<-.record` <- function(x, i, value) {
  value <- vec_cast(value, x)

  if (missing(i)) {
    replace <- function(x, value) {
      x[] <- value
      x
    }
  } else {
    replace <- function(x, value) {
      x[i] <- value
      x
    }
  }
  out <- map2(vec_data(x), vec_data(value), replace)
  record_reconstruct(out, x)
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
anyNA.record <- if (getRversion() >= "3.2") {
  function(x, recursive = FALSE) {
    stop_unimplemented(x, .Method)
  }
} else {
  function(x) {
    stop_unimplemented(x, .Method)
  }
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

tuple <- function(x = integer(), y = integer()) {
  fields <- vec_recycle(
    x = vec_cast(x, integer()),
    y = vec_cast(y, integer())
  )
  new_record(fields, class = "tuple")
}

format.tuple <- function(x, ...) {
  paste0("(", field(x, "x"), ",", field(x, "y"), ")")
}

vec_type2.tuple <- function(x, y)  UseMethod("vec_type2.tuple", y)
vec_type2.tuple.unknown <- function(x, y) tuple()
vec_type2.tuple.tuple <- function(x, y) tuple()
vec_type2.tuple.default <- function(x, y) stop_incompatible_type(x, y)

vec_cast.tuple <- function(x, to) UseMethod("vec_cast.tuple")
vec_cast.tuple.list <- function(x, to) vec_list_cast(x, to)
vec_cast.tuple.tuple <- function(x, to) x

vec_grp_numeric.tuple <- function(generic, x, y) {
  rec <- vec_recycle(x, y)
  tuple(
    vec_generic_call(generic, field(rec[[1]], "x"), field(rec[[2]], "x")),
    vec_generic_call(generic, field(rec[[1]], "y"), field(rec[[2]], "y"))
  )
}

# nocov end
