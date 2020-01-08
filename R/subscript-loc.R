#' Create a vector of locations
#'
#' @description
#'
#' These helpers provide a means of standardizing common indexing
#' methods such as integer, character or logical indexing.
#'
#' * `vec_as_location()` accepts integer, character, or logical vectors
#'   of any size. The output is always an integer vector that is
#'   suitable for subsetting with `[` or [vec_slice()]. It might be a
#'   different size than the input because negative selections are
#'   transformed to positive ones and logical vectors are transformed
#'   to a vector of indices for the `TRUE` locations.
#'
#' * `vec_as_location2()` accepts a single number or string. It returns
#'   a single location as a integer vector of size 1. This is suitable
#'   for extracting with `[[`.
#'
#' @inheritParams vec_slice
#' @param n A single integer representing the total size of the
#'   object that `i` is meant to index into.
#' @param names If `i` is a character vector, `names` should be a character
#'   vector that `i` will be matched against to construct the index. Otherwise,
#'   not used. The default value of `NULL` will result in an error
#'   if `i` is a character vector.
#' @param arg The argument name to be displayed in error messages when
#'   `vec_as_location()` and `vec_as_location2()` are used to check the
#'   type of a function input.
#' @param convert_values Experimental. Character vector indicating
#'   what types of values should be converted. Can currently only be
#'   set to `"negative"`.
#'
#' @return `vec_as_location()` returns an integer vector that can be used
#'   as an index in a subsetting operation. `vec_as_location2()`
#'   returns an integer of size 1 that can be used a scalar index for
#'   extracting an element.
#'
#' @examples
#' x <- array(1:6, c(2, 3))
#' dimnames(x) <- list(c("r1", "r2"), c("c1", "c2", "c3"))
#'
#' # The most common use case validates row indices
#' vec_as_location(1, vec_size(x))
#'
#' # Negative indices can be used to index from the back
#' vec_as_location(-1, vec_size(x))
#'
#' # Character vectors can be used if `names` are provided
#' vec_as_location("r2", vec_size(x), rownames(x))
#'
#' # You can also construct an index for dimensions other than the first
#' vec_as_location(c("c2", "c1"), ncol(x), colnames(x))
#'
#' @keywords internal
#' @export
vec_as_location <- function(i,
                            n,
                            names = NULL,
                            ...,
                            convert_values = "negative",
                            arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()

  i <- vec_as_subscript(i, arg = arg)

  convert_values <- as_opts_location_convert_values(convert_values, arg = arg)
  .Call(vctrs_as_location, i, n, names, convert_values)
}
#' @rdname vec_as_location
#' @param allow_values Experimental. Character vector indicating zero,
#'   one or several types of values to be allowed as input:
#'   `"negative"` or `"missing"`. By default, locations can't be
#'   negative or missing.
#' @export
vec_as_location2 <- function(i,
                             n,
                             names = NULL,
                             ...,
                             allow_values = NULL,
                             arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()
  result_get(vec_as_location2_result(
    i,
    n = n,
    names = names,
    allow_values = allow_values,
    arg = arg
  ))
}

vec_as_location2_result <- function(i,
                                    n,
                                    names,
                                    allow_values,
                                    arg) {
  allow_values <- as_opts_location_values(allow_values, arg = arg)
  allow_missing <- allow_values[["missing"]]
  allow_negative <- allow_values[["negative"]]

  result <- vec_as_subscript2_result(
    i = i,
    arg = arg,
    indicator = "error"
  )

  if (!is_null(result$err)) {
    parent <- result$err
    return(result(err = new_error_location2_bad_type(
      i = i,
      .arg = arg,
      # FIXME: Should body fields in parents be automatically inherited?
      body = function(...) cnd_body(parent),
      parent = parent
    )))
  }

  # Locations must be size 1, can't be NA, and must be positive
  i <- result$ok

  if (length(i) != 1L) {
    return(result(err = new_error_location2_bad_type(
      i = i,
      .arg = arg,
      body = cnd_bullets_location2_need_scalar
    )))
  }

  neg <- typeof(i) == "integer" && !is.na(i) && i < 0L
  if (allow_negative && neg) {
    i <- -i
  }

  if (is.na(i)) {
    if (!allow_missing && is.na(i)) {
      result <- result(err = new_error_location2_bad_type(
        i = i,
        .arg = arg,
        body = cnd_bullets_location2_need_present
      ))
    } else {
      result <- result(i)
    }
    return(result)
  }

  if (i == 0L) {
    return(result(err = new_error_location2_bad_type(
      i = i,
      .arg = arg,
      body = cnd_bullets_location2_need_non_zero
    )))
  }

  if (!allow_negative && neg) {
    return(result(err = new_error_location2_bad_type(
      i = i,
      .arg = arg,
      body = cnd_bullets_location2_need_non_negative
    )))
  }

  # FIXME: Use result approach in internal implementation?
  err <- NULL
  i <- tryCatch(
    vec_as_location(i, n, names = names, arg = arg),
    vctrs_error_subscript_bad_type = function(err) {
      err <<- err
      i
    }
  )

  if (neg) {
    i <- -i
  }

  if (is_null(err)) {
    result(i)
  } else {
    result(err = new_error_location2_bad_type(
      i = i,
      parent = err,
      .arg = arg
    ))
  }
}


location_values_opts <- c("missing", "negative")

as_opts_location_values <- function(x, arg = NULL) {
  if (inherits(x, "vctrs_opts_location_values")) {
    return(x)
  }
  new_opts(
    x,
    location_values_opts,
    subclass = "vctrs_opts_location_values",
    arg = arg
  )
}


location_convert_values_opts <- "negative"

as_opts_location_convert_values <- function(x, arg = NULL) {
  if (inherits(x, "vctrs_opts_location_convert_values")) {
    return(x)
  }
  new_opts(
    x,
    location_convert_values_opts,
    subclass = "vctrs_opts_location_convert_values",
    arg = arg
  )
}


new_error_location2_bad_type <- function(i,
                                         ...,
                                         .arg = "i",
                                         .subclass = NULL) {
  new_error_subscript2_bad_type(
    .subclass = c(.subclass, "vctrs_error_location2_bad_type"),
    i = i,
    indicator = "error",
    location = "coerce",
    name = "coerce",
    .arg = .arg,
    ...
  )
}


cnd_bullets_location2_need_scalar <- function(cnd, ...) {
  arg <- cnd$.arg %||% "i"
  size <- length(cnd$i)
  format_error_bullets(c(
    x = glue::glue("`{arg}` has the wrong size {size}."),
    i = "This subscript must be size 1."
  ))
}
cnd_bullets_location2_need_present <- function(cnd, ...) {
  arg <- cnd$.arg %||% "i"
  format_error_bullets(c(
    x = glue::glue("`{arg}` can't be `NA`."),
    i = "This subscript can't be missing."
  ))
}
cnd_bullets_location2_need_non_zero <- function(cnd, ...) {
  arg <- cnd$.arg %||% "i"
  format_error_bullets(c(
    x = glue::glue("`{arg}` can't be zero."),
    i = "This subscript must be a positive integer."
  ))
}
cnd_bullets_location2_need_non_negative <- function(cnd, ...) {
  arg <- cnd$.arg %||% "i"
  format_error_bullets(c(
    x = glue::glue("`{arg}` (with value {cnd$i}) has the wrong sign."),
    i = "This subscript must be a positive integer."
  ))
}
