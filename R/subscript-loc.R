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
                            arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()

  i <- vec_as_subscript(i, arg = arg)
  .Call(vctrs_as_location, i, n, names, "invert")
}
#' @rdname vec_as_location
#' @param negative Whether to `"invert"` negative values to positive
#'   locations, throw an informative `"error"`, or `"ignore"` them.
#' @export
num_as_location <- function(i,
                            n,
                            names = NULL,
                            ...,
                            negative = c("invert", "error", "ignore"),
                            arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()

  if (!is_integer(i) && !is_double(i)) {
    abort("`i` must be a numeric vector.")
  }
  .Call(vctrs_as_location, i, n, names, negative)
}

#' @rdname vec_as_location
#' @param missing Whether to throw an `"error"` when `i` is a missing
#'   value, or `"ignore"` it (return it as is).
#' @export
vec_as_location2 <- function(i,
                             n,
                             names = NULL,
                             ...,
                             missing = c("error", "ignore"),
                             arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()
  result_get(vec_as_location2_result(
    i,
    n = n,
    names = names,
    negative = "error",
    missing = missing,
    arg = arg
  ))
}
#' @rdname vec_as_location
#' @param negative Whether to throw an `"error"` when `i` is a
#'   negative location value, or `"ignore"` it.
#' @export
num_as_location2 <- function(i,
                             n,
                             names = NULL,
                             ...,
                             negative = c("error", "ignore"),
                             missing = c("error", "ignore"),
                             arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()

  if (!is_integer(i) && !is_double(i)) {
    abort("`i` must be a numeric vector.")
  }
  result_get(vec_as_location2_result(
    i,
    n = n,
    names = names,
    negative = negative,
    missing = missing,
    arg = arg
  ))
}

vec_as_location2_result <- function(i,
                                    n,
                                    names,
                                    missing,
                                    negative,
                                    arg) {
  allow_missing <- arg_match(missing, c("error", "ignore")) == "ignore"
  allow_negative <- arg_match(negative, c("error", "ignore")) == "ignore"

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


new_error_location_bad_type <- function(i,
                                        ...,
                                        .arg = "i",
                                        .subclass = NULL) {
  new_error_subscript_bad_type(
    .subclass = c(.subclass, "vctrs_error_location_bad_type"),
    i = i,
    indicator = "error",
    location = "coerce",
    name = "coerce",
    .arg = .arg,
    ...
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
  cnd$.arg <- cnd$.arg %||% "i"
  format_error_bullets(c(
    x = glue::glue_data(cnd, "`{.arg}` (with value {i}) has the wrong sign."),
    i = "This subscript must be a positive integer."
  ))
}

cnd_bullets_location_need_non_negative <- function(cnd, ...) {
  cnd$.arg <- cnd$.arg %||% "i"
  format_error_bullets(c(
    x = glue::glue_data(cnd, "`{.arg}` contains negative locations."),
    i = "These subscripts must be positive integers."
  ))
}

stop_location_negative <- function(i, ..., .arg = "i") {
  stop(new_error_location_bad_type(
    i,
    .arg = .arg,
    body = cnd_bullets_location_need_non_negative
  ))
}
