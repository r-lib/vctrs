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
                            arg = NULL) {
  if (!missing(...)) ellipsis::check_dots_empty()

  i <- vec_as_subscript(i, arg = arg)
  .Call(
    vctrs_as_location,
    i = i,
    n = n,
    names = names,
    loc_negative = "invert",
    loc_oob = "error",
    arg = arg
  )
}
#' @rdname vec_as_location
#' @param negative Whether to `"invert"` negative values to positive
#'   locations, throw an informative `"error"`, or `"ignore"` them.
#' @param oob If `"error"`, throws an informative `"error"` if some
#'   elements are out-of-bounds. If `"extend"`, out-of-bounds
#'   locations are allowed if they are consecutive after the end. This
#'   can be used to implement extendable vectors like `letters[1:30]`.
#' @export
num_as_location <- function(i,
                            n,
                            ...,
                            negative = c("invert", "error", "ignore"),
                            oob = c("error", "extend"),
                            arg = NULL) {
  if (!missing(...)) ellipsis::check_dots_empty()

  if (!is_integer(i) && !is_double(i)) {
    abort("`i` must be a numeric vector.")
  }
  .Call(
    vctrs_as_location,
    i = i,
    n = n,
    names = NULL,
    loc_negative = negative,
    loc_oob = oob,
    arg = arg
  )
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
                             arg = NULL) {
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
                             ...,
                             negative = c("error", "ignore"),
                             missing = c("error", "ignore"),
                             arg = NULL) {
  if (!missing(...)) ellipsis::check_dots_empty()

  if (!is_integer(i) && !is_double(i)) {
    abort("`i` must be a numeric vector.")
  }
  result_get(vec_as_location2_result(
    i,
    n = n,
    names = NULL,
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
    return(result(err = new_error_location2_type(
      i = i,
      arg = arg,
      # FIXME: Should body fields in parents be automatically inherited?
      body = function(...) cnd_body(parent),
      parent = parent
    )))
  }

  # Locations must be size 1, can't be NA, and must be positive
  i <- result$ok

  if (length(i) != 1L) {
    return(result(err = new_error_location2_type(
      i = i,
      arg = arg,
      body = cnd_bullets_location2_need_scalar
    )))
  }

  neg <- typeof(i) == "integer" && !is.na(i) && i < 0L
  if (allow_negative && neg) {
    i <- -i
  }

  if (is.na(i)) {
    if (!allow_missing && is.na(i)) {
      result <- result(err = new_error_location2_type(
        i = i,
        arg = arg,
        body = cnd_bullets_location2_need_present
      ))
    } else {
      result <- result(i)
    }
    return(result)
  }

  if (i == 0L) {
    return(result(err = new_error_location2_type(
      i = i,
      arg = arg,
      body = cnd_bullets_location2_need_non_zero
    )))
  }

  if (!allow_negative && neg) {
    return(result(err = new_error_location2_type(
      i = i,
      arg = arg,
      body = cnd_bullets_location2_need_non_negative
    )))
  }

  # FIXME: Use result approach in internal implementation?
  err <- NULL
  i <- tryCatch(
    vec_as_location(i, n, names = names, arg = arg),
    vctrs_error_subscript_type = function(err) {
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
    result(err = new_error_location2_type(
      i = i,
      parent = err,
      arg = arg
    ))
  }
}


stop_location_negative_missing <- function(i) {
  cnd_signal(new_error_subscript_type(
    i,
    body = cnd_body_vctrs_error_subscript_type
  ))
}
cnd_body_vctrs_error_subscript_type <- function(cnd, ...) {
  missing_loc <- which(is.na(cnd$i))

  if (length(missing_loc) == 1) {
    loc <- glue::glue("The subscript has a missing value at location {missing_loc}.")
  } else {
    n_loc <- length(missing_loc)
    missing_loc <- enumerate(missing_loc)
    loc <- glue::glue(
      "The subscript has {n_loc} missing values at locations {missing_loc}."
    )
  }
  format_error_bullets(c(
    x = "Negative locations can't have missing values.",
    i = loc
  ))
}

stop_location_negative_positive <- function(i) {
  cnd_signal(new_error_subscript_type(
    i,
    body = cnd_body_vctrs_error_location_negative_positive
  ))
}
cnd_body_vctrs_error_location_negative_positive <- function(cnd, ...) {
  positive_loc <- which(cnd$i > 0)

  if (length(positive_loc) == 1) {
    loc <- glue::glue("The subscript has a positive value at location {positive_loc}.")
  } else {
    n_loc <- length(positive_loc)
    positive_loc <- enumerate(positive_loc)
    loc <- glue::glue(
      "The subscript has {n_loc} missing values at locations {positive_loc}."
    )
  }
  format_error_bullets(c(
    x = "Negative locations can't be mixed with positive locations.",
    i = loc
  ))
}


new_error_location2_type <- function(i,
                                     ...,
                                     arg = "i",
                                     class = NULL) {
  new_error_subscript2_type(
    class = class,
    i = i,
    indicator = "error",
    location = "coerce",
    name = "coerce",
    arg = arg,
    ...
  )
}
cnd_bullets_location2_need_scalar <- function(cnd, ...) {
  cnd$arg <- arg_as_string(cnd$arg %||% "i")
  format_error_bullets(c(
    x = glue::glue_data(cnd, "`{arg}` has the wrong size {length(i)}."),
    i = "This subscript must be size 1."
  ))
}
cnd_bullets_location2_need_present <- function(cnd, ...) {
  cnd$arg <- arg_as_string(cnd$arg %||% "i")
  format_error_bullets(c(
    x = glue::glue_data(cnd, "`{arg}` can't be `NA`."),
    i = "This subscript can't be missing."
  ))
}
cnd_bullets_location2_need_non_zero <- function(cnd, ...) {
  cnd$arg <- arg_as_string(cnd$arg %||% "i")
  format_error_bullets(c(
    x = glue::glue_data(cnd, "`{arg}` can't be zero."),
    i = "The subscript must be a positive location."
  ))
}
cnd_bullets_location2_need_non_negative <- function(cnd, ...) {
  cnd$arg <- arg_as_string(cnd$arg %||% "i")
  format_error_bullets(c(
    x = glue::glue_data(cnd, "`{arg}` (with value {i}) has the wrong sign."),
    i = "The subscript must be a positive location."
  ))
}

stop_location_negative <- function(i, ...) {
  cnd_signal(new_error_subscript_type(
    i,
    body = cnd_bullets_location_need_non_negative,
    ...
  ))
}
cnd_bullets_location_need_non_negative <- function(cnd, ...) {
  cnd$arg <- arg_as_string(cnd$arg %||% "i")
  format_error_bullets(c(
    x = glue::glue_data(cnd, "`{arg}` contains negative locations."),
    i = "The subscript must contain positive locations."
  ))
}

stop_indicator_size <- function(i, n, ...) {
  cnd_signal(new_error_subscript_size(
    i,
    n = n,
    ...,
    body = cnd_body_vctrs_error_indicator_size
  ))
}
cnd_body_vctrs_error_indicator_size <- function(cnd, ...) {
  cnd$arg <- append_arg("the subscript", cnd$arg)
  glue_data_bullets(
    cnd,
    i = "Logical subscripts must match the size of the indexed input.",
    x = "The input has size {n} but {arg} has size {vec_size(i)}."
  )
}

stop_subscript_oob <- function(i, subscript_type, ...) {
  stop_subscript(
    class = "vctrs_error_subscript_oob",
    i = i,
    subscript_type = subscript_type,
    ...
  )
}

#' @export
cnd_header.vctrs_error_subscript_oob <- function(cnd, ...) {
  if (cnd_subscript_oob_non_consecutive(cnd)) {
    return(cnd_header_vctrs_error_subscript_oob_non_consecutive(cnd, ...))
  }
  arg <- cnd$arg
  elt <- cnd_subscript_element(cnd)
  action <- cnd_subscript_action(cnd)

  if (is_null(arg)) {
    glue::glue("Must {action} existing {elt[[2]]}.")
  } else {
    arg <- arg_as_string(arg)
    glue::glue("Must {action} existing {elt[[2]]} in `{arg}`.")
  }
}

#' @export
cnd_body.vctrs_error_subscript_oob <- function(cnd, ...) {
  switch(cnd_subscript_type(cnd),
    location =
      if (cnd_subscript_oob_non_consecutive(cnd)) {
        cnd_body_vctrs_error_subscript_oob_non_consecutive(cnd, ...)
      } else {
        cnd_body_vctrs_error_subscript_oob_location(cnd, ...)
      },
    name =
      cnd_body_vctrs_error_subscript_oob_name(cnd, ...),
    abort("Internal error: subscript type can't be `indicator` for OOB errors.")
  )
}
cnd_body_vctrs_error_subscript_oob_location <- function(cnd, ...) {
  i <- cnd$i
  elt <- cnd_subscript_element(cnd)
  action <- cnd_subscript_action(cnd)

  # In case of negative indexing
  i <- abs(i)

  # In case of missing locations
  i <- i[!is.na(i)]

  oob <- i[i > cnd$size]
  oob_enum <- enumerate(oob)

  format_error_bullets(c(
    x = glue::glue(ngettext(
      length(oob),
      "Can't {action} location {oob_enum}.",
      "Can't {action} locations {oob_enum}."
    )),
    i = glue::glue(ngettext(
      cnd$size,
      "There are only {cnd$size} {elt[[1]]}.",
      "There are only {cnd$size} {elt[[2]]}.",
    ))
  ))
}
cnd_body_vctrs_error_subscript_oob_name <- function(cnd, ...) {
  elt <- cnd_subscript_element(cnd)
  action <- cnd_subscript_action(cnd)

  oob <- cnd$i[!cnd$i %in% cnd$names]
  oob_enum <- enumerate(glue::backtick(oob))

  format_error_bullets(c(
    x = glue::glue(ngettext(
      length(oob),
      "Can't {action} {elt[[1]]} with unknown name {oob_enum}.",
      "Can't {action} {elt[[2]]} with unknown names {oob_enum}."
    ))
  ))
}

stop_location_oob_non_consecutive <- function(i, size, ...) {
  stop_subscript_oob(
    i = i,
    size = size,
    subscript_type = "location",
    subscript_oob_non_consecutive = TRUE,
    ...
  )
}

cnd_header_vctrs_error_subscript_oob_non_consecutive <- function(cnd, ...) {
  "Can't index beyond the end with non-consecutive locations."
}
cnd_body_vctrs_error_subscript_oob_non_consecutive <- function(cnd, ...) {
  i <- sort(cnd$i)
  i <- i[i > cnd$size]

  non_consecutive <- i[c(TRUE, diff(i) != 1L)]

  if (length(non_consecutive) == 1) {
    x <- glue::glue("The location {non_consecutive} is not consecutive to the end.")
  } else {
    non_consecutive <- enumerate(non_consecutive)
    x <- glue::glue("The locations {non_consecutive} are not consecutive.")
  }

  glue_data_bullets(
    cnd,
    i = "The input has size {size}.",
    x = x
  )
}

cnd_subscript_oob_non_consecutive <- function(cnd) {
  out <- cnd$subscript_oob_non_consecutive %||% FALSE
  stopifnot(is_bool(out))
  out
}
