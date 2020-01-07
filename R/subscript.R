#' Convert to a base subscript type
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("experimental")}
#'
#' Convert `i` to the base type expected by [vec_as_location()] or
#' [vec_as_location2()]. The values of the subscript type are
#' not checked in any way (length, missingness, negative elements).
#'
#' @inheritParams vec_as_location
#' @keywords internal
#' @export
vec_as_subscript <- function(i,
                             ...,
                             allow_types = c("indicator", "location", "name"),
                             arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()
  result_get(vec_as_subscript_result(
    i,
    arg = arg,
    allow_types = allow_types
  ))
}
#' @rdname vec_as_subscript
#' @export
vec_as_subscript2 <- function(i,
                              ...,
                              allow_types = c("location", "name"),
                              arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()
  result_get(vec_as_subscript2_result(i, arg, allow_types = allow_types))
}

vec_as_subscript_result <- function(i, arg, allow_types) {
  allow_types <- as_opts_subscript_type(allow_types, arg = arg)

  if (!vec_is(i)) {
    return(result(err = new_error_subscript_bad_type(
      i = i,
      .arg = arg,
      allow_types = allow_types
    )))
  }

  nms <- names(i)

  if (is.object(i)) {
    if (vec_is_subtype(i, lgl())) {
      i <- vec_cast(i, lgl())
    } else if (vec_is_subtype(i, int())) {
      i <- vec_cast(i, int())
    } else if (vec_is_subtype(i, chr())) {
      i <- vec_cast(i, chr())
    } else {
      return(result(err = new_error_subscript_bad_type(
        i,
        .arg = arg,
        allow_types = allow_types
      )))
    }
  } else if (is_double(i)) {
    result <- tryCatch(
    {
      i <- vec_coercible_cast(i, int(), x_arg = arg, to_arg = "")
      names(i) <- nms
      result(i)
    },
    vctrs_error_cast_lossy = function(err) {
      result(err = new_error_subscript_bad_type(
        i = i,
        parent = err,
        body = cnd_bullets_subscript_lossy_cast,
        allow_types = allow_types
      ))
    })
    return(result)
  }

  # Coerce unspecified vectors to integer only if logical indices
  # are not allowed
  if (!allow_types[["indicator"]] && is_unspecified(i)) {
    if (allow_types[["location"]]) {
      i <- vec_cast(i, int())
    } else {
      i <- vec_cast(i, chr())
    }
  }

  allowed <- switch(typeof(i),
    logical = allow_types[["indicator"]],
    integer = allow_types[["location"]],
    character = allow_types[["name"]],
    FALSE
  )
  if (!allowed) {
    return(result(err = new_error_subscript_bad_type(
      i = i,
      .arg = arg,
      allow_types = allow_types
    )))
  }

  # FIXME: Work around lack of name restoration in `vec_cast()`
  names(i) <- nms

  result(i)
}

vec_as_subscript2_result <- function(i, arg, allow_types) {
  allow_types <- as_opts_subscript2_type(allow_types, arg = arg)
  result <- vec_as_subscript_result(i, arg, allow_types = allow_types)

  # Return a subclass of subscript error
  if (!is_null(result$err)) {
    parent <- result$err$parent
    if (inherits(parent, "vctrs_error_cast_lossy")) {
      bullets <- cnd_bullets_subscript_lossy_cast
    } else {
      bullets <- cnd_bullets_location_bad_base_type
    }

    result$err <- new_error_location_bad_type(
      i = result$err$i,
      allow_types = allow_types,
      .arg = arg,
      body = bullets,
      parent = result$err$parent
    )

    return(result)
  }

  i <- result$ok

  if (typeof(i) == "logical") {
    return(result(err = new_error_location_bad_type(
      i = i,
      allow_types = allow_types,
      .arg = arg,
      body = cnd_bullets_location_bad_base_type
    )))
  }

  result
}


subscript_type_opts <- c("indicator", "location", "name")
subscript_type_opts_indefinite_singular <- c("an indicator", "a location", "a name")
subscript_type_opts_indefinite_plural <- c("indicators", "locations", "names")

collapse_subscript_type <- function(allow_types, plural = FALSE) {
  if (plural) {
    types <- subscript_type_opts_indefinite_plural
  } else {
    types <- subscript_type_opts_indefinite_singular
  }
  types <- types[force(allow_types)]
  types <- glue::glue_collapse(types, sep = ", ", last = " or ")
  types
}

as_opts_subscript_type <- function(x, arg = NULL) {
  if (inherits(x, "vctrs_opts_subscript_type")) {
    return(x)
  }
  new_opts(
    x,
    subscript_type_opts,
    subclass = "vctrs_opts_subscript_type",
    arg = arg
  )
}
as_opts_subscript2_type <- function(x, arg = NULL) {
  if ("indicator" %in% x) {
    abort("Logical indicators can't be converted to a single location.")
  }
  as_opts_subscript_type(x, arg = arg)
}


#' @export
cnd_header.vctrs_error_subscript_bad_type <- function(cnd) {
  "Must subset with a proper subscript vector."
}
#' @export
cnd_body.vctrs_error_subscript_bad_type <- function(cnd) {
  arg <- cnd$.arg %||% "i"
  type <- obj_type(cnd$i)
  expected_types <- collapse_subscript_type(cnd$allow_types, plural = TRUE)

  format_error_bullets(c(
    x = glue::glue("`{arg}` has the wrong type `{type}`."),
    i = glue::glue("These indices must be {expected_types}.")
  ))
}
cnd_bullets_subscript_lossy_cast <- function(cnd, ...) {
  format_error_bullets(c(x = cnd_header(cnd$parent)))
}
