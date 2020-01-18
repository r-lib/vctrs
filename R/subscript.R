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
#' @param indicator,location,name How to handle indicator (logical),
#'   location (numeric), name (character), and `NULL` subscripts.
#'
#'   If `"coerce"` and the subscript is not one of the three base
#'   types (logical, integer or character), the subscript is coerced
#'   to the relevant base type, e.g. factors are coerced to
#'   character. `NULL` is treated as an empty integer vector, and is
#'   thus coercible depending on the setting of `location`. Symbols
#'   are treated as character vectors and thus coercible depending on
#'   the setting of `name`.
#'
#'   If `"error"`, the subscript type is disallowed and triggers an
#'   informative error.
#' @keywords internal
#' @export
vec_as_subscript <- function(i,
                             ...,
                             # FIXME: Should it be "cast" instead of "coerce"?
                             indicator = c("coerce", "error"),
                             location = c("coerce", "error"),
                             name = c("coerce", "error"),
                             arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()
  result_get(vec_as_subscript_result(
    i,
    arg = arg,
    indicator = indicator,
    location = location,
    name = name
  ))
}
vec_as_subscript_result <- function(i, arg, indicator, location, name) {
  indicator <- arg_match(indicator, c("coerce", "error"))
  location <- arg_match(location, c("coerce", "error"))
  name <- arg_match(name, c("coerce", "error"))

  if (is_null(i) && location == "coerce") {
    i <- integer()
  }
  if (is_symbol(i) && name == "coerce") {
    i <- as_string(i)
  }

  if (!vec_is(i)) {
    return(result(err = new_error_subscript_bad_type(
      i = i,
      arg = arg,
      indicator = indicator,
      location = location,
      name = name
    )))
  }

  nms <- names(i)
  orig <- i

  # Coerce to base types
  if (is.object(i)) {
    if (vec_is_coercible(i, lgl())) {
      i <- vec_cast(i, lgl())
    } else if (vec_is_coercible(i, int())) {
      i <- vec_cast(i, int())
    } else if (vec_is_coercible(i, chr())) {
      i <- vec_cast(i, chr())
    } else {
      return(result(err = new_error_subscript_bad_type(
        i,
        arg = arg,
        indicator = indicator,
        location = location,
        name = name
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
        indicator = indicator,
        location = location,
        name = name
      ))
    })
    return(result)
  }

  # Coerce unspecified vectors to integer only if logical indices
  # are not allowed
  if (indicator == "error" && is_unspecified(i)) {
    if (location == "coerce") {
      i <- vec_cast(i, int())
    } else {
      i <- vec_cast(i, chr())
    }
  }

  action <- switch(typeof(i),
    logical = indicator,
    integer = location,
    character = name,
    "error"
  )

  if (action == "error") {
    result(err = new_error_subscript_bad_type(
      i = i,
      arg = arg,
      indicator = indicator,
      location = location,
      name = name
    ))
  } else {
    # FIXME: Work around lack of name restoration in `vec_cast()`
    names(i) <- nms
    result(i)
  }
}


#' @rdname vec_as_subscript
#' @export
vec_as_subscript2 <- function(i,
                              ...,
                              indicator = c("coerce", "error"),
                              location = c("coerce", "error"),
                              name = c("coerce", "error"),
                              arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()
  result_get(vec_as_subscript2_result(
    i,
    arg,
    indicator = indicator,
    location = location,
    name = name
  ))
}
vec_as_subscript2_result <- function(i,
                                     arg,
                                     indicator = "coerce",
                                     location = "coerce",
                                     name = "coerce") {
  indicator <- arg_match(indicator, c("coerce", "error"))
  location <- arg_match(location, c("coerce", "error"))
  name <- arg_match(name, c("coerce", "error"))

  result <- vec_as_subscript_result(
    i,
    arg = arg,
    indicator = indicator,
    location = location,
    name = name
  )

  # Return a subclass of subscript error
  if (!is_null(result$err)) {
    parent <- result$err$parent
    if (inherits(parent, "vctrs_error_cast_lossy")) {
      bullets <- cnd_bullets_subscript_lossy_cast
    } else {
      bullets <- cnd_body.vctrs_error_subscript_bad_type
    }

    result$err <- new_error_subscript2_bad_type(
      i = result$err$i,
      indicator = indicator,
      location = location,
      name = name,
      arg = arg,
      body = bullets,
      parent = result$err$parent
    )

    return(result)
  }

  i <- result$ok

  if (typeof(i) == "logical") {
    return(result(err = new_error_subscript2_bad_type(
      i = i,
      indicator = indicator,
      location = location,
      name = name,
      arg = arg,
      body = cnd_body.vctrs_error_subscript_bad_type
    )))
  }

  result
}


subscript_type_opts <- c("indicator", "location", "name")
subscript_type_opts_indefinite_singular <- c("an indicator", "a location", "a name")
subscript_type_opts_indefinite_plural <- c("indicators", "locations", "names")

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


new_error_subscript <- function(class = NULL, i, ..., arg = "i") {
  error_cnd(
    c(class, "vctrs_error_subscript"),
    i = i,
    arg = arg,
    ...
  )
}
new_error_subscript_bad_type <- function(i,
                                         indicator = "coerce",
                                         location = "coerce",
                                         name = "coerce",
                                         ...,
                                         arg = "i",
                                         class = NULL) {
  new_error_subscript(
    class = c(class, "vctrs_error_subscript_bad_type"),
    i = i,
    indicator = indicator,
    location = location,
    name = name,
    arg = arg,
    ...
  )
}

#' @export
cnd_header.vctrs_error_subscript_bad_type <- function(cnd) {
  "Must subset with a proper subscript vector."
}
#' @export
cnd_body.vctrs_error_subscript_bad_type <- function(cnd) {
  arg <- cnd$arg %||% "i"
  type <- obj_type(cnd$i)
  expected_types <- collapse_subscript_type(cnd, plural = TRUE)

  format_error_bullets(c(
    x = glue::glue("`{arg}` has the wrong type `{type}`."),
    i = glue::glue("The subscript must contain {expected_types}.")
  ))
}
cnd_bullets_subscript_lossy_cast <- function(cnd, ...) {
  format_error_bullets(c(x = cnd_header(cnd$parent)))
}

collapse_subscript_type <- function(cnd, plural = FALSE) {
  if (plural) {
    types <- subscript_type_opts_indefinite_plural
  } else {
    types <- subscript_type_opts_indefinite_singular
  }

  allowed <- cnd[c("indicator", "location", "name")] != "error"
  types <- types[allowed]

  glue::glue_collapse(types, sep = ", ", last = " or ")
}

new_error_subscript_bad_size <- function(i,
                                         ...,
                                         class = NULL) {
  new_error_subscript(
    class = c(class, "vctrs_error_subscript_bad_size"),
    i = i,
    ...
  )
}
#' @export
cnd_header.vctrs_error_subscript_bad_size <- function(cnd, ...) {
  cnd_header.vctrs_error_subscript_bad_type(cnd, ...)
}

new_error_subscript2_bad_type <- function(i,
                                          indicator,
                                          location,
                                          name,
                                          ...,
                                          arg = "i",
                                          class = NULL) {
  new_error_subscript(
    class = c(class, "vctrs_error_subscript2_bad_type"),
    i = i,
    indicator = indicator,
    location = location,
    name = name,
    arg = arg,
    ...
  )
}
#' @export
cnd_header.vctrs_error_subscript2_bad_type <- function(cnd) {
  "Must extract with a single subscript."
}

cnd_subscript_element <- function(cnd) {
  elt <- cnd$subscript_elt %||% "element"

  if (!is_string(elt, c("element", "row", "column"))) {
    abort(paste0(
      "Internal error: `cnd$subscript_elt` must be one of ",
      "`element`, `row`, or `column`."
    ))
  }

  switch(elt,
    element = c("element", "elements"),
    row = c("row", "rows"),
    column = c("column", "columns")
  )
}

subscript_actions <- c(
  "subset", "extract", "assign", "rename", "remove", "negate"
)
cnd_subscript_action <- function(cnd) {
  action <- cnd$subscript_action %||% "subset"

  if (!is_string(action, subscript_actions)) {
    abort(paste0(
      "Internal error: `cnd$subscript_action` must be one of ",
      "`subset`, `extract`, `assign`, `rename`, `remove`, or `negate`."
    ))
  }

  action
}

cnd_subscript_type <- function(cnd) {
  type <- cnd$subscript_type

  if (!is_string(type, c("indicator", "location", "name"))) {
    abort("Internal error: `cnd$subscript_type` must be `indicator`, `location`, or `name`.")
  }

  type
}
