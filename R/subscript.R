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
#' @param logical,location,character How to handle logical, numeric,
#'   and character subscripts.
#'
#'   If `"cast"` and the subscript is not one of the three base types
#'   (logical, integer or character), the subscript is
#'   [cast][vec_cast] to the relevant base type, e.g. factors are
#'   coerced to character. `NULL` is treated as an empty integer
#'   vector, and is thus coercible depending on the setting of
#'   `numeric`. Symbols are treated as character vectors and thus
#'   coercible depending on the setting of `character`.
#'
#'   If `"error"`, the subscript type is disallowed and triggers an
#'   informative error.
#' @keywords internal
#' @export
vec_as_subscript <- function(i,
                             ...,
                             logical = c("cast", "error"),
                             numeric = c("cast", "error"),
                             character = c("cast", "error"),
                             arg = NULL) {
  if (!missing(...)) ellipsis::check_dots_empty()
  result_get(vec_as_subscript_result(
    i,
    arg = arg,
    logical = logical,
    numeric = numeric,
    character = character
  ))
}
vec_as_subscript_result <- function(i, arg, logical, numeric, character) {
  logical <- arg_match(logical, c("cast", "error"))
  numeric <- arg_match(numeric, c("cast", "error"))
  character <- arg_match(character, c("cast", "error"))

  if (is_null(i) && numeric == "cast") {
    i <- integer()
  }
  if (is_symbol(i) && character == "cast") {
    i <- as_string(i)
  }

  if (!vec_is(i)) {
    return(result(err = new_error_subscript_type(
      i = i,
      subscript_arg = arg,
      logical = logical,
      numeric = numeric,
      character = character
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
      return(result(err = new_error_subscript_type(
        i,
        subscript_arg = arg,
        logical = logical,
        numeric = numeric,
        character = character
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
      result(err = new_error_subscript_type(
        i = i,
        parent = err,
        body = cnd_bullets_subscript_lossy_cast,
        logical = logical,
        numeric = numeric,
        character = character
      ))
    })
    return(result)
  }

  # Coerce unspecified vectors to integer only if logical indices
  # are not allowed
  if (logical == "error" && is_unspecified(i)) {
    if (numeric == "cast") {
      i <- vec_cast(i, int())
    } else {
      i <- vec_cast(i, chr())
    }
  }

  action <- switch(typeof(i),
    logical = logical,
    integer = numeric,
    character = character,
    "error"
  )

  if (action == "error") {
    result(err = new_error_subscript_type(
      i = i,
      subscript_arg = arg,
      logical = logical,
      numeric = numeric,
      character = character
    ))
  } else {
    # FIXME: Work around lack of character restoration in `vec_cast()`
    names(i) <- nms
    result(i)
  }
}


#' @rdname vec_as_subscript
#' @export
vec_as_subscript2 <- function(i,
                              ...,
                              logical = c("cast", "error"),
                              numeric = c("cast", "error"),
                              character = c("cast", "error"),
                              arg = NULL) {
  if (!missing(...)) ellipsis::check_dots_empty()
  result_get(vec_as_subscript2_result(
    i,
    arg,
    logical = logical,
    numeric = numeric,
    character = character
  ))
}
vec_as_subscript2_result <- function(i,
                                     arg,
                                     logical = "cast",
                                     numeric = "cast",
                                     character = "cast") {
  logical <- arg_match(logical, c("cast", "error"))
  numeric <- arg_match(numeric, c("cast", "error"))
  character <- arg_match(character, c("cast", "error"))

  result <- vec_as_subscript_result(
    i,
    arg = arg,
    logical = logical,
    numeric = numeric,
    character = character
  )

  # Return a subclass of subscript error
  if (!is_null(result$err)) {
    parent <- result$err$parent
    if (inherits(parent, "vctrs_error_cast_lossy")) {
      bullets <- cnd_bullets_subscript_lossy_cast
    } else {
      bullets <- cnd_body.vctrs_error_subscript_type
    }

    result$err <- new_error_subscript2_type(
      i = result$err$i,
      logical = logical,
      numeric = numeric,
      character = character,
      subscript_arg = arg,
      body = bullets,
      parent = result$err$parent
    )

    return(result)
  }

  i <- result$ok

  if (typeof(i) == "logical") {
    return(result(err = new_error_subscript2_type(
      i = i,
      logical = logical,
      numeric = numeric,
      character = character,
      subscript_arg = arg,
      body = cnd_body.vctrs_error_subscript_type
    )))
  }

  result
}


subscript_type_opts <- c("logical", "numeric", "character")
subscript_type_opts_indefinite_singular <- c("a logical flag", "a location", "a name")
subscript_type_opts_indefinite_plural <- c("logical flags", "locations", "names")

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
  if ("logical" %in% x) {
    abort("Logical subscripts can't be converted to a single location.")
  }
  as_opts_subscript_type(x, arg = arg)
}


stop_subscript <- function(i, ..., class = NULL) {
  abort(
    class = c(class, "vctrs_error_subscript"),
    i = i,
    ...
  )
}
new_error_subscript <- function(class = NULL, i, ...) {
  error_cnd(
    c(class, "vctrs_error_subscript"),
    i = i,
    ...
  )
}
new_error_subscript_type <- function(i,
                                     logical = "cast",
                                     numeric = "cast",
                                     character = "cast",
                                     ...,
                                     class = NULL) {
  new_error_subscript(
    class = c(class, "vctrs_error_subscript_type"),
    i = i,
    logical = logical,
    numeric = numeric,
    character = character,
    ...
  )
}

#' @export
cnd_header.vctrs_error_subscript_type <- function(cnd) {
  action <- cnd_subscript_action(cnd)
  elt <- cnd_subscript_element(cnd)
  if (cnd_subscript_scalar(cnd)) {
    glue::glue("Must {action} {elt[[1]]} with a single subscript.")
  } else {
    glue::glue("Must {action} {elt[[2]]} with a valid subscript vector.")
  }
}
#' @export
cnd_body.vctrs_error_subscript_type <- function(cnd) {
  arg <- append_arg("The subscript", cnd$subscript_arg)
  type <- obj_type(cnd$i)
  expected_types <- collapse_subscript_type(cnd)

  format_error_bullets(c(
    x = glue::glue("{arg} has the wrong type `{type}`."),
    i = glue::glue("It must be {expected_types}.")
  ))
}
cnd_bullets_subscript_lossy_cast <- function(cnd, ...) {
  format_error_bullets(c(x = cnd_header(cnd$parent)))
}

collapse_subscript_type <- function(cnd) {
  types <- c("logical", "numeric", "character")
  allowed <- cnd[types] != "error"
  types <- types[allowed]

  if (length(types) == 2) {
    last <- " or "
  } else {
    last <- ", or "
  }

  glue::glue_collapse(types, sep = ", ", last = last)
}

new_error_subscript_size <- function(i,
                                     ...,
                                     class = NULL) {
  new_error_subscript(
    class = c(class, "vctrs_error_subscript_size"),
    i = i,
    ...
  )
}
#' @export
cnd_header.vctrs_error_subscript_size <- function(cnd, ...) {
  cnd_header.vctrs_error_subscript_type(cnd, ...)
}

new_error_subscript2_type <- function(i,
                                      logical,
                                      numeric,
                                      character,
                                      ...) {
  new_error_subscript_type(
    i = i,
    logical = logical,
    numeric = numeric,
    character = character,
    subscript_scalar = TRUE,
    ...
  )
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
cnd_subscript_action <- function(cnd, assign_to = TRUE) {
  action <- cnd$subscript_action

  if (is_null(action)) {
    if (cnd_subscript_scalar(cnd)) {
      action <- "extract"
    } else {
      action <- "subset"
    }
  }

  if (!is_string(action, subscript_actions)) {
    abort(paste0(
      "Internal error: `cnd$subscript_action` must be one of ",
      "`subset`, `extract`, `assign`, `rename`, `remove`, or `negate`."
    ))
  }

  if (assign_to && action == "assign") {
    "assign to"
  } else {
    action
  }
}

cnd_subscript_type <- function(cnd) {
  type <- cnd$subscript_type

  if (!is_string(type, c("logical", "numeric", "character"))) {
    abort("Internal error: `cnd$subscript_type` must be `logical`, `numeric`, or `character`.")
  }

  type
}

cnd_subscript_scalar <- function(cnd) {
  out <- cnd$subscript_scalar %||% FALSE

  if (!is_bool(out)) {
    abort("Internal error: `cnd$subscript_scalar` must be a boolean.")
  }

  out
}
