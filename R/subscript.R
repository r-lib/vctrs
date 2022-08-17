#' Convert to a base subscript type
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Convert `i` to the base type expected by [vec_as_location()] or
#' [vec_as_location2()]. The values of the subscript type are
#' not checked in any way (length, missingness, negative elements).
#'
#' @inheritParams vec_as_location
#'
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
                             arg = NULL,
                             call = caller_env()) {
  check_dots_empty0(...)

  .Call(
    ffi_as_subscript,
    i = i,
    logical = logical,
    numeric = numeric,
    character = character,
    frame = environment()
  )
}
vec_as_subscript_result <- function(i,
                                    arg,
                                    call,
                                    logical,
                                    numeric,
                                    character) {
  .Call(
    ffi_as_subscript_result,
    i = i,
    logical = logical,
    numeric = numeric,
    character = character,
    frame = environment()
  )
}


#' @rdname vec_as_subscript
#' @export
vec_as_subscript2 <- function(i,
                              ...,
                              logical = c("cast", "error"),
                              numeric = c("cast", "error"),
                              character = c("cast", "error"),
                              arg = NULL,
                              call = caller_env()) {
  check_dots_empty0(...)
  result_get(vec_as_subscript2_result(
    i,
    arg,
    call,
    logical = logical,
    numeric = numeric,
    character = character
  ))
}
vec_as_subscript2_result <- function(i,
                                     arg,
                                     call,
                                     logical = "cast",
                                     numeric = "cast",
                                     character = "cast") {
  logical <- arg_match0(logical, c("cast", "error"))
  numeric <- arg_match0(numeric, c("cast", "error"))
  character <- arg_match0(character, c("cast", "error"))

  result <- vec_as_subscript_result(
    i,
    arg = arg,
    call = call,
    logical = logical,
    numeric = numeric,
    character = character
  )

  # Return a child of subscript error. The child error messages refer
  # to single subscripts instead of subscript vectors.
  if (!is_null(result$err)) {
    parent <- result$err$parent
    if (inherits(parent, "vctrs_error_cast_lossy")) {
      bullets <- new_cnd_bullets_subscript_lossy_cast(parent)
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
      call = call
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
      body = cnd_body.vctrs_error_subscript_type,
      call = call
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


stop_subscript <- function(i,
                           ...,
                           class = NULL,
                           call = caller_env()) {
  abort(
    class = c(class, "vctrs_error_subscript"),
    i = i,
    ...,
    call = vctrs_error_call(call)
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
                                     call = NULL,
                                     class = NULL) {
  new_error_subscript(
    class = c(class, "vctrs_error_subscript_type"),
    i = i,
    logical = logical,
    numeric = numeric,
    character = character,
    ...,
    call = vctrs_error_call(call)
  )
}

#' @export
cnd_header.vctrs_error_subscript_type <- function(cnd) {
  action <- cnd_subscript_action(cnd)
  elt <- cnd_subscript_element(cnd)
  if (cnd_subscript_scalar(cnd)) {
    glue::glue("Must {action} {elt[[1]]} with a single valid subscript.")
  } else {
    glue::glue("Must {action} {elt[[2]]} with a valid subscript vector.")
  }
}
#' @export
cnd_body.vctrs_error_subscript_type <- function(cnd) {
  arg <- append_arg("Subscript", cnd$subscript_arg)
  type <- obj_type(cnd$i)
  expected_types <- collapse_subscript_type(cnd)

  format_error_bullets(c(
    x = glue::glue("{arg} has the wrong type `{type}`."),
    i = glue::glue("It must be {expected_types}.")
  ))
}
new_cnd_bullets_subscript_lossy_cast <- function(lossy_err) {
  function(cnd, ...) {
    format_error_bullets(c(x = cnd_header(lossy_err)))
  }
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

cnd_body_subscript_dim <- function(cnd, ...) {
  arg <- append_arg("Subscript", cnd$subscript_arg)

  dim <- length(dim(cnd$i))
  if (dim < 2) {
    abort("Internal error: Unexpected dimensionality in `cnd_body_subcript_dim()`.")
  }
  if (dim == 2) {
    shape <- "a matrix"
  } else {
    shape <- "an array"
  }

  format_error_bullets(c(
    x = glue::glue("{arg} must be a simple vector, not {shape}.")
  ))
}

cnd_subscript_element <- function(cnd, capital = FALSE) {
  elt <- cnd$subscript_elt %||% "element"

  if (!is_string(elt, c("element", "row", "column", "table"))) {
    abort(paste0(
      "Internal error: `cnd$subscript_elt` must be one of ",
      "`element`, `row`, `column` or `table`."
    ))
  }

  if (capital) {
    switch(
      elt,
      element = c("Element", "Elements"),
      row = c("Row", "Rows"),
      column = c("Column", "Columns"),
      table = c("Table", "Tables")
    )
  } else {
    switch(elt,
      element = c("element", "elements"),
      row = c("row", "rows"),
      column = c("column", "columns"),
      table = c("table", "tables")
    )
  }
}

cnd_subscript_element_cli <- function(n, cnd, capital = FALSE) {
  elt <- cnd$subscript_elt %||% "element"

  if (!is_string(elt, c("element", "row", "column", "table"))) {
    abort(paste0(
      "Internal error: `cnd$subscript_elt` must be one of ",
      "`element`, `row`, `column` or `table`."
    ))
  }

  if (capital) {
    elt <- switch(
      elt,
      element = "Element{?s}",
      row = "Row{?s}",
      column = "Column{?s}",
      table = "Table{?s}"
    )
  } else {
    elt <- switch(
      elt,
      element = "element{?s}",
      row = "row{?s}",
      column = "column{?s}",
      table = "table{?s}"
    )
  }

  cli::pluralize("{n} ", elt)
}

subscript_actions <- c(
  "select", "subset", "extract",
  "assign", "rename", "relocate",
  "remove", "negate"
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
    subscript_actions <- glue::backtick(subscript_actions)
    subscript_actions <- glue::glue_collapse(
      subscript_actions,
      sep = ", ",
      last = ", or "
    )
    message <- glue::glue("`cnd$subscript_action` must be one of {subscript_actions}.")
    abort(message, .internal = TRUE)
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
