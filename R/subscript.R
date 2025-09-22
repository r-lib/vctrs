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
#' @param logical,numeric,character How to handle logical, numeric,
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
vec_as_subscript <- function(
  i,
  ...,
  logical = c("cast", "error"),
  numeric = c("cast", "error"),
  character = c("cast", "error"),
  arg = NULL,
  call = caller_env()
) {
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
vec_as_subscript_result <- function(i, arg, call, logical, numeric, character) {
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
vec_as_subscript2 <- function(
  i,
  ...,
  numeric = c("cast", "error"),
  character = c("cast", "error"),
  arg = NULL,
  call = caller_env()
) {
  check_dots <- function(..., logical = "error", call = caller_env()) {
    if (!is_string(logical, "error")) {
      abort(
        "`vctrs::vec_as_subscript2(logical = 'cast')` is deprecated.",
        call = caller_env()
      )
    }
    check_dots_empty0(..., call = call)
  }
  check_dots(...)

  result_get(vec_as_subscript2_result(
    i,
    arg,
    call,
    numeric = numeric,
    character = character
  ))
}
vec_as_subscript2_result <- function(
  i,
  arg,
  call,
  numeric = "cast",
  character = "cast"
) {
  numeric <- arg_match0(numeric, c("cast", "error"))
  character <- arg_match0(character, c("cast", "error"))

  result <- vec_as_subscript_result(
    i,
    arg = arg,
    call = call,
    logical = "error",
    numeric = numeric,
    character = character
  )

  # This should normally be a `vctrs_error_subscript`. Indicate to
  # message methods that this error refers to a `[[` subscript.
  if (!is_null(result$err)) {
    result$err$subscript_scalar <- TRUE
  }

  result
}


subscript_type_opts <- c("logical", "numeric", "character")
subscript_type_opts_indefinite_singular <- c(
  "a logical flag",
  "a location",
  "a name"
)
subscript_type_opts_indefinite_plural <- c(
  "logical flags",
  "locations",
  "names"
)

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


stop_subscript <- function(i, ..., class = NULL, call = caller_env()) {
  abort(
    class = c(class, "vctrs_error_subscript"),
    i = i,
    ...,
    call = call
  )
}
new_error_subscript <- function(class = NULL, i, ...) {
  error_cnd(
    c(class, "vctrs_error_subscript"),
    i = i,
    ...
  )
}
new_error_subscript_type <- function(
  i,
  logical = "cast",
  numeric = "cast",
  character = "cast",
  ...,
  call = NULL,
  class = NULL
) {
  new_error_subscript(
    class = c(class, "vctrs_error_subscript_type"),
    i = i,
    logical = logical,
    numeric = numeric,
    character = character,
    ...,
    call = call
  )
}

#' @export
cnd_header.vctrs_error_subscript_type <- function(cnd, ...) {
  arg <- cnd[["subscript_arg"]]
  if (is_subscript_arg(arg)) {
    with <- glue::glue(" with {format_subscript_arg(arg)}")
  } else {
    with <- ""
  }

  action <- cnd_subscript_action(cnd, assign_to = FALSE)
  elt <- cnd_subscript_element(cnd)

  if (cnd_subscript_scalar(cnd)) {
    glue::glue("Can't {action} {elt[[1]]}{with}.")
  } else {
    glue::glue("Can't {action} {elt[[2]]}{with}.")
  }
}
#' @export
cnd_body.vctrs_error_subscript_type <- function(cnd, ...) {
  arg <- cnd_subscript_arg(cnd)
  type <- obj_type_friendly(cnd$i)
  expected_types <- cnd_subscript_expected_types(cnd)

  format_error_bullets(c(
    x = cli::format_inline("{arg} must be {.or {expected_types}}, not {type}.")
  ))
}
new_cnd_bullets_subscript_lossy_cast <- function(lossy_err) {
  function(cnd, ...) {
    format_error_bullets(c(x = cnd_header(lossy_err)))
  }
}

collapse_subscript_type <- function(cnd) {
  types <- cnd_subscript_expected_types(cnd)

  if (length(types) == 2) {
    last <- " or "
  } else {
    last <- ", or "
  }

  glue::glue_collapse(types, sep = ", ", last = last)
}
cnd_subscript_expected_types <- function(cnd) {
  types <- c("logical", "numeric", "character")
  allowed <- cnd[types] != "error"
  types[allowed]
}

new_error_subscript_size <- function(i, ..., class = NULL) {
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

new_error_subscript2_type <- function(i, numeric, character, ...) {
  new_error_subscript_type(
    i = i,
    logical = "error",
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
    abort(
      "Internal error: Unexpected dimensionality in `cnd_body_subcript_dim()`."
    )
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
    switch(
      elt,
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
  "select",
  "subset",
  "extract",
  "assign",
  "rename",
  "relocate",
  "remove",
  "negate"
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
    cli::cli_abort(
      "`cnd$subscript_action` must be one of {.or {.arg {subscript_actions}}}.",
      .internal = TRUE
    )
  }

  if (assign_to && action == "assign") {
    "assign to"
  } else {
    action
  }
}

cnd_subscript_arg <- function(cnd, ...) {
  format_subscript_arg(cnd[["subscript_arg"]], ...)
}
format_subscript_arg <- function(arg, capitalise = TRUE) {
  if (is_subscript_arg(arg)) {
    if (!is_string(arg)) {
      arg <- as_label(arg)
    }
    cli::format_inline("{.arg {arg}}")
  } else {
    if (capitalise) {
      "Subscript"
    } else {
      "subscript"
    }
  }
}
is_subscript_arg <- function(x) {
  !is_null(x) && !is_string(x, "")
}

cnd_subscript_type <- function(cnd) {
  type <- cnd$subscript_type

  if (!is_string(type, c("logical", "numeric", "character"))) {
    abort(
      "Internal error: `cnd$subscript_type` must be `logical`, `numeric`, or `character`."
    )
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
