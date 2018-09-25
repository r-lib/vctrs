#' Custom conditions for vctrs package
#'
#' These errors and warnings have custom classes and structures to make
#' testing easier.
#'
#' @keywords internal
#' @param x,y Vectors
#' @param details Any additional human readable details
#' @param subclass Use if you want to further customise the class
#' @param locations For `warn_lossy_cast()`, an optional vector giving the
#'   locations where `x` lost information.
#' @param ...,message,.subclass Only use these fields when creating a subclass.
#' @name vctrs-conditions
NULL

stop_incompatible <- function(x, y, details = NULL, ..., message = NULL, .subclass = NULL) {
  if (is.null(message)) {
    message <- glue::glue("Incompatible inputs: <{vec_ptype_full(x)}> and <{vec_ptype_full(y)}>")
    if (!is.null(details)) {
      message <- paste0(message, "\n", details)
    }
  }

  abort(
    message,
    .subclass = c(.subclass, "error_incompatible"),
    x = x,
    y = y,
    details = details,
    ...
  )
}

#' @rdname vctrs-conditions
#' @export
stop_incompatible_type <- function(x, y, details = NULL, ..., message = NULL, .subclass = NULL) {
  message <- message %||% glue_lines(
    "No common type for <{vec_ptype_full(x)}> and <{vec_ptype_full(y)}>",
    details
  )

  stop_incompatible(
    x, y,
    message = message,
    details = details,
    ...,
    .subclass = c(.subclass, "error_incompatible_type")
  )
}

#' @rdname vctrs-conditions
#' @export
stop_incompatible_cast <- function(x, y, details = NULL, ..., message = NULL, .subclass = NULL) {

  message <- message %||% glue_lines(
    "Can't cast <{vec_ptype_full(x)}> to <{vec_ptype_full(y)}>",
    details
  )

  stop_incompatible(
    x, y,
    message = message,
    details = details,
    ...,
    .subclass = c(.subclass, "error_incompatible_cast")
  )
}

#' @rdname vctrs-conditions
#' @export
stop_incompatible_op <- function(op, x, y, details = NULL, ..., message = NULL, .subclass = NULL) {

  message <- message %||% glue_lines(
    "<{vec_ptype_full(x)}> {op} <{vec_ptype_full(y)}> is not permitted",
    details
  )

  stop_incompatible(
    x, y,
    op = op,
    message = message,
    details = details,
    ...,
    .subclass = c(.subclass, "error_incompatible_op")
  )
}

#' @rdname vctrs-conditions
#' @export
warn_lossy_cast <- function(x, y, locations = NULL, details = NULL, ..., message = NULL, .subclass = NULL) {

  message <- message %||% glue_lines(
    "Lossy cast from <{vec_ptype_full(x)}> to <{vec_ptype_full(y)}>",
    inline_list("Locations: ", locations),
    details
  )

  warn(
    message,
    x = x,
    y = y,
    locations = locations,
    details = details,
    ...,
    .subclass = c("warning_lossy_cast", .subclass),
  )
}

stop_unsupported <- function(x, method) {
  msg <- glue::glue("`{method}.{class(x)[[1]]}()` not supported")
  abort(
    "error_unsupported",
    message = msg,
    x = x,
    method = method
  )
}

stop_unimplemented <- function(x, method) {
  msg <- glue::glue("`{method}.{class(x)[[1]]}()` not implemented")
  abort(
    "error_unimplemented",
    message = msg,
    x = x,
    method = method
  )
}

# helpers -----------------------------------------------------------------

glue_lines <- function(..., env = parent.frame()) {
  lines <- c(...)
  out <- map_chr(lines, glue::glue, .envir = env)
  paste(out, collapse = "\n")
}
