#' Custom conditions for vctrs package
#'
#' @description
#'
#' These errors and warnings have custom classes and structures to make
#' testing easier.
#'
#' By default, lossy casts are an error. Use
#' `allow_lossy_cast()` to silence these errors and continue
#' with the partial results. The lost values may be set to `NA` or to
#' a lower value resolution, depending on the type of cast.
#'
#' @keywords internal
#' @param x,y Vectors
#' @param details Any additional human readable details
#' @param subclass Use if you want to further customise the class
#' @param locations For `stop_lossy_cast()`, an optional vector giving the
#'   locations where `x` lost information.
#' @param ...,message,.subclass Only use these fields when creating a subclass.
#'
#' @name vctrs-conditions
NULL

stop_incompatible <- function(x, y, details = NULL, ..., message = NULL, .subclass = NULL) {
  abort(
    message,
    .subclass = c(.subclass, "vctrs_error_incompatible"),
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
    "No common type for <{vec_ptype_full(x)}> and <{vec_ptype_full(y)}>.",
    details
  )

  stop_incompatible(
    x, y,
    details = details,
    ...,
    message = message,
    .subclass = c(.subclass, "vctrs_error_incompatible_type")
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
    details = details,
    ...,
    message = message,
    .subclass = c(.subclass, "vctrs_error_incompatible_cast")
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
    details = details,
    ...,
    message = message,
    .subclass = c(.subclass, "vctrs_error_incompatible_op")
  )
}

#' @rdname vctrs-conditions
#' @param result The result of a potentially lossy cast.
#' @param to Type to cast to.
#' @param lossy A logical vector indicating which elements of `result`
#'   were lossy.
#'
#'   Can also be a single `TRUE`, but note that `locations` picks up
#'   locations from this vector by default. In this case, supply your
#'   own location vector, possibly empty.
#' @export
maybe_lossy_cast <- function(result, x, to,
                             lossy = NULL,
                             locations = NULL,
                             details = NULL,
                             ...,
                             message = NULL,
                             .subclass = NULL) {
  if (!any(lossy)) {
    return(result)
  }

  locations <- locations %||% which(lossy)

  withRestarts(
    vctrs_restart_error_cast_lossy = function() result,
    stop_lossy_cast(x, to, result, locations = NULL, ...)
  )
}
stop_lossy_cast <- function(x, to, result,
                            locations = NULL,
                            details = NULL,
                            ...,
                            message = NULL,
                            .subclass = NULL) {
  if (length(locations)) {
    locations <- inline_list("Locations: ", locations)
  }
  message <- message %||% glue_lines(
    "Lossy cast from <{vec_ptype_full(x)}> to <{vec_ptype_full(to)}>.",
    locations,
    details
  )

  abort(
    message,
    x = x,
    y = to,
    to = to,
    result = result,
    locations = locations,
    details = details,
    ...,
    .subclass = c(.subclass, "vctrs_error_cast_lossy")
  )
}
#' @rdname vctrs-conditions
#' @param x_ptype,to_ptype Suppress only the casting errors where `x`
#'   or `to` match these [prototypes][vec_type].
#' @export
allow_lossy_cast <- function(expr, x_ptype = NULL, to_ptype = NULL) {
  withCallingHandlers(
    vctrs_error_cast_lossy = function(err) {
      if (!is_null(x_ptype) && !vec_is(err$x, x_ptype)) {
        return()
      }
      if (!is_null(to_ptype) && !vec_is(err$to, to_ptype)) {
        return()
      }

      invokeRestart("vctrs_restart_error_cast_lossy")
    },
    expr
  )
}

stop_unsupported <- function(x, method) {
  msg <- glue::glue("`{method}.{class(x)[[1]]}()` not supported.")
  abort(
    "vctrs_error_unsupported",
    message = msg,
    x = x,
    method = method
  )
}

stop_unimplemented <- function(x, method) {
  msg <- glue::glue("`{method}.{class(x)[[1]]}()` not implemented.")
  abort(
    "vctrs_error_unimplemented",
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
