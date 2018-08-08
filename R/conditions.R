#' Custom conditions for vctrs package
#'
#' These errors and warnings have custom classes and structures to make
#' testing easier.
#'
#' @keywords internal
#' @param x,y Vectors
#' @param details Any additional human readable details
#' @param subclass Use if you want to further customise the class
#' @param is_lossy A logical vector describing which elements of
#'   `x` lost resolution.
#' @name vctrs-conditions
NULL

stop_incompatible <- function(x, y, details = NULL, subclass = NULL) {
  type_x <- as_vec_type(x)
  type_y <- as_vec_type(y)

  abort(
    c(subclass, "error_incompatible"),
    message = "Incompatible types",
    type_x = type_x,
    type_y = type_y,
    details = details
  )
}

#' @rdname vctrs-conditions
#' @export
stop_incompatible_type <- function(x, y, details = NULL) {
  stop_incompatible(
    x, y,
    details = details,
    subclass = "error_incompatible_type"
  )
}

#' @rdname vctrs-conditions
#' @export
stop_incompatible_cast <- function(x, y, details = NULL) {
  stop_incompatible(
    x, y,
    details = details,
    subclass = "error_incompatible_cast"
  )
}

#' @export
conditionMessage.stop_incompatible <- function(c) {
  msg <- glue::glue_data(c, "Incompatible types: {type_x} + {type_y}")
  if (!is.null(c$details)) {
    msg <- paste0(c$msg, "\n", c$details)
  }
  msg
}

#' @export
conditionMessage.error_incompatible_type <- function(c) {
  msg <- glue::glue_data(c, "No common type for {type_x} and {type_y}")
  if (!is.null(c$details)) {
    msg <- paste0(c$msg, "\n", c$details)
  }
  msg
}

#' @export
conditionMessage.error_incompatible_cast <- function(c) {
  msg <- glue::glue_data(c, "Can't cast {type_x} to {type_y}")
  if (!is.null(c$details)) {
    msg <- paste0(c$msg, "\n", c$details)
  }
  msg
}

warn_cast_lossy <- function(message = NULL, .subclass = NULL, from, to, ..., class) {
  from <- as_vec_type(from)
  to <- as_vec_type(to)

  if (is.null(message)) {
    message <- glue::glue("Lossy conversion from {from} to {to}")
  }

  warn(
    c(.subclass, "warning_cast_lossy"),
    message = message,
    from = from,
    to = to,
    ...
  )
}

#' @export
#' @rdname vctrs-conditions
warn_cast_lossy_vector <- function(from, to, is_lossy) {
  which <- which(is_lossy)
  if (length(which) == 0) {
    return()
  }

  from <- as_vec_type(from)
  to <- as_vec_type(to)

  pos <- glue::glue_collapse(which, ", ", width = 80)
  msg <- glue::glue("Lossy conversion from {from} to {to} [Locations: {pos}]")

  warn_cast_lossy(
    "warning_cast_lossy_vector",
    message = msg,
    from = from,
    to = to,
    which = which
  )
}

warn_cast_lossy_dataframe <- function(from, to, dropped) {
  from <- as_vec_type(from)
  to <- as_vec_type(to)

  vars <- glue::glue_collapse(dropped, width = 80)
  msg <- glue::glue("
    Lossy conversion from data.frame to data.frame
    Dropped variables: {vars}"
  )

  warn_cast_lossy(
    "warning_cast_lossy_dataframe",
    message = msg,
    from = from,
    to = to,
    dropped = dropped
  )
}
