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
  abort(
    c(subclass, "error_incompatible"),
    message = "Incompatible types",
    x = x,
    y = y,
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
conditionMessage.error_incompatible_type <- function(c) {
  msg <- glue::glue_data(c, "No common type for {vec_type_string(x)} and {vec_type_string(y)}")
  if (!is.null(c$details)) {
    msg <- paste0(msg, "\n", c$details)
  }
  msg
}

#' @export
conditionMessage.error_incompatible_cast <- function(c) {
  msg <- glue::glue_data(c, "Can't cast {vec_type_string(x)} to {vec_type_string(y)}")
  if (!is.null(c$details)) {
    msg <- paste0(msg, "\n", c$details)
  }
  msg
}

warn_cast_lossy <- function(message = NULL, .subclass = NULL, from, to, ..., class) {
  if (is.null(message)) {
    message <- glue::glue("Lossy conversion from {vec_type_string(from)} to {vec_type_string(to)}")
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

  pos <- glue::glue_collapse(which, ", ", width = 80)
  msg <- glue::glue("Lossy conversion from {vec_type_string(from)} to {vec_type_string(to)} [Locations: {pos}]")

  warn_cast_lossy(
    "warning_cast_lossy_vector",
    message = msg,
    from = from,
    to = to,
    which = which
  )
}

warn_cast_lossy_dataframe <- function(from, to, dropped) {
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
