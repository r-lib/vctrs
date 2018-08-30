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

#' @rdname vctrs-conditions
#' @export
warn_lossy_cast <- function(x, y, locations = NULL, details = NULL) {
  warn(
    "warning_lossy_cast",
    message = "Lossy cast",
    x = x,
    y = y,
    locations = locations,
    details = details
  )
}

#' @export
conditionMessage.error_incompatible_type <- function(c) {
  msg <- glue::glue_data(c, "No common type for {vec_ptype_full(x)} and {vec_ptype_full(y)}")
  if (!is.null(c$details)) {
    msg <- paste0(msg, "\n", c$details)
  }
  msg
}

#' @export
conditionMessage.error_incompatible_cast <- function(c) {
  msg <- glue::glue_data(c, "Can't cast {vec_ptype_full(x)} to {vec_ptype_full(y)}")
  if (!is.null(c$details)) {
    msg <- paste0(msg, "\n", c$details)
  }
  msg
}

#' @export
conditionMessage.warning_lossy_cast <- function(c) {
  msg <- glue::glue_data(c, "Lossy cast from {vec_ptype_full(x)} to {vec_ptype_full(y)}")

  if (!is.null(c$locations)) {
    msg <- paste0(msg, "\n", inline_list("Locations: ", c$locations))
  }

  if (!is.null(c$details)) {
    msg <- paste0(msg, "\n", c$details)
  }
  msg

}
