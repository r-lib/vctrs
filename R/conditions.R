abort_no_max_type <- function(x, y) {
  type_x <- as_vec_type(x)
  type_y <- as_vec_type(y)

  msg <- glue::glue("No common type for {type_x} and {type_y}")
  abort(
    "error_no_max_type",
    message = msg,
    type_x = type_x,
    type_y = type_y,
  )
}

abort_no_cast <- function(from, to, details = NULL) {
  from <- as_vec_type(from)
  to <- as_vec_type(to)

  msg <- glue::glue("Can't cast {from} to {to}")
  if (!is.null(details)) {
    msg <- paste0(msg, "\n", details)
  }
  abort(
    "error_no_cast",
    message = msg,
    from = from,
    to = to,
    details = details
  )
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
