new_date <- function(n = 0L) {
  structure(
    double(n),
    class = "Date"
  )
}

new_datetime <- function(n = 0L, tzone = NULL) {
  structure(
    double(n),
    class = c("POSIXct", "POSIXt"),
    tzone = tzone
  )
}

new_duration <- function(x = double(), units = "secs") {
  stopifnot(is.double(x))

  structure(
    x,
    units = units,
    class = "difftime"
  )
}

tzone <- function(x) {
  attr(x, "tzone")
}

tzone_union <- function(x, y) {
  x_tz <- attr(x, "tzone")
  y_tz <- attr(y, "tzone")

  if (is_null(x_tz) || identical(x_tz, "")) {
    y_tz
  } else {
    x_tz
  }
}

units_union <- function(x, y) {
  if (identical(units(x), units(y))) {
    units(x)
  } else {
    "secs"
  }
}
