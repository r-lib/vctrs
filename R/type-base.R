# Factors -----------------------------------------------------------------

new_factor <- function(n = 0L, levels = character()) {
  factor(character(n), levels = levels)
}

new_ordered <- function(n = 0L, levels = character()) {
  ordered(character(n), levels = levels)
}

levels_union <- function(x, y) {
  union(levels(x), levels(y))
}

# Date times --------------------------------------------------------------

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

new_difftime <- function(n = 0L, units = "secs") {
  structure(
    double(n),
    units = units,
    class = "difftime"
  )
}

units_union <- function(x, y) {
  if (identical(units(x), units(y))) {
    units(x)
  } else {
    "secs"
  }
}
