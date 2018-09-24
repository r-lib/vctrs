new_date <- function(x = double()) {
  stopifnot(is.double(x))

  structure(
    x,
    class = "Date"
  )
}

new_datetime <- function(x = double(), tzone = "") {
  stopifnot(is.double(x))
  stopifnot(is.character(tzone))

  structure(
    x,
    tzone = tzone,
    class = c("POSIXct", "POSIXt")
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

tzone_is_local <- function(x) {
  tz <- tzone(x)
  is.null(tz) || identical(tz, "")
}

tzone_union <- function(x, y) {
  if (tzone_is_local(x)) {
    tzone(y)
  } else {
    tzone(x)
  }
}

units_union <- function(x, y) {
  if (identical(units(x), units(y))) {
    units(x)
  } else {
    "secs"
  }
}
