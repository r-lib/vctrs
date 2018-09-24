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


# Coerce ------------------------------------------------------------------



# Cast --------------------------------------------------------------------

#' @rdname vec_cast
#' @export vec_cast.Date
#' @method vec_cast Date
#' @export
vec_cast.Date <- function(x, to) {
  UseMethod("vec_cast.Date")
}
#' @export
#' @method vec_cast.Date NULL
vec_cast.Date.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.Date double
vec_cast.Date.double <- function(x, to) {
  as.Date(x, origin = "1970-01-01")
}
#' @export
#' @method vec_cast.Date character
vec_cast.Date.character <- function(x, to) {
  as.Date(x, format = "%Y-%m-%d")
}
#' @export
#' @method vec_cast.Date Date
vec_cast.Date.Date <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.Date POSIXt
vec_cast.Date.POSIXt <- function(x, to) {
  out <- as.Date(x)

  lossy <- abs(x - as.POSIXct(out)) > 1e-9
  if (any(lossy)) {
    warn_lossy_cast(x, to, locations = which(lossy))
  }

  out
}
#' @export
#' @method vec_cast.Date list
vec_cast.Date.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.Date default
vec_cast.Date.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @rdname vec_cast
#' @export vec_cast.POSIXt
#' @method vec_cast POSIXt
#' @export
vec_cast.POSIXt <- function(x, to) {
  UseMethod("vec_cast.POSIXt")
}
#' @export
#' @method vec_cast.POSIXt NULL
vec_cast.POSIXt.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.POSIXt double
vec_cast.POSIXt.double <- function(x, to) {
  x <- as.POSIXct(x, origin = "1970-01-01")
  attr(x, "tzone") <- attr(to, "tzone")
  x
}
#' @export
#' @method vec_cast.POSIXt character
vec_cast.POSIXt.character <- function(x, to) {
  as.POSIXct(x, tz = attr(to, "tzone") %||% "")
}
#' @export
#' @method vec_cast.POSIXt Date
vec_cast.POSIXt.Date <- function(x, to) {
  as.POSIXct(as.character(x), tz = attr(to, "tzone") %||% "")
}
#' @export
#' @method vec_cast.POSIXt POSIXt
vec_cast.POSIXt.POSIXt <- function(x, to) {
  attr(x, "tzone") <- attr(to, "tzone")
  x
}
#' @export
#' @method vec_cast.POSIXt list
vec_cast.POSIXt.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.POSIXt default
vec_cast.POSIXt.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @rdname vec_cast
#' @export vec_cast.difftime
#' @method vec_cast difftime
#' @export
vec_cast.difftime <- function(x, to) {
  UseMethod("vec_cast.difftime")
}
#' @export
#' @method vec_cast.difftime NULL
vec_cast.difftime.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.difftime double
vec_cast.difftime.double <- function(x, to) {
  structure(
    as.double(x), # strip attributes
    class = "difftime",
    units = units(to)
  )
}
#' @export
#' @method vec_cast.difftime difftime
vec_cast.difftime.difftime <- function(x, to) {
  if (identical(units(x), units(to))) {
    x
  } else {
    # Hack: I can't see any obvious way of changing the units
    origin <- as.POSIXct(0, origin = "1970-01-01")
    difftime(origin, origin - x, units = units(to))
  }
}
#' @export
#' @method vec_cast.difftime list
vec_cast.difftime.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.difftime default
vec_cast.difftime.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}


