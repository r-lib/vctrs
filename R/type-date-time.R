#' Date, date-time, and duration S3 classes
#'
#' These function help the date ([Date]), datetime ([POSIXct]), and duration
#' ([difftime]) classes fit into the vctrs type system by providing low-level
#' constructors, as well as coercion and casting generics + methods.
#'
#' @param x A double vector representing the number of days since UNIX
#'   epoch for `new_date()`, number of seconds since UNIX epoch for
#'   `new_datetime()`, and number of `units` for `new_duration()`.
#' @param tzone Time zone. Either `""` for local time zone, or a value
#'   from [OlsonNames()]
#' @param units Units of duration.
#' @export
#' @keywords internal
#' @examples
#' new_date(0)
#' new_datetime(0, tzone = "UTC")
#' new_duration(1, "hour")
new_date <- function(x = double()) {
  stopifnot(is.double(x))

  structure(
    x,
    class = "Date"
  )
}

#' @export
#' @rdname new_date
new_datetime <- function(x = double(), tzone = "") {
  tzone <- tzone %||% ""

  stopifnot(is.double(x))
  stopifnot(is.character(tzone))

  structure(
    x,
    tzone = tzone,
    class = c("POSIXct", "POSIXt")
  )
}

#' @export
#' @rdname new_date
new_duration <- function(x = double(), units = c("secs", "mins", "hours", "days", "weeks")) {
  stopifnot(is.double(x))
  units <- match.arg(units)

  structure(
    x,
    units = units,
    class = "difftime"
  )
}

# Print ------------------------------------------------------------------

#' @export
vec_ptype_full.Date <- function(x) {
  "date"
}

#' @export
vec_ptype_abbr.Date <- function(x) {
  "date"
}

#' @export
vec_ptype_full.POSIXt <- function(x) {
  tzone <- if (tzone_is_local(x)) "local" else tzone(x)
  paste0("datetime<", tzone, ">")
}

#' @export
vec_ptype_abbr.POSIXct <- function(x) {
  "dttm"
}

#' @export
vec_ptype_full.difftime <- function(x) {
  paste0("duration<", attr(x, "units"), ">")
}

#' @export
vec_ptype_abbr.difftime <- function(x) {
  "drtn"
}

# Coerce ------------------------------------------------------------------

#' @rdname new_date
#' @export vec_type2.Date
#' @method vec_type2 Date
#' @export
vec_type2.Date   <- function(x, y) UseMethod("vec_type2.Date", y)
#' @method vec_type2.Date default
#' @export
vec_type2.Date.default   <- function(x, y) stop_incompatible_type(x, y)
#' @method vec_type2.Date NULL
#' @export
vec_type2.Date.NULL      <- function(x, y) new_date()
#' @method vec_type2.Date Date
#' @export
vec_type2.Date.Date      <- function(x, y) new_date()

#' @rdname new_date
#' @export vec_type2.POSIXt
#' @method vec_type2 POSIXt
#' @export
vec_type2.POSIXt <- function(x, y) UseMethod("vec_type2.POSIXt", y)
#' @method vec_type2.POSIXt default
#' @export
vec_type2.POSIXt.default <- function(x, y) stop_incompatible_type(x, y)
#' @method vec_type2.POSIXt Date
#' @export
vec_type2.POSIXt.Date    <- function(x, y) new_datetime(tzone = tzone(x))
#' @method vec_type2.Date POSIXt
#' @export
vec_type2.Date.POSIXt    <- function(x, y) new_datetime(tzone = tzone(y))
#' @method vec_type2.POSIXt POSIXt
#' @export
vec_type2.POSIXt.POSIXt  <- function(x, y) new_datetime(tzone = tzone_union(x, y))

#' @rdname new_date
#' @export vec_type2.difftime
#' @method vec_type2 difftime
#' @export
vec_type2.difftime <- function(x, y) UseMethod("vec_type2.difftime", y)
#' @method vec_type2.difftime default
#' @export
vec_type2.difftime.default  <- function(x, y) stop_incompatible_type(x, y)
#' @method vec_type2.difftime difftime
#' @export
vec_type2.difftime.difftime <- function(x, y) new_duration(units = units_union(x, y))

# Cast --------------------------------------------------------------------

#' @rdname new_date
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
  new_date(x)
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

#' @rdname new_date
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
  new_datetime(x, tzone = tzone(to))
}
#' @export
#' @method vec_cast.POSIXt character
vec_cast.POSIXt.character <- function(x, to) {
  as.POSIXct(x, tz = tzone(to))
}
#' @export
#' @method vec_cast.POSIXt Date
vec_cast.POSIXt.Date <- function(x, to) {
  as.POSIXct(as.character(x), tz = tzone(to))
}
#' @export
#' @method vec_cast.POSIXt POSIXt
vec_cast.POSIXt.POSIXt <- function(x, to) {
  new_datetime(vec_data(x), tzone = tzone(to))
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

#' @rdname new_date
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
  new_duration(vec_data(x), units = units(to))
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

# Arithmetic --------------------------------------------------------------

#' @rdname new_date
#' @export vec_arith.Date
#' @method vec_arith Date
#' @export
vec_arith.Date <- function(op, x, y) UseMethod("vec_arith.Date", y)
#' @rdname new_date
#' @export vec_arith.POSIXct
#' @method vec_arith POSIXct
#' @export
vec_arith.POSIXct <- function(op, x, y) UseMethod("vec_arith.POSIXct", y)
#' @rdname new_date
#' @export vec_arith.difftime
#' @method vec_arith difftime
#' @export
vec_arith.difftime <- function(op, x, y) UseMethod("vec_arith.difftime", y)

#' @method vec_arith.Date default
#' @export
vec_arith.Date.default <- function(op, x, y) stop_incompatible_op(op, x, y)
#' @method vec_arith.POSIXct default
#' @export
vec_arith.POSIXct.default <- function(op, x, y) stop_incompatible_op(op, x, y)
#' @method vec_arith.difftime default
#' @export
vec_arith.difftime.default <- function(op, x, y) stop_incompatible_op(op, x, y)

#' @method vec_arith.Date Date
#' @export
vec_arith.Date.Date <- function(op, x, y) {
  switch(op,
    `-` = difftime(x, y, units = "days"),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.POSIXct POSIXct
#' @export
vec_arith.POSIXct.POSIXct <- function(op, x, y) {
  switch(op,
    `-` = difftime(x, y, units = "secs"),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.POSIXct Date
#' @export
vec_arith.POSIXct.Date <- vec_arith.POSIXct.POSIXct
#' @method vec_arith.Date POSIXct
#' @export
vec_arith.Date.POSIXct <- vec_arith.POSIXct.POSIXct

#' @method vec_arith.Date numeric
#' @export
vec_arith.Date.numeric <- function(op, x, y) {
  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), x),
    `-` = vec_restore(vec_arith_base(op, x, y), x),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.numeric Date
#' @export
vec_arith.numeric.Date <- function(op, x, y) {
  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), y),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.POSIXct numeric
#' @export
vec_arith.POSIXct.numeric <- vec_arith.Date.numeric
#' @method vec_arith.numeric POSIXct
#' @export
vec_arith.numeric.POSIXct <- vec_arith.numeric.Date

#' @method vec_arith.POSIXct difftime
#' @export
vec_arith.POSIXct.difftime <- function(op, x, y) {
  y <- vec_cast(y, new_duration(units = "secs"))

  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), x),
    `-` = vec_restore(vec_arith_base(op, x, y), x),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.difftime POSIXct
#' @export
vec_arith.difftime.POSIXct <- function(op, x, y) {
  x <- vec_cast(x, new_duration(units = "secs"))

  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), y),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.Date difftime
#' @export
vec_arith.Date.difftime <- function(op, x, y) {
  # Need to warn if non-integer number of days
  y <- vec_cast(y, new_duration(units = "days"))

  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), x),
    `-` = vec_restore(vec_arith_base(op, x, y), x),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.difftime Date
#' @export
vec_arith.difftime.Date <- function(op, x, y) {
  x <- vec_cast(x, new_duration(units = "days"))

  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), y),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.difftime difftime
#' @export
vec_arith.difftime.difftime <- function(op, x, y) {
  # Ensure x and y have same units
  c(x, y) %<-% as.list(vec_coerce(x, y))

  switch(op,
    `+`   = vec_restore(vec_arith_base(op, x, y), x),
    `-`   = vec_restore(vec_arith_base(op, x, y), x),
    `/`   = vec_arith_base(op, x, y),
    `%/%` = vec_arith_base(op, x, y),
    `%%`  = vec_arith_base(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.difftime MISSING
#' @export
vec_arith.difftime.MISSING <- function(op, x, y) {
  switch(op,
    `-` = vec_restore(-vec_data(x), x),
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.difftime numeric
#' @export
vec_arith.difftime.numeric <- function(op, x, y) {
  vec_restore(vec_arith_base(op, x, y), x)
}
#' @method vec_arith.numeric difftime
#' @export
vec_arith.numeric.difftime <- function(op, x, y) {
  switch(op,
    `/` = stop_incompatible_op(op, x, y),
    vec_restore(vec_arith_base(op, x, y), y)
  )
}

# Helpers -----------------------------------------------------------------

tzone <- function(x) {
  attr(x, "tzone") %||% ""
}

tzone_is_local <- function(x) {
  identical(tzone(x), "")
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

# Math --------------------------------------------------------------------

#' @export
vec_math.Date <- function(fun, x, ...) {
  stop_unsupported(x, fun)
}
#' @export
vec_math.POSIXct <- function(fun, x, ...) {
  stop_unsupported(x, fun)
}
