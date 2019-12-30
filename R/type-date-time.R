#' Date, date-time, and duration S3 classes
#'
#' * A `date` ([Date]) is a double vector. Its value represent the number
#'   of days since the Unix "epoch", 1970-01-01. It has no attributes.
#' * A `datetime` ([POSIXct] is a double vector. Its value represents the
#'   number of seconds since the Unix "Epoch", 1970-01-01. It has a single
#'   attribute: the timezone (`tzone`))
#' * A `duration` ([difftime])
#'
#' These function help the base `Date`, `POSIXct`, and `difftime` classes fit
#' into the vctrs type system by providing constructors, coercion functions,
#' and casting functions.
#'
#' @param x A double vector representing the number of days since UNIX
#'   epoch for `new_date()`, number of seconds since UNIX epoch for
#'   `new_datetime()`, and number of `units` for `new_duration()`.
#' @param tzone Time zone. A character vector of length 1. Either `""` for
#'   the local time zone, or a value from [OlsonNames()]
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

  if (is.integer(x)) {
    x <- as.double(x)
  }
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

#' @export
vec_proxy.Date <- function(x, ...) {
  as_double_date(x)
}

#' @export
vec_proxy.POSIXct <- function(x, ...) {
  new_datetime(x, attr(x, "tzone"))
}

#' @export
vec_proxy.POSIXlt <- function(x, ...) {
  new_data_frame(unclass(x))
}
#' @export
vec_proxy_compare.POSIXlt <- function(x, ..., relax = FALSE) {
  new_data_frame(vec_data(x)[c("year", "mon", "mday", "hour", "min", "sec")], n = length(x))
}


# Print ------------------------------------------------------------------

#' @export
vec_ptype_full.Date <- function(x, ...) {
  "date"
}

#' @export
vec_ptype_abbr.Date <- function(x, ...) {
  "date"
}

#' @export
vec_ptype_full.POSIXct <- function(x, ...) {
  tzone <- if (tzone_is_local(x)) "local" else tzone(x)
  paste0("datetime<", tzone, ">")
}

#' @export
vec_ptype_full.POSIXlt <- function(x, ...) {
  tzone <- if (tzone_is_local(x)) "local" else tzone(x)
  paste0("POSIXlt<", tzone, ">")
}

#' @export
vec_ptype_abbr.POSIXt <- function(x, ...) {
  "dttm"
}

#' @export
vec_ptype_full.difftime <- function(x, ...) {
  paste0("duration<", attr(x, "units"), ">")
}

#' @export
vec_ptype_abbr.difftime <- function(x, ...) {
  "drtn"
}

# Coerce ------------------------------------------------------------------

#' @rdname new_date
#' @export vec_ptype2.Date
#' @method vec_ptype2 Date
#' @export
vec_ptype2.Date <- function(x, y, ...) UseMethod("vec_ptype2.Date", y)
#' @method vec_ptype2.Date default
#' @export
vec_ptype2.Date.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_ptype2.Date Date
#' @export
vec_ptype2.Date.Date <- function(x, y, ...) new_date()

#' @rdname new_date
#' @export vec_ptype2.POSIXt
#' @method vec_ptype2 POSIXt
#' @export
vec_ptype2.POSIXt <- function(x, y, ...) UseMethod("vec_ptype2.POSIXt", y)
#' @method vec_ptype2.POSIXt default
#' @export
vec_ptype2.POSIXt.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_ptype2.POSIXt Date
#' @export
vec_ptype2.POSIXt.Date <- function(x, y, ...) new_datetime(tzone = tzone(x))
#' @method vec_ptype2.Date POSIXt
#' @export
vec_ptype2.Date.POSIXt <- function(x, y, ...) new_datetime(tzone = tzone(y))
#' @method vec_ptype2.POSIXt POSIXt
#' @export
vec_ptype2.POSIXt.POSIXt <- function(x, y, ...) new_datetime(tzone = tzone_union(x, y))

#' @rdname new_date
#' @export vec_ptype2.difftime
#' @method vec_ptype2 difftime
#' @export
vec_ptype2.difftime <- function(x, y, ...) UseMethod("vec_ptype2.difftime", y)
#' @method vec_ptype2.difftime default
#' @export
vec_ptype2.difftime.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_ptype2.difftime difftime
#' @export
vec_ptype2.difftime.difftime <- function(x, y, ...) new_duration(units = units_union(x, y))

# Cast --------------------------------------------------------------------

#' @rdname new_date
#' @export vec_cast.Date
#' @method vec_cast Date
#' @export
vec_cast.Date <- function(x, to, ...) {
  UseMethod("vec_cast.Date")
}
#' @export
#' @method vec_cast.Date double
vec_cast.Date.double <- function(x, to, ...) {
  new_date(x)
}
#' @export
#' @method vec_cast.Date character
vec_cast.Date.character <- function(x, to, ...) {
  as.Date(x, format = "%Y-%m-%d")
}
#' @export
#' @method vec_cast.Date Date
vec_cast.Date.Date <- function(x, to, ...) {
  as_double_date(x)
}
#' @export
#' @method vec_cast.Date POSIXt
vec_cast.Date.POSIXt <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  out <- as.Date(x)
  lossy <- abs(x - as.POSIXct(out)) > 1e-9
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.Date list
vec_cast.Date.list <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.Date default
vec_cast.Date.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @rdname new_date
#' @export vec_cast.POSIXct
#' @method vec_cast POSIXct
#' @export
vec_cast.POSIXct <- function(x, to, ...) {
  UseMethod("vec_cast.POSIXct")
}
#' @export
#' @method vec_cast.POSIXct double
vec_cast.POSIXct.double <- function(x, to, ...) {
  new_datetime(x, tzone = tzone(to))
}
#' @export
#' @method vec_cast.POSIXct character
vec_cast.POSIXct.character <- function(x, to, ...) {
  as.POSIXct(x, tz = tzone(to))
}
#' @export
#' @method vec_cast.POSIXct Date
vec_cast.POSIXct.Date <- function(x, to, ...) {
  as.POSIXct(as.character(x), tz = tzone(to))
}
#' @export
#' @method vec_cast.POSIXct POSIXlt
vec_cast.POSIXct.POSIXlt <- function(x, to, ...) {
  new_datetime(as.POSIXct(x), tzone = tzone(to))
}
#' @export
#' @method vec_cast.POSIXct POSIXct
vec_cast.POSIXct.POSIXct <- function(x, to, ...) {
  new_datetime(vec_data(x), tzone = tzone(to))
}
#' @export
#' @method vec_cast.POSIXct list
vec_cast.POSIXct.list <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.POSIXct default
vec_cast.POSIXct.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @rdname new_date
#' @export vec_cast.POSIXlt
#' @method vec_cast POSIXlt
#' @export
vec_cast.POSIXlt <- function(x, to, ...) {
  UseMethod("vec_cast.POSIXlt")
}
#' @export
#' @method vec_cast.POSIXlt double
vec_cast.POSIXlt.double <- function(x, to, ...) {
  as.POSIXlt(new_datetime(x, tzone = tzone(to)))
}
#' @export
#' @method vec_cast.POSIXlt character
vec_cast.POSIXlt.character <- function(x, to, ...) {
  as.POSIXlt(x, tz = tzone(to))
}
#' @export
#' @method vec_cast.POSIXlt Date
vec_cast.POSIXlt.Date <- function(x, to, ...) {
  as.POSIXlt(as.character(x), tz = tzone(to))
}
#' @export
#' @method vec_cast.POSIXlt POSIXlt
vec_cast.POSIXlt.POSIXlt <- function(x, to, ...) {
  as.POSIXlt(x, tz = tzone(to))
}
#' @export
#' @method vec_cast.POSIXlt POSIXct
vec_cast.POSIXlt.POSIXct <- function(x, to, ...) {
  as.POSIXlt(x, tz = tzone(to))
}
#' @export
#' @method vec_cast.POSIXlt list
vec_cast.POSIXlt.list <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.POSIXlt default
vec_cast.POSIXlt.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}


#' @rdname new_date
#' @export vec_cast.difftime
#' @method vec_cast difftime
#' @export
vec_cast.difftime <- function(x, to, ...) {
  UseMethod("vec_cast.difftime")
}
#' @export
#' @method vec_cast.difftime double
vec_cast.difftime.double <- function(x, to, ...) {
  new_duration(vec_data(x), units = units(to))
}
#' @export
#' @method vec_cast.difftime difftime
vec_cast.difftime.difftime <- function(x, to, ...) {
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
vec_cast.difftime.list <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.difftime default
vec_cast.difftime.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}


# Arithmetic --------------------------------------------------------------

#' @rdname new_date
#' @export vec_arith.Date
#' @method vec_arith Date
#' @export
vec_arith.Date <- function(op, x, y, ...) UseMethod("vec_arith.Date", y)
#' @rdname new_date
#' @export vec_arith.POSIXct
#' @method vec_arith POSIXct
#' @export
vec_arith.POSIXct <- function(op, x, y, ...) UseMethod("vec_arith.POSIXct", y)
#' @rdname new_date
#' @export vec_arith.difftime
#' @method vec_arith difftime
#' @export
vec_arith.difftime <- function(op, x, y, ...) UseMethod("vec_arith.difftime", y)

#' @method vec_arith.Date default
#' @export
vec_arith.Date.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)
#' @method vec_arith.POSIXct default
#' @export
vec_arith.POSIXct.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)
#' @method vec_arith.difftime default
#' @export
vec_arith.difftime.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)

#' @method vec_arith.Date Date
#' @export
vec_arith.Date.Date <- function(op, x, y, ...) {
  switch(op,
    `-` = difftime(x, y, units = "days"),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.POSIXct POSIXct
#' @export
vec_arith.POSIXct.POSIXct <- function(op, x, y, ...) {
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
vec_arith.Date.numeric <- function(op, x, y, ...) {
  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), x),
    `-` = vec_restore(vec_arith_base(op, x, y), x),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.numeric Date
#' @export
vec_arith.numeric.Date <- function(op, x, y, ...) {
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
vec_arith.POSIXct.difftime <- function(op, x, y, ...) {
  y <- vec_cast(y, new_duration(units = "secs"))

  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), x),
    `-` = vec_restore(vec_arith_base(op, x, y), x),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.difftime POSIXct
#' @export
vec_arith.difftime.POSIXct <- function(op, x, y, ...) {
  x <- vec_cast(x, new_duration(units = "secs"))

  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), y),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.Date difftime
#' @export
vec_arith.Date.difftime <- function(op, x, y, ...) {
  y <- vec_cast(y, new_duration(units = "days"))

  switch(op,
    `+` = ,
    `-` = vec_restore(vec_arith_base(op, x, lossy_floor(y, x)), x),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.difftime Date
#' @export
vec_arith.difftime.Date <- function(op, x, y, ...) {
  x <- vec_cast(x, new_duration(units = "days"))

  switch(op,
    `+` = vec_restore(vec_arith_base(op, lossy_floor(x, y), y), y),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.difftime difftime
#' @export
vec_arith.difftime.difftime <- function(op, x, y, ...) {
  # Ensure x and y have same units
  args <- vec_cast_common(x, y)
  x <- args[[1L]]
  y <- args[[2L]]

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
vec_arith.difftime.MISSING <- function(op, x, y, ...) {
  switch(op,
    `-` = vec_restore(-vec_data(x), x),
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.difftime numeric
#' @export
vec_arith.difftime.numeric <- function(op, x, y, ...) {
  vec_restore(vec_arith_base(op, x, y), x)
}
#' @method vec_arith.numeric difftime
#' @export
vec_arith.numeric.difftime <- function(op, x, y, ...) {
  switch(op,
    `/` = stop_incompatible_op(op, x, y),
    vec_restore(vec_arith_base(op, x, y), y)
  )
}

# Helpers -----------------------------------------------------------------

# The tz attribute for POSIXlt can have 3 components
# (time zone name, abbreviated name, abbreviated DST name)
tzone <- function(x) {
  attr(x, "tzone")[[1]] %||% ""
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

as_double_date <- function(x) {
  if (is.integer(x)) {
    new_date(as.double(x))
  } else {
    x
  }
}

# Math --------------------------------------------------------------------

#' @export
vec_math.Date <- function(.fn, .x, ...) {
  stop_unsupported(.x, .fn)
}
#' @export
vec_math.POSIXct <- function(.fn, .x, ...) {
  stop_unsupported(.x, .fn)
}
