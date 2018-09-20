#' @examples
#' d <- as.Date("2018-01-01")
#' dt <- as.POSIXct("2018-01-02 12:00")
#' t <- as.difftime(12, unit = "hours")
#'
#' vec_arith("-", dt, 1)
#' vec_arith("-", dt, t)
#' vec_arith("-", dt, d)
#'
#' vec_arith("+", dt, 86400)
#' vec_arith("+", dt, t)
#' vec_arith("+", t, t)
#'
#' vec_arith("/", t, t)
#' vec_arith("/", t, 2)
#'
#' vec_arith("*", t, 2)
vec_arith <- function(op, x, y) {
  UseMethod("vec_arith", x)
}

#' @export
vec_arith.default <- function(op, x, y) {
  stop_incompatible_op(op, x, y)
}

# Atomic vectors ----------------------------------------------------------

vec_arith.logical <- function(op, x, y) UseMethod("vec_arith.logical", y)
vec_arith.numeric <- function(op, x, y) UseMethod("vec_arith.numeric", y)

vec_arith.logical.default <- function(op, x, y) stop_incompatible_op(op, x, y)
vec_arith.numeric.default <- function(op, x, y) stop_incompatible_op(op, x, y)

vec_arith.logical.logical <- function(op, x, y) base_arith(op, x, y)
vec_arith.logical.numeric <- function(op, x, y) base_arith(op, x, y)
vec_arith.numeric.logical <- function(op, x, y) base_arith(op, x, y)
vec_arith.numeric.numeric <- function(op, x, y) base_arith(op, x, y)

# Date/times --------------------------------------------------------------

vec_arith.Date <- function(op, x, y) UseMethod("vec_arith.Date", y)
vec_arith.POSIXct <- function(op, x, y) UseMethod("vec_arith.POSIXct", y)
vec_arith.difftime <- function(op, x, y) UseMethod("vec_arith.difftime", y)

vec_arith.Date.default <- function(op, x, y) stop_incompatible_op(op, x, y)
vec_arith.POSIXct.default <- function(op, x, y) stop_incompatible_op(op, x, y)
vec_arith.difftime.default <- function(op, x, y) stop_incompatible_op(op, x, y)

vec_arith.Date.Date <- function(op, x, y) {
  switch(op,
    `-` = difftime(x, y, units = "days"),
    stop_incompatible_op(op, x, y)
  )
}
vec_arith.POSIXct.POSIXct <- function(op, x, y) {
  switch(op,
    `-` = difftime(x, y, units = "secs"),
    stop_incompatible_op(op, x, y)
  )
}
vec_arith.POSIXct.Date <- vec_arith.POSIXct.POSIXct
vec_arith.Date.POSIXct <- vec_arith.POSIXct.POSIXct

vec_arith.Date.numeric <- function(op, x, y) {
  switch(op,
    `+` = ,
    `-` = vec_restore(base_arith(op, x, y), x),
    stop_incompatible_op(op, x, y)
  )
}
vec_arith.numeric.Date <- function(op, x, y) {
  switch(op,
    `+` = ,
    `-` = vec_restore(base_arith(op, x, y), y),
    stop_incompatible_op(op, x, y)
  )
}
vec_arith.POSIXct.numeric <- vec_arith.Date.numeric
vec_arith.numeric.POSIXct <- vec_arith.numeric.Date

vec_arith.POSIXct.difftime <- function(op, x, y) {
  y <- vec_cast(y, new_difftime(units = "secs"))

  switch(op,
    `+` = ,
    `-` = vec_restore(base_arith(op, x, y), x),
    stop_incompatible_op(op, x, y)
  )
}
vec_arith.difftime.POSIXct <- function(op, x, y) {
  x <- vec_cast(x, new_difftime(units = "secs"))

  switch(op,
    `+` = ,
    `-` = vec_restore(base_arith(op, x, y), y),
    stop_incompatible_op(op, x, y)
  )
}
vec_arith.Date.difftime <- function(op, x, y) {
  # Need to warn if non-integer number of days
  y <- vec_cast(y, new_difftime(units = "days"))

  switch(op,
    `+` = ,
    `-` = vec_restore(base_arith(op, x, y), x),
    stop_incompatible_op(op, x, y)
  )
}
vec_arith.difftime.Date <- function(op, x, y) {
  x <- vec_cast(x, new_difftime(units = "days"))

  switch(op,
    `+` = ,
    `-` = vec_restore(base_arith(op, x, y), y),
    stop_incompatible_op(op, x, y)
  )
}

vec_arith.difftime.difftime <- function(op, x, y) {
  # Ensure x and y have same units
  c(x, y) %<-% as.list(vec_coerce(x, y))

  switch(op,
    `+`   = ,
    `-`   = vec_restore(base_arith(op, x, y), x),
    `/`   = ,
    `%/%` = ,
    `%%`  = base_arith(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}

vec_arith.difftime.numeric <- function(op, x, y) {
  vec_restore(base_arith(op, x, y), x)
}
vec_arith.numeric.difftime <- function(op, x, y) {
  vec_restore(base_arith(op, x, y), y)
}

# Helpers -----------------------------------------------------------------

base_arith <- function(op, x, y) {
  c(x, y) %<-% vec_recycle(x, y)

  op_fun <- getExportedValue("base", op)
  op_fun(vec_data(x), vec_data(y))
}

MISSING <- function() {
  structure(list(), class = "MISSING")
}
