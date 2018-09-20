#' Arithmetic operations
#'
#' This method provides a common double dispatch mechnaism for all arithmetic
#' operators (`+`, `-`, `/`, `*`, `%%` and `%/%`). It is used to power the
#' default arithmetic operators for [vctr]s objects, overcoming the limitations
#' of the base [Ops] generic.
#'
#' `vec_base_arith()` is provided as a convenience for writing methods. It
#' recycles `x` and `y` to common length then uses calls `op` on the underlying
#' [vec_data()].
#'
#' @param op An arithmetic operator as a string
#' @param x,y A pair of vectors. For unary `+` and `-`, `y` can be a
#'   sentinel object of class `MISSING`, as created by `MISSING()`.
#' @seealso [stop_incompatible_op()] for signalling that an arithmetic
#'   operation is not permitted/supported.
#' @export
#' @keywords internal
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
#' @rdname vec_arith
vec_arith.default <- function(op, x, y) {
  stop_incompatible_op(op, x, y)
}

# Atomic vectors ----------------------------------------------------------

#' @rdname vec_arith
#' @export vec_arith.logical
#' @method vec_arith logical
#' @export
vec_arith.logical <- function(op, x, y) UseMethod("vec_arith.logical", y)
#' @method vec_arith.logical default
#' @export
vec_arith.logical.default <- function(op, x, y) stop_incompatible_op(op, x, y)
#' @method vec_arith.logical logical
#' @export
vec_arith.logical.logical <- function(op, x, y) vec_arith_base(op, x, y)
#' @method vec_arith.logical numeric
#' @export
vec_arith.logical.numeric <- function(op, x, y) vec_arith_base(op, x, y)

#' @rdname vec_arith
#' @export vec_arith.numeric
#' @method vec_arith numeric
#' @export
vec_arith.numeric <- function(op, x, y) UseMethod("vec_arith.numeric", y)
#' @method vec_arith.numeric default
#' @export
vec_arith.numeric.default <- function(op, x, y) stop_incompatible_op(op, x, y)
#' @method vec_arith.numeric logical
#' @export
vec_arith.numeric.logical <- function(op, x, y) vec_arith_base(op, x, y)
#' @method vec_arith.numeric numeric
#' @export
vec_arith.numeric.numeric <- function(op, x, y) vec_arith_base(op, x, y)

# Date/times --------------------------------------------------------------

#' @rdname vec_arith
#' @export vec_arith.Date
#' @method vec_arith Date
#' @export
vec_arith.Date <- function(op, x, y) UseMethod("vec_arith.Date", y)
#' @rdname vec_arith
#' @export vec_arith.POSIXct
#' @method vec_arith POSIXct
#' @export
vec_arith.POSIXct <- function(op, x, y) UseMethod("vec_arith.POSIXct", y)
#' @rdname vec_arith
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
  y <- vec_cast(y, new_difftime(units = "secs"))

  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), x),
    `-` = vec_restore(vec_arith_base(op, x, y), x),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.difftime POSIXct
#' @export
vec_arith.difftime.POSIXct <- function(op, x, y) {
  x <- vec_cast(x, new_difftime(units = "secs"))

  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), y),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.Date difftime
#' @export
vec_arith.Date.difftime <- function(op, x, y) {
  # Need to warn if non-integer number of days
  y <- vec_cast(y, new_difftime(units = "days"))

  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), x),
    `-` = vec_restore(vec_arith_base(op, x, y), x),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.difftime Date
#' @export
vec_arith.difftime.Date <- function(op, x, y) {
  x <- vec_cast(x, new_difftime(units = "days"))

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

#' @export
#' @rdname vec_arith
vec_arith_base <- function(op, x, y) {
  c(x, y) %<-% vec_recycle(x, y)

  op_fun <- getExportedValue("base", op)
  op_fun(vec_data(x), vec_data(y))
}

#' @export
#' @rdname vec_arith
MISSING <- function() {
  structure(list(), class = "MISSING")
}
