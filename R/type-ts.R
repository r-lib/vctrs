#' `ts` S3 class
#'
#' These functions help the base `ts` class fit into the vctrs type system
#' by providing coercion and casting functions.
#'
#' The `ts` class is a bit strange for vctrs:
#'
#' - It allows for arbitrary storage types.
#' - It has a data dependent `tsp` attribute.
#' - The `ts()` function prevents you from creating a size 0 version of a `ts`
#'   object, presumably because you can't make the `tsp` attribute without some
#'   data.
#' - The `stats::[.ts` method drops off all attributes.
#' - The `c()` default method drops off all attributes.
#'
#' Because of all of this, we aggressively drive coercion towards the underlying
#' atomic type, which mostly matches the `[` and `c()` methods.
#'
#' @keywords internal
#' @name vctrs-ts
NULL

#' @export
vec_ptype.ts <- function(x, ...) {
  vec_ptype(proxy_data(x))
}

#' @export
vec_ptype2.ts.ts <- function(x, y, ..., x_arg = "", y_arg = "") {
  vec_ptype2(proxy_data(x), proxy_data(y))
}

#' @export
vec_cast.ts.ts <- function(x, to, ...) {
  abort("Can't cast directly from <ts> to <ts>.")
}

# Needed because `vec_ptype2()` returns underlying storage
#' @export
vec_cast.integer.ts <- function(x, to, ...) {
  ts_cast_atomic(x, to)
}

# Needed because `vec_ptype2()` returns underlying storage
#' @export
vec_cast.double.ts <- function(x, to, ...) {
  ts_cast_atomic(x, to)
}

ts_cast_atomic <- function(x, to) {
  x <- proxy_data(x)
  vec_cast(x, to)
}
