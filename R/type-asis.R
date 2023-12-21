#' AsIs S3 class
#'
#' These functions help the base AsIs class fit into the vctrs type system
#' by providing coercion and casting functions.
#'
#' @keywords internal
#' @name as-is
NULL

# ------------------------------------------------------------------------------
# Printing

#' @export
vec_ptype_full.AsIs <- function(x, ...) {
  x <- asis_strip(x)
  paste0("I<", vec_ptype_full(x), ">")
}

#' @export
vec_ptype_abbr.AsIs <- function(x, ...) {
  x <- asis_strip(x)
  paste0("I<", vec_ptype_abbr(x), ">")
}

# ------------------------------------------------------------------------------
# Proxy / restore

# Arises with base df ctor: `data.frame(x = I(list(1, 2:3)))`

#' @export
vec_proxy.AsIs <- function(x, ...) {
  x <- asis_strip(x)
  vec_proxy(x)
}

#' @export
vec_restore.AsIs <- function(x, to, ...) {
  asis_restore(NextMethod())
}

#' @export
vec_proxy_equal.AsIs <- function(x, ...) {
  x <- asis_strip(x)
  vec_proxy_equal(x)
}

#' @export
vec_proxy_compare.AsIs <- function(x, ...) {
  x <- asis_strip(x)
  vec_proxy_compare(x)
}

#' @export
vec_proxy_order.AsIs <- function(x, ...) {
  x <- asis_strip(x)
  vec_proxy_order(x)
}

# ------------------------------------------------------------------------------
# Coercion

#' @rdname as-is
#' @export vec_ptype2.AsIs
#' @method vec_ptype2 AsIs
#' @export
vec_ptype2.AsIs <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2.AsIs")
}
#' @method vec_ptype2.AsIs AsIs
#' @export
vec_ptype2.AsIs.AsIs <- function(x, y, ..., x_arg = "", y_arg = "") {
  x <- asis_strip(x)
  y <- asis_strip(y)
  vec_ptype2_asis(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

vec_ptype2_asis_left <- function(x, y, ...) {
  x <- asis_strip(x)
  vec_ptype2_asis(x, y, ...)
}
vec_ptype2_asis_right <- function(x, y, ...) {
  y <- asis_strip(y)
  vec_ptype2_asis(x, y, ...)
}
vec_ptype2_asis <- function(x, y, ...) {
  out <- vec_ptype2(x, y, ...)
  asis_restore(out)
}

# ------------------------------------------------------------------------------
# Casting

vec_cast_from_asis <- function(x, to, ..., call = caller_env()) {
  x <- asis_strip(x)
  vec_cast(x, to, ..., call = call)
}

vec_cast_to_asis <- function(x, to, ..., call = caller_env()) {
  to <- asis_strip(to)
  out <- vec_cast(x, to, ..., call = call)
  asis_restore(out)
}

# ------------------------------------------------------------------------------

is_asis <- function(x) {
  inherits(x, "AsIs")
}

asis_strip <- function(x) {
  class(x) <- setdiff(class(x), "AsIs")
  x
}

asis_restore <- function(x) {
  # Using `oldClass()` here to return `NULL` for atomics
  # so that their implicit class isn't added
  class(x) <- unique.default(c("AsIs", oldClass(x)))
  x
}
