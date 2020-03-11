# AsIS --------------------------------------------------------------------

# Arises with base df ctor: `data.frame(x = I(list(1, 2:3)))`

#' @export
vec_proxy.AsIs <- function(x, ...) {
  class(x) <- setdiff(class(x), "AsIs")
  vec_proxy(x)
}
#' @export
vec_restore.AsIs <- function(x, to, ...) {
  I(x)
}

# AsIs --------------------------------------------------------------------

#' @export
vec_ptype_full.AsIs <- function(x, ...) {
  class(x) <- setdiff(class(x), "AsIs")
  paste0("I<", vec_ptype_full(x), ">")
}

#' @export
vec_ptype_abbr.AsIs <- function(x, ...) {
  class(x) <- setdiff(class(x), "AsIs")
  paste0("I<", vec_ptype_abbr(x), ">")
}

