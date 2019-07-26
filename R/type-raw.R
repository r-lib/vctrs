
#' @export
vec_proxy_compare.raw <- function(x, ...) {
  # because:
  # order(as.raw(1:3))
  # #> Error in order(as.raw(1:3)): unimplemented type 'raw' in 'orderVector1'
  as.integer(x)
}
