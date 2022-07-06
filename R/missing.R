#' @export
#' @rdname vec_equal
vec_equal_na <- function(x) {
  .Call(ffi_vec_equal_na, x)
}
