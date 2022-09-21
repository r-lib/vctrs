#' @export
#' @rdname vec_equal
vec_equal_na <- function(x) {
  vec_detect_missing(x)
}

vec_detect_missing <- function(x) {
  .Call(ffi_vec_detect_missing, x)
}

vec_any_missing <- function(x) {
  .Call(ffi_vec_any_missing, x)
}
