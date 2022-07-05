#' @export
#' @rdname vec_equal
vec_equal_na <- function(x) {
  .Call(vctrs_equal_na, x)
}
