vec_slice_complete <- function(x) {
  .Call(vctrs_slice_complete, x)
}

vec_locate_complete <- function(x) {
  .Call(vctrs_locate_complete, x)
}

vec_detect_complete <- function(x) {
  .Call(vctrs_detect_complete, x)
}
