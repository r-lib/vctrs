vec_radix_order <- function(x, decreasing = FALSE, na_last = TRUE) {
  .Call(vctrs_radix_order, x, decreasing, na_last)
}

vec_radix_order_old <- function(x, na_last = TRUE, decreasing = FALSE) {
  .Call(vctrs_radix_order_old, x, na_last, decreasing)
}

