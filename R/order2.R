vec_radix_order <- function(x, decreasing = FALSE, na_last = TRUE) {
  .Call(vctrs_radix_order, x, decreasing, na_last)
}
