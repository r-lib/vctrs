int_radix_order <- function(x, decreasing = FALSE, na_last = TRUE) {
  .Call(vctrs_int_radix_order, x, decreasing, na_last)
}
