vec_radix_order <- function(x, na_last = TRUE) {
  .Call(vctrs_radix_order, x, na_last)
}
