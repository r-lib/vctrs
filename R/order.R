vec_order2 <- function(x, decreasing = FALSE, na_last = TRUE) {
  .Call(vctrs_order, x, decreasing, na_last)
}

vec_order_groups <- function(x, decreasing = FALSE, na_last = TRUE) {
  .Call(vctrs_order_groups, x, decreasing, na_last)
}
