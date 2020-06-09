vec_order2 <- function(x, decreasing = FALSE, na_last = TRUE) {
  .Call(vctrs_order, x, decreasing, na_last, FALSE)
}

vec_order_groups <- function(x, decreasing = FALSE, na_last = TRUE) {
  .Call(vctrs_order, x, decreasing, na_last, TRUE)
}
