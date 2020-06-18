vec_order2 <- function(x, direction = "asc", na_value = "largest") {
  .Call(vctrs_order, x, direction, na_value)
}

vec_order_groups <- function(x, direction = "asc", na_value = "largest") {
  .Call(vctrs_order_groups, x, direction, na_value)
}
