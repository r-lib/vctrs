# Keep in sync with macros in `order.c`
GROUP_DATA_SIZE_DEFAULT <- 100000L
INT_INSERTION_ORDER_BOUNDARY <- 128L
INT_COUNTING_ORDER_RANGE_BOUNDARY <- 100000L

lst_order <- function(x, na.last = TRUE, decreasing = FALSE) {
  rlang::exec("order", !!!unname(x), na.last = na.last, decreasing = decreasing)
}
