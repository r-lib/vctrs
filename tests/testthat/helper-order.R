# Keep in sync with macros in `order.c`
GROUP_DATA_SIZE_DEFAULT <- 100000L
ORDER_INSERTION_BOUNDARY <- 128L
INT_ORDER_COUNTING_RANGE_BOUNDARY <- 100000L

# Force radix method for character comparisons
base_order <- function(x, na.last = TRUE, decreasing = FALSE) {
  if (is.data.frame(x)) {
    x <- unname(x)
  } else {
    x <- list(x)
  }

  args <- list(
    na.last = na.last,
    decreasing = decreasing,
    method = "radix"
  )

  args <- c(x, args)

  rlang::exec("order", !!!args)
}
