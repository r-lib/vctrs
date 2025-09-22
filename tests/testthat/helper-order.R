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

  args <- list(na.last = na.last, decreasing = decreasing)

  # `method` didn't exist on R < 3.3.
  # It would sometimes use radix sorting automatically.
  if (getRversion() < "3.3.0") {
    method <- list()
  } else {
    method <- list(method = "radix")
  }

  args <- c(x, args, method)

  rlang::exec("order", !!!args)
}
