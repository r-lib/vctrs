
expect_size <- function(object, n) {
  expect_identical(vec_size(object), vec_cast(n, int()))
}

zap_dimnames <- function(x) {
  attr(x, "dimnames") <- NULL
  x
}
