expect_dim <- function(x, type) {
  dim <- vec_dim(x)
  expect_equal(vec_dim(x), !!dim)
}

data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}
