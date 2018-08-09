expect_dim <- function(x, shape) {
  dim <- vec_dim(x)
  expect_equal(dim, !!shape)
}
