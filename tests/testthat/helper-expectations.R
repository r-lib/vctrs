expect_dim <- function(x, type) {
  dim <- vec_dim(x)
  expect_equal(vec_dim(x), !!dim)
}
