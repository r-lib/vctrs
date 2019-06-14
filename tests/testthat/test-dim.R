context("test-dim")

# vec_dim -----------------------------------------------------------------

test_that("dim is dimensions", {
  expect_equal(vec_dim(array(dim = c(1))), c(1))
  expect_equal(vec_dim(array(dim = c(1, 1))), c(1, 1))
  expect_equal(vec_dim(array(dim = c(1, 1, 1))), c(1, 1, 1))
})


test_that("dim_n is number of dimensions", {
  expect_equal(vec_dim_n(array(dim = c(1))), 1)
  expect_equal(vec_dim_n(array(dim = c(1, 1))), 2)
  expect_equal(vec_dim_n(array(dim = c(1, 1, 1))), 3)
})

test_that("vector and 1-d array are equivalent", {
  x1 <- 1:5
  x2 <- array(x1)

  expect_equal(vec_dim(x1), 5)
  expect_equal(vec_dim(x2), 5)

  expect_equal(vec_size(x1), 5)
  expect_equal(vec_size(x2), 5)
})
