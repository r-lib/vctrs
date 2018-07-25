context("test-dim")

test_that("length is number of rows", {
  expect_equal(vec_length(1:2), 2)
  expect_equal(vec_length(matrix(nrow = 2, ncol = 3)), 2)
  expect_equal(vec_length(array(dim = c(2, 1, 5))), 2)
})

test_that("dim is dimensions", {
  expect_equal(vec_dim(array(dim = c(1))), c(1))
  expect_equal(vec_dim(array(dim = c(1, 1))), c(1, 1))
  expect_equal(vec_dim(array(dim = c(1, 1, 1))), c(1, 1, 1))
})


test_that("dims is number of dimensions", {
  expect_equal(vec_dims(array(dim = c(1))), 1)
  expect_equal(vec_dims(array(dim = c(1, 1))), 2)
  expect_equal(vec_dims(array(dim = c(1, 1, 1))), 3)
})

test_that("vector and 1-d array are equivalent", {
  x1 <- 1:5
  x2 <- array(x1)

  expect_equal(vec_dim(x1), 5)
  expect_equal(vec_dim(x2), 5)

  expect_equal(vec_length(x1), 5)
  expect_equal(vec_length(x2), 5)
})
