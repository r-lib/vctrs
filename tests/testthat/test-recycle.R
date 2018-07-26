context("test-recycle")

test_that("NULL is idempotent", {
  expect_equal(recycle2(1:5, NULL), list(x = 1:5, y = NULL))
  expect_equal(recycle2(NULL, 1:5), list(x = NULL, y = 1:5))
})

test_that("equal lengths returned as is", {
  x <- 1:3

  expect_equal(recycle2(x, x), list(x = x, y = x))
  expect_equal(recycle2(x[1], x[1]), list(x = x[1], y = x[1]))
  expect_equal(recycle2(x[0], x[0]), list(x = x[0], y = x[0]))
})

test_that("length 1 recycled to length of longest", {
  x1 <- 1
  x3 <- rep(1, 3)

  expect_equal(recycle2(x1, x3), list(x = x3, y = x3))
  expect_equal(recycle2(x3, x1), list(x = x3, y = x3))
})

test_that("length 0 causes both outputs to be zero", {
  x <- 1:3

  expect_equal(recycle2(x, x[0]), list(x = x[0], y = x[0]))
  expect_equal(recycle2(x[0], x), list(x = x[0], y = x[0]))
})

test_that("incompatible lengths get error messages", {
  expect_error(recycle2(1:2, 1:3), "Incompatible")
  expect_error(recycle2(1:3, 1:2), "Incompatible")
})
