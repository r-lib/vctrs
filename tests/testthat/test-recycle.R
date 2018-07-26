context("test-recycle")

# Vectors -----------------------------------------------------------------

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


# Matrices ----------------------------------------------------------------

test_that("can recycle matrices", {
  x <- matrix(nrow = 4, ncol = 4)
  x1 <- x[1, , drop = FALSE]
  x0 <- x[0, , drop = FALSE]

  expect_equal(recycle2(x1, x), list(x = x, y = x))
  expect_equal(recycle2(x0, x), list(x = x0, y = x0))
})

test_that("can recycle data frames", {
  x <- data.frame(a = rep(1, 3), b = rep(2, 3))
  x1 <- x[1, , drop = FALSE]
  x0 <- x[0, , drop = FALSE]

  expect_equivalent(recycle2(x1, x), list(x = x, y = x)) # ignore row names
  expect_equal(recycle2(x0, x), list(x = x0, y = x0))
})
