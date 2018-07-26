context("test-recycle")

# Vectors -----------------------------------------------------------------

test_that("NULL is idempotent", {
  expect_equal(recycle(NULL, NULL), list(NULL, NULL))
  expect_equal(recycle(1:5, NULL), list(1:5, NULL))
  expect_equal(recycle(NULL, 1:5), list(NULL, 1:5))
})

test_that("equal lengths returned as is", {
  x <- 1:3

  expect_equal(recycle(x, x), list(x, x))
  expect_equal(recycle(x[1], x[1]), list(x[1], x[1]))
  expect_equal(recycle(x[0], x[0]), list(x[0], x[0]))
})

test_that("length 1 recycled to length of longest", {
  x1 <- 1
  x3 <- rep(1, 3)

  expect_equal(recycle(x1, x3), list(x3, x3))
  expect_equal(recycle(x3, x1), list(x3, x3))
})

test_that("length 0 causes both outputs to be zero", {
  x <- 1:3

  expect_equal(recycle(x, x[0]), list(x[0], x[0]))
  expect_equal(recycle(x[0], x), list(x[0], x[0]))
})

test_that("incompatible lengths get error messages", {
  expect_error(recycle(1:2, 1:3), "Incompatible")
  expect_error(recycle(1:3, 1:2), "Incompatible")
})

# Matrices ----------------------------------------------------------------

test_that("can recycle matrices", {
  x <- matrix(nrow = 4, ncol = 4)
  x1 <- x[1, , drop = FALSE]
  x0 <- x[0, , drop = FALSE]

  expect_equal(recycle(x, x), list(x, x))
  expect_equal(recycle(x1, x), list(x, x))
  expect_equal(recycle(x0, x), list(x0, x0))
})

test_that("can recycle data frames", {
  x <- data.frame(a = rep(1, 3), b = rep(2, 3))
  x1 <- vec_subset(x, 1L)
  x0 <- vec_subset(x, 0L)

  expect_equal(recycle(x, x), list(x, x))
  expect_equal(recycle(x1, x), list(x, x))
  expect_equal(recycle(x0, x), list(x0, x0))
})

test_that("can recycle matrix and data frame", {
  mt <- matrix(nrow = 2, ncol = 2)
  df <- data.frame(x = c(1, 1), y = c(2, 2))

  expect_equal(
    recycle(vec_subset(mt, 0L), df),
    list(vec_subset(mt, 0L), vec_subset(df, 0L))
  )
  expect_equal(
    recycle(vec_subset(mt, 1L), df),
    list(mt, df)
  )

  expect_equal(
    recycle(mt, vec_subset(df, 0L)),
    list(vec_subset(mt, 0L), vec_subset(df, 0L))
  )

  expect_equal(
    recycle(mt, vec_subset(df, 1L)),
    list(mt, df)
  )
})
