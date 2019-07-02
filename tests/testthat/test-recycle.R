context("test-recycle")

# vec_recycle -------------------------------------------------------------

test_that("vec_recycle recycles size 1 to any other size", {
  x <- 1
  x0 <- numeric()
  x2 <- c(x, x)

  expect_equal(vec_recycle(x, 1), x)
  expect_equal(vec_recycle(x, 0), x0)
  expect_equal(vec_recycle(x, 2), x2)
})

test_that("incompatible lengths get error messages", {
  x2 <- c(1, 2)

  expect_error(vec_recycle(x2, 1), "2, 1")
  expect_error(vec_recycle(x2, 0), "2, 0")
  expect_error(vec_recycle(x2, 3), "2, 3")
})

test_that("can recycle arrays", {
  x <- matrix(1:2, 1)
  x2 <- matrix(1:2, 2, 2, byrow = TRUE)
  x0 <- matrix(integer(), 0, 2)

  expect_equal(vec_recycle(x, 1), x)
  expect_equal(vec_recycle(x, 0), x0)
  expect_equal(vec_recycle(x, 2), x2)

  # List arrays
  data <- c(list(1), list(2))
  x <- matrix(data, 1)
  x2 <- matrix(data, 2, 2, byrow = TRUE)
  x0 <- matrix(list(), 0, 2)

  expect_equal(vec_recycle(x, 1), x)
  expect_equal(vec_recycle(x, 0), x0)
  expect_equal(vec_recycle(x, 2), x2)
})

# Empty -------------------------------------------------------------------

test_that("empty input returns empty list", {
  expect_equal(vec_recycle_common(), list())
})

# Vectors -----------------------------------------------------------------

test_that("NULL is idempotent", {
  expect_equal(vec_recycle_common(NULL, NULL), list(NULL, NULL))
  expect_equal(vec_recycle_common(1:5, NULL), list(1:5, NULL))
  expect_equal(vec_recycle_common(NULL, 1:5), list(NULL, 1:5))
})

test_that("equal lengths returned as is", {
  x <- 1:3

  expect_equal(vec_recycle_common(x, x), list(x, x))
  expect_equal(vec_recycle_common(x[1], x[1]), list(x[1], x[1]))
  expect_equal(vec_recycle_common(x[0], x[0]), list(x[0], x[0]))
})

test_that("vec_recycle_common recycles size 1 to any other size", {
  x1 <- 1
  x3 <- rep(1, 3)
  x0 <- numeric()

  expect_equal(vec_recycle_common(x1, x3), list(x3, x3))
  expect_equal(vec_recycle_common(x3, x1), list(x3, x3))
  expect_equal(vec_recycle_common(x1, x0), list(x0, x0))
})

test_that("incompatible lengths get error messages", {
  expect_error(vec_recycle_common(1:2, 1:3), class = "vctrs_error_incompatible_size")
  expect_error(vec_recycle_common(1:3, 1:2), class = "vctrs_error_incompatible_size")
  expect_error(vec_recycle_common(numeric(), 1:2), class = "vctrs_error_incompatible_size")
})

# Matrices ----------------------------------------------------------------

test_that("can vec_recycle_common matrices", {
  x <- matrix(nrow = 4, ncol = 4)
  x1 <- x[1, , drop = FALSE]

  expect_equal(vec_recycle_common(x, x), list(x, x))
  expect_equal(vec_recycle_common(x1, x), list(x, x))
})

test_that("recycling matrices respects incompatible sizes", {
  x <- matrix(nrow = 4, ncol = 4)
  x2 <- x[1:2, , drop = FALSE]
  x0 <- x[0, , drop = FALSE]

  expect_error(vec_recycle_common(x2, x), class = "vctrs_error_incompatible_size")
  expect_error(vec_recycle_common(x0, x), class = "vctrs_error_incompatible_size")
})

test_that("can vec_recycle_common data frames", {
  x <- data.frame(a = rep(1, 3), b = rep(2, 3))
  x1 <- vec_slice(x, 1L)

  expect_equal(vec_recycle_common(x, x), list(x, x))
  expect_equal(vec_recycle_common(x1, x), list(x, x))
})

test_that("recycling data frames respects incompatible sizes", {
  x <- data.frame(a = rep(1, 3), b = rep(2, 3))
  x2 <- vec_slice(x, 1:2)
  x0 <- vec_slice(x, integer())

  expect_error(vec_recycle_common(x2, x), class = "vctrs_error_incompatible_size")
  expect_error(vec_recycle_common(x0, x), class = "vctrs_error_incompatible_size")
})

test_that("can vec_recycle_common matrix and data frame", {
  mt <- matrix(nrow = 2, ncol = 2)
  df <- data.frame(x = c(1, 1), y = c(2, 2))

  expect_equal(
    vec_recycle_common(vec_slice(mt, 1L), df),
    list(mt, df)
  )

  expect_equal(
    vec_recycle_common(mt, vec_slice(df, 1L)),
    list(mt, df)
  )
})

test_that("recycling data frames with matrices respects incompatible sizes", {
  mt <- matrix(nrow = 2, ncol = 2)
  df <- data.frame(x = c(1, 1), y = c(2, 2))

  expect_error(
    vec_recycle_common(vec_slice(mt, integer()), df),
    class = "vctrs_error_incompatible_size"
  )

  expect_error(
    vec_recycle_common(mt, vec_slice(df, 0L)),
    class = "vctrs_error_incompatible_size"
  )
})
