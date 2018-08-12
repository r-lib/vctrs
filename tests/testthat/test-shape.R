context("test-shape")

test_that("can't coerce non-vectors", {
  expect_error(as_vec_shape(environment()), "vector")
})

test_that("has useful print method", {
  expect_known_output(
    file = test_path("test-shape-print.txt"),
    {
      print(vec_shape(10))
      print(vec_shape(1, 2, 3, 4, 5))
    }
  )
})

# vec_shape ---------------------------------------------------------------

test_that("NULLs are idempotent", {
  x <- 1:10

  expect_equal(vec_shape(NULL), NULL)
  expect_equal(vec_shape(NULL, x), vec_shape(x))
  expect_equal(vec_shape(x, NULL), vec_shape(x))
})

test_that("arrays expand to highest dimensionality", {
  x1 <- ones(1)
  x2 <- ones(1, 2)
  x3 <- ones(1, 2, 3)

  expect_equal(vec_shape(x1, x2), vec_shape(x2))
  expect_equal(vec_shape(x2, x1), vec_shape(x2))

  expect_equal(vec_shape(x1, x3), vec_shape(x3))
  expect_equal(vec_shape(x3, x1), vec_shape(x3))

  expect_equal(vec_shape(x2, x3), vec_shape(x3))
  expect_equal(vec_shape(x3, x2), vec_shape(x3))
})

test_that("1-d slices recycled to longest", {
  expect_equal(vec_shape(1, 1:10), vec_shape(1:10))
  expect_equal(vec_shape(ones(1, 10), ones(10, 10)), vec_shape(ones(10, 10)))
  expect_equal(vec_shape(ones(10, 1), ones(10, 10)), vec_shape(ones(10, 10)))
})

test_that("data frames can't expand to more than 2d", {
  expect_error(
    vec_shape(data.frame(x = 1), ones(1, 2, 3)),
    "must be 2d"
  )
})

test_that("data frames can't recycle their columns", {
  expect_error(
    vec_shape(data.frame(x = 1), ones(1, 2)),
    "Can't expand"
  )
  expect_error(
    vec_shape(ones(1, 2), data.frame(x = 1)),
    "Can't expand"
  )
})

# vec_reshape -------------------------------------------------------------

test_that("NULLs are idempotent", {
  expect_equal(vec_reshape(NULL, c(1, 1)), NULL)
  expect_equal(vec_reshape(ones(1, 2), NULL), ones(1, 2))
})

test_that("can recyle vector to vector or array", {
  expect_equal(vec_reshape(1, 0), rep(1, 0))
  expect_equal(vec_reshape(1, 5), rep(1, 5))
  expect_equal(vec_reshape(1, c(5, 5)), array(1, c(5, 5)))

  expect_equal(vec_reshape(1:5, 0), rep(1, 0))
  expect_equal(vec_reshape(1:5, 5), 1:5)
  expect_equal(vec_reshape(1:5, c(5, 5)), array(1:5, c(5, 5)))
})

test_that("vector must have correct length", {
  expect_error(
    vec_reshape(1:10, 2),
    "Can't recycle"
  )
  expect_error(
    vec_reshape(1:10, 1),
    "Can't recycle"
  )
})

test_that("can recycle rows of data frame", {
  df <- data_frame(x = 1)

  expect_equal(
    vec_reshape(df, c(3, 1)),
    data_frame(x = c(1, 1, 1))
  )
})

test_that("recylcling data frame is contrained", {
  df <- data.frame(x = 1, y = 2)

  expect_error(
    vec_reshape(df, 1),
    "data frame"
  )
  expect_error(
    vec_reshape(df, c(1, 3)),
    "data frame"
  )
  expect_error(
    vec_reshape(df, c(1, 3, 1)),
    "data frame"
  )
})

test_that("low-d arrays are broadcast to higher-d", {
  x_col <- matrix(1:2, ncol = 1)
  x_row <- matrix(1:2, ncol = 2)

  expect_equal(vec_reshape(x_col, c(2, 2)), matrix(c(1, 2, 1, 2), nrow = 2))
  expect_equal(vec_reshape(x_row, c(2, 2)), matrix(c(1, 1, 2, 2), nrow = 2))
})

test_that("can not reduce dimensionality", {
  expect_error(
    vec_reshape(ones(2, 2), 1),
    "increase"
  )
})
