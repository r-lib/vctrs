context("test-shape-max")


# NULL --------------------------------------------------------------------

test_that("NULLs are ignored", {
  x <- vec_shape(1, 2, 3)

  expect_equal(max.vec_shape(x, NULL), x)
  expect_equal(max.vec_shape(NULL, x), x)
})

# Arrays ------------------------------------------------------------------

test_that("arrays expand to highest dimensionality", {
  x1 <- vec_shape(1)
  x2 <- vec_shape(1, 2)
  x3 <- vec_shape(1, 2, 3)

  expect_equal(max(x1, x2), vec_shape(1, 2))
  expect_equal(max(x2, x1), vec_shape(1, 2))

  expect_equal(max(x1, x3), vec_shape(1, 2, 3))
  expect_equal(max(x3, x1), vec_shape(1, 2, 3))

  expect_equal(max(x2, x3), vec_shape(1, 2, 3))
  expect_equal(max(x3, x2), vec_shape(1, 2, 3))
})

test_that("1-d slices recycled to longest", {
  expect_equal(max(vec_shape(1), vec_shape(10, 10)), vec_shape(10, 10))
  expect_equal(max(vec_shape(1, 10), vec_shape(10, 10)), vec_shape(10, 10))
})

# Data frame --------------------------------------------------------------

test_that("data frames can't expand to more than 2d", {
  expect_error(
    vec_shape2(data.frame(x = 1), ones(1, 2, 3)),
    "must be 2d"
  )
})

test_that("data frames can't recycle their columns", {
  expect_error(
    vec_shape2(data.frame(x = 1), ones(1, 2)),
    "Can't expand"
  )
  expect_error(
    vec_shape2(ones(1, 2), data.frame(x = 1)),
    "Can't expand"
  )
})
