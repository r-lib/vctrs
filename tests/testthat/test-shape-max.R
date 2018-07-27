context("test-shape-max")


# Data frame --------------------------------------------------------------

test_that("data frames can't expand to more than 2d", {
  expect_error(
    vecshape_max(data.frame(x = 1), ones(1, 2, 3)),
    "must be 2d"
  )
})

test_that("data frames can't recycle their columns", {
  expect_error(
    vecshape_max(data.frame(x = 1), ones(1, 2)),
    "Can't expand"
  )
  expect_error(
    vecshape_max(ones(1, 2), data.frame(x = 1)),
    "Can't expand"
  )
})
