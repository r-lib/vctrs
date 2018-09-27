context("test-shape")

int <- function(...) {
  array(NA_integer_, c(...))
}

# common shape ------------------------------------------------------------

test_that("length is ignored", {
  expect_equal(shape_common(int(5), int(10)), integer())
  expect_equal(shape_common(int(5, 1), int(10, 1)), 1L)
  expect_equal(shape_common(int(5, 1, 2), int(10, 1, 2)), c(1L, 2L))
})

test_that("recycling rules applied", {
  expect_equal(shape_common(int(1), int(1, 5, 5)), c(5L, 5L))
  expect_equal(shape_common(int(1, 1), int(1, 5, 5)), c(5L, 5L))
  expect_equal(shape_common(int(1, 1, 1), int(1, 5, 5)), c(5L, 5L))

  expect_equal(shape_common(int(1, 1, 5), int(1, 5, 1)), c(5L, 5L))
  expect_equal(shape_common(int(1, 5, 1), int(1, 1, 5)), c(5L, 5L))
  expect_equal(shape_common(int(1, 1, 1), int(1, 5, 5)), c(5L, 5L))

  expect_equal(shape_common(int(1, 0, 5), int(1, 5, 1)), c(0L, 5L))
  expect_equal(shape_common(int(1, 5, 0), int(1, 1, 5)), c(5L, 0L))
})

# broadcasting -------------------------------------------------------------

test_that("can broadcast to higher dimension, but not lower", {
  expect_equal(
    shape_broadcast(1, int(0, 4)),
    array(1, c(1, 4))
  )
  expect_error(
    shape_broadcast(int(1, 1, 1), int(4, 4)),
    class = "error_incompatible_cast"
  )
})

test_that("recyling rules applied", {
  expect_equal(
    shape_broadcast(array(1:4, c(1, 1, 4)), int(0, 4, 4))[1, , ],
    matrix(1:4, 4, 4, byrow = TRUE)
  )

  expect_equal(
    shape_broadcast(array(1:4, c(1, 4, 1)), int(0, 4, 4))[1, , ],
    matrix(1:4, 4, 4)
  )
})
