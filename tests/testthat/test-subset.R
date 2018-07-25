context("test-subset")

test_that("can subset object of an dimensionality", {
  ones <- function(...) {
    array(1, dim = c(...))
  }

  x0 <- c(1, 1)
  x1 <- ones(2)
  x2 <- ones(2, 3)
  x3 <- ones(2, 3, 4)
  x4 <- ones(2, 3, 4, 5)

  expect_equal(vec_subset(x0, 1L), 1)
  expect_equal(vec_subset(x1, 1L), ones(1))
  expect_equal(vec_subset(x2, 1L), ones(1, 3))
  expect_equal(vec_subset(x3, 1L), ones(1, 3, 4))
  expect_equal(vec_subset(x4, 1L), ones(1, 3, 4, 5))
})
