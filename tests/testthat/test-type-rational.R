context("type-rational.R")

# These tests check the rational type from the S3 vignette

test_that("equality proxy is taken (#375)", {
  scoped_rational_class()
  x <- rational(c(1, 2, 1, 2, 6), c(1, 1, 2, 2, 2))

  expect_identical(x == rational(3, 1), c(FALSE, FALSE, FALSE, FALSE, TRUE))

  expect_identical(unique(x), rational(c(1, 2, 1, 6), c(1, 1, 2, 2)))
})

test_that("compare proxy is taken", {
  scoped_rational_class()
  x <- rational(c(1, 2, 1, 2, 6), c(1, 1, 2, 2, 2))
  expect_identical(sort(x), rational(c(1, 1, 2, 2, 6), c(2, 1, 2, 1, 2)))
})

test_that("can find common type and cast to rational", {
  scoped_rational_class()
  x <- rational(1:2, 2:1)
  expect_identical(vec_cast_common(x, x), list(x, x))
})
