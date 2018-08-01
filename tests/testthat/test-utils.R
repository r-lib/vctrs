context("test-utils")

# outer_names --------------------------------------------------------------

test_that("names preserved if outer name is missing", {
  x <- c(a = 1, z = 1, 2)

  expect_equal(outer_names(x, NULL), names(x))
  expect_equal(outer_names(x, ""), names(x))
  expect_equal(outer_names(x, NA), names(x))
})

test_that("outer name vectorised if needed", {
  expect_equal(outer_names(1L, "x"), c("x"))
  expect_equal(outer_names(1:2, "x"), c("x1", "x2"))
})

test_that("outer and inner names are combined", {
  expect_equal(outer_names(c(x = 1), "y"), c("y.x"))
})
