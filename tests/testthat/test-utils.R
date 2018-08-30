context("test-utils")

# outer_names --------------------------------------------------------------

test_that("names preserved if outer name is missing", {
  x <- c("a", "z", "")

  expect_equal(outer_names(NULL, x, 3), x)
  expect_equal(outer_names("", x, 3), x)
  expect_equal(outer_names(NA, x, 3), x)
})

test_that("outer name vectorised if needed", {
  expect_equal(outer_names("x", NULL, 1L), c("x"))
  expect_equal(outer_names("x", NULL, 2L), c("x1", "x2"))
})

test_that("outer and inner names are combined", {
  expect_equal(outer_names("y", "x", 1), c("y..x"))
})
