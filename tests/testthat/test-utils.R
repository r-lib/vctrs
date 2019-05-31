context("test-utils")

# outer_names --------------------------------------------------------------

test_that("names preserved if outer name is missing", {
  x <- c("a", "z", "")

  expect_equal(outer_names(x, NULL, 3), x)
  expect_equal(outer_names(x, "", 3), x)
  expect_equal(outer_names(x, na_chr, 3), x)
})

test_that("outer name vectorised if needed", {
  expect_equal(outer_names(NULL, "x", 1L), c("x"))
  expect_equal(outer_names(NULL, "x", 2L), c("x1", "x2"))
})

test_that("outer and inner names are combined", {
  expect_equal(outer_names("x", "y", 1), c("y..x"))
})
