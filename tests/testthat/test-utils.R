context("test-utils")

# name_outer --------------------------------------------------------------

test_that("names preserved if outer name is missing", {
  x <- c(a = 1, z = 1, 2)

  expect_equal(name_outer(x, ""), x)
  expect_equal(name_outer(x, NA), x)
})

test_that("outer name vectorised if needed", {
  expect_equal(name_outer(1L, "x"), c(x = 1L))
  expect_equal(name_outer(1:2, "x"), c(x1 = 1L, x2 = 2L))
})

test_that("outer and inner names are combined", {
  expect_equal(name_outer(c(x = 1), "y"), c(y.x = 1))
})
