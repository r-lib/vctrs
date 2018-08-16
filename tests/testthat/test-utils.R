context("test-utils")

test_that("can get and set object bit", {
  expect_equal(obj_get(1), FALSE)
  expect_equal(obj_get(mtcars), TRUE)

  x <- factor("x")
  expect_equal(obj_get(x), TRUE)
  expect_equal(obj_set(x, FALSE), TRUE)
  expect_equal(obj_get(x), FALSE)
})

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
