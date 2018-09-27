context("test-c")

test_that("zero length input returns NULL", {
  expect_equal(vec_c(), NULL)
  expect_equal(vec_c(NULL), NULL)
})

test_that("NULL is idempotent", {
  expect_equal(vec_c(NULL, 1:10), 1:10)
  expect_equal(vec_c(1:10, NULL), 1:10)
})

test_that("different types are coerced to common", {
  expect_equal(vec_c(TRUE, 1L, 1), c(1, 1, 1))
  expect_equal(vec_c(TRUE, 2:4), 1:4)
})

test_that("specified .ptypes allows more casts", {
  expect_equal(vec_c(TRUE, .ptype = character()), "TRUE")
})

test_that("combines outer an inner names", {
  expect_equal(vec_c(x = 1), c(x = 1))
  expect_equal(vec_c(c(x = 1)), c(x = 1))

  expect_equal(vec_c(c(x = 1:2)), c(x1 = 1, x2 = 2))
  expect_equal(vec_c(y = c(x = 1)), c(y..x = 1))
})
