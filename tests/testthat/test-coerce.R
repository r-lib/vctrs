context("test-coerce")


# vec_coerce --------------------------------------------------------------

test_that("empty input returns list()", {
  expect_equal(vec_coerce(), list())
})

test_that("output has consistent type", {
  expect_equal(vec_coerce(TRUE, 1), list_of(1, 1))
})

# vec_c -------------------------------------------------------------------

test_that("zero length input returns NULL", {
  expect_equal(vec_c(), NULL)
  expect_equal(vec_c(NULL), NULL)
})

test_that("NULL is idempotent", {
  expect_equal(vec_c(NULL, 1:10), 1:10)
  expect_equal(vec_c(1:10, NULL), 1:10)
})

test_that("all inputs must be 1d", {
  expect_error(vec_c(mtcars))
})

test_that("different types are coerced to common", {
  expect_equal(vec_c(TRUE, 1L, 1), c(1, 1, 1))
  expect_equal(vec_c(TRUE, 2:4), 1:4)
})

test_that("specified .types allows more casts", {
  expect_equal(vec_c(TRUE, .type = character()), "TRUE")
})
