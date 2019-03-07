context("test-assert")

test_that("basic assert is idempotent", {
  x <- new_vctr(1:4)

  expect_true(vec_is(x))
  expect_identical(vec_assert(x), x)

  expect_identical(vec_assert(x), new_vctr(1:4))
  expect_false(withVisible(vec_assert(x))$visible)

  expect_true(vec_is(1:4))
  expect_identical(vec_assert(1:4), 1:4)
})

test_that("asserting ptype", {
  x <- new_vctr(1:4)
  good <- new_vctr(integer())

  expect_true(vec_is(x, good))
  expect_error(vec_assert(x, good), NA)

  # Is this the correct error message?
  bad <- new_vctr(double())
  expect_false(vec_is(x, bad))
  expect_error(vec_assert(x, bad), class = "vctrs_error_assert_ptype")
})

test_that("asserting size", {
  x <- new_vctr(1:4)

  expect_true(vec_is(x, size = 4))
  expect_error(vec_assert(x, size = 4), NA)

  expect_false(vec_is(x, size = 5))
  expect_error(vec_assert(x, size = 5), class = "vctrs_error_assert_size")
})

test_that("vec_assert() labels input", {
  expect_error(
    vec_assert(new_vctr(1:4), size = 5),
    regexp = "`new_vctr\\(1:4\\)` must have",
    class = "vctrs_error_assert_size"
  )
  expect_error(
    vec_assert(new_vctr(1:4), size = 5, arg = "foobar"),
    regexp = "`foobar` must have",
    class = "vctrs_error_assert_size"
  )
})
