context("test-assert")

test_that("basic assert is idempotent", {
  x <- new_vctr(1:4)
  expect_identical(vec_assert(x), x)

  expect_identical(vec_assert(new_vctr(1:4)), new_vctr(1:4))
  expect_false(withVisible(vec_assert(new_vctr(1:4)))$visible)

  expect_identical(vec_assert(1:4), 1:4)
})

test_that("asserting ptype", {
  expect_error(vec_assert(new_vctr(1:4), new_vctr(integer())), NA)

  # Is this the correct error message?
  expect_error(
    vec_assert(new_vctr(1:4), new_vctr(double())),
    class = "vctrs_error_assert_ptype"
  )
})

test_that("asserting size", {
  expect_error(vec_assert(new_vctr(1:4), size = 4), NA)

  expect_error(
    vec_assert(new_vctr(1:4), size = 5),
    class = "vctrs_error_assert_size"
  )
})

test_that("vec_assert() labels input", {
  expect_error(
    vec_assert(new_vctr(1:4), size = 5),
    regexp = "new_vctr\\(1:4\\)` must have"
  )
})
