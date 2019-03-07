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

test_that("bare atomic vectors are vectors but not recursive", {
  expect_true(vec_is_vector(TRUE))
  expect_true(vec_is_vector(1L))
  expect_true(vec_is_vector(1))
  expect_true(vec_is_vector(1i))
  expect_true(vec_is_vector("foo"))
  expect_true(vec_is_vector(as.raw(1)))
})

test_that("S3 atomic vectors are vectors", {
  vec_is_vector.foobar <- function(...) stop("should not be called")
  expect_true(vec_is_vector(foobar(TRUE)))
  expect_true(vec_is_vector(foobar(1L)))
  expect_true(vec_is_vector(foobar(1)))
  expect_true(vec_is_vector(foobar(1i)))
  expect_true(vec_is_vector(foobar("foo")))
  expect_true(vec_is_vector(foobar(as.raw(1))))
})

test_that("bare lists are recursive", {
  expect_true(vec_is_vector(list()))
})

test_that("S3 lists are not vectors by default", {
  expect_false(vec_is_vector(foobar()))
})

test_that("data frames are recursive vectors", {
  vec_is_vector.data.frame <- function(...) stop("should not be called")
  expect_true(vec_is_vector(mtcars))
})

test_that("non-vector base types are scalars", {
  expect_identical(vec_typeof(quote(foo)), "scalar")
  expect_identical(vec_typeof(pairlist("")), "scalar")
  expect_identical(vec_typeof(function() NULL), "scalar")
  expect_identical(vec_typeof(env()), "scalar")
  expect_identical(vec_typeof(~foo), "scalar")
  expect_identical(vec_typeof(base::`{`), "scalar")
  expect_identical(vec_typeof(base::c), "scalar")
  expect_identical(vec_typeof(expression()), "scalar")

  expect_false(vec_is_vector(quote(foo)))
  expect_false(vec_is_vector(pairlist("")))
  expect_false(vec_is_vector(function() NULL))
  expect_false(vec_is_vector(env()))
  expect_false(vec_is_vector(~foo))
  expect_false(vec_is_vector(base::`{`))
  expect_false(vec_is_vector(base::c))
  expect_false(vec_is_vector(expression()))
})
