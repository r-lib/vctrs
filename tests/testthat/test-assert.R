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

test_that("can override `vec_is_vector()` for S3 lists", {
  scoped_bindings(.env = global_env(),
    vec_proxy.vctrs_foobar = function(x) unclass(x)
  )
  expect_true(vec_is_vector(foobar()))
})

test_that("data frames and records are vectors", {
  expect_true(vec_is_vector(mtcars))
  expect_true(vec_is_vector(new_rcrd(list(x = 1, y = 2))))
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

  expect_false(vec_is(quote(foo)))
  expect_false(vec_is(pairlist("")))
  expect_false(vec_is(function() NULL))
  expect_false(vec_is(env()))
  expect_false(vec_is(~foo))
  expect_false(vec_is(base::`{`))
  expect_false(vec_is(base::c))
  expect_false(vec_is(expression()))

  expect_error(vec_assert(quote(foo)), "must be a vector")
  expect_error(vec_assert(pairlist("")), "must be a vector")
  expect_error(vec_assert(function() NULL), "must be a vector")
  expect_error(vec_assert(env()), "must be a vector")
  expect_error(vec_assert(~foo), "must be a vector")
  expect_error(vec_assert(base::`{`), "must be a vector")
  expect_error(vec_assert(base::c), "must be a vector")
  expect_error(vec_assert(expression()), "must be a vector")
})

test_that("vec_assert() uses friendly type in error messages", {
   # Friendly type will be generated in rlang in the future. Upstream
   # changes should not cause CRAN failures.
  skip_on_cran()
  expect_error(vec_assert(function() NULL), "must be a vector, not a function")
})

test_that("vec_typeof() handles all types", {
  for (i in seq_along(empty_types)) {
    expect_identical(vec_typeof(!!empty_types[[i]]), !!names(empty_types)[[i]])
  }
})

test_that("bare prototypes act as partial types", {
  scoped_bindings(.env = global_env(),
    vec_slice.vctrs_foobar = function(x, i) foobar(x[i])
  )
  expect_true(vec_is(foobar(1), dbl()))
  expect_error(NA, object = vec_assert(foobar(1), dbl()))
})

test_that("data frames are always classified as such even when dispatch is off", {
  expect_identical(vec_typeof_bare(mtcars), "dataframe")
})

test_that("assertion is not applied on proxy", {
  scoped_global_bindings(
    vec_proxy.vctrs_foobar = unclass,
    vec_restore.vctrs_foobar = function(x, ...) foobar(x),
    `[.vctrs_foobar` = function(x, i) vec_slice_native(x, i)
  )
  x <- foobar(list())

  expect_true(vec_is(x, x))
  expect_false(vec_is(x, list()))

  expect_error(vec_assert(x, list()), class = "vctrs_error_assert_ptype")
  expect_error(vec_assert(x, x), regexp = NA)
})

test_that("attributes of unclassed vectors are asserted", {
  x <- structure(FALSE, foo = "bar")
  y <- structure(TRUE, foo = "bar")
  expect_false(vec_is(x, FALSE))
  expect_false(vec_is(FALSE, x))
  expect_true(vec_is(y, x))
  expect_true(vec_is(x, y))
})

test_that("unspecified is finalised before assertion", {
  expect_true(vec_is(NA, TRUE))
  expect_true(vec_is(tibble(x = NA), tibble(x = lgl())))

  expect_error(regexp = NA, vec_assert(NA, TRUE))
  expect_error(regexp = NA, vec_assert(tibble(x = NA), tibble(x = lgl())))
})
