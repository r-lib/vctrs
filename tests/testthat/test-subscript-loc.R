
test_that("vec_as_location2() returns a position", {
  expect_identical(vec_as_location2(2, 2L), 2L)
  expect_identical(vec_as_location2("foo", 2L, c("bar", "foo")), 2L)
})

test_that("vec_as_location2() requires integer or character inputs", {
  expect_error(vec_as_location2(TRUE, 10L), class = "vctrs_error_location2_bad_type")
  expect_error(vec_as_location2(mtcars, 10L), class = "vctrs_error_location2_bad_type")
  expect_error(vec_as_location2(env(), 10L), class = "vctrs_error_location2_bad_type")
  expect_error(vec_as_location2(foobar(), 10L), class = "vctrs_error_location2_bad_type")
  expect_error(vec_as_location2(2.5, 10L), class = "vctrs_error_location2_bad_type")

  verify_output(test_path("out", "error-position-type.txt"), {
    vec_as_location2(TRUE, 10L)
    vec_as_location2(mtcars, 10L)
    vec_as_location2(env(), 10L)
    vec_as_location2(foobar(), 10L)
    vec_as_location2(2.5, 3L)

    "# Custom `arg`"
    vec_as_location2(foobar(), 10L, arg = "foo")
    vec_as_location2(2.5, 3L, arg = "foo")
  })
})

test_that("vec_as_location() requires integer, character, or logical inputs", {
  expect_error(vec_as_location(mtcars, 10L), class = "vctrs_error_subscript_bad_type")
  expect_error(vec_as_location(env(), 10L), class = "vctrs_error_subscript_bad_type")
  expect_error(vec_as_location(foobar(), 10L), class = "vctrs_error_subscript_bad_type")
  expect_error(vec_as_location(2.5, 10L), class = "vctrs_error_subscript_bad_type")
  expect_error(vec_as_location(list(), 10L), class = "vctrs_error_subscript_bad_type")
  expect_error(vec_as_location(function() NULL, 10L), class = "vctrs_error_subscript_bad_type")

  verify_output(test_path("out", "error-index-type.txt"), {
    vec_as_location(mtcars, 10L)
    vec_as_location(env(), 10L)
    vec_as_location(foobar(), 10L)
    vec_as_location(2.5, 3L)
    vec_as_location(list(), 10L)
    vec_as_location(function() NULL, 10L)

    "# Custom `arg`"
    vec_as_location(env(), 10L, arg = "foo")
    vec_as_location(foobar(), 10L, arg = "foo")
    vec_as_location(2.5, 3L, arg = "foo")
  })
})

test_that("vec_as_location2() and vec_as_location() require integer- or character-like OO inputs", {
  expect_identical(vec_as_location2(factor("foo"), 2L, c("bar", "foo")), 2L)
  expect_identical(vec_as_location(factor("foo"), 2L, c("bar", "foo")), 2L)
  expect_error(vec_as_location2(foobar(1L), 10L), class = "vctrs_error_location2_bad_type")
  expect_error(vec_as_location(foobar(1L), 10L), class = "vctrs_error_subscript_bad_type")

  # Define subtype of logical and integer
  local_methods(
    vec_ptype2.vctrs_foobar = function(x, y, ...) UseMethod("vec_ptype2.vctrs_foobar", y),
    vec_ptype2.vctrs_foobar.default = function(x, y, ...) vec_default_ptype2(x, y, ...),
    vec_ptype2.vctrs_foobar.logical = function(x, y, ...) logical(),
    vec_ptype2.vctrs_foobar.integer = function(x, y, ...) integer(),
    vec_ptype2.logical.vctrs_foobar = function(x, y, ...) logical(),
    vec_ptype2.integer.vctrs_foobar = function(x, y, ...) integer(),
    vec_cast.vctrs_foobar = function(x, to, ...) UseMethod("vec_cast.vctrs_foobar"),
    vec_cast.vctrs_foobar.integer = function(x, to, ...) foobar(x),
    vec_cast.integer.vctrs_foobar = function(x, to, ...) vec_cast(unclass(x), int()),
    vec_cast.logical.vctrs_foobar = function(x, to, ...) vec_cast(unclass(x), lgl())
  )
  expect_error(vec_as_location2(foobar(TRUE), 10L), class = "vctrs_error_location2_bad_type")
  expect_identical(vec_as_location(foobar(TRUE), 10L), 1:10)
  expect_identical(vec_as_location(foobar(FALSE), 10L), int())
})

test_that("vec_as_location2() and vec_as_location() require existing elements", {
  expect_error(vec_as_location2(10L, 2L), class = "vctrs_error_subscript_oob_location")
  expect_error(vec_as_location2("foo", 1L, names = "bar"), class = "vctrs_error_subscript_oob_name")
  expect_error(vec_as_location(10L, 2L), class = "vctrs_error_subscript_oob_location")
  expect_error(vec_as_location("foo", 1L, names = "bar"), class = "vctrs_error_subscript_oob_name")
})

test_that("vec_as_location2() requires length 1 inputs", {
  expect_error(vec_as_location2(1:2, 2L), class = "vctrs_error_location2_bad_type")
  expect_error(vec_as_location2(c("foo", "bar"), 2L, c("foo", "bar")), class = "vctrs_error_location2_bad_type")

  verify_output(test_path("out", "error-position-size.txt"), {
    vec_as_location2(1:2, 2L)
    vec_as_location2(mtcars, 10L)

    "# Custom `arg`"
    vec_as_location2(1:2, 2L, arg = "foo")
    vec_as_location2(mtcars, 10L, arg = "foo")
    vec_as_location2(1:2, 2L, arg = "foo")
  })
})

test_that("vec_as_location2() requires positive integers", {
  expect_error(vec_as_location2(0, 2L), class = "vctrs_error_location2_bad_type")
  expect_error(vec_as_location2(-1, 2L), class = "vctrs_error_location2_bad_type")

  verify_output(test_path("out", "error-position-sign.txt"), {
    vec_as_location2(0, 2L)
    vec_as_location2(-1, 2L)

    "# Custom `arg`"
    vec_as_location2(0, 2L, arg = "foo")
  })
})

test_that("vec_as_location2() fails with NA", {
  expect_error(vec_as_location2(na_int, 2L), class = "vctrs_error_location2_bad_type")
  expect_error(vec_as_location2(na_chr, 1L, names = "foo"), class = "vctrs_error_location2_bad_type")

  verify_output(test_path("out", "error-position-na.txt"), {
    vec_as_location2(na_int, 2L)
    vec_as_location2(na_chr, 1L, names = "foo")

    "# Custom `arg`"
    vec_as_location2(na_int, 2L)
  })
})

test_that("vec_as_location2() doesn't allow lossy casts", {
  expect_error(vec_as_location2(2^31, 3L), class = "vctrs_error_location2_bad_type")

  # Lossy casts generate missing values, which are disallowed
  expect_error(allow_lossy_cast(vec_as_location2(2^31, 3L)), class = "vctrs_error_location2_bad_type")
})

test_that("all subscript errors inherit from `vctrs_error_subscript`", {
  expect_error(vec_as_location(100, 2L), class = "vctrs_error_subscript")
  expect_error(vec_as_location("foo", 2L, names = c("bar", "baz")), class = "vctrs_error_subscript")
  expect_error(vec_as_location(foobar(1L), 2L), class = "vctrs_error_subscript")
  expect_error(vec_as_location(1.5, 2L), class = "vctrs_error_subscript")
  expect_error(vec_as_location2(TRUE, 2L), class = "vctrs_error_subscript")
  expect_error(vec_as_location2(1.5, 2L), class = "vctrs_error_subscript")
})

test_that("all OOB errors inherit from `vctrs_error_subscript_oob`", {
  expect_error(vec_as_location(100, 2L), class = "vctrs_error_subscript_oob")
  expect_error(vec_as_location("foo", 2L, names = c("bar", "baz")), class = "vctrs_error_subscript_oob")
})

test_that("vec_as_location() preserves names if possible", {
  expect_identical(vec_as_location(c(a = 1L, b = 3L), 3L), c(a = 1L, b = 3L))
  expect_identical(vec_as_location(c(a = 1, b = 3), 3L), c(a = 1L, b = 3L))
  expect_identical(vec_as_location(c(a = "z", b = "y"), 26L, letters), c(a = 26L, b = 25L))

  expect_identical(vec_as_location(c(foo = TRUE, bar = FALSE, baz = TRUE), 3L), c(foo = 1L, baz = 3L))
  expect_identical(vec_as_location(c(foo = TRUE), 3L), c(foo = 1L, foo = 2L, foo = 3L))
  expect_identical(vec_as_location(c(foo = NA), 3L), c(foo = na_int, foo = na_int, foo = na_int))

  # Names of negative selections are dropped
  expect_identical(vec_as_location(c(a = -1L, b = -3L), 3L), 2L)
})

test_that("vec_as_location2() optionally allows missing values", {
  expect_identical(vec_as_location2(NA, 2L, missing = "ignore"), na_int)
  expect_error(vec_as_location2(NA, 2L, missing = "error"), class = "vctrs_error_location2_bad_type")
})

test_that("num_as_location2() optionally allows missing and negative locations", {
  expect_identical(num_as_location2(na_dbl, 2L, missing = "ignore"), na_int)
  expect_identical(num_as_location2(-1, 2L, negative = "ignore"), -1L)
  expect_error(num_as_location2(-3, 2L, negative = "ignore"), class = "vctrs_error_subscript_oob_location")
  expect_error(num_as_location2(0, 2L, negative = "ignore"), class = "vctrs_error_location2_bad_type")
})

test_that("num_as_location() optionally allows negative indices", {
  expect_identical(num_as_location(dbl(1, -1), 2L, negative = "ignore"), int(1L, -1L))
  expect_error(num_as_location(c(1, -10), 2L, negative = "ignore"), class = "vctrs_error_subscript_oob_location")
})

test_that("num_as_location() optionally forbids negative indices", {
  expect_error(num_as_location(dbl(1, -1), 2L, negative = "error"), class = "vctrs_error_location_bad_type")
  expect_error(num_as_location(c(1, -10), 2L, negative = "error"), class = "vctrs_error_location_bad_type")
})

test_that("conversion to locations has informative error messages", {
  verify_output(test_path("error", "test-subscript-loc.txt"), {
    "Negative forbidden"
    num_as_location(dbl(1, -1), 2L, negative = "error")

    "Logical size mismatch"
    vec_as_location(c(TRUE, FALSE), 3)
  })
})
