context("test-cast")

# vec_cast ---------------------------------------------------------------

test_that("new classes are uncoercible by default", {
  x <- structure(1:10, class = "vctrs_nonexistant")
  expect_error(vec_cast(1, x), class = "vctrs_error_incompatible_cast")
  expect_error(vec_cast(x, 1), class = "vctrs_error_incompatible_cast")
})

test_that("casting requires vectors", {
  expect_error(vec_cast(NULL, quote(name)), class = "vctrs_error_scalar_type")
  expect_error(vec_cast(NA, quote(name)), class = "vctrs_error_scalar_type")
  expect_error(vec_cast(list(), quote(name)), class = "vctrs_error_scalar_type")
  expect_error(vec_cast(quote(name), NULL), class = "vctrs_error_scalar_type")
  expect_error(vec_cast(quote(name), NA), class = "vctrs_error_scalar_type")
  expect_error(vec_cast(quote(name), list()), class = "vctrs_error_scalar_type")
  expect_error(vec_cast(quote(name), quote(name)), class = "vctrs_error_scalar_type")
})

test_that("casting between `NULL` and partial types is allowed", {
  expect_identical(vec_cast(NULL, partial_factor()), NULL)
  expect_identical(vec_cast(partial_factor(), NULL), partial_factor())
})

test_that("dimensionality matches output" ,{
  x1 <- matrix(TRUE, nrow = 1, ncol = 1)
  x2 <- matrix(1, nrow = 0, ncol = 2)
  expect_dim(vec_cast(x1, x2), c(1, 2))
  expect_dim(vec_cast(TRUE, x2), c(1, 2))

  x <- matrix(1, nrow = 2, ncol = 2)
  expect_error(vec_cast(x, logical()), class = "vctrs_error_incompatible_cast")
})

test_that("empty input to vec_cast_common() returns list()", {
  expect_equal(vec_cast_common(), list())
  expect_equal(vec_cast_common(NULL, NULL), list(NULL, NULL))
})

test_that("identical structures can be cast to each other", {
  expect_identical(vec_cast(foobar("foo"), foobar("bar")), foobar("foo"))
  expect_identical(vec_coercible_cast(foobar("foo"), foobar("bar")), foobar("foo"))
})

test_that("inputs to vec_coercible_cast() are checked", {
  expect_error(vec_coercible_cast("", "", x_arg = 1), "must be a string")
  expect_error(vec_coercible_cast("", "", to_arg = chr()), "must be a string")
})

test_that("cast common preserves names", {
  expect_identical(vec_cast_common(foo = 1, bar = 2L), list(foo = 1, bar = 2))
})

test_that("cast errors create helpful messages (#57, #225)", {
  expect_known_output(file = test_path("test-cast-error-nested.txt"), {
    # Lossy cast
    try2(vec_cast("foo", 10))

    # Incompatible cast
    try2(vec_cast(factor("foo"), 10))


    ## Nested data frames

    # Lossy cast
    x <- tibble(a = tibble(b = "foo"))
    y <- tibble(a = tibble(b = 10))
    try2(vec_cast(x, y))

    # Incompatible cast
    x <- tibble(a = tibble(b = factor("foo")))
    try2(vec_cast(x, y))

    # Common cast error
    try2(vec_cast_common(x, y))
  })
})

# Conditions --------------------------------------------------------------

test_that("can suppress cast errors selectively", {
  f <- function() vec_cast(factor("a"), to = factor("b"))
  expect_error(regexp = NA, allow_lossy_cast(f()))
  expect_error(regexp = NA, allow_lossy_cast(f(), x_ptype = factor("a")))
  expect_error(regexp = NA, allow_lossy_cast(f(), to_ptype = factor("b")))
  expect_error(regexp = NA, allow_lossy_cast(f(), x_ptype = factor("a"), to_ptype = factor("b")))
  expect_error(allow_lossy_cast(f(), x_ptype = factor("c")), class = "vctrs_error_cast_lossy")
  expect_error(allow_lossy_cast(f(), x_ptype = factor("b"), to_ptype = factor("a")), class = "vctrs_error_cast_lossy")
  expect_error(allow_lossy_cast(f(), x_ptype = factor("a"), to_ptype = factor("c")), class = "vctrs_error_cast_lossy")
})

test_that("can signal deprecation warnings for lossy casts", {
  local_lifecycle_warnings()

  lossy_cast <- function() {
    maybe_lossy_cast(TRUE, factor("foo"), factor("bar"), lossy = TRUE, .deprecation = TRUE)
  }

  expect_true(expect_warning(lossy_cast(), "detected a lossy transformation"))
  expect_true(expect_warning(regexp = NA, allow_lossy_cast(lossy_cast())))
  expect_true(expect_warning(regexp = NA, allow_lossy_cast(lossy_cast(), factor("foo"), factor("bar"))))
  expect_true(expect_warning(allow_lossy_cast(lossy_cast(), factor("bar"), double())))
})
