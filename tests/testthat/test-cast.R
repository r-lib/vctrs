context("test-cast")

# NULL --------------------------------------------------------------------

test_that("NULL is idempotent", {
  expect_equal(vec_cast(NULL, NULL), NULL)
  expect_equal(vec_cast(list(1:3), NULL), list(1:3))
})

# Default --------------------------------------------------------------------

test_that("new classes are uncoercible by default", {
  x <- structure(1:10, class = "vctrs_nonexistant")
  expect_error(vec_cast(1, x), class = "vctrs_error_incompatible_cast")
  expect_error(vec_cast(x, 1), class = "vctrs_error_incompatible_cast")
})

# Logical -----------------------------------------------------------------

test_that("safe casts work as expected", {
  exp <- lgl(TRUE, FALSE)
  expect_equal(vec_cast(NULL, logical()), NULL)
  expect_equal(vec_cast(lgl(TRUE, FALSE), logical()), exp)
  expect_equal(vec_cast(int(1L, 0L), logical()), exp)
  expect_equal(vec_cast(dbl(1, 0), logical()), exp)
  expect_equal(vec_cast(chr("T", "F"), logical()), exp)
  expect_equal(vec_cast(chr("TRUE", "FALSE"), logical()), exp)
  expect_equal(vec_cast(chr("true", "false"), logical()), exp)
  expect_equal(vec_cast(list(1, 0), logical()), exp)
})

test_that("lossy casts generate warning", {
  expect_condition(vec_cast(c(2L, 1L), logical()), class = "warning_lossy_cast")
  expect_condition(vec_cast(c(2, 1), logical()), class = "warning_lossy_cast")
  expect_condition(vec_cast(c("x", "TRUE"), logical()), class = "warning_lossy_cast")
  expect_condition(vec_cast(list(c(TRUE, FALSE), TRUE), logical()), class = "warning_lossy_cast")
  expect_condition(vec_cast(c("t", "T"), logical()), class = "warning_lossy_cast")
  expect_condition(vec_cast(c("f", "F"), logical()), class = "warning_lossy_cast")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), logical()), class = "vctrs_error_incompatible_cast")
})

test_that("dimensionality matches output" ,{
  x1 <- matrix(TRUE, nrow = 1, ncol = 1)
  x2 <- matrix(1, nrow = 0, ncol = 2)
  expect_dim(vec_cast(x1, x2), c(1, 2))
  expect_dim(vec_cast(TRUE, x2), c(1, 2))

  x <- matrix(1, nrow = 2, ncol = 2)
  expect_error(vec_cast(x, logical()), class = "vctrs_error_incompatible_cast")
})

# Integer -----------------------------------------------------------------

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, integer()), NULL)
  expect_equal(vec_cast(lgl(TRUE, FALSE), integer()), int(1L, 0L))
  expect_equal(vec_cast(int(1L, 2L), integer()), int(1L, 2L))
  expect_equal(vec_cast(dbl(1, 2), integer()), int(1L, 2L))
  expect_equal(vec_cast(chr("1", "2"), integer()), int(1L, 2L))
  expect_equal(vec_cast(list(1L, 2L), integer()), int(1L, 2L))
})

test_that("lossy casts generate warning", {
  expect_condition(vec_cast(c(2.5, 2), integer()), class = "warning_lossy_cast")
  expect_condition(vec_cast(c("2.5", "2"), integer()), class = "warning_lossy_cast")

  out <- expect_lossy_cast(vec_cast(c(.Machine$integer.max + 1, 1), int()))
  expect_identical(out, int(NA, 1L))

  out <- expect_lossy_cast(vec_cast(c(-.Machine$integer.max - 1, 1), int()))
  expect_identical(out, int(NA, 1L))
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), integer()), class = "vctrs_error_incompatible_cast")
})

# Double ------------------------------------------------------------------

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, double()), NULL)
  expect_equal(vec_cast(lgl(TRUE, FALSE), double()), dbl(1, 0))
  expect_equal(vec_cast(int(1, 0), double()), dbl(1, 0))
  expect_equal(vec_cast(dbl(1, 1.5), double()), dbl(1, 1.5))
  expect_equal(vec_cast(chr("1", "1.5"), double()), dbl(1, 1.5))
  expect_equal(vec_cast(list(1, 1.5), double()), dbl(1, 1.5))
})

test_that("lossy casts generate warning", {
  expect_condition(vec_cast(c("2.5", "x"), double()), class = "warning_lossy_cast")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), double()), class = "vctrs_error_incompatible_cast")
})

# Character ---------------------------------------------------------------

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, character()), NULL)
  expect_equal(vec_cast(NA, character()), NA_character_)
  expect_equal(vec_cast(lgl(TRUE, FALSE), character()), chr("TRUE", "FALSE"))
  expect_equal(vec_cast(list("x", "y"), character()), chr("x", "y"))
})

test_that("difftime gets special treatment", {
  dt1 <- as.difftime(600, units = "secs")

  expect_equal(vec_cast(dt1, character()), "600 secs")
})

# Raw ---------------------------------------------------------------------

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, raw()), NULL)
  expect_equal(vec_cast(list(raw(1)), raw()), raw(1))
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(raw(1), double()), class = "vctrs_error_incompatible_cast")
  expect_error(vec_cast(double(1), raw()), class = "vctrs_error_incompatible_cast")
})

# Lists  ------------------------------------------------------------------

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, list()), NULL)
  expect_equal(vec_cast(NA, list()), list(NA))
  expect_equal(vec_cast(1:2, list()), list(1L, 2L))
  expect_equal(vec_cast(list(1L, 2L), list()), list(1L, 2L))
})

test_that("dimensionality matches to" ,{
  x1 <- matrix(TRUE, nrow = 1, ncol = 1)
  x2 <- matrix(list(), nrow = 0, ncol = 2)
  expect_dim(vec_cast(x1, x2), c(1, 2))
  expect_dim(vec_cast(TRUE, x2), c(1, 2))
})

# vec_cast_common ---------------------------------------------------------

test_that("empty input returns list()", {
  expect_equal(vec_cast_common(), list())
  expect_equal(vec_cast_common(NULL, NULL), list(NULL, NULL))
})

# vec_restore -------------------------------------------------------------

test_that("default vec_restore() restores attributes except names", {
  to <- structure(NA, foo = "foo", bar = "bar")
  expect_identical(vec_restore.default(NA, to), structure(NA, foo = "foo", bar = "bar"))

  to <- structure(NA, names = "a", foo = "foo", bar = "bar")
  expect_identical(vec_restore.default(NA, to), structure(NA, foo = "foo", bar = "bar"))

  to <- structure(NA, foo = "foo", names = "a", bar = "bar")
  expect_identical(vec_restore.default(NA, to), structure(NA, foo = "foo", bar = "bar"))

  to <- structure(NA, foo = "foo", bar = "bar", names = "a")
  expect_identical(vec_restore.default(NA, to), structure(NA, foo = "foo", bar = "bar"))
})

test_that("default vec_restore() restores objectness", {
  to <- structure(NA, class = "foo")
  x <- vec_restore.default(NA, to)
  expect_true(is.object(x))
  expect_is(x, "foo")
})

test_that("data frame vec_restore() checks type", {
  expect_error(vec_restore(NA, mtcars), "Attempt to restore data frame from a logical")
})

test_that("can use vctrs primitives from vec_restore() without inflooping", {
  scoped_global_bindings(
    vec_restore.vctrs_foobar = function(x, to, ...) {
      vec_type(x)
      vec_na(x)
      vec_assert(x)
      vec_slice(x, 0)
      "woot"
    }
  )

  foobar <- new_vctr(1:3, class = "vctrs_foobar")
  expect_identical(vec_slice(foobar, 2), "woot")
})
