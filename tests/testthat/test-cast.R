context("test-cast")

# NULL --------------------------------------------------------------------

test_that("NULL is idempotent", {
  expect_equal(vec_cast(NULL, NULL), NULL)
  expect_equal(vec_cast(list(1:3), NULL), list(1:3))
})

# Default --------------------------------------------------------------------

test_that("new classes are uncoercible by default", {
  x <- structure(1:10, class = "vctrs_nonexistant")
  expect_error(vec_cast(1, x), class = "error_incompatible_cast")
  expect_error(vec_cast(x, 1), class = "error_incompatible_cast")
})

# Logical -----------------------------------------------------------------

test_that("safe casts work as expeced", {
  expect_equal(vec_cast(NULL, logical()), NULL)
  expect_equal(vec_cast(TRUE, logical()), TRUE)
  expect_equal(vec_cast(1L, logical()), TRUE)
  expect_equal(vec_cast(1, logical()), TRUE)
  expect_equal(vec_cast("TRUE", logical()), TRUE)
  expect_equal(vec_cast(list(1), logical()), TRUE)
})

test_that("lossy casts generate warning", {
  expect_condition(vec_cast(c(2L, 1L), logical()), class = "warning_lossy_cast")
  expect_condition(vec_cast(c(2, 1), logical()), class = "warning_lossy_cast")
  expect_condition(vec_cast(c("x", "TRUE"), logical()), class = "warning_lossy_cast")
  expect_condition(vec_cast(list(c(TRUE, FALSE), TRUE), logical()), class = "warning_lossy_cast")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), logical()), class = "error_incompatible_cast")
})

test_that("dimensionality matches output" ,{
  x1 <- matrix(TRUE, nrow = 1, ncol = 1)
  x2 <- matrix(1, nrow = 0, ncol = 2)
  expect_dim(vec_cast(x1, x2), c(1, 2))
  expect_dim(vec_cast(TRUE, x2), c(1, 2))

  x <- matrix(1, nrow = 2, ncol = 2)
  expect_error(vec_cast(x, logical()), class = "error_incompatible_cast")
})

# Integer -----------------------------------------------------------------

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, integer()), NULL)
  expect_equal(vec_cast(TRUE, integer()), 1L)
  expect_equal(vec_cast(1L, integer()), 1L)
  expect_equal(vec_cast(1, integer()), 1L)
  expect_equal(vec_cast("1", integer()), 1L)
  expect_equal(vec_cast(list(1L), integer()), 1L)
})

test_that("lossy casts generate warning", {
  expect_condition(vec_cast(c(2.5, 2), integer()), class = "warning_lossy_cast")
  expect_condition(vec_cast(c("2.5", "2"), integer()), class = "warning_lossy_cast")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), integer()), class = "error_incompatible_cast")
})

# Double ------------------------------------------------------------------

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, double()), NULL)
  expect_equal(vec_cast(TRUE, double()), 1L)
  expect_equal(vec_cast(1.5, double()), 1.5)
  expect_equal(vec_cast(1.5, double()), 1.5)
  expect_equal(vec_cast("1.5", double()), 1.5)
  expect_equal(vec_cast(list(1.5), double()), 1.5)
})

test_that("lossy casts generate warning", {
  expect_condition(vec_cast(c("2.5", "x"), double()), class = "warning_lossy_cast")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), double()), class = "error_incompatible_cast")
})

# Character ---------------------------------------------------------------

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, character()), NULL)
  expect_equal(vec_cast(NA, character()), NA_character_)
  expect_equal(vec_cast(TRUE, character()), "TRUE")
  expect_equal(vec_cast(list("x"), character()), "x")
})

test_that("difftime gets special treatment", {
  dt1 <- as.difftime(600, units = "secs")

  expect_equal(vec_cast(dt1, character()), "600 secs")
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
