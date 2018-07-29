context("test-cast")

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
  expect_condition(vec_cast(2L, logical()), class = "warning_cast_lossy")
  expect_condition(vec_cast(2, logical()), class = "warning_cast_lossy")
  expect_condition(vec_cast("x", logical()), class = "warning_cast_lossy")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), logical()), class = "error_no_cast")
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
  expect_condition(vec_cast(2.5, integer()), class = "warning_cast_lossy")
  expect_condition(vec_cast("2.5", integer()), class = "warning_cast_lossy")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), integer()), class = "error_no_cast")
})
