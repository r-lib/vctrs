context("test-cast")

# NULL --------------------------------------------------------------------

test_that("NULL is idempotent", {
  expect_equal(vec_cast(NULL, NULL), NULL)
  expect_equal(vec_cast(list(1:3), NULL), list(1:3))
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
  expect_condition(vec_cast("x", double()), class = "warning_cast_lossy")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), double()), class = "error_no_cast")
})

# Character ---------------------------------------------------------------

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, character()), NULL)
  expect_equal(vec_cast(TRUE, character()), "TRUE")
  expect_equal(vec_cast(list("x"), character()), "x")
})

# Lists  ------------------------------------------------------------------

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, list()), NULL)
  expect_equal(vec_cast(1:2, list()), list(1L, 2L))
})

# Factors -----------------------------------------------------------------

test_that("safe casts work as expected", {
  fa <- factor("a")
  fab <- factor(c("a", "b"))

  expect_equal(vec_cast(NULL, fa), NULL)

  expect_equal(vec_cast(fa, fa), fa)
  expect_equal(vec_cast(fa, fab), fab[1])
  expect_equal(vec_cast("a", fab), fab[1])

  expect_equal(vec_cast(list("a", "b"), fab), fab)
})

test_that("lossy casts generate warning", {
  fa <- factor("a")
  fb <- factor("b")

  expect_condition(vec_cast(fa, fb), class = "warning_cast_lossy")
  expect_condition(vec_cast("a", fb), class = "warning_cast_lossy")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(double(), factor("a")), class = "error_no_cast")
})
