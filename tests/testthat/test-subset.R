context("test-subset")

test_that("can subset object of an dimensionality", {
  ones <- function(...) {
    array(1, dim = c(...))
  }

  x0 <- c(1, 1)
  x1 <- ones(2)
  x2 <- ones(2, 3)
  x3 <- ones(2, 3, 4)
  x4 <- ones(2, 3, 4, 5)

  expect_equal(vec_subset(x0, 1L), 1)
  expect_equal(vec_subset(x1, 1L), ones(1))
  expect_equal(vec_subset(x2, 1L), ones(1, 3))
  expect_equal(vec_subset(x3, 1L), ones(1, 3, 4))
  expect_equal(vec_subset(x4, 1L), ones(1, 3, 4, 5))
})

test_that("can modify subset", {
  x1 <- c(2, 1)
  vec_subset(x1, 1L) <- 1
  expect_equal(x1, c(1, 1))

  x2 <- array(c(2, 1, 2, 1), c(2, 2))
  vec_subset(x2, 1L) <- 1
  expect_equal(x2, array(1, c(2, 2)))

  x3 <- array(c(2, 1, 2, 1, 2, 1, 2, 1), c(2, 2, 2))
  vec_subset(x3, 1L) <- 1
  expect_equal(x3, array(1, c(2, 2, 2)))
})

# vec_na ------------------------------------------------------------------

test_that("na of atomic vectors is as expected", {
  expect_equal(vec_na(TRUE), NA)
  expect_equal(vec_na(1L), NA_integer_)
  expect_equal(vec_na(1), NA_real_)
  expect_equal(vec_na("x"), NA_character_)
  expect_equal(vec_na(1i), NA_complex_)
})

test_that("na of factor preserves levels", {
  f1 <- factor("a", levels = c("a", "b"))
  f2 <- vec_na(f1)

  expect_equal(levels(f1), levels(f2))
})

test_that("na of POSIXct preserves tz", {
  dt1 <- as.POSIXct("2010-01-01", tz = "America/New_York")
  dt2 <- vec_na(dt1)
  expect_equal(attr(dt2, "tzone"), "America/New_York")
})

test_that("na of list is list(NULL)", {
  expect_equal(vec_na(list()), list(NULL))
})

test_that("na of array is 1d slice", {
  x1 <- array(1:12, c(2, 3, 4))
  x2 <- vec_na(x1)

  expect_equal(x2, array(NA_integer_, c(1, 3, 4)))
})

test_that("na of list-array is 1d slice", {
  x1 <- array(as.list(1:12), c(2, 3, 4))
  x2 <- vec_na(x1)

  expect_equal(x2, array(list(), c(1, 3, 4)))
})
