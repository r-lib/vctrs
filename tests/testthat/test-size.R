context("test-size")

# vec_size -----------------------------------------------------------------

test_that("vec_size must be called with vector", {
  expect_error(vec_size(mean), "not a vector")
})

test_that("length is number of rows", {
  expect_equal(vec_size(integer()), 0)
  expect_equal(vec_size(array(integer())), 0)

  expect_equal(vec_size(1:2), 2)
  expect_equal(vec_size(array(dim = 2)), 2)

  expect_equal(vec_size(matrix(nrow = 2, ncol = 3)), 2)
  expect_equal(vec_size(array(dim = c(2, 1, 5))), 2)
})

test_that("length of record is number of rows, not fields", {
  r <- new_rcrd(list(x = 1:10))
  expect_equal(vec_size(r), 10)
})

test_that("handles three types of data frame rownames", {
  df1 <- df2 <- df3 <- data.frame(x = 1:3)
  rownames(df1) <- NULL
  rownames(df2) <- 3:1
  rownames(df3) <- letters[1:3]

  expect_equal(vec_size(df1), 3)
  expect_equal(vec_size(df2), 3)
  expect_equal(vec_size(df3), 3)
})

# vec_slice --------------------------------------------------------------

test_that("can subset object of any dimensionality", {
  x0 <- c(1, 1)
  x1 <- ones(2)
  x2 <- ones(2, 3)
  x3 <- ones(2, 3, 4)
  x4 <- ones(2, 3, 4, 5)

  expect_equal(vec_slice(x0, 1L), 1)
  expect_equal(vec_slice(x1, 1L), ones(1))
  expect_equal(vec_slice(x2, 1L), ones(1, 3))
  expect_equal(vec_slice(x3, 1L), ones(1, 3, 4))
  expect_equal(vec_slice(x4, 1L), ones(1, 3, 4, 5))
})

test_that("can modify subset", {
  x1 <- c(2, 1)
  vec_slice(x1, 1L) <- 1
  expect_equal(x1, c(1, 1))

  x2 <- array(c(2, 1, 2, 1), c(2, 2))
  vec_slice(x2, 1L) <- 1
  expect_equal(x2, array(1, c(2, 2)))

  x3 <- array(c(2, 1, 2, 1, 2, 1, 2, 1), c(2, 2, 2))
  vec_slice(x3, 1L) <- 1
  expect_equal(x3, array(1, c(2, 2, 2)))
})

test_that("ignores NA in logical subsetting", {
  x <- c(NA, 1, 2)
  expect_equal(vec_slice(x, x > 0), c(1, 2))
  expect_equal(`vec_slice<-`(x, x > 0, 1), c(NA, 1, 1))
  expect_equal(`vec_slice<-`(x, x > 0, 2:1), c(NA, 2, 1))
})

# vec_na ------------------------------------------------------------------

test_that("vec_slice throws error with non-vector inputs", {
  expect_error(vec_slice(environment(), 1L), "a vector")

  x <- environment()
  expect_error(vec_slice(x, 1L) <- 1L, "a vector")
})

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

