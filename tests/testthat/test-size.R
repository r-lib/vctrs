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

test_that("handles positive short row names (#220)", {
  data <- structure(mtcars, row.names = c(NA, 32))
  expect_identical(vec_size(data), 32L)
})


# vec_slice --------------------------------------------------------------

test_that("can subset atomic vectors", {
  i <- 2:3
  expect_identical(vec_slice(lgl(1, 0, 1), i), lgl(0, 1))
  expect_identical(vec_slice(int(1, 2, 3), i), int(2, 3))
  expect_identical(vec_slice(dbl(1, 2, 3), i), dbl(2, 3))
  expect_identical(vec_slice(cpl(1, 2, 3), i), cpl(2, 3))
  expect_identical(vec_slice(chr("1", "2", "3"), i), chr("2", "3"))
  expect_identical(vec_slice(bytes(1, 2, 3), i), bytes(2, 3))
})

test_that("can subset with missing indices", {
  for (i in list(int(2L, NA), lgl(FALSE, TRUE, NA))) {
    expect_identical(vec_slice(lgl(1, 0, 1), i), lgl(0, NA))
    expect_identical(vec_slice(int(1, 2, 3), i), int(2, NA))
    expect_identical(vec_slice(dbl(1, 2, 3), i), dbl(2, NA))
    expect_identical(vec_slice(cpl(1, 2, 3), i), cpl(2, NA))
    expect_identical(vec_slice(chr("1", "2", "3"), i), c("2", NA))
    expect_identical(vec_slice(bytes(1, 2, 3), i), bytes(2, 0))
  }
})

test_that("can subset with a single logical NA", {
  expect_identical(vec_slice(1:3, NA), na_int)
})

test_that("can subset object of any dimensionality", {
  x0 <- c(1, 1)
  x1 <- ones(2)
  x2 <- ones(2, 3)
  x3 <- ones(2, 3, 4)
  x4 <- ones(2, 3, 4, 5)
  x5 <- NULL

  expect_equal(vec_slice(x0, 1L), 1)
  expect_identical(vec_slice(x1, 1L), ones(1))
  expect_identical(vec_slice(x2, 1L), ones(1, 3))
  expect_identical(vec_slice(x3, 1L), ones(1, 3, 4))
  expect_identical(vec_slice(x4, 1L), ones(1, 3, 4, 5))
  expect_identical(vec_slice(x5, 1L), NULL)
})

test_that("can subset using logical index", {
  x0 <- c(1, 1)

  expect_identical(vec_slice(x0, TRUE), x0)
  expect_identical(vec_slice(x0, c(TRUE, FALSE)), 1)

  expect_error(
    vec_slice(x0, c(TRUE, FALSE, TRUE)),
    "Incompatible lengths: 3, 2",
    fixed = TRUE
  )

  expect_error(
    vec_slice(x0, lgl()),
    "Incompatible lengths: 0, 2",
    fixed = TRUE
  )
})

test_that("can subset data frame columns", {
  df <- data.frame(x = 1:2)
  df$y <- data.frame(a = 2:1)

  expect_equal(vec_slice(df, 1L)$y, vec_slice(df$y, 1L))
})

test_that("can subset empty data frames", {
  df <- new_data_frame(n = 3L)
  expect_equal(vec_size(vec_slice(df, integer())), 0)
  expect_equal(vec_size(vec_slice(df, 1L)), 1)
  expect_equal(vec_size(vec_slice(df, 1:3)), 3)

  df$df <- df
  expect_equal(vec_size(vec_slice(df, integer())), 0)
  expect_equal(vec_size(vec_slice(df, 1L)), 1)
  expect_equal(vec_size(vec_slice(df, 1:3)), 3)
})

test_that("can modify subset", {
  x0 <- NULL
  vec_slice(x0, 1L) <- 1
  expect_identical(x0, NULL)

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

test_that("can modify subset using logical index", {
  x1 <- c(2, 1)
  vec_slice(x1, TRUE) <- 3
  expect_equal(x1, c(3, 3))
  vec_slice(x1, c(TRUE, FALSE)) <- 4
  expect_equal(x1, c(4, 3))

  expect_error(
    vec_slice(x1, c(TRUE, FALSE, TRUE)) <- 5,
    "Incompatible lengths: 3, 2",
    fixed = TRUE
  )
})

test_that("ignores NA in logical subsetting", {
  x <- c(NA, 1, 2)
  expect_equal(vec_slice(x, x > 0), c(NA, 1, 2))
  expect_equal(`vec_slice<-`(x, x > 0, 1), c(NA, 1, 1))
  expect_equal(`vec_slice<-`(x, x > 0, 2:1), c(NA, 2, 1))
})

test_that("ignores NA in integer subsetting", {
  x <- 0:2
  expect_equal(vec_slice(x, c(NA, 2:3)), c(NA, 1, 2))
  expect_equal(`vec_slice<-`(x, c(NA, 2:3), 1), c(0, 1, 1))
  expect_equal(`vec_slice<-`(x, c(NA, 2:3), 2:1), c(0, 2, 1))
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

# sequences ---------------------------------------------------------------

test_that("vec_seq_along returns size-0 output for size-0 input", {
  expect_equal(vec_seq_along(character()), integer())
  expect_equal(vec_seq_along(data.frame()), integer())
})

test_that("vec_na_along can be called with single argument", {
  expect_equal(vec_na_along(1:3), rep(NA_integer_, 3))
})

# vec_names ---------------------------------------------------------------

test_that("can retrieve names", {
  expect_null(vec_names(letters))
  expect_identical(vec_names(set_names(letters)), letters)
  expect_null(vec_names(mtcars))
  expect_identical(vec_names(Titanic), dimnames(Titanic)[[1]])
})

test_that("can set names", {
  x <- letters
  vec_names(x) <- letters
  expect_identical(vec_names(x), letters)
  vec_names(x) <- NULL
  expect_null(vec_names(x))

  y <- iris
  vec_names(y) <- as.character(-seq_len(vec_size(y)))
  expect_identical(row.names(y), row.names(iris))
  expect_null(vec_names(y))

  z <- ones(3, 2, 1)
  vec_names(z) <- as.character(1:3)
  expect_identical(vec_names(z), as.character(1:3))
})
