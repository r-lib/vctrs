# vec_slice_complete -----------------------------------------------------------

test_that("can slice complete", {
  x <- c(1, NA, 3)
  df <- data_frame(x = x, y = c(1, 2, NA))

  expect_identical(vec_slice_complete(x), vec_slice(x, c(1, 3)))
  expect_identical(vec_slice_complete(df), vec_slice(df, 1))
})

test_that("works with size 0 input", {
  expect_identical(vec_slice_complete(integer()), integer())
  expect_identical(vec_slice_complete(data.frame()), data.frame())
})

# vec_locate_complete ----------------------------------------------------------

test_that("can locate complete", {
  x <- c(1, NA, 3)
  df <- data_frame(x = x, y = c(1, 2, NA))

  expect_identical(vec_locate_complete(x), c(1L, 3L))
  expect_identical(vec_locate_complete(df), 1L)
})

test_that("works with size 0 input", {
  expect_identical(vec_locate_complete(integer()), integer())
  expect_identical(vec_locate_complete(data.frame()), integer())
})

# vec_detect_complete ----------------------------------------------------------

test_that("works with size zero input", {
  expect <- logical()
  expect_identical(vec_detect_complete(integer()), expect)
  expect_identical(vec_detect_complete(data.frame()), expect)
})

test_that("works with atomic input of various types", {
  expect <- c(TRUE, TRUE, FALSE, TRUE, FALSE)

  expect_identical(vec_detect_complete(c(TRUE, TRUE, NA, FALSE, NA)), expect)
  expect_identical(vec_detect_complete(c(1L, 1L, NA, 2L, NA)), expect)
  expect_identical(vec_detect_complete(c(1, 1, NA, 2, NA)), expect)
  expect_identical(vec_detect_complete(complex(real = c(1, 1, NA, 2, 2), imaginary = c(1, 1, 2, 2, NA))), expect)
  expect_identical(vec_detect_complete(c("a", "a", NA, "b", NA)), expect)
  expect_identical(vec_detect_complete(list(1, 1, NULL, 2, NULL)), expect)

  # No missing raw value
  expect_identical(vec_detect_complete(as.raw(c(1, 1, 2, 2, 3))), rep(TRUE, 5))
})

test_that("NA_real_ and NaN are both missing", {
  expect_identical(vec_detect_complete(c(NA_real_, NaN)), c(FALSE, FALSE))
})

test_that("errors on scalars", {
  expect_error(vec_detect_complete(foobar()), class = "vctrs_error_scalar_type")
})

test_that("works with data frames rowwise", {
  df <- data_frame(x = c(NA, 1, NA, 2), y = c(NA, NA, 2, 3))
  expect <- c(FALSE, FALSE, FALSE, TRUE)
  expect_identical(vec_detect_complete(df), expect)

  df <- data_frame(x = c(1, 1), y = c(2, 2), z = c(1, NA))
  expect <- c(TRUE, FALSE)
  expect_identical(vec_detect_complete(df), expect)
})

test_that("works with data frames with rows but no columns", {
  expect_identical(vec_detect_complete(new_data_frame(n = 5L)), rep(TRUE, 5))
})

test_that("works with data frame columns", {
  col <- data_frame(a = c(1, NA, 2, 2), b = c(1, 2, NA, 3))
  df <- data_frame(x = rep(1, 4), y = col)
  expect <- c(TRUE, FALSE, FALSE, TRUE)
  expect_identical(vec_detect_complete(df), expect)
})

test_that("works with columns of various types", {
  # Use two columns to keep the data frame from being squashed to a vector
  add_col <- function(col) {
    x <- rep(1L, 5)
    data_frame(x = x, y = col)
  }

  expect <- c(TRUE, TRUE, FALSE, TRUE, FALSE)

  expect_identical(vec_detect_complete(add_col(c(TRUE, TRUE, NA, FALSE, NA))), expect)
  expect_identical(vec_detect_complete(add_col(c(1L, 1L, NA, 2L, NA))), expect)
  expect_identical(vec_detect_complete(add_col(c(1, 1, NA, 2, NA))), expect)
  expect_identical(vec_detect_complete(add_col(complex(real = c(1, 1, NA, 2, 2), imaginary = c(1, 1, 2, 2, NA)))), expect)
  expect_identical(vec_detect_complete(add_col(c("a", "a", NA, "b", NA))), expect)
  expect_identical(vec_detect_complete(add_col(list(1, 1, NULL, 2, NULL))), expect)

  # No missing raw value
  expect_identical(vec_detect_complete(add_col(as.raw(c(1, 1, 2, 2, 3)))), rep(TRUE, 5))
})

test_that("takes the equality proxy", {
  x <- as.POSIXlt(c(NA, 0), origin = "1970-01-01")
  df <- data_frame(a = 1:2, x = x)

  expect <- c(FALSE, TRUE)

  expect_identical(vec_detect_complete(x), expect)
  expect_identical(vec_detect_complete(df), expect)
})
