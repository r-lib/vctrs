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

# df_detect_complete -----------------------------------------------------------

test_that("works with size zero input", {
  expect_identical(df_detect_complete(data.frame()), logical())
})

test_that("NA_real_ and NaN are both missing", {
  expect_identical(df_detect_complete(data_frame(x = c(NA_real_, NaN))), c(FALSE, FALSE))
})

test_that("errors on non-data frame input", {
  expect_error(df_detect_complete(1), "must be a data frame")
})

test_that("works rowwise", {
  df <- data_frame(x = c(NA, 1, NA, 2), y = c(NA, NA, 2, 3))
  expect <- c(FALSE, FALSE, FALSE, TRUE)
  expect_identical(df_detect_complete(df), expect)

  df <- data_frame(x = c(1, 1), y = c(2, 2), z = c(1, NA))
  expect <- c(TRUE, FALSE)
  expect_identical(df_detect_complete(df), expect)
})

test_that("works with data frames with rows but no columns", {
  expect_identical(df_detect_complete(new_data_frame(n = 5L)), rep(TRUE, 5))
})

test_that("works with data frame columns", {
  col <- data_frame(a = c(1, NA, 2, 2), b = c(1, 2, NA, 3))
  df <- data_frame(x = rep(1, 4), y = col)
  expect <- c(TRUE, FALSE, FALSE, TRUE)
  expect_identical(df_detect_complete(df), expect)
})

test_that("works with columns of various types", {
  add_col <- function(col) {
    data_frame(x = col)
  }

  expect <- c(TRUE, TRUE, FALSE, TRUE, FALSE)

  expect_identical(df_detect_complete(add_col(c(TRUE, TRUE, NA, FALSE, NA))), expect)
  expect_identical(df_detect_complete(add_col(c(1L, 1L, NA, 2L, NA))), expect)
  expect_identical(df_detect_complete(add_col(c(1, 1, NA, 2, NA))), expect)
  expect_identical(df_detect_complete(add_col(complex(real = c(1, 1, NA, 2, 2), imaginary = c(1, 1, 2, 2, NA)))), expect)
  expect_identical(df_detect_complete(add_col(c("a", "a", NA, "b", NA))), expect)
  expect_identical(df_detect_complete(add_col(list(1, 1, NULL, 2, NULL))), expect)

  # No missing raw value
  expect_identical(df_detect_complete(add_col(as.raw(c(1, 1, 2, 2, 3)))), rep(TRUE, 5))
})

test_that("takes the equality proxy of each column", {
  x <- as.POSIXlt(c(NA, 0), origin = "1970-01-01")
  df <- data_frame(a = 1:2, x = x)

  expect <- c(FALSE, TRUE)

  expect_identical(df_detect_complete(df), expect)
})

test_that("columns with a data frame proxy are only incomplete if all rows are incomplete", {
  df <- data_frame(
    x = c(NA, 1, 2, 3),
    y = new_rcrd(list(a = c(1, 1, NA, NA), b = c(2, 2, 2, NA))),
    z = new_rcrd(list(a = c(1, NA, 1, 1), b = c(2, NA, NA, 1)))
  )

  expect_identical(df_detect_complete(df), c(FALSE, FALSE, TRUE, FALSE))
})

test_that("can have rcrd fields of all types", {
  add_rcrd_col <- function(col) {
    data_frame(x = new_rcrd(list(col = col)))
  }

  expect <- c(TRUE, TRUE, FALSE, TRUE, FALSE)

  expect_identical(df_detect_complete(add_rcrd_col(c(TRUE, TRUE, NA, FALSE, NA))), expect)
  expect_identical(df_detect_complete(add_rcrd_col(c(1L, 1L, NA, 2L, NA))), expect)
  expect_identical(df_detect_complete(add_rcrd_col(c(1, 1, NA, 2, NA))), expect)
  expect_identical(df_detect_complete(add_rcrd_col(complex(real = c(1, 1, NA, 2, 2), imaginary = c(1, 1, 2, 2, NA)))), expect)
  expect_identical(df_detect_complete(add_rcrd_col(c("a", "a", NA, "b", NA))), expect)
  expect_identical(df_detect_complete(add_rcrd_col(list(1, 1, NULL, 2, NULL))), expect)

  # No missing raw value
  expect_identical(df_detect_complete(add_rcrd_col(as.raw(c(1, 1, 2, 2, 3)))), rep(TRUE, 5))
})
