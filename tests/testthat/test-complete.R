# vec_slice_complete -----------------------------------------------------------

test_that("can slice complete", {
  df <- data_frame(x = c(1, NA, 3), y = c(1, 2, NA))
  expect_identical(vec_slice_complete(df), vec_slice(df, 1))
})

test_that("works with size 0 input", {
  expect_identical(vec_slice_complete(data.frame()), data.frame())
})

# vec_locate_complete ----------------------------------------------------------

test_that("can locate complete", {
  df <- data_frame(x = c(1, NA, 3), y = c(1, 2, NA))
  expect_identical(vec_locate_complete(df), 1L)
})

test_that("works with size 0 input", {
  expect_identical(vec_locate_complete(data.frame()), integer())
})

# vec_detect_complete ----------------------------------------------------------

test_that("works with size zero input", {
  expect_identical(vec_detect_complete(data.frame()), logical())
})

test_that("NA_real_ and NaN are both missing", {
  expect_identical(vec_detect_complete(data_frame(x = c(NA_real_, NaN))), c(FALSE, FALSE))
})

test_that("works rowwise", {
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
  add_col <- function(col) {
    data_frame(x = col)
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

test_that("takes the equality proxy of each column", {
  x <- as.POSIXlt(c(NA, 0), origin = "1970-01-01")
  df <- data_frame(a = 1:2, x = x)

  expect <- c(FALSE, TRUE)

  expect_identical(vec_detect_complete(df), expect)
})

test_that("columns with a data frame proxy are only incomplete if all rows are incomplete", {
  df <- data_frame(
    x = c(NA, 1, 2, 3),
    y = new_rcrd(list(a = c(1, 1, NA, NA), b = c(2, 2, 2, NA))),
    z = new_rcrd(list(a = c(1, NA, 1, 1), b = c(2, NA, NA, 1)))
  )

  expect_identical(vec_detect_complete(df), c(FALSE, FALSE, TRUE, FALSE))
})

test_that("can have rcrd fields of all types", {
  add_rcrd_col <- function(col) {
    data_frame(x = new_rcrd(list(col = col)))
  }

  expect <- c(TRUE, TRUE, FALSE, TRUE, FALSE)

  expect_identical(vec_detect_complete(add_rcrd_col(c(TRUE, TRUE, NA, FALSE, NA))), expect)
  expect_identical(vec_detect_complete(add_rcrd_col(c(1L, 1L, NA, 2L, NA))), expect)
  expect_identical(vec_detect_complete(add_rcrd_col(c(1, 1, NA, 2, NA))), expect)
  expect_identical(vec_detect_complete(add_rcrd_col(complex(real = c(1, 1, NA, 2, 2), imaginary = c(1, 1, 2, 2, NA)))), expect)
  expect_identical(vec_detect_complete(add_rcrd_col(c("a", "a", NA, "b", NA))), expect)
  expect_identical(vec_detect_complete(add_rcrd_col(list(1, 1, NULL, 2, NULL))), expect)

  # No missing raw value
  expect_identical(vec_detect_complete(add_rcrd_col(as.raw(c(1, 1, 2, 2, 3)))), rep(TRUE, 5))
})

# vec_proxy_complete -----------------------------------------------------------

test_that("generally returns equality proxy", {
  x <- 1:5
  y <- as.POSIXlt("2019-01-01") + 1:5

  expect_identical(vec_proxy_complete(x), vec_proxy_equal(x))
  expect_identical(vec_proxy_complete(y), vec_proxy_equal(y))
  expect_identical(vec_proxy_complete(mtcars), vec_proxy_equal(mtcars))
})

test_that("returns equality proxy with arrays", {
  x <- array(1)
  y <- array(1, c(2, 2))
  z <- array(1, c(2, 2, 2))

  expect_identical(vec_proxy_complete(x), vec_proxy_equal(x))
  expect_identical(vec_proxy_complete(y), vec_proxy_equal(y))
  expect_identical(vec_proxy_complete(z), vec_proxy_equal(z))
})

test_that("non data frame input that has a data frame equality proxy has the correct completeness proxy", {
  x <- 1:2
  y <- new_rcrd(list(a = c(NA, 1), b = c(NA, NA)))
  z <- data_frame(c = 1:2, d = 3:4)

  df <- data_frame(x = x, y = y, z = z)

  y_expect <- c(NA, FALSE)
  df_expect <- data_frame(x = x, y = y_expect, z)

  expect_identical(vec_proxy_complete(y), y_expect)
  expect_identical(vec_proxy_complete(df), df_expect)
})


