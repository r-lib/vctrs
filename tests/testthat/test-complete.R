# vec_slice_complete -----------------------------------------------------------

test_that("can slice complete", {
  df <- data_frame(x = c(1, NA, 3), y = c(1, 2, NA))
  expect_identical(vec_slice_complete(df), vec_slice(df, 1))
})

test_that("vec_slice_complete() works with size 0 input", {
  expect_identical(vec_slice_complete(integer()), integer())
  expect_identical(vec_slice_complete(data.frame()), data.frame())
})

# vec_locate_complete ----------------------------------------------------------

test_that("vec_locate_complete() can locate complete", {
  df <- data_frame(x = c(1, NA, 3), y = c(1, 2, NA))
  expect_identical(vec_locate_complete(df), 1L)
})

test_that("vec_locate_complete() works with size 0 input", {
  expect_identical(vec_locate_complete(logical()), integer())
  expect_identical(vec_locate_complete(data.frame()), integer())
})

# vec_detect_complete ----------------------------------------------------------

test_that("works with size zero input", {
  expect_identical(vec_detect_complete(integer()), logical())
  expect_identical(vec_detect_complete(data.frame()), logical())
})

test_that("NA_real_ and NaN are both missing", {
  expect_identical(vec_detect_complete(c(NA_real_, NaN)), c(FALSE, FALSE))
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

test_that("works with various types", {
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

test_that("takes the equality proxy", {
  x <- as.POSIXlt(c(NA, 0), origin = "1970-01-01")
  df <- data_frame(a = 1:2, x = x)

  expect <- c(FALSE, TRUE)

  expect_identical(vec_detect_complete(x), expect)
  expect_identical(vec_detect_complete(df), expect)
})

test_that("columns with a data frame proxy are incomplete if any columns of the proxy are incomplete", {
  df <- data_frame(
    x = c(NA, 0, 1, 2, 3),
    y = new_rcrd(list(a = c(1, 1, 1, NA, NA), b = c(2, 2, 2, 2, NA))),
    z = new_rcrd(list(a = c(1, 1, NA, 1, 1), b = c(2, 2, NA, NA, 1)))
  )

  expect_identical(vec_detect_complete(df), c(FALSE, TRUE, FALSE, FALSE, FALSE))
})

test_that("can have rcrd fields of all types", {
  make_rcrd <- function(x) {
    new_rcrd(list(x = x))
  }

  expect <- c(TRUE, TRUE, FALSE, TRUE, FALSE)

  expect_identical(vec_detect_complete(make_rcrd(c(TRUE, TRUE, NA, FALSE, NA))), expect)
  expect_identical(vec_detect_complete(make_rcrd(c(1L, 1L, NA, 2L, NA))), expect)
  expect_identical(vec_detect_complete(make_rcrd(c(1, 1, NA, 2, NA))), expect)
  expect_identical(vec_detect_complete(make_rcrd(complex(real = c(1, 1, NA, 2, 2), imaginary = c(1, 1, 2, 2, NA)))), expect)
  expect_identical(vec_detect_complete(make_rcrd(c("a", "a", NA, "b", NA))), expect)
  expect_identical(vec_detect_complete(make_rcrd(list(1, 1, NULL, 2, NULL))), expect)

  # No missing raw value
  expect_identical(vec_detect_complete(make_rcrd(as.raw(c(1, 1, 2, 2, 3)))), rep(TRUE, 5))
})

test_that("works with arrays", {
  x <- array(c(1, 2, 3, NA), c(2, 2))
  y <- array(c(1:3, NA, 5:8), c(2, 2, 2))

  expect_identical(vec_detect_complete(x), c(TRUE, FALSE))
  expect_identical(vec_detect_complete(y), c(TRUE, FALSE))
})
