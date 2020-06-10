# ------------------------------------------------------------------------------
# vec_order2(<integer>) - insertion

test_that("can order integers", {
  x <- c(2L, 3L, 1L, 5L)
  expect_identical(vec_order2(x), order(x))
})

test_that("can order sorted vector", {
  x <- 1:5
  expect_identical(vec_order2(x), order(x))
})

test_that("orders correctly around the UINT8_MAX boundary", {
  x <- 251:255
  expect_identical(vec_order2(x), order(x))
})

test_that("ordering on ties is done stably", {
  x <- c(1L, 3L, 1L, 3L)
  expect_identical(vec_order2(x)[1:2], c(1L, 3L))
  expect_identical(vec_order2(x)[3:4], c(2L, 4L))
})

test_that("`NA` order defaults to last", {
  x <- c(1L, NA_integer_, 3L)
  expect_identical(vec_order2(x), c(1L, 3L, 2L))
})

test_that("`NA` order can be first", {
  x <- c(1L, NA_integer_, 3L)
  expect_identical(vec_order2(x, na_last = FALSE), c(2L, 1L, 3L))
})

test_that("`decreasing` can be set to `TRUE`", {
  x <- c(1L, .Machine$integer.max, 3L)
  expect_identical(vec_order2(x, decreasing = TRUE), c(2L, 3L, 1L))
})

test_that("all combinations of `decreasing` and `na_last` work", {
  x <- c(3L, NA_integer_, 1L, 2L)

  expect_identical(
    x[vec_order2(x, na_last = TRUE, decreasing = FALSE)],
    x[order(x, na.last = TRUE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order2(x, na_last = FALSE, decreasing = FALSE)],
    x[order(x, na.last = FALSE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order2(x, na_last = TRUE, decreasing = TRUE)],
    x[order(x, na.last = TRUE, decreasing = TRUE)]
  )
  expect_identical(
    x[vec_order2(x, na_last = FALSE, decreasing = TRUE)],
    x[order(x, na.last = FALSE, decreasing = TRUE)]
  )
})

test_that("all `NA` values works", {
  x <- c(NA_integer_, NA_integer_)
  expect_identical(vec_order2(x), order(x))
})

# ------------------------------------------------------------------------------
# vec_order2(<integer>) - counting

# To trigger counting ordering, get above the insertion order boundary and then
# have a range less than the counting order range boundary.

test_that("can order integers with counting order", {
  x <- (INT_INSERTION_ORDER_BOUNDARY + 1L):1L
  expect_identical(vec_order2(x), order(x))
})

test_that("can order sorted vector", {
  x <- 1:(INT_INSERTION_ORDER_BOUNDARY + 1L)
  expect_identical(vec_order2(x), order(x))
})

test_that("ordering on ties is done stably", {
  x <- c(1:INT_INSERTION_ORDER_BOUNDARY, 1L)
  expect_identical(vec_order2(x)[1:2], c(1L, INT_INSERTION_ORDER_BOUNDARY + 1L))
})

test_that("all combinations of `decreasing` and `na_last` work", {
  x <- c(3L, NA_integer_, 1L, 2L, 1:INT_INSERTION_ORDER_BOUNDARY)

  expect_identical(
    x[vec_order2(x, na_last = TRUE, decreasing = FALSE)],
    x[order(x, na.last = TRUE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order2(x, na_last = FALSE, decreasing = FALSE)],
    x[order(x, na.last = FALSE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order2(x, na_last = TRUE, decreasing = TRUE)],
    x[order(x, na.last = TRUE, decreasing = TRUE)]
  )
  expect_identical(
    x[vec_order2(x, na_last = FALSE, decreasing = TRUE)],
    x[order(x, na.last = FALSE, decreasing = TRUE)]
  )
})

# ------------------------------------------------------------------------------
# vec_order2(<integer>) - radix

# To trigger radix ordering, get above the insertion order boundary and then
# have a range greater than the counting order range boundary.

test_that("can order integers with counting order", {
  x <- c(INT_COUNTING_ORDER_RANGE_BOUNDARY + 1L, 1:INT_INSERTION_ORDER_BOUNDARY)
  expect_identical(vec_order2(x), order(x))
})

test_that("can order sorted vector", {
  x <- c(1:INT_INSERTION_ORDER_BOUNDARY, INT_COUNTING_ORDER_RANGE_BOUNDARY + 1L)
  expect_identical(vec_order2(x), order(x))
})

test_that("ordering on ties is done stably", {
  x <- c(1:INT_INSERTION_ORDER_BOUNDARY, 1L, INT_COUNTING_ORDER_RANGE_BOUNDARY + 1L)
  expect_identical(vec_order2(x)[1:2], c(1L, INT_INSERTION_ORDER_BOUNDARY + 1L))
})

test_that("all combinations of `decreasing` and `na_last` work", {
  x <- c(3L, NA_integer_, 1L, 2L, 1:INT_INSERTION_ORDER_BOUNDARY, INT_COUNTING_ORDER_RANGE_BOUNDARY + 1L)

  expect_identical(
    x[vec_order2(x, na_last = TRUE, decreasing = FALSE)],
    x[order(x, na.last = TRUE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order2(x, na_last = FALSE, decreasing = FALSE)],
    x[order(x, na.last = FALSE, decreasing = FALSE)]
  )
  expect_identical(
    x[vec_order2(x, na_last = TRUE, decreasing = TRUE)],
    x[order(x, na.last = TRUE, decreasing = TRUE)]
  )
  expect_identical(
    x[vec_order2(x, na_last = FALSE, decreasing = TRUE)],
    x[order(x, na.last = FALSE, decreasing = TRUE)]
  )
})

test_that("all `NA` values works - ensures that we can compute the 'range' of all NAs", {
  x <- rep(NA_integer_, INT_INSERTION_ORDER_BOUNDARY + 1L)
  expect_identical(vec_order2(x), order(x))
})

# ------------------------------------------------------------------------------
# vec_order2(<data.frame>) - insertion

test_that("data frame with no columns and no rows returns integer()", {
  x <- data.frame()
  expect_identical(vec_order2(x), integer())
})

test_that("data frame with no columns and some rows returns sequential rows", {
  x <- new_data_frame(n = 5L)
  expect_identical(vec_order2(x), 1:5)
})

test_that("can order with multiple pre-sorted keys", {
  df <- data.frame(x = 1:2, y = 3:4)
  expect_identical(vec_order2(df), 1:2)
})

test_that("first column has ordering presedence", {
  df <- data.frame(x = c(3L, 2L, 1L), y = c(1L, 2L, 3L))
  expect_identical(vec_order2(df), 3:1)
})

test_that("secondary columns break ties", {
  df <- data.frame(
    x = c(1L, 2L, 1L),
    y = c(3L, 2L, 1L)
  )
  expect_identical(vec_order2(df), c(3L, 1L, 2L))
})

test_that("orders correctly when first column is already ordered but second isn't", {
  df <- data.frame(
    x = c(1L, 1L, 2L, 2L),
    y = c(3L, 2L, 4L, 1L)
  )
  expect_identical(vec_order2(df), c(2L, 1L, 4L, 3L))
})

test_that("`decreasing` is recycled", {
  df <- data.frame(
    x = c(1L, 1L, 2L, 2L),
    y = c(3L, 2L, 4L, 1L)
  )
  expect_identical(vec_order2(df, decreasing = TRUE), c(3L, 4L, 1L, 2L))
})

test_that("`decreasing` can be a vector", {
  df <- data.frame(
    x = c(1L, 1L, 2L, 2L),
    y = c(3L, 2L, 4L, 1L)
  )
  expect_identical(vec_order2(df, decreasing = c(TRUE, FALSE)), c(4L, 3L, 2L, 1L))
})

# ------------------------------------------------------------------------------
# vec_order2(<data.frame>) - counting

test_that("can order 2+ integer columns with counting sort", {
  half <- floor(INT_INSERTION_ORDER_BOUNDARY / 2) + 1L
  quarter_low <- floor(half / 2)
  quarter_high <- ceiling(half / 2)

  df <- data.frame(
    x = c(rep(1L, half), rep(2L, half)),
    y = c(rep(2L, quarter_low), rep(1L, quarter_high), rep(3L, half))
  )

  expect_identical(vec_order2(df), lst_order(df))
})

# ------------------------------------------------------------------------------
# vec_order2(<data.frame>) - radix

test_that("can order 2+ integer columns with radix sort", {
  half <- floor(INT_INSERTION_ORDER_BOUNDARY / 2) + 1L
  quarter_low <- floor(half / 2)
  quarter_high <- ceiling(half / 2)

  df <- data.frame(
    x = c(rep(1L, half), rep(2L, half), 1L),
    y = c(rep(2L, quarter_low), rep(1L, quarter_high), rep(3L, half), INT_COUNTING_ORDER_RANGE_BOUNDARY + 1L)
  )

  expect_identical(vec_order2(df), lst_order(df))
})

# ------------------------------------------------------------------------------
# vec_order2() - error checking

test_that("`na_last` is checked", {
  expect_error(vec_order2(1L, na_last = "x"), "`TRUE` or `FALSE`")
  expect_error(vec_order2(1L, na_last = c(TRUE, TRUE)), "`TRUE` or `FALSE`")
  expect_error(vec_order2(1L, na_last = NA), "`TRUE` or `FALSE`")
})

test_that("`decreasing` is checked", {
  expect_error(vec_order2(1L, decreasing = "x"), "must be logical")
  expect_error(vec_order2(1L, decreasing = c(TRUE, TRUE)), "length 1")
  expect_error(vec_order2(1L, decreasing = NA), "missing values")
  expect_error(vec_order2(data.frame(x = 1), decreasing = c(TRUE, TRUE)), "length 1 or")
})

test_that("`x` is checked", {
  expect_error(vec_order2(list()), "not supported")
})

# ------------------------------------------------------------------------------
# vec_order2() - groups

test_that("groups can be reallocated if we exceed the max group data size", {
  set.seed(123)

  # The first column has all unique groups so 1 more than the default group
  # data size is needed and will be reallocated on the fly
  df <- data.frame(
    x = sample(GROUP_DATA_SIZE_DEFAULT + 1L),
    y = sample(GROUP_DATA_SIZE_DEFAULT + 1L)
  )

  expect_identical(vec_order2(df), lst_order(df))
})
