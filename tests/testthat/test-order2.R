# ------------------------------------------------------------------------------
# vec_radix_order(<integer>)

test_that("can order integers", {
  x <- c(2L, 3L, 1L, 5L)
  expect_identical(vec_radix_order(x), order(x))
})

test_that("can order sorted vector", {
  x <- 1:5
  expect_identical(vec_radix_order(x), order(x))
})

test_that("orders correctly around the UINT8_MAX boundary", {
  x <- 251:255
  expect_identical(vec_radix_order(x), order(x))
})

test_that("ordering on ties is done stably", {
  x <- c(1L, 3L, 1L, 3L)
  expect_identical(vec_radix_order(x)[1:2], c(1L, 3L))
  expect_identical(vec_radix_order(x)[3:4], c(2L, 4L))
})

test_that("`NA` order defaults to last", {
  x <- c(1L, NA_integer_, 3L)
  expect_identical(vec_radix_order(x), c(1L, 3L, 2L))
})

test_that("`NA` order can be first", {
  x <- c(1L, NA_integer_, 3L)
  expect_identical(vec_radix_order(x, na_last = FALSE), c(2L, 1L, 3L))
})

test_that("`decreasing` can be set to `TRUE`", {
  x <- c(1L, .Machine$integer.max, 3L)
  expect_identical(vec_radix_order(x, decreasing = TRUE), c(2L, 3L, 1L))
})

test_that("`decreasing` doesn't interact with `na_last`", {
  x <- c(3L, NA_integer_, 1L, 2L)

  expect_identical(
    x[vec_radix_order(x, na_last = TRUE, decreasing = FALSE)],
    c(1L, 2L, 3L, NA_integer_)
  )
  expect_identical(
    x[vec_radix_order(x, na_last = FALSE, decreasing = FALSE)],
    c(NA_integer_, 1L, 2L, 3L)
  )
  expect_identical(
    x[vec_radix_order(x, na_last = TRUE, decreasing = TRUE)],
    c(3L, 2L, 1L, NA_integer_)
  )
  expect_identical(
    x[vec_radix_order(x, na_last = FALSE, decreasing = TRUE)],
    c(NA_integer_, 3L, 2L, 1L)
  )
})
