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

# ------------------------------------------------------------------------------
# vec_radix_order(<data.frame>)

test_that("can order with multiple pre-sorted keys", {
  df <- data.frame(x = 1:2, y = 3:4)
  expect_identical(vec_radix_order(df), 1:2)
})

test_that("first column has ordering presedence", {
  df <- data.frame(x = c(3L, 2L, 1L), y = c(1L, 2L, 3L))
  expect_identical(vec_radix_order(df), 3:1)
})

test_that("secondary columns break ties", {
  df <- data.frame(
    x = c(1L, 2L, 1L),
    y = c(3L, 2L, 1L)
  )
  expect_identical(vec_radix_order(df), c(3L, 1L, 2L))
})

test_that("orders correctly when first column is already ordered but second isn't", {
  df <- data.frame(
    x = c(1L, 1L, 2L, 2L),
    y = c(3L, 2L, 4L, 1L)
  )
  expect_identical(vec_radix_order(df), c(2L, 1L, 4L, 3L))
})
