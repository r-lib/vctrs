# ------------------------------------------------------------------------------
# vec_radix_order(<integer>)

test_that("can order integers", {
  x <- c(2L, 3L, 1L, 5L)
  expect_identical(int_radix_order(x), order(x))
})

test_that("can order sorted vector", {
  x <- 1:5
  expect_identical(int_radix_order(x), order(x))
})

test_that("orders correctly around the UINT8_MAX boundary", {
  x <- 251:255
  expect_identical(int_radix_order(x), order(x))
})

test_that("ordering on ties is done stably", {
  x <- c(1L, 3L, 1L, 3L)
  expect_identical(int_radix_order(x)[1:2], c(1L, 3L))
  expect_identical(int_radix_order(x)[3:4], c(2L, 4L))
})
