context("test-arith")

test_that("logical/integer/numeric works", {
  expect_equal(vec_arith("+", TRUE, TRUE), 2L)
  expect_equal(vec_arith("+", TRUE, 1L), 2L)
  expect_equal(vec_arith("+", TRUE, 1), 2)
  expect_equal(vec_arith("+", 1L, TRUE), 2L)
  expect_equal(vec_arith("+", 1L, 1L), 2L)
  expect_equal(vec_arith("+", 1L, 1), 2)
  expect_equal(vec_arith("+", 1, TRUE), 2L)
  expect_equal(vec_arith("+", 1, 1L), 2L)
  expect_equal(vec_arith("+", 1, 1), 2)
})

test_that("default is error", {
  f <- new_vctr(1:10, class = "foo")

  expect_error(vec_arith("+", f, 1), class = "error_incompatible_op")

  expect_error(vec_arith("+", TRUE, f), class = "error_incompatible_op")
  expect_error(vec_arith("+", 1L, f), class = "error_incompatible_op")
  expect_error(vec_arith("+", 1, f), class = "error_incompatible_op")
})

# Date/times --------------------------------------------------------------

test_that("default is error", {
  d <- as.Date("2018-01-01")
  dt <- as.POSIXct("2018-01-02 12:00")
  t <- as.difftime(12, units = "hours")
  f <- factor("x")

  expect_error(vec_arith("+", d, f), class = "error_incompatible_op")
  expect_error(vec_arith("+", dt, f), class = "error_incompatible_op")
  expect_error(vec_arith("+", t, f), class = "error_incompatible_op")
})

test_that("date-time vs date-time", {
  d <- as.Date("2018-01-01")
  dt <- as.POSIXct(d)

  expect_error(vec_arith("+", d, d), class = "error_incompatible_op")
  expect_equal(vec_arith("-", d, d), d - d)

  expect_error(vec_arith("+", d, dt), class = "error_incompatible_op")
  expect_equal(vec_arith("-", d, dt), difftime(d, dt))

  expect_error(vec_arith("+", dt, d), class = "error_incompatible_op")
  expect_equal(vec_arith("-", dt, d), difftime(dt, d))

  expect_error(vec_arith("+", dt, dt), class = "error_incompatible_op")
  expect_equal(vec_arith("-", dt, dt), dt - dt)
})

test_that("date-time vs numeric", {
   d <- as.Date("2018-01-01")
   dt <- as.POSIXct(d)

   expect_equal(vec_arith("+", d, 1), d + 1)
   expect_equal(vec_arith("+", 1, d), d + 1)
   expect_equal(vec_arith("-", d, 1), d - 1)
   expect_error(vec_arith("-", 1, d), class = "error_incompatible_op")

   expect_error(vec_arith("*", 1, d), class = "error_incompatible_op")
   expect_error(vec_arith("*", d, 1), class = "error_incompatible_op")
})

test_that("date-time vs difftime", {
  d <- as.Date("2018-01-01")
  dt <- as.POSIXct(d)
  t <- as.difftime(1, units = "days")

  expect_equal(vec_arith("+", dt, t), dt + t)
  expect_equal(vec_arith("+", d, t), d + t)
  expect_equal(vec_arith("-", dt, t), dt - t)
  expect_equal(vec_arith("-", d, t), d - t)

  expect_equal(vec_arith("+", t, dt), dt + t)
  expect_equal(vec_arith("+", t, d), d + t)
  expect_error(vec_arith("-", t, dt), class = "error_incompatible_op")
  expect_error(vec_arith("-", t, d), class = "error_incompatible_op")
})

test_that("difftime vs difftime/numeric", {
  t <- as.difftime(12, units = "hours")

  expect_equal(vec_arith("-", t, MISSING()), -t)
  expect_equal(vec_arith("+", t, MISSING()), t)

  expect_equal(vec_arith("-", t, t), t - t)
  expect_equal(vec_arith("-", t, 1), t - 1)
  expect_equal(vec_arith("-", 1, t), 1 - t)

  expect_equal(vec_arith("+", t, t), 2 * t)
  expect_equal(vec_arith("+", t, 1), t + 1)
  expect_equal(vec_arith("+", 1, t), t + 1)

  expect_equal(vec_arith("*", 2, t), 2 * t)
  expect_equal(vec_arith("*", t, 2), 2 * t)
  expect_error(vec_arith("*", t, t), class = "error_incompatible_op")

  expect_equal(vec_arith("/", t, 2), t / 2)
  expect_error(vec_arith("/", 2, t), class = "error_incompatible_op")

  expect_equal(vec_arith("/", t, t), 1)
  expect_equal(vec_arith("%/%", t, t), 1)
  expect_equal(vec_arith("%%", t, t), 0)
})
