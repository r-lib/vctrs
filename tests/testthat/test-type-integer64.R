context("test-type-integer64")

test_that("new_int64 works", {
  x <- new_int64()
  expect_is(x, "integer64")
  expect_error(new_int64(1:10))
})

test_that("casting of integer64 works", {
  x <- bit64::as.integer64(1:10)
  expect_equal(vec_cast(x, bit64::integer64()), x)

  expect_equal(vec_cast(x, integer()), 1:10)
  expect_equal(vec_cast(1:10, bit64::integer64()), x)

  expect_equal(vec_cast(x, double()), as.double(x))
  expect_equal(vec_cast(as.numeric(1:10), bit64::integer64()), x)

  expect_equal(vec_cast(x, character()), as.character(x))
  expect_equal(vec_cast(as.character(1:10), bit64::integer64()), x)

  expect_equal(vec_cast(x, logical()), rep(TRUE, 10L))
  expect_equal(vec_cast(c(TRUE, FALSE), bit64::integer64()), bit64::as.integer64(c(1, 0)))

  expect_error(vec_cast(x, factor()))
  expect_error(vec_cast(factor(c("a", "b"))))
})

test_that("vec_type2 for integer64 works", {
  x <- bit64::as.integer64(1:10)
  expect_equal(vec_type2(x, x), bit64::integer64())

  expect_equal(vec_type2(x, 1L), bit64::integer64())
  expect_equal(vec_type2(1L, x), bit64::integer64())

  expect_equal(vec_type2(x, TRUE), bit64::integer64())
  expect_equal(vec_type2(TRUE, x), bit64::integer64())

  expect_error(vec_type2(x, 1))
  expect_error(vec_type2(1, x))

  expect_error(vec_type2(x, ""))
  expect_error(vec_type2("", x))

  expect_error(vec_type2(data.frame(), x))
  expect_error(vec_type2(x, data.frame()))
})

test_that("vec_ptype_abbr.integer64", {
  expect_equal(vec_ptype_abbr(bit64::as.integer64(1:10)), "int64")
  expect_equal(vec_ptype_full(bit64::as.integer64(1:10)), "integer64")
})

# arithmetic --------------------------------------------------------------

test_that("default is error", {
  x <- new_int64(bit64::as.integer64(1:10))
  s <- character()

  expect_error(vec_arith("+", x, s), class = "error_incompatible_op")
  expect_error(vec_arith("+", s, x), class = "error_incompatible_op")
})

test_that("integer64 vs integer64", {
  x <- new_int64(bit64::as.integer64(1:10))

  expect_equal(vec_arith("+", x, x), x + x)
  expect_is(vec_arith("+", x, x), "integer64")

  expect_equal(vec_arith("-", x, x), x - x)
  expect_is(vec_arith("-", x, x), "integer64")
})

test_that("integer64 vs integer", {
  x <- new_int64(bit64::as.integer64(1:10))
  y <- 1:10

  expect_equal(vec_arith("+", x, y), x + y)
  expect_equal(vec_arith("+", y, x), x + y)
  expect_is(vec_arith("+", x, y), "integer64")
  expect_is(vec_arith("+", y, x), "integer64")

  expect_equal(vec_arith("-", x, y), x - y)
  expect_equal(vec_arith("-", y, x), y - x)
  expect_is(vec_arith("-", x, y), "integer64")
  expect_is(vec_arith("-", y, x), "integer64")
})

test_that("integer64 vs double", {
  x <- new_int64(bit64::as.integer64(1:10))
  y <- as.numeric(1:10)

  expect_equal(vec_arith("+", x, y), x + y)
  expect_equal(vec_arith("+", y, x), x + y)
  expect_is(vec_arith("+", x, y), "integer64")
  expect_is(vec_arith("+", y, x), "integer64")

  expect_equal(vec_arith("-", x, y), x - y)
  expect_equal(vec_arith("-", y, x), y - x)
  expect_is(vec_arith("-", x, y), "integer64")
  expect_is(vec_arith("-", y, x), "integer64")
})

test_that("integer64 vs logical", {
  x <- new_int64(bit64::as.integer64(1:2))
  y <- c(TRUE, FALSE)

  expect_equal(vec_arith("+", x, y), x + y)
  expect_equal(vec_arith("+", y, x), x + y)
  expect_is(vec_arith("+", x, y), "integer64")
  expect_is(vec_arith("+", y, x), "integer64")

  expect_equal(vec_arith("-", x, y), x - y)
  expect_equal(vec_arith("-", y, x), y - x)
  expect_is(vec_arith("-", x, y), "integer64")
  expect_is(vec_arith("-", y, x), "integer64")
})
