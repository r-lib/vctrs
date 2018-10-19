context("test-bit64")

test_that("casting of integer64 works", {
  x <- bit64::as.integer64(1:10)
  expect_equal(vec_cast(x, bit64::integer64()), x)

  expect_equal(vec_cast(x, integer()), 1:10)
  expect_equal(vec_cast(x, double()), as.numeric(1:10))
  expect_equal(vec_cast(x, character()), as.character(1:10))
  expect_equal(vec_cast(x, logical()), rep(TRUE, 10L))
  expect_error(vec_cast(x, factor()))

  expect_equal(vec_cast(1:10, bit64::integer64()), x)
  expect_equal(vec_cast(as.numeric(1:10), bit64::integer64()), x)
  expect_equal(vec_cast(as.character(1:10), bit64::integer64()), x)
  expect_equal(vec_cast(c(TRUE, FALSE), bit64::integer64()), bit64::as.integer64(c(1, 0)))
  expect_equal(vec_cast(factor(c("a", "b")), bit64::integer64()), bit64::as.integer64(c(1, 2)))
})

test_that("vec_type2 for integer64 works", {
  x <- bit64::as.integer64(1:10)
  expect_equal(vec_type2(x, x), bit64::integer64())

  expect_equal(vec_type2(x, 1L), bit64::integer64())
  expect_equal(vec_type2(1L, x), bit64::integer64())

  expect_equal(vec_type2(x, 1), bit64::integer64())
  expect_equal(vec_type2(1, x), bit64::integer64())

  expect_equal(vec_type2(x, ""), character())
  expect_equal(vec_type2("", x), character())

  expect_equal(vec_type2(x, TRUE), bit64::integer64())
  expect_equal(vec_type2(TRUE, x), bit64::integer64())
})
