context("test-type-integer64")

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

  expect_equal(vec_cast(NA, bit64::integer64()), bit64::as.integer64(NA))
  expect_equal(vec_cast(unspecified(2), bit64::integer64()), bit64::as.integer64(c(NA, NA)))

  expect_error(vec_cast(x, factor()), class = "vctrs_error_incompatible_cast")
  expect_error(vec_cast(factor(), x), class = "vctrs_error_incompatible_cast")
})

test_that("vec_ptype2 for integer64 works", {
  x <- bit64::as.integer64(1:10)
  expect_equal(vec_ptype2(x, x), bit64::integer64())

  expect_equal(vec_ptype2(x, 1L), bit64::integer64())
  expect_equal(vec_ptype2(1L, x), bit64::integer64())

  expect_equal(vec_ptype2(x, TRUE), bit64::integer64())
  expect_equal(vec_ptype2(TRUE, x), bit64::integer64())

  expect_equal(vec_ptype2(x, NA), bit64::integer64())
  expect_equal(vec_ptype2(NA, x), bit64::integer64())

  expect_equal(vec_ptype2(unspecified(), x), bit64::integer64())
  expect_equal(vec_ptype2(x, unspecified()), bit64::integer64())

  expect_error(vec_ptype2(x, 1))
  expect_error(vec_ptype2(1, x))

  expect_error(vec_ptype2(x, ""))
  expect_error(vec_ptype2("", x))

  expect_error(vec_ptype2(data.frame(), x))
  expect_error(vec_ptype2(x, data.frame()))
})

test_that("vec_ptype_abbr.integer64", {
  expect_equal(vec_ptype_abbr(bit64::as.integer64(1:10)), "int64")
  expect_equal(vec_ptype_full(bit64::as.integer64(1:10)), "integer64")
})

test_that("can sort integer64", {
  x <- bit64::as.integer64(c(-1, -3, -2, 1))
  expect_identical(vec_order(x), int(2, 3, 1, 4))
  expect_identical(x[vec_order(x)], bit64::as.integer64(c(-3, -2, -1, 1)))
})
