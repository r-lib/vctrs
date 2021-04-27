
test_that("casting of integer64 works", {
  x <- bit64::as.integer64(1:10)
  expect_equal(vec_cast(x, bit64::integer64()), x)

  expect_equal(vec_cast(x, integer()), 1:10)
  expect_equal(vec_cast(1:10, bit64::integer64()), x)

  expect_equal(vec_cast(x, double()), as.double(x))
  expect_equal(vec_cast(as.numeric(1:10), bit64::integer64()), x)

  expect_equal(vec_cast(x, logical()), rep(TRUE, 10L))
  expect_equal(vec_cast(c(TRUE, FALSE), bit64::integer64()), bit64::as.integer64(c(1, 0)))

  expect_equal(vec_cast(NA, bit64::integer64()), bit64::as.integer64(NA))
  expect_equal(vec_cast(unspecified(2), bit64::integer64()), bit64::as.integer64(c(NA, NA)))

  expect_error(vec_cast(x, factor()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(factor(), x), class = "vctrs_error_incompatible_type")

  # These used to be allowed
  expect_error(vec_cast(x, character()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(as.character(1:10), bit64::integer64()), class = "vctrs_error_incompatible_type")
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

  expect_error(vec_ptype2(x, 1), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(1, x), class = "vctrs_error_incompatible_type")

  expect_error(vec_ptype2(x, ""), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2("", x), class = "vctrs_error_incompatible_type")

  expect_error(vec_ptype2(data.frame(), x), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(x, data.frame()), class = "vctrs_error_incompatible_type")
})

test_that("vec_ptype_abbr.integer64", {
  expect_equal(vec_ptype_abbr(bit64::as.integer64(1:10)), "int64")
  expect_equal(vec_ptype_full(bit64::as.integer64(1:10)), "integer64")
})

test_that("can sort integer64", {
  x <- bit64::as.integer64(c(-1, -3, -2, 1))
  expect_identical(vec_order_radix(x), int(2, 3, 1, 4))
  expect_identical(x[vec_order_radix(x)], bit64::as.integer64(c(-3, -2, -1, 1)))
})

test_that("can slice integer64 objects of all dimensions", {
  x <- bit64::as.integer64(1:8)
  expect <- bit64::as.integer64(c(1, 3))
  expect_identical(vec_slice(x, c(1, 3)), expect)

  dim(x) <- c(4, 2)
  expect <- bit64::as.integer64(c(1, 3, 5, 7))
  dim(expect) <- c(2, 2)
  expect_identical(vec_slice(x, c(1, 3)), expect)

  dim(x) <- c(2, 2, 2)
  expect <- bit64::as.integer64(c(2, 4, 6, 8))
  dim(expect) <- c(1, 2, 2)
  expect_identical(vec_slice(x, 2), expect)
})

test_that("can slice integer64 objects with `NA_integer_`", {
  idx <- c(NA_integer_, 1)

  x <- bit64::as.integer64(1:8)
  expect <- bit64::as.integer64(c(NA, 1))
  expect_identical(vec_slice(x, idx), expect)

  dim(x) <- c(4, 2)
  expect <- bit64::as.integer64(c(NA, 1, NA, 5))
  dim(expect) <- c(2, 2)
  expect_identical(vec_slice(x, idx), expect)

  dim(x) <- c(2, 2, 2)
  expect <- bit64::as.integer64(c(NA, 1, NA, 3, NA, 5, NA, 7))
  dim(expect) <- c(2, 2, 2)
  expect_identical(vec_slice(x, idx), expect)
})

test_that("can init integer64 objects", {
  idx <- c(NA_integer_, NA_integer_)

  x <- bit64::as.integer64(1:8)
  expect_identical(vec_init(x, 2), vec_slice(x, idx))

  dim(x) <- c(4, 2)
  expect_identical(vec_init(x, 2), vec_slice(x, idx))

  dim(x) <- c(2, 2, 2)
  expect_identical(vec_init(x, 2), vec_slice(x, idx))
})

test_that("can chop integer64 objects with `NA_integer_` indices", {
  idx <- list(NA_integer_, 1)

  x <- bit64::as.integer64(1:8)
  expect <- list(
    bit64::as.integer64(NA),
    bit64::as.integer64(1)
  )

  expect_identical(vec_chop(x, idx), expect)

  dim(x) <- c(4, 2)
  expect <- list(
    bit64::as.integer64(c(NA, NA)),
    bit64::as.integer64(c(1, 5))
  )
  dim(expect[[1]]) <- c(1, 2)
  dim(expect[[2]]) <- c(1, 2)

  expect_identical(vec_chop(x, idx), expect)

  dim(x) <- c(2, 2, 2)
  expect <- list(
    bit64::as.integer64(c(NA, NA, NA, NA)),
    bit64::as.integer64(c(1, 3, 5, 7))
  )
  dim(expect[[1]]) <- c(1, 2, 2)
  dim(expect[[2]]) <- c(1, 2, 2)

  expect_identical(vec_chop(x, idx), expect)
})
