.rando <- setClass("rando", contains = "numeric", slots = list(.Data = "numeric"))
rando <- function(n = 0) {
  withr::with_seed(20200311, .rando(.Data = runif(n)))
}

as_rando <- function(x) {
  rando(length(x))
}

setMethod("[", "rando", function(x, i, j, ..., drop = TRUE) {
  new_n <- length(vec_as_location(i, length(x@.Data), names(x@.Data)))
  rando(new_n)
})

test_that("basics", {
  x <- rando(10)

  expect_true(vec_is(x))
  expect_equal(vec_size(x), 10)
  expect_identical(vec_ptype_common(x, x), vec_ptype(x))
})

test_that("casting of rando works", {
  x <- as_rando(1:10)
  expect_equal(vec_cast(x, rando()), x)

  expect_equal(vec_cast(NA, rando()), as_rando(NA))
  expect_equal(vec_cast(unspecified(2), rando()), as_rando(c(NA, NA)))

  expect_error(vec_cast(x, factor()), class = "vctrs_error_incompatible_cast")
  expect_error(vec_cast(factor(), x), class = "vctrs_error_incompatible_cast")
})

test_that("vec_ptype2 for rando works", {
  x <- as_rando(1:10)
  expect_equal(vec_ptype(vec_ptype2(x, x)), rando())

  expect_equal(vec_ptype2(x, NA), rando())
  expect_equal(vec_ptype2(NA, x), rando())

  expect_equal(vec_ptype2(unspecified(), x), rando())
  expect_equal(vec_ptype2(x, unspecified()), rando())

  expect_error(vec_ptype2(x, 1))
  expect_error(vec_ptype2(1, x))

  expect_error(vec_ptype2(x, ""))
  expect_error(vec_ptype2("", x))

  expect_error(vec_ptype2(data.frame(), x))
  expect_error(vec_ptype2(x, data.frame()))
})

test_that("vec_ptype_abbr.rando", {
  expect_equal(vec_ptype_abbr(as_rando(1:10)), "rando")
  expect_equal(vec_ptype_full(as_rando(1:10)), "rando")
})

test_that("can sort rando", {
  x <- as_rando(c(-1, -3, -2, 1))
  expect_silent(vec_order(x))
})

test_that("can slice rando objects of all dimensions", {
  x <- as_rando(1:8)
  expect <- as_rando(c(1, 3))
  expect_identical(vec_slice(x, c(1, 3)), expect)
})

test_that("can slice rando objects with `NA_integer_`", {
  idx <- c(NA_integer_, 1)

  x <- as_rando(1:8)
  expect <- as_rando(c(NA, 1))
  expect_identical(vec_slice(x, idx), expect)
})

test_that("can init rando objects", {
  idx <- c(NA_integer_, NA_integer_)

  x <- as_rando(1:8)
  expect_identical(vec_init(x, 2), vec_slice(x, idx))

  dim(x) <- c(4, 2)
  expect_identical(vec_init(x, 2), vec_slice(x, idx))

  dim(x) <- c(2, 2, 2)
  expect_identical(vec_init(x, 2), vec_slice(x, idx))
})

test_that("can chop rando objects with `NA_integer_` indices", {
  idx <- list(NA_integer_, 1)

  x <- as_rando(1:8)
  expect <- list(
    as_rando(NA),
    as_rando(1)
  )

  expect_identical(vec_chop(x, idx), expect)
})

test_that("proxy and data", {
  x <- rando(10)

  expect_identical(vec_ptype(vec_proxy(x)), x[0])
  expect_identical(vec_data(x), x@.Data)

  expect_false(isS4(vec_data(x)))

  expect_s4_class(vec_restore(vec_data(x), x), "rando")
  expect_true(isS4(vec_restore(vec_data(x), x)))
})
