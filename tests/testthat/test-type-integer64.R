
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
  expect_identical(vec_order(x), int(2, 3, 1, 4))
  expect_identical(x[vec_order(x)], bit64::as.integer64(c(-3, -2, -1, 1)))
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

test_that("equality proxy converts atomic input to data frames of doubles", {
  x <- bit64::as.integer64(1)
  expect_identical(
    vec_proxy_equal(x),
    data_frame(left = 2147483648, right = 1)
  )
})

test_that("equality proxy works with 1-D arrays", {
  x <- bit64::as.integer64(1:6)
  y <- x
  dim(y) <- 6

  expect_identical(
    vec_proxy_equal(x),
    vec_proxy_equal(y)
  )
})

test_that("equality proxy on >=2-D input converts to data frame and proxies each column", {
  x <- bit64::as.integer64(1:8)
  dim(x) <- c(2, 2, 2)

  proxy1 <- integer64_proxy(x[1:2, 1, 1])
  proxy2 <- integer64_proxy(x[1:2, 2, 1])
  proxy3 <- integer64_proxy(x[1:2, 1, 2])
  proxy4 <- integer64_proxy(x[1:2, 2, 2])

  expect_identical(
    vec_proxy_equal(x),
    vec_cbind(proxy1, proxy2, proxy3, proxy4, .name_repair = "minimal")
  )
})

test_that("can detect missing values with integer64 (#1304)", {
  x <- bit64::as.integer64(c(NA, NA, 2, NA, 2, 2))

  expect_identical(vec_detect_missing(x), c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

  dim(x) <- c(3, 2)
  expect_identical(vec_detect_missing(x), c(TRUE, FALSE, FALSE))
})

test_that("can fill missing values with integer64", {
  x <- bit64::as.integer64(c(NA, NA, 2, NA, 2, 2))

  expect <- bit64::as.integer64(c(NA, NA, 2, 2, 2, 2))
  expect_identical(vec_fill_missing(x, "down"), expect)

  dim(x) <- c(3, 2)
  expect <- bit64::as.integer64(c(NA, NA, 2, 2, 2, 2))
  dim(expect) <- c(3, 2)
  expect_identical(vec_fill_missing(x, "up"), expect)
})

test_that("can compare values with integer64", {
  x <- bit64::as.integer64(c(1, NA, 2))
  y <- bit64::as.integer64(c(0, 2, 3))

  expect_identical(vec_compare(x, y), c(1L, NA, -1L))

  x <- bit64::as.integer64(1:8)
  y <- bit64::as.integer64(c(1, 2, 1, 5, 1, 5, 1, 5))
  dim(x) <- c(2, 2, 2)
  dim(y) <- c(2, 2, 2)

  expect_identical(vec_compare(x, y), c(1L, -1L))
})

test_that("integer64 <-> data frame works as expected", {
  x <- bit64::as.integer64(c(-2, -1, 0, 1))
  proxy <- integer64_proxy(x)

  expect_identical(proxy$left, c(2147483647, 2147483647, 2147483648, 2147483648))
  expect_identical(proxy$right, c(4294967294, 4294967295, 0, 1))
  expect_identical(integer64_restore(proxy), x)

  x <- bit64::as.integer64("9223372036854775807") + -1:0
  proxy <- integer64_proxy(x)

  expect_identical(proxy$left, c(4294967295, 4294967295))
  expect_identical(proxy$right, c(4294967294, 4294967295))
  expect_identical(integer64_restore(proxy), x)

  x <- bit64::as.integer64("-9223372036854775807") + 0:1
  proxy <- integer64_proxy(x)

  expect_identical(proxy$left, c(0, 0))
  expect_identical(proxy$right, c(1, 2))
  expect_identical(integer64_restore(proxy), x)

  x <- bit64::as.integer64(NA)
  proxy <- integer64_proxy(x)

  expect_identical(proxy$left, NA_real_)
  expect_identical(proxy$right, NA_real_)
  expect_identical(integer64_restore(proxy), x)
})

test_that("`integer64_proxy()` doesn't allow arrays", {
  x <- bit64::as.integer64(1:6)
  dim(x) <- c(3, 2)
  expect_error(integer64_proxy(x), "should not have a `dim` attribute")
})
