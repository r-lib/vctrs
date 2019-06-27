context("test-compare")

test_that("inputs must be vectors", {
  expect_error(vec_compare(NULL, 1), class = "vctrs_error_scalar_type")
  expect_error(vec_compare(1, NULL), class = "vctrs_error_scalar_type")
})

test_that("matches R ordering", {
  expect_same <- function(x, y) {
    expect_equal(vec_compare(!!x, !!y), cmp(!!x, !!y))
  }

  expect_same(c(NA, FALSE, TRUE), FALSE)
  expect_same(c(NA, -100L, 0L, 100L), 0L)
  expect_same(c(NA, -Inf, -100, 100, Inf), 0L)
  expect_same(c(NA, NaN, 0), NA)
  expect_same(c(NA, "a", "b", "c"), "b")
})

test_that("NAs equal when requested", {
  expect_value <- function(x, y, val, .ptype = NULL) {
    expect_equal(vec_compare(!!x, !!y, .ptype = .ptype, na_equal = TRUE), !!val)
  }

  expect_value(NA, NA, 0L)
  expect_value(NA, FALSE, -1L)
  expect_value(FALSE, NA, 1L)

  expect_value(NA_integer_, NA_integer_, 0L)
  expect_value(NA_integer_, 0L, -1L)
  expect_value(0L, NA_integer_, 1L)

  expect_value(NA_character_, NA_character_, 0L)
  expect_value(NA_character_, "", -1L)
  expect_value("", NA_character_, 1L)

  expect_value(0, NA_real_, 1L)
  expect_value(0, NaN, 1L)
  expect_value(0, 0, 0L)
  expect_value(NA_real_, NA_real_, 0L)
  expect_value(NA_real_, NaN, 1L)
  expect_value(NA_real_, 0, -1L)
  expect_value(NaN, NA_real_, -1L)
  expect_value(NaN, NaN, 0L)
  expect_value(NaN, 0, -1L)
})

test_that("data frames are compared column by column", {
  df1 <- data.frame(x = c(1, 1, 1), y = c(-1, 0, 1))

  expect_equal(vec_compare(df1, df1[2, ]), c(-1, 0, 1))
  expect_equal(vec_compare(df1[1], df1[2, 1, drop = FALSE]), c(0, 0, 0))
  expect_equal(vec_compare(df1[2], df1[2, 2, drop = FALSE]), c(-1, 0, 1))
  expect_equal(vec_compare(df1[2:1], df1[2, 2:1]), c(-1, 0, 1))
})

test_that("C code doesn't crash with bad inputs", {
  df <- data.frame(x = c(1, 1, 1), y = c(-1, 0, 1))

  expect_error(.Call(vctrs_compare, df, df[1], TRUE), "not comparable")
  expect_error(.Call(vctrs_compare, df, setNames(df, c("x", "z")), TRUE), "not comparable")
})

test_that("xtfrm.vctrs_vctr works for variety of base classes", {
  df <- data.frame(x = c(NA, 1, 1), y = c(1, 2, 1))
  expect_equal(xtfrm.vctrs_vctr(df), c(3, 2, 1))

  x <- c(2, 3, 1)
  expect_equal(xtfrm.vctrs_vctr(x), x)
  expect_equal(xtfrm.vctrs_vctr(letters[x]), x)
})

test_that("vec_proxy_compare() refuses to deal with lists", {
  expect_error(vec_proxy_compare(list()), class = "vctrs_error_unsupported")
})

test_that("vec_compare() calls vec_proxy_compare()", {
  scoped_global_bindings(
    vec_proxy_compare.vctrs_foobar = function(x) rev(x),
    vec_ptype2.integer.vctrs_foobar = function(...) foobar(int()),
    vec_ptype2.vctrs_foobar = function(...) foobar(int()),
    vec_cast.vctrs_foobar = function(x, ...) x
  )
  expect_identical(vec_compare(1:3, 1:3), int(0, 0, 0))
  expect_identical(vec_compare(1:3, foobar(1:3)), int(-1, 0, 1))
})

test_that("vec_proxy_compare() preserves data frames and vectors", {
  df <- data_frame(x = 1:2, y = c("a", "b"))
  expect_identical(vec_proxy_compare(df), df)

  x <- c(NA, "a", "b", "c")
  expect_identical(vec_proxy_compare(x), x)
})


# order/sort --------------------------------------------------------------

test_that("can request NAs sorted first", {
  expect_equal(vec_order(c(1, NA), "asc", "large"), 1:2)
  expect_equal(vec_order(c(1, NA), "desc", "large"), 2:1)

  expect_equal(vec_order(c(1, NA), "asc", "small"), 2:1)
  expect_equal(vec_order(c(1, NA), "desc", "small"), 1:2)
})

test_that("can sort data frames", {
  df <- data.frame(x = c(1, 2, 1), y = c(1, 2, 2))

  out1 <- vec_sort(df)
  expect_equal(out1, data.frame(x = c(1, 1, 2), y = c(1, 2, 2)))

  out2 <- vec_sort(df, "desc")
  expect_equal(out2, data.frame(x = c(2, 1, 1), y = c(2, 2, 1)))
})

test_that("can sort empty data frames (#356)", {
  df1 <- data.frame()
  expect_equal(vec_sort(df1), df1)

  df2 <- data.frame(x = numeric(), y = integer())
  expect_equal(vec_sort(df2), df2)
})

test_that("can order tibbles that contain non-comparable objects", {
  expect_equal(vec_order(data_frame(list(10, 2, 1))), 1:3)
})

test_that("can order matrices and arrays (#306)", {
  x <- matrix(c(1, 1, 1, 1, 2, 1), ncol = 2)
  expect_identical(vec_order(x), c(1L, 3L, 2L))

  x <- array(1:8, c(2, 2, 2))
  x[2] <- 1
  x[3] <- 5
  expect_identical(vec_order(x), 2:1)
})

test_that("can order empty data frames (#356)", {
  df1 <- data.frame()
  expect_equal(vec_order(df1), integer())

  df2 <- data.frame(x = numeric(), y = integer())
  expect_equal(vec_order(df2), integer())
})
