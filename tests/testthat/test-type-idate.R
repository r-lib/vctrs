# Never run on CRAN, even if they have data.table, because we don't regularly
# check these on CI and we don't want a change in data.table to force a CRAN
# failure for vctrs.
skip_on_cran()

# Avoids adding `data.table` to Suggests.
# These tests are only run on the devs' machines.
testthat_import_from("data.table", "as.IDate")

# `as.IDate()` drops names https://github.com/Rdatatable/data.table/issues/7252
as_IDate_with_names <- function(x) {
  out <- as.IDate(x)
  names(out) <- names(x)
  out
}

# ------------------------------------------------------------------------------
# ptype

test_that("ptype", {
  x <- as.IDate("2019-01-01")
  expect_identical(vec_ptype(x), as.IDate(integer()))

  # Note how names are correctly dropped (#2025)
  x <- as_IDate_with_names(c(a = "2019-01-01"))
  expect_identical(vec_ptype(x), as.IDate(integer()))
})

test_that("ptype abbr", {
  x <- as.IDate("2019-01-01")
  expect_identical(vec_ptype_abbr(x), "IDate")
})

test_that("ptype full", {
  x <- as.IDate("2019-01-01")
  expect_identical(vec_ptype_full(x), "IDate")
})

# ------------------------------------------------------------------------------
# ptype2

test_that("can get common type of IDate and IDate", {
  # Note how names are correctly dropped (#2025)
  x <- as_IDate_with_names(c(a = "2019-01-01"))
  expect <- as.IDate(integer())
  expect_identical(vec_ptype2(x, x), expect)
})

test_that("can't get common type of Date and IDate", {
  x <- as.Date("2019-01-01")
  y <- as.IDate("2019-01-01")

  expect_snapshot(error = TRUE, {
    vec_ptype2(x, y)
  })
  expect_snapshot(error = TRUE, {
    vec_ptype2(y, x)
  })
})

# ------------------------------------------------------------------------------
# cast

test_that("can cast IDate to IDate", {
  x <- as_IDate_with_names(c(a = "2019-01-01"))
  expect_identical(vec_cast(x, x), x)
})

test_that("can't cast Date to IDate", {
  x <- as.Date("2019-01-01")
  y <- as.IDate("2019-01-01")

  expect_snapshot(error = TRUE, {
    vec_cast(x, y)
  })
})

test_that("can't cast IDate to Date", {
  x <- as.IDate("2019-01-01")
  y <- as.Date("2019-01-01")

  expect_snapshot(error = TRUE, {
    vec_cast(x, y)
  })
})

# ------------------------------------------------------------------------------
# proxy / restore

test_that("vec_proxy", {
  # Retains integer type https://github.com/r-lib/vctrs/issues/1961
  x <- as.IDate("2019-01-01")
  expect_identical(vec_proxy(x), x)
})

test_that("vec_restore", {
  x <- as.IDate("2019-01-01")
  proxy <- vec_proxy(x)
  expect_identical(vec_restore(proxy, x), x)
})

test_that("proxy / restore retains names", {
  x <- as_IDate_with_names(c(a = "2019-01-01"))

  proxy <- vec_proxy(x)
  expect_named(proxy, "a")

  restored <- vec_restore(proxy, x)
  expect_named(restored, "a")
})

test_that("vec_proxy_equal, vec_proxy_compare, vec_proxy_order", {
  # Doesn't change type, stays integer storage as well
  x <- as.IDate("2019-01-01")

  expect_identical(vec_proxy_equal(x), x)
  expect_identical(vec_proxy_compare(x), x)
  expect_identical(vec_proxy_order(x), x)
})

# ------------------------------------------------------------------------------
# manipulation

test_that("ptype retains integer type", {
  x <- as.IDate(c("2019-01-01", "2019-01-02"))
  expect_identical(typeof(vec_ptype(x)), "integer")
  expect_identical(vec_ptype(x), as.IDate(integer()))
})

test_that("slicing retains integer type", {
  x <- as.IDate(c("2019-01-01", "2019-01-02"))
  expect_identical(typeof(vec_slice(x, 1)), "integer")
})

test_that("slicing retains names", {
  x <- as_IDate_with_names(c(a = "2019-01-01"))
  expect_identical(vec_slice(x, 1), x)
})
