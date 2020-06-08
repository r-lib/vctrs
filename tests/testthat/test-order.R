# # ------------------------------------------------------------------------------
# # vec_radix_order(<integer>)
#
# test_that("can order integers", {
#   x <- c(2L, 3L, 1L, 5L)
#   expect_identical(vec_radix_order(x), order(x))
# })
#
# test_that("can order sorted vector", {
#   x <- 1:5
#   expect_identical(vec_radix_order(x), order(x))
# })
#
# test_that("ordering on ties is done stably", {
#   x <- c(1L, 3L, 1L, 3L)
#   expect_identical(vec_radix_order(x)[1:2], c(1L, 3L))
#   expect_identical(vec_radix_order(x)[3:4], c(2L, 4L))
# })
#
# test_that("`NA` order defaults to last", {
#   x <- c(1L, NA_integer_, 3L)
#   expect_identical(vec_radix_order(x), c(1L, 3L, 2L))
# })
#
# test_that("`NA` order can be first", {
#   x <- c(1L, NA_integer_, 3L)
#   expect_identical(vec_radix_order(x, na_last = FALSE), c(2L, 1L, 3L))
# })
#
# test_that("`decreasing` can be set to `TRUE`", {
#   x <- c(1L, .Machine$integer.max, 3L)
#   expect_identical(vec_radix_order(x, decreasing = TRUE), c(2L, 3L, 1L))
# })
#
# test_that("`decreasing` doesn't interact with `na_last`", {
#   x <- c(3L, NA_integer_, 1L, 2L)
#
#   expect_identical(
#     x[vec_radix_order(x, na_last = TRUE, decreasing = FALSE)],
#     c(1L, 2L, 3L, NA_integer_)
#   )
#   expect_identical(
#     x[vec_radix_order(x, na_last = FALSE, decreasing = FALSE)],
#     c(NA_integer_, 1L, 2L, 3L)
#   )
#   expect_identical(
#     x[vec_radix_order(x, na_last = TRUE, decreasing = TRUE)],
#     c(3L, 2L, 1L, NA_integer_)
#   )
#   expect_identical(
#     x[vec_radix_order(x, na_last = FALSE, decreasing = TRUE)],
#     c(NA_integer_, 3L, 2L, 1L)
#   )
# })
#
# # ------------------------------------------------------------------------------
# # vec_radix_order(<data.frame>)
#
# test_that("can order with multiple pre-sorted keys", {
#   df <- data.frame(x = 1:2, y = 3:4)
#   expect_identical(vec_radix_order(df), 1:2)
# })
#
# test_that("first column has ordering presedence", {
#   df <- data.frame(x = c(3L, 2L, 1L), y = c(1L, 2L, 3L))
#   expect_identical(vec_radix_order(df), 3:1)
# })
#
# test_that("secondary columns break ties", {
#   df <- data.frame(
#     x = c(1L, 2L, 1L),
#     y = c(3L, 2L, 1L)
#   )
#   expect_identical(vec_radix_order(df), c(3L, 1L, 2L))
# })
#
# test_that("orders correctly when first column is already ordered but second isn't", {
#   df <- data.frame(
#     x = c(1L, 1L, 2L, 2L),
#     y = c(3L, 2L, 4L, 1L)
#   )
#   expect_identical(vec_radix_order(df), c(2L, 1L, 4L, 3L))
# })
#
# test_that("`decreasing` is recycled", {
#   df <- data.frame(
#     x = c(1L, 1L, 2L, 2L),
#     y = c(3L, 2L, 4L, 1L)
#   )
#   expect_identical(vec_radix_order(df, decreasing = TRUE), c(3L, 4L, 1L, 2L))
# })
#
# test_that("`decreasing` can be a vector", {
#   df <- data.frame(
#     x = c(1L, 1L, 2L, 2L),
#     y = c(3L, 2L, 4L, 1L)
#   )
#   expect_identical(vec_radix_order(df, decreasing = c(TRUE, FALSE)), c(4L, 3L, 2L, 1L))
# })
#
# # ------------------------------------------------------------------------------
# # vec_radix_order() - error checking
#
# test_that("`na_last` is checked", {
#   expect_error(vec_radix_order(1L, na_last = "x"), "`TRUE` or `FALSE`")
#   expect_error(vec_radix_order(1L, na_last = c(TRUE, TRUE)), "`TRUE` or `FALSE`")
#   expect_error(vec_radix_order(1L, na_last = NA), "`TRUE` or `FALSE`")
# })
#
# test_that("`decreasing` is checked", {
#   expect_error(vec_radix_order(1L, decreasing = "x"), "must be logical")
#   expect_error(vec_radix_order(1L, decreasing = c(TRUE, TRUE)), "length 1")
#   expect_error(vec_radix_order(1L, decreasing = NA), "missing values")
#   expect_error(vec_radix_order(data.frame(x = 1), decreasing = c(TRUE, TRUE)), "length 1 or")
# })
