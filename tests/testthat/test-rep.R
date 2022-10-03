# ------------------------------------------------------------------------------
# vec_rep()

test_that("`vec_rep()` can repeat vectors", {
  expect_identical(vec_rep(1:2, 5), rep(1:2, 5))
  expect_identical(vec_rep(list(1, "x"), 5), rep(list(1, "x"), 5))
})

test_that("`vec_rep()` repeats data frames row wise", {
  x <- data.frame(x = 1:2, y = 3:4)
  expect_identical(vec_rep(x, 2), vec_slice(x, c(1:2, 1:2)))
})

test_that("`vec_rep()` can repeat 0 `times`", {
  expect_identical(vec_rep(1, 0), numeric())
})

test_that("`vec_rep()` can repeat 1 `time`", {
  expect_identical(vec_rep(1:3, 1), 1:3)
})

test_that("`vec_rep()` can repeat `x` of size 1", {
  expect_identical(vec_rep(1, 2), c(1, 1))
})

test_that("`vec_rep()` errors on long vector output", {
  # Exact error message may be platform specific
  expect_error(vec_rep(1:2, .Machine$integer.max), "output size must be less than")
})

test_that("`vec_rep()` validates `times`", {
  expect_snapshot({
    (expect_error(my_vec_rep(1, "x"), class = "vctrs_error_incompatible_type"))
    (expect_error(my_vec_rep(1, c(1, 2))))
    (expect_error(my_vec_rep(1, -1)))
    (expect_error(my_vec_rep(1, NA_integer_)))
  })
})

# ------------------------------------------------------------------------------
# vec_rep_each()

test_that("`vec_rep_each()` can repeat each element of vectors", {
  expect_identical(vec_rep_each(1:2, 5), rep(1:2, each = 5))
  expect_identical(vec_rep_each(list(1, "x"), 5), rep(list(1, "x"), each = 5))
})

test_that("`vec_rep_each()` `times` is vectorized", {
  expect_identical(vec_rep_each(1:2, 1:2), rep(1:2, times = 1:2))
})

test_that("`vec_rep_each()` repeats data frames row wise", {
  x <- data.frame(x = 1:2, y = 3:4)
  expect_identical(vec_rep_each(x, c(2, 1)), vec_slice(x, c(1, 1, 2)))
})

test_that("`vec_rep_each()` can repeat 0 `times`", {
  expect_identical(vec_rep_each(1:2, 0), integer())
})

test_that("`vec_rep_each()` finalizes type when repeating 0 times (#1673)", {
  expect_identical(vec_rep_each(NA, 0), logical())
})

test_that("`vec_rep_each()` retains names when repeating 0 times (#1673)", {
  x <- c(a = 1, b = 2)
  expect_identical(vec_rep_each(x, 0), named(numeric()))
})

test_that("`vec_rep_each()` can repeat 1 `time`", {
  expect_identical(vec_rep_each(1:2, 1), 1:2)
})

test_that("`vec_rep_each()` errors on long vector output", {
  # Exact error message may be platform specific
  expect_error(vec_rep_each(1:2, .Machine$integer.max), "output size must be less than")
})

test_that("`vec_rep_each()` validates `times`", {
  expect_snapshot({
    (expect_error(my_vec_rep_each(1, "x"), class = "vctrs_error_incompatible_type"))
    (expect_error(my_vec_rep_each(1, -1)))
    (expect_error(my_vec_rep_each(c(1, 2), c(1, -1))))
    (expect_error(my_vec_rep_each(1, NA_integer_)))
    (expect_error(my_vec_rep_each(c(1, 2), c(1, NA_integer_))))
  })
})

test_that("`vec_rep_each()` uses recyclying errors", {
  expect_snapshot({
    (expect_error(my_vec_rep_each(1:2, 1:3), class = "vctrs_error_recycle_incompatible_size"))
  })
})

# ------------------------------------------------------------------------------

test_that("`vec_rep()` validates `times`", {
  expect_snapshot(error = TRUE, my_vec_rep(1, "x"))
  expect_snapshot(error = TRUE, my_vec_rep(1, c(1, 2)))
  expect_snapshot(error = TRUE, my_vec_rep(1, -1))
  expect_snapshot(error = TRUE, my_vec_rep(1, NA_integer_))
})

test_that("`vec_rep_each()` validates `times`", {
  expect_snapshot(error = TRUE, my_vec_rep_each(1, "x"))
  expect_snapshot(error = TRUE, my_vec_rep_each(1, -1))
  expect_snapshot(error = TRUE, my_vec_rep_each(c(1, 2), c(1, -1)))
  expect_snapshot(error = TRUE, my_vec_rep_each(1, NA_integer_))
  expect_snapshot(error = TRUE, my_vec_rep_each(c(1, 2), c(1, NA_integer_)))
})

test_that("`vec_rep_each()` uses recyclying errors", {
  expect_snapshot(error = TRUE, my_vec_rep_each(1:2, 1:3))
})

# vec_unrep --------------------------------------------------------------------

test_that("can unrep a vector", {
  x <- c(1, 3, 3, 1, 5, 5, 6)

  expect <- data_frame(
    key = c(1, 3, 1, 5, 6),
    times = c(1L, 2L, 1L, 2L, 1L)
  )

  expect_identical(vec_unrep(x), expect)
})

test_that("can unrep a data frame", {
  df <- data_frame(
    x = c(1, 1, 2, 2, 2),
    y = c(1, 1, 1, 1, 2)
  )

  expect <- data_frame(
    key = vec_slice(df, c(1, 3, 5)),
    times = c(2L, 2L, 1L)
  )

  expect_identical(vec_unrep(df), expect)
})

test_that("works with size zero input", {
  expect_identical(vec_unrep(integer()), data_frame(key = integer(), times = integer()))
})

test_that("can roundtrip empty input", {
  x <- integer()
  compressed <- vec_unrep(x)
  expect_identical(vec_rep_each(compressed$key, compressed$times), x)

  x <- data_frame()
  compressed <- vec_unrep(x)
  expect_identical(vec_rep_each(compressed$key, compressed$times), x)
})

test_that("works with data frames with rows but no columns", {
  x <- data_frame(.size = 5)
  expect <- data_frame(key = data_frame(.size = 1L), times = 5L)
  expect_identical(vec_unrep(x), expect)
})
