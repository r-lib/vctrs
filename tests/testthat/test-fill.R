test_that("works with empty input", {
  x <- integer()

  expect_identical(vec_fill_missing(x, direction = "down"), x)
  expect_identical(vec_fill_missing(x, direction = "up"), x)
  expect_identical(vec_fill_missing(x, direction = "downup"), x)
  expect_identical(vec_fill_missing(x, direction = "updown"), x)
  expect_identical(vec_fill_missing(x, direction = "downup", max_fill = 1), x)
  expect_identical(vec_fill_missing(x, direction = "updown", max_fill = 1), x)
})

test_that("works with data frames with rows but no columns", {
  x <- new_data_frame(n = 2L)

  expect_identical(vec_fill_missing(x, direction = "down"), x)
  expect_identical(vec_fill_missing(x, direction = "up"), x)
  expect_identical(vec_fill_missing(x, direction = "downup"), x)
  expect_identical(vec_fill_missing(x, direction = "updown"), x)
  expect_identical(vec_fill_missing(x, direction = "downup", max_fill = 1), x)
  expect_identical(vec_fill_missing(x, direction = "updown", max_fill = 1), x)
})

test_that("vectors with all missing values are left unchanged", {
  x <- c(NA, NA, NA)

  expect_identical(vec_fill_missing(x, direction = "down"), x)
  expect_identical(vec_fill_missing(x, direction = "up"), x)
  expect_identical(vec_fill_missing(x, direction = "downup"), x)
  expect_identical(vec_fill_missing(x, direction = "updown"), x)
  expect_identical(vec_fill_missing(x, direction = "downup", max_fill = 1), x)
  expect_identical(vec_fill_missing(x, direction = "updown", max_fill = 1), x)
})

test_that("`NA_real_` and `NaN` are both considered missing", {
  expect_identical(
    vec_fill_missing(c(1, NA_real_, NaN)),
    c(1, 1, 1)
  )
})

test_that("missings are filled correctly", {
  x <- c(NA, 1, NA, 2, NA, NA)

  expect_identical(vec_fill_missing(x, "down"), c(NA, 1, 1, 2, 2, 2))
  expect_identical(vec_fill_missing(x, "up"), c(1, 1, 2, 2, NA, NA))
  expect_identical(vec_fill_missing(x, "downup"), c(1, 1, 1, 2, 2, 2))
  expect_identical(vec_fill_missing(x, "updown"), c(1, 1, 2, 2, 2, 2))
})

test_that("`max_fill` limits the sequential fill amount", {
  x <- c(NA, NA, 1, NA, NA, NA, 3, NA, NA)

  expect_identical(
    vec_fill_missing(x, "down", max_fill = 1),
    c(NA, NA, 1, 1, NA, NA, 3, 3, NA)
  )
  expect_identical(
    vec_fill_missing(x, "downup", max_fill = 1),
    c(NA, 1, 1, 1, NA, NA, 3, 3, NA)
  )
  expect_identical(
    vec_fill_missing(x, "down", max_fill = 2),
    c(NA, NA, 1, 1, 1, NA, 3, 3, 3)
  )
  expect_identical(
    vec_fill_missing(x, "downup", max_fill = 2),
    c(1, 1, 1, 1, 1, NA, 3, 3, 3)
  )
  expect_identical(
    vec_fill_missing(x, "up", max_fill = 1),
    c(NA, 1, 1, NA, NA, 3, 3, NA, NA)
  )
  expect_identical(
    vec_fill_missing(x, "updown", max_fill = 1),
    c(NA, 1, 1, NA, NA, 3, 3, 3, NA)
  )
  expect_identical(
    vec_fill_missing(x, "up", max_fill = 2),
    c(1, 1, 1, NA, 3, 3, 3, NA, NA)
  )
  expect_identical(
    vec_fill_missing(x, "updown", max_fill = 2),
    c(1, 1, 1, NA, 3, 3, 3, 3, 3)
  )
})

test_that("fills data frames", {
  df <- data_frame(x = c(NA, NA, NA, 2), y = c(NA, 1, NA, 3))

  expect_identical(vec_fill_missing(df, "down"), vec_slice(df, c(1, 2, 2, 4)))
  expect_identical(vec_fill_missing(df, "up"), vec_slice(df, c(2, 2, 4, 4)))
})

test_that("can fill rcrd types", {
  x <- new_rcrd(list(x = c(1, NA, NA), y = c(1, 2, NA)))

  expect_identical(vec_fill_missing(x, "down"), vec_slice(x, c(1, 2, 2)))
  expect_identical(vec_fill_missing(x, "up"), vec_slice(x, c(1, 2, 3)))
  expect_identical(vec_fill_missing(x, "updown"), vec_slice(x, c(1, 2, 2)))
})

test_that("validates `direction`", {
  expect_error(vec_fill_missing(1, 1), "`direction` must be one of")
  expect_error(vec_fill_missing(1, "foo"), "`direction` must be one of")
})

test_that("validates `max_fill`", {
  expect_error(vec_fill_missing(1, max_fill = -1), "`max_fill` must be")
  expect_error(vec_fill_missing(1, max_fill = c(1L, 2L)), "`max_fill` must be")
  expect_error(
    vec_fill_missing(1, max_fill = NA_integer_),
    "`max_fill` must be"
  )
  expect_error(
    vec_fill_missing(1, max_fill = "x"),
    class = "vctrs_error_incompatible_type"
  )
})
