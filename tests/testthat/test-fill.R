test_that("works with empty input", {
  x <- integer()

  expect_identical(vec_fill(x, direction = "down"), x)
  expect_identical(vec_fill(x, direction = "up"), x)
  expect_identical(vec_fill(x, direction = "down", leading = TRUE), x)
  expect_identical(vec_fill(x, direction = "up", leading = TRUE), x)
})

test_that("works with data frames with rows but no columns", {
  x <- new_data_frame(n = 2L)

  expect_identical(vec_fill(x, direction = "down"), x)
  expect_identical(vec_fill(x, direction = "up"), x)
  expect_identical(vec_fill(x, direction = "down", leading = TRUE), x)
  expect_identical(vec_fill(x, direction = "up", leading = TRUE), x)
})

test_that("vectors with all missing values are left unchanged", {
  x <- c(NA, NA, NA)

  expect_identical(vec_fill(x, direction = "down"), x)
  expect_identical(vec_fill(x, direction = "up"), x)
  expect_identical(vec_fill(x, direction = "down", leading = TRUE), x)
  expect_identical(vec_fill(x, direction = "up", leading = TRUE), x)
})

test_that("`NA_real_` and `NaN` are both considered missing", {
  expect_identical(
    vec_fill(c(1, NA_real_, NaN)),
    c(1, 1, 1)
  )
})

test_that("missings are filled correctly", {
  x <- c(NA, 1, NA, 2, NA, NA)

  expect_identical(vec_fill(x, "down"), c(NA, 1, 1, 2, 2, 2))
  expect_identical(vec_fill(x, "up"), c(1, 1, 2, 2, NA, NA))
  expect_identical(vec_fill(x, "down", leading = TRUE), c(1, 1, 1, 2, 2, 2))
  expect_identical(vec_fill(x, "up", leading = TRUE), c(1, 1, 2, 2, 2, 2))
})

test_that("fills data frames", {
  df <- data_frame(x = c(NA, NA, NA, 2), y = c(NA, 1, NA, 3))

  expect_identical(vec_fill(df, "down"), vec_slice(df, c(1, 2, 2, 4)))
  expect_identical(vec_fill(df, "up"), vec_slice(df, c(2, 2, 4, 4)))
})

test_that("can fill rcrd types", {
  x <- new_rcrd(list(x = c(1, NA, NA), y = c(1, 2, NA)))

  expect_identical(vec_fill(x, "down"), vec_slice(x, c(1, 2, 2)))
  expect_identical(vec_fill(x, "up"), vec_slice(x, c(1, 2, 3)))
  expect_identical(vec_fill(x, "up", leading = TRUE), vec_slice(x, c(1, 2, 2)))
})

test_that("validates `direction`", {
  expect_error(vec_fill(1, 1), "`direction` must be either")
  expect_error(vec_fill(1, "foo"), "`direction` must be either")
})

test_that("validates `leading`", {
  expect_error(vec_fill(1, leading = NA), "`leading` must be")
  expect_error(vec_fill(1, leading = c(TRUE, FALSE)), "`leading` must be")
})
