# ------------------------------------------------------------------------------
# vec_detect_missing()

test_that("can detect different types of NA", {
  expect_true(vec_detect_missing(NA))
  expect_true(vec_detect_missing(NA_integer_))
  expect_true(vec_detect_missing(NA_real_))
  expect_true(vec_detect_missing(NA_complex_))
  expect_true(vec_detect_missing(complex(real = NA, imaginary = 1)))
  expect_true(vec_detect_missing(NaN))
  expect_true(vec_detect_missing(NA_character_))
  expect_true(vec_detect_missing(list(NULL)))
})

test_that("can detect different types of NA in data frames", {
  # using multiple columns to prevent proxy unwrapping
  expect_true(vec_detect_missing(data.frame(x = NA, y = NA)))
  expect_true(vec_detect_missing(data.frame(x = NA_integer_, y = NA_integer_)))
  expect_true(vec_detect_missing(data.frame(x = NA_real_, y = NaN)))
  expect_true(vec_detect_missing(data.frame(x = NA_complex_, y = NA_complex_)))
  expect_true(vec_detect_missing(data.frame(x = complex(real = NA, imaginary = 1), y = complex(real = 1, imaginary = NA))))
  expect_true(vec_detect_missing(data.frame(x = NA_character_, y = NA_character_)))
  expect_true(vec_detect_missing(new_data_frame(list(x = list(NULL), y = list(NULL)))))
})

test_that("raw vectors can never be NA", {
  expect_false(vec_detect_missing(raw(1)))
  expect_false(vec_detect_missing(data.frame(x = raw(1), y = raw(1))))
})

test_that("vectorised over rows of a data frame", {
  df <- data.frame(x = c(1, 1, NA, NA), y = c(1, NA, 1, NA))
  expect_equal(vec_detect_missing(df), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("works recursively with data frame columns", {
  df <- data.frame(x = c(1, 1, NA, NA))
  df$df <- data.frame(y = c(NA, 1, 1, NA), z = c(1, NA, 1, NA))
  expect_equal(vec_detect_missing(df), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("0 row, N col data frame always returns `logical()` (#1585)", {
  expect_identical(vec_detect_missing(data_frame()), logical())
  expect_identical(vec_detect_missing(data_frame(x = integer(), y = double())), logical())
})

test_that(">0 row, 0 col data frame always returns `TRUE` for each row (#1585)", {
  # `vec_detect_missing()` returns `TRUE` for each row because it (in theory) does
  # `all()` on each row, and since there are 0 columns we get
  # `all(logical()) == TRUE` for each row.
  expect_identical(
    vec_detect_missing(data_frame(.size = 2L)),
    c(TRUE, TRUE)
  )
})

test_that("works with `NULL` input (#1494)", {
  expect_identical(vec_detect_missing(NULL), logical())
})

# ------------------------------------------------------------------------------
# vec_any_missing()

test_that("can check for any missing with all base vector types", {
  expect_false(vec_any_missing(TRUE))
  expect_false(vec_any_missing(1L))
  expect_false(vec_any_missing(1))
  expect_false(vec_any_missing(complex(real = 1, imaginary = 1)))
  expect_false(vec_any_missing("1"))
  expect_false(vec_any_missing(list(1)))

  expect_true(vec_any_missing(c(TRUE, NA)))
  expect_true(vec_any_missing(c(1L, NA_integer_)))
  expect_true(vec_any_missing(c(1, NA_real_)))
  expect_true(vec_any_missing(complex(real = c(1, NA), imaginary = c(1, NA))))
  expect_true(vec_any_missing(c("1", NA_character_)))
  expect_true(vec_any_missing(list(1, NULL)))
})

test_that("raw vectors can never be missing", {
  expect_false(vec_any_missing(raw(1)))
  expect_false(vec_any_missing(data.frame(x = raw(1), y = raw(1))))
})

test_that("works with empty vectors", {
  # Like `any(logical())`
  expect_false(vec_any_missing(integer()))
})

test_that("correctly detects complex missingness", {
  expect_false(vec_any_missing(complex(real = 1, imaginary = 1)))
  expect_true(vec_any_missing(complex(real = 1, imaginary = NA)))
  expect_true(vec_any_missing(complex(real = NA, imaginary = 1)))
  expect_true(vec_any_missing(complex(real = NA, imaginary = NA)))
})

test_that("treats NaN as missing", {
  expect_true(vec_any_missing(NaN))
})

test_that("works with `NULL` input", {
  expect_false(vec_any_missing(NULL))
})

test_that("entire row of a data frame must be missing", {
  df <- data.frame(x = c(1, 1, NA, NA), y = c(1, NA, 1, NA))
  expect_true(vec_any_missing(df))
  expect_false(vec_any_missing(df[-4,]))
})

test_that("works recursively with data frame columns", {
  df <- data.frame(x = c(1, 1, NA, NA))
  df$df <- data.frame(y = c(NA, 1, 1, NA), z = c(1, NA, 1, NA))
  expect_true(vec_any_missing(df))
  expect_false(vec_any_missing(df[-4,]))
})

test_that("0 row, N col data frame always returns `FALSE` (#1585)", {
  df <- data_frame()

  expect_false(vec_any_missing(df))
  expect_false(vec_any_missing(data_frame(x = integer(), y = double())))

  # This is consistent with `vec_detect_missing()` returning `logical()` for 0 row
  # data frames. Then `any(logical()) == FALSE` to get `vec_any_missing()`.
  expect_identical(
    vec_any_missing(df),
    any(vec_detect_missing(df))
  )
})

test_that(">0 row, 0 col data frame always returns `TRUE` (#1585)", {
  df <- data_frame(.size = 2L)

  expect_true(vec_any_missing(df))

  # This is consistent with `vec_detect_missing()` returning `TRUE` for each row
  # because it (in theory) does `all()` on each row, and since there are 0
  # columns we get `all(logical()) == TRUE` for each row.
  # Then `any(c(TRUE, TRUE)) == TRUE` to get `vec_any_missing()`.
  expect_identical(
    vec_any_missing(df),
    any(vec_detect_missing(df))
  )
})

# ------------------------------------------------------------------------------
# vec_proxy_missing()

test_that("vec_proxy_missing()/vec_any_missing() takes vec_proxy_equal() by default", {
  local_methods(
    vec_proxy_equal.vctrs_foobar = function(x, ...) (ifelse(x == -99, NA, x)),
  )

  expect_identical(vec_detect_missing(foobar(c(1, 2, -99, 3))), c(FALSE, FALSE, TRUE, FALSE))
  expect_identical(vec_any_missing(foobar(c(1, 2, -99, 3))), TRUE)
  expect_identical(vec_any_missing(foobar(c(1, 2, 3))), FALSE)
})

test_that("vec_detect_missing() calls vec_proxy_missing(), if implemented", {
  local_methods(
    vec_proxy_missing.vctrs_foobar = function(x, ...) (ifelse(x == -99, NA, x)),
  )
  expect_identical(vec_detect_missing(foobar(c(1, 2, -99, 3))), c(FALSE, FALSE, TRUE, FALSE))
  expect_identical(vec_any_missing(foobar(c(1, 2, -99, 3))), TRUE)
  expect_identical(vec_any_missing(foobar(c(1, 2, 3))), FALSE)
})
