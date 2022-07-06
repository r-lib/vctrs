# ------------------------------------------------------------------------------
# vec_equal_na()

test_that("can detect different types of NA", {
  expect_true(vec_equal_na(NA))
  expect_true(vec_equal_na(NA_integer_))
  expect_true(vec_equal_na(NA_real_))
  expect_true(vec_equal_na(NA_complex_))
  expect_true(vec_equal_na(complex(real = NA, imaginary = 1)))
  expect_true(vec_equal_na(NaN))
  expect_true(vec_equal_na(NA_character_))
  expect_true(vec_equal_na(list(NULL)))
})

test_that("can detect different types of NA in data frames", {
  # using multiple columns to prevent proxy unwrapping
  expect_true(vec_equal_na(data.frame(x = NA, y = NA)))
  expect_true(vec_equal_na(data.frame(x = NA_integer_, y = NA_integer_)))
  expect_true(vec_equal_na(data.frame(x = NA_real_, y = NaN)))
  expect_true(vec_equal_na(data.frame(x = NA_complex_, y = NA_complex_)))
  expect_true(vec_equal_na(data.frame(x = complex(real = NA, imaginary = 1), y = complex(real = 1, imaginary = NA))))
  expect_true(vec_equal_na(data.frame(x = NA_character_, y = NA_character_)))
  expect_true(vec_equal_na(new_data_frame(list(x = list(NULL), y = list(NULL)))))
})

test_that("raw vectors can never be NA", {
  expect_false(vec_equal_na(raw(1)))
  expect_false(vec_equal_na(data.frame(x = raw(1), y = raw(1))))
})

test_that("vectorised over rows of a data frame", {
  df <- data.frame(x = c(1, 1, NA, NA), y = c(1, NA, 1, NA))
  expect_equal(vec_equal_na(df), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("works recursively with data frame columns", {
  df <- data.frame(x = c(1, 1, NA, NA))
  df$df <- data.frame(y = c(NA, 1, 1, NA), z = c(1, NA, 1, NA))
  expect_equal(vec_equal_na(df), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("works with `NULL` input (#1494)", {
  expect_identical(vec_equal_na(NULL), logical())
})
