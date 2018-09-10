context("test-hash")

# Vectorised --------------------------------------------------------------

test_that("F, T, and NA hash to different values", {
  x <- vec_hash(c(TRUE, FALSE, NA))
  expect_length(unique(x), 3)
})

test_that("vec_hash of double produces different values", {
  x <- vec_hash(c(1, 1, 2))
  expect_true(x[[1]] == x[[2]])
  expect_false(x[[2]] == x[[3]])
})

test_that("NA and NaN hash to different values", {
  x <- vec_hash(c(NA, NaN))
  expect_false(x[[1]] == x[[2]])
})

test_that("same string hashes to same value", {
  x <- vec_hash(c("1", "1", "2"))
  expect_true(x[[1]] == x[[2]])
  expect_false(x[[2]] == x[[3]])
})

test_that("list hashes to values of individual values", {
  x <- vec_hash(list(1:3, letters))
  expect_equal(x[1], obj_hash(1:3))
  expect_equal(x[2], obj_hash(letters))
})

test_that("hash of data frame works down rows", {
  df <- data.frame(x = 1:3, y = 1:3)
  x <- vec_hash(df)
  expect_length(x, 3)
  expect_equal(x[1], vec_hash(df[1, ]))
})

test_that("hashes are consistent from run to run", {
  # no string, since we're currently hashing the address in string pool
  df <- list(
    lgl = c(TRUE, FALSE, NA),
    int = 1:100,
    dbl1 = as.double(1:100),
    dbl2 = seq(0, 1, length = 100)
  )
  hash <- lapply(df, vec_hash)

  expect_known_output(print(hash), file = test_path("test-hash-hash.txt"))
})

# Equality ----------------------------------------------------------------

test_that("throws error for unsuported type", {
  expect_error(.Call(vctrs_equal, expression(x), expression(x)), "Unsupported")
})

test_that("correct behaviour for basic vectors", {
  expect_equal(vec_equal(c(TRUE, FALSE), TRUE), c(TRUE, FALSE))
  expect_equal(vec_equal(c(1L, 2L), 1L), c(TRUE, FALSE))
  expect_equal(vec_equal(c(1, 2), 1), c(TRUE, FALSE))
  expect_equal(vec_equal(c("1", "2"), "1"), c(TRUE, FALSE))
  expect_equal(vec_equal(list(1:3, 1:2), list(1:3)), c(TRUE, FALSE))
})

test_that("NAs are equal", {
  expect_true(vec_equal(NA, NA, .ptype = logical()))
  expect_true(vec_equal(NA_integer_, NA_integer_))
  expect_true(vec_equal(NA_real_, NA_real_))
  expect_true(vec_equal(NA_character_, NA_character_))
})

test_that("double special values", {
  expect_equal(vec_equal(c(NaN, NA), NaN), c(TRUE, FALSE))
  expect_equal(vec_equal(c(NA, NaN), NA), c(TRUE, FALSE))
  expect_true(vec_equal(Inf, Inf))
  expect_true(vec_equal(-Inf, -Inf))
})

test_that("works for data frames", {
  df <- data.frame(x = 1:2, y = letters[2:1], stringsAsFactors = FALSE)
  expect_equal(vec_equal(df, df[1, ]), c(TRUE, FALSE))
})
