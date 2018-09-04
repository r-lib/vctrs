context("test-hash")

# Vectorised --------------------------------------------------------------

test_that("F, T, and NA hash to different values", {
  x <- hash(c(TRUE, FALSE, NA))
  expect_length(unique(x), 3)
})

test_that("integer hash to own value", {
  x <- hash(1:10)
  expect_equal(x, 1:10)
})

test_that("hash of double produces different values", {
  x <- hash(c(1, 1, 2))
  expect_true(x[[1]] == x[[2]])
  expect_false(x[[2]] == x[[3]])
})

test_that("NA and NaN hash to different values", {
  x <- hash(c(NA, NaN))
  expect_false(x[[1]] == x[[2]])
})

test_that("same string hashes to same value", {
  x <- hash(c("1", "1", "2"))
  expect_true(x[[1]] == x[[2]])
  expect_false(x[[2]] == x[[3]])
})

test_that("list hashes to values of individual values", {
  x <- hash(list(1:3, letters))
  expect_equal(x[[1]], hash_vector(1:3))
  expect_equal(x[[2]], hash_vector(letters))
})

test_that("hash of data frame works down rows", {
  df <- data.frame(x = 1:3, y = 1:3)
  x <- hash(df)
  expect_length(x, 3)
  expect_equal(x[[1]], hash_vector(1:2))
})
