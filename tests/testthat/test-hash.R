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

test_that("can hash list of non-vectors", {
  x <- list(quote(x), mean)

  expect_equal(
    vec_hash(x),
    as.hexmode(c(obj_hash(x[[1]]), obj_hash(x[[2]])))
  )
})


# Object ------------------------------------------------------------------

test_that("equal objects hash to same value", {
  # just test function since they'll recurse through every other object type
  f1 <- function(x, y = NULL) x + y
  attr(f1, "srcref") <- NULL
  f2 <- function(x, y = NULL) x + y
  attr(f2, "srcref") <- NULL

  expect_equal(obj_hash(f1), obj_hash(f2))
})
