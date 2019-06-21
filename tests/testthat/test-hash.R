context("test-hash")

# Vectorised --------------------------------------------------------------

test_that("vec_hash() produces same hash for same values", {
  x <- vec_hash(1:3)
  y <- do.call(c, map(1:3, vec_hash))
  expect_identical(x, y)
})

test_that("F, T, and NA hash to different values", {
  x <- map(c(TRUE, FALSE, NA), vec_hash)
  expect_length(unique(x), 3)
})

test_that("vec_hash of double produces different values", {
  x <- vec_hash(c(1, 1, 2))
  expect_true(identical(x[1:4], x[5:8]))
  expect_false(identical(x[5:8], x[9:12]))
})

test_that("NA and NaN hash to different values", {
  x <- vec_hash(c(NA, NaN))
  expect_false(identical(x[1:4], x[5:8]))
})

test_that("same string hashes to same value", {
  x <- vec_hash(c("1", "1", "2"))
  expect_true(identical(x[1:4], x[5:8]))
  expect_false(identical(x[5:8], x[9:12]))
})

test_that("list hashes to values of individual values", {
  x <- vec_hash(list(1:3, letters))
  expect_identical(x[1:4], obj_hash(1:3))
  expect_identical(x[5:8], obj_hash(letters))

  x <- map(list(list(1:3), list(letters)), vec_hash)
  expect_identical(x[[1]], obj_hash(1:3))
  expect_identical(x[[2]], obj_hash(letters))
})

test_that("hash of data frame works down rows", {
  df <- data.frame(x = 1:3, y = 1:3)
  x <- vec_hash(df)
  expect_length(x, 4 * vec_size(df))
  expect_identical(x[1:4], vec_hash(df[1, ]))
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

  scoped_options(max.print = 99999)
  expect_known_output(print(hash), file = test_path("test-hash-hash.txt"))
})

test_that("can hash list of non-vectors", {
  x <- list(quote(x), mean)

  expect_equal(
    vec_hash(x),
    c(obj_hash(x[[1]]), obj_hash(x[[2]]))
  )
})

test_that("can hash matrices rowwise", {
  x <- matrix(c(1, 2, 3, 4), c(2, 2))
  expect_identical(
    vec_hash(x, rowwise = TRUE),
    vec_hash(x, rowwise = TRUE)
  )

  y <- matrix(c(1, 2, 3, 5), c(2, 2))
  expect_false(identical(
    vec_hash(x, rowwise = TRUE),
    vec_hash(y, rowwise = TRUE)
  ))
})

test_that("can hash matrices non-rowwise", {
  x <- matrix(c(1, 1, 1, 2, 2, 1), c(3, 2))

  expect_identical(
    vec_hash(x, rowwise = FALSE),
    vec_hash(x, rowwise = FALSE)
  )

  x <- matrix(c(1, 2, 3, 4), c(2, 2))

  expect_identical(
    vec_hash(x, rowwise = FALSE),
    vec_hash(x, rowwise = FALSE)
  )

  expect_false(identical(
    vec_hash(x, rowwise = FALSE),
    vec_hash(c(1, 2), rowwise = FALSE)
  ))

  y <- matrix(c(1, 2, 3, 5), c(2, 2))

  expect_false(identical(
    vec_hash(x, rowwise = FALSE),
    vec_hash(y, rowwise = FALSE)
  ))
})

test_that("can hash with non-rowwise method", {
  expect_identical(
    vec_hash(NA, rowwise = FALSE),
    vec_hash(NA, rowwise = FALSE),
  )
})

test_that("can hash 1D arrays", {
  # 1D arrays are dispatched to `as.data.frame.vector()` which
  # currently does not strip dimensions. This caused an infinite
  # recursion.
  expect_length(vec_hash(array(1:2), TRUE), 8)
  expect_identical(vec_hash(array(1:2), FALSE), vec_hash(1:2, FALSE))
})


# Object ------------------------------------------------------------------

test_that("equal objects hash to same value", {
  # just test function since they'll recurse through every other object type
  f1 <- function(x, y = NULL) x + y
  f2 <- function(x, y = NULL) x + y
  expect_false(identical(obj_hash(f1), obj_hash(f2)))
  expect_false(identical(vec_hash(data_frame(x = list(f1))), vec_hash(data_frame(x = list(f2)))))

  attr(f1, "srcref") <- NULL
  attr(f2, "srcref") <- NULL
  expect_equal(obj_hash(f1), obj_hash(f2))
  expect_equal(vec_hash(data_frame(x = list(f1))), vec_hash(data_frame(x = list(f2))))
})
