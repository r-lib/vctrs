context("test-group")

# group id ----------------------------------------------------------------

test_that("vec_group_id detects groups in order of appearance", {
  x <- c(2, 4, 2, 1, 4)
  expect <- structure(c(1L, 2L, 1L, 3L, 2L), n = 3L)
  expect_equal(vec_group_id(x), expect)
})

test_that("vec_group_id works for size 0 input", {
  expect <- structure(integer(), n = 0L)
  expect_equal(vec_group_id(NULL), expect)
  expect_equal(vec_group_id(numeric()), expect)
})

test_that("vec_group_id works on base S3 objects", {
  x <- factor(c("x", "y", "x"))
  expect <- structure(c(1L, 2L, 1L), n = 2L)
  expect_equal(vec_group_id(x), expect)

  x <- new_date(c(0, 1, 0))
  expect <- structure(c(1L, 2L, 1L), n = 2L)
  expect_equal(vec_group_id(x), expect)
})

test_that("vec_group_id works row wise on data frames", {
  df <- data.frame(x = c(1, 2, 1, 1), y = c(2, 3, 2, 3))
  expect <- structure(c(1L, 2L, 1L, 3L), n = 3L)
  expect_equal(vec_group_id(df), expect)
})

test_that("vec_group_id works row wise on arrays", {
  x <- array(c(1, 1, 1, 2, 4, 2), c(3, 2))
  expect <- structure(c(1L, 2L, 1L), n = 2L)
  expect_equal(vec_group_id(x), expect)
})

test_that("vec_group_id works with different encodings", {
  expect <- structure(c(1L, 1L, 1L), n = 1L)
  expect_equal(vec_group_id(encodings()), expect)
})

test_that("vec_group_id takes the equality proxy", {
  scoped_comparable_tuple()
  x <- tuple(c(1, 2, 1, 1), c(1, 1, 1, 2))
  # Compares on only the first field
  expect <- structure(c(1L, 2L, 1L, 1L), n = 2L)
  expect_equal(vec_group_id(x), expect)
})

# group rle ---------------------------------------------------------------

test_that("vec_group_rle returns a `vctrs_group_rle` object", {
  expect_is(vec_group_rle(1), "vctrs_group_rle")
})

test_that("vec_group_rle works with size 0 input", {
  expect <- new_group_rle(integer(), integer(), 0L)
  expect_equal(vec_group_rle(integer()), expect)
  expect_equal(vec_group_rle(NULL), expect)
})

test_that("vec_group_rle detects groups in order of appearance", {
  x <- c(2, 2, 3, 1, 1)
  expect <- new_group_rle(1:3, c(2L, 1L, 2L), 3L)
  expect_equal(vec_group_rle(x), expect)
})

test_that("vec_group_rle can refer to groups it has already seen", {
  x <- c(2, 3, 2)
  expect <- new_group_rle(c(1L, 2L, 1L), rep(1L, 3), 2L)
  expect_equal(vec_group_rle(x), expect)
})

test_that("vec_group_rle works on base S3 objects", {
  expect <- new_group_rle(c(1L, 2L, 1L, 3L), c(1L, 2L, 1L, 1L), 3L)

  x <- factor(c("x", "y", "y", "x", "z"))
  expect_equal(vec_group_rle(x), expect)

  x <- new_date(c(0, 1, 1, 0, 2))
  expect_equal(vec_group_rle(x), expect)
})

test_that("vec_group_rle takes the equality proxy", {
  scoped_comparable_tuple()
  x <- tuple(c(1, 2, 1, 1), c(1, 1, 1, 2))
  # Compares on only the first field
  expect <- new_group_rle(c(1L, 2L, 1L), c(1L, 1L, 2L), 2L)
  expect_equal(vec_group_rle(x), expect)
})

test_that("vec_group_rle works row wise on data frames", {
  df <- data.frame(x = c(1, 1, 2, 1), y = c(2, 2, 3, 2))
  expect <- new_group_rle(c(1L, 2L, 1L), c(2L, 1L, 1L), 2L)
  expect_equal(vec_group_rle(df), expect)
})

test_that("print method is useful", {
  x <- new_group_rle(c(1L, 2L, 1L), c(3L, 2L, 1L), 2L)
  expect_known_output(print(x), file = test_path("test-type-group-rle.txt"))
})
