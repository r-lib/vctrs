context("test-dictionary")


# counting ----------------------------------------------------------------

test_that("vec_count counts number observations", {
  x <- vec_count(rep(1:3, 1:3), sort = "key")
  expect_equal(x, data.frame(key = 1:3, count = 1:3))
})

test_that("vec_count works with matrices", {
  x <- matrix(c(1, 1, 1, 2, 2, 1), c(3, 2))

  out <- vec_count(x)
  exp <- data_frame(key = c(NA, NA), count = int(2L, 1L))
  exp$key <- vec_slice(x, c(1, 3))

  expect_identical(out, exp)
})

test_that("vec_count works with arrays", {
  x <- array(c(rep(1, 3), rep(2, 3)), dim = c(3, 2, 1))
  expect <- data.frame(key = NA, count = 3)
  expect$key <- vec_slice(x, 1L)
  expect_equal(vec_count(x), expect)
})

test_that("vec_count works for zero-length input", {
  x <- vec_count(integer(), sort = "none")
  expect_equal(x, data.frame(key = integer(), count = integer()))
})


# duplicates and uniques --------------------------------------------------

test_that("vec_duplicated reports on duplicates regardless of position", {
  x <- c(1, 1, 2, 3, 4, 4)
  expect_equal(vec_duplicate_detect(x), c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
})

test_that("vec_duplicate_any returns single TRUE/FALSE", {
  expect_false(vec_duplicate_any(c(1:10)))
  expect_true(vec_duplicate_any(c(1:10, 1)))
})

test_that("vec_duplicate_id gives position of first found", {
  x <- c(1, 2, 3, 1, 4)
  expect_equal(vec_duplicate_id(x), c(1, 2, 3, 1, 5))
})

test_that("vec_unique matches unique", {
  x <- sample(100, 1000, replace = TRUE)
  expect_equal(vec_unique(x), unique(x))
})

test_that("vec_unique matches unique for matrices", {
  x <- matrix(c(1, 1, 2, 2), ncol = 2)
  expect_equal(vec_unique(x), unique(x))
})

test_that("vec_unique_count matches length + unique", {
  x <- sample(100, 1000, replace = TRUE)
  expect_equal(vec_unique_count(x), length(unique(x)))
})

test_that("also works for data frames", {
  df <- data.frame(x = 1:3, y = letters[3:1], stringsAsFactors = FALSE)
  idx <- c(1L, 1L, 1L, 2L, 2L, 3L)
  df2 <- df[idx, , drop = FALSE]
  rownames(df2) <- NULL

  expect_equal(vec_duplicate_detect(df2), vec_duplicate_detect(idx))
  expect_equal(vec_unique(df2), vec_slice(df, vec_unique(idx)))

  count <- vec_count(df2, sort = "key")
  expect_equal(count$key, df)
  expect_equal(count$count, vec_count(idx)$count)

  exp <- tibble(x = c(1, 1, 2), y = c(1, 2, 3))
  expect_identical(vec_unique(vec_slice(exp, c(1, 1, 2, 3))), exp)
})

test_that("vec_unique() handles matrices (#327)", {
  x <- matrix(c(1, 2, 3, 4), c(2, 2))
  y <- matrix(c(1, 2, 3, 5), c(2, 2))
  expect_identical(vec_unique(list(x, x)), list(x))
  expect_identical(vec_unique(list(x, y)), list(x, y))

  x <- matrix(c(1, 2, 1, 1, 2, 1), nrow = 3)
  expect_identical(vec_unique(x), vec_slice(x, 1:2))
})

test_that("vec_unique() works with 1D arrays", {
  # 1D arrays are dispatched to `as.data.frame.vector()` which
  # currently does not strip dimensions. This caused an infinite
  # recursion.
  expect_identical(vec_unique(array(1:2)), array(1:2))

  x <- new_vctr(c(1, 1, 1, 2, 1, 2), dim = c(3, 2))
  expect_identical(vec_unique(x), new_vctr(c(1, 1, 2, 1), dim = c(2, 2)))
})

test_that("unique functions take the equality proxy (#375)", {
  scoped_comparable_tuple()
  x <- tuple(c(1, 2, 1), 1:3)

  expect_true(vec_in(tuple(2, 100), x))
  expect_identical(vec_match(tuple(2, 100), x), 2L)
})


# matching ----------------------------------------------------------------

test_that("vec_match() matches match()", {
  n <- c(1:3, NA)
  h <- c(4, 2, 1, NA)

  expect_equal(vec_match(n, h), match(n, h))
})

test_that("vec_in() matches %in%", {
  n <- c(1:3, NA)
  h <- c(4, 2, 1, NA)

  expect_equal(vec_in(n, h), n %in% h)
})

test_that("vec_match works with empty data frame", {
  out <- vec_match(
    new_data_frame(n = 3L),
    new_data_frame(n = 0L)
  )
  expect_equal(out, vec_init(integer(), 3))
})

test_that("matching functions take the equality proxy (#375)", {
  scoped_comparable_tuple()
  x <- tuple(c(1, 2, 1), 1:3)

  expect_identical(vec_unique_loc(x), 1:2)
  expect_identical(unique(x), tuple(c(1, 2), 1:2))

  expect_true(vec_duplicate_any(x))
  expect_identical(vec_duplicate_id(x), c(1L, 2L, 1L))
  expect_identical(vec_unique_count(x), 2L)

  expect_identical(vec_duplicate_detect(x), c(TRUE, FALSE, TRUE))
})

test_that("can take the unique loc of 1d arrays (#461)", {
  x <- array(c(1, 1, 2, 2, 3))
  y <- array(c(1, 1, 2, 2, 3), dimnames = list(NULL))
  expect_identical(vctrs::vec_unique_loc(x), int(1, 3, 5))
  expect_identical(vctrs::vec_unique_loc(y), int(1, 3, 5))

  z <- array(c(1, 1, 2, 2, 3, 4), c(3, 2))
  expect_silent(expect_identical(vctrs::vec_unique_loc(y), int(1, 3, 5)))
})


# splits ------------------------------------------------------------------

test_that("can split empty vector", {
  out <- vec_split(integer(), character())

  expect_s3_class(out, "data.frame")
  expect_equal(out$key, character())
  expect_equal(out$val, list_of(.ptype = integer()))
})

test_that("split data frame with data frame", {
  df <- data.frame(x = c(1, 1, 2), y = c(1, 1, 1))
  out <- vec_split(df, df)

  expect_s3_class(out, "data.frame")
  expect_equal(out$key, data.frame(x = c(1, 2), y = c(1, 1)))
  expect_equal(out$val, list_of(
    data.frame(x = c(1, 1), y = c(1, 1)),
    data.frame(x = 2, y = 1)
  ))
})

test_that("x and by must be same size", {
  expect_error(
    vec_split(1:3, 1:2),
    "same size"
  )
})

test_that("split takes the equality proxy (#375)", {
  scoped_comparable_tuple()
  x <- tuple(c(1, 2, 1), 1:3)
  expect_identical(nrow(vec_split(1:3, x)), 2L)
})
