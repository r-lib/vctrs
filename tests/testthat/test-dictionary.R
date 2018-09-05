context("test-dictionary")

test_that("vec_duplicated matches duplicated", {
  x <- sample(100, 1000, replace = TRUE)
  expect_equal(duplicated(x), vec_duplicated(x))
})

test_that("vec_unique matches unique", {
  x <- sample(100, 1000, replace = TRUE)
  expect_equal(unique(x), unique(x))
})

test_that("vec_n_distinct matches length + unique", {
  x <- sample(100, 1000, replace = TRUE)
  expect_equal(vec_n_distinct(x), length(unique(x)))
})

test_that("vec_id gives position of first found", {
  x <- c(1, 2, 3, 1, 4)
  expect_equal(vec_id(x), c(1, 2, 3, 1, 5))
})

test_that("vec_count counts number observations", {
  x <- vec_count(rep(1:3, 1:3))
  expect_equal(x, data.frame(key = 1:3, count = 1:3))
})

test_that("vec_match() matches match()", {
  n <- c(1:3, NA)
  h <- c(4, 2, 1, NA)

  expect_equal(vec_match(n, h), match(n, h))
})

test_that("everything works for data frames", {
  df <- data.frame(x = 1:2, y = letters[2:1], stringsAsFactors = FALSE)
  idx <- c(1L, 1L, 1L, 2L, 2L)
  df2 <- df[idx, , drop = FALSE]
  rownames(df2) <- NULL

  expect_equal(vec_duplicated(df2), vec_duplicated(idx))
  expect_equal(vec_unique(df2), vec_subset(df, vec_unique(idx)))

  count <- vec_count(df2)
  expect_equal(count$key, df)
  expect_equal(count$count, vec_count(idx)$count)
})
