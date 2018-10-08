context("test-dictionary")


# counting ----------------------------------------------------------------

test_that("vec_count counts number observations", {
  x <- vec_count(rep(1:3, 1:3), sort = "key")
  expect_equal(x, data.frame(key = 1:3, count = 1:3))
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
  expect_equal(unique(x), unique(x))
})

test_that("vec_unique_count matches length + unique", {
  x <- sample(100, 1000, replace = TRUE)
  expect_equal(vec_unique_count(x), length(unique(x)))
})

test_that("also works for data frames", {
  df <- data.frame(x = 1:2, y = letters[2:1], stringsAsFactors = FALSE)
  idx <- c(1L, 1L, 1L, 2L, 2L)
  df2 <- df[idx, , drop = FALSE]
  rownames(df2) <- NULL

  expect_equal(vec_duplicate_detect(df2), vec_duplicate_detect(idx))
  expect_equal(vec_unique(df2), vec_slice(df, vec_unique(idx)))

  count <- vec_count(df2)
  expect_equal(count$key, df)
  expect_equal(count$count, vec_count(idx)$count)
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

