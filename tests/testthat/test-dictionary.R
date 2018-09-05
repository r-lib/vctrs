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
