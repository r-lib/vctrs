
test_that("can split empty vector", {
  out <- vec_split(integer(), character())

  expect_s3_class(out, "data.frame")
  expect_equal(out$key, character())
  expect_equal(out$val, list())
})

test_that("split data frame with data frame", {
  df <- data.frame(x = c(1, 1, 2), y = c(1, 1, 1))
  out <- vec_split(df, df)

  expect_s3_class(out, "data.frame")
  expect_equal(out$key, data.frame(x = c(1, 2), y = c(1, 1)))
  expect_equal(out$val, list(
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
  local_comparable_tuple()
  x <- tuple(c(1, 2, 1), 1:3)
  expect_identical(nrow(vec_split(1:3, x)), 2L)
})

test_that("split works with different encodings", {
  encs <- encodings()
  expect_identical(nrow(vec_split(1:3, encs)), 1L)
})

test_that("`key` and `value` retain names", {
  x <- c(a = 1, b = 2, c = 1, a = 1)
  split <- vec_split(x, x)
  expect_identical(split$key, c(a = 1, b = 2))
  expect_identical(split$val[[1]], c(a = 1, c = 1, a = 1))
  expect_identical(split$val[[2]], c(b = 2))
})

