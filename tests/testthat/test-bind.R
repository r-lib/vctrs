context("test-bind")

test_that("empty inputs return an empty data frame", {
  expect_equal(vec_rbind(), data_frame())
  expect_equal(vec_rbind(NULL, NULL), data_frame())
})

test_that("NULL is idempotent", {
  df <- data_frame(x = 1)
  expect_equal(vec_rbind(df, NULL), df)
})


test_that("type of column is common type of individual columns", {
  x_int <- data_frame(x = 1L)
  x_dbl <- data_frame(x = 2.5)
  x_chr <- data_frame(x = "a")

  expect_equal(vec_rbind(x_int, x_int), data_frame(x = c(1L, 1L)))
  expect_equal(vec_rbind(x_int, x_dbl), data_frame(x = c(1, 2.5)))

  expect_error(vec_rbind(x_int, x_chr), class = "error_no_max_type")
})

test_that("result contains union of columns", {
  expect_named(
    vec_rbind(
      data_frame(x = 1),
      data_frame(y = 1)
    ),
    c("x" , "y")
  )

  expect_named(
    vec_rbind(
      data_frame(y = 1, x = 1),
      data_frame(y = 1, z = 2)
    ),
    c("y", "x", "z")
  )
})

test_that("all inputs coerced to data frames", {
  expect_equal(
    vec_rbind(data_frame(x = 1L), c(x = 1.5)),
    data_frame(x = c(1, 1.5))
  )
})

test_that("names are supplied if needed", {
  expect_message(out <- vec_rbind(data_frame(..1 = 1), 1), "->")
  expect_equal(out, data_frame(..1 = c(1, 1)))
})
