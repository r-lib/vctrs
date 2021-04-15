test_that("can rank with different types of `ties`", {
  x <- c(2L, 5L, 1L, 1L, 2L)

  expect_identical(vec_rank(x, ties = "min"), rank(x, ties.method = "min"))
  expect_identical(vec_rank(x, ties = "max"), rank(x, ties.method = "max"))
  expect_identical(vec_rank(x, ties = "sequential"), rank(x, ties.method = "first"))
  expect_identical(vec_rank(x, ties = "dense"), c(2L, 3L, 1L, 1L, 2L))
})

test_that("can rank in descending order", {
  x <- c(2L, 5L, 1L, 1L, 2L)

  expect_identical(vec_rank(x, ties = "min", direction = "desc"), rank(-x, ties.method = "min"))
  expect_identical(vec_rank(x, ties = "max", direction = "desc"), rank(-x, ties.method = "max"))
  expect_identical(vec_rank(x, ties = "sequential", direction = "desc"), rank(-x, ties.method = "first"))
  expect_identical(vec_rank(x, ties = "dense", direction = "desc"), c(2L, 1L, 3L, 3L, 2L))
})

test_that("can propagate missing values", {
  x <- c(2, NA, 4, NaN, 4, 2, NA)

  expect_identical(vec_rank(x, ties = "min", na_propagate = TRUE), rank(x, ties.method = "min", na.last = "keep"))
  expect_identical(vec_rank(x, ties = "max", na_propagate = TRUE), rank(x, ties.method = "max", na.last = "keep"))
  expect_identical(vec_rank(x, ties = "sequential", na_propagate = TRUE), rank(x, ties.method = "first", na.last = "keep"))
  expect_identical(vec_rank(x, ties = "dense", na_propagate = TRUE), c(1L, NA, 2L, NA, 2L, 1L, NA))

  # NaN are treated as missing, regardless of whether or not they are distinct from NA_real_
  expect_identical(
    vec_rank(x, ties = "min", na_propagate = TRUE, nan_distinct = TRUE),
    vec_rank(x, ties = "min", na_propagate = TRUE, nan_distinct = FALSE)
  )
})

test_that("works correctly when `na_propagate = TRUE` with no missing values", {
  x <- c(1, 2, 1, 5, 2)
  expect_identical(vec_rank(x, na_propagate = TRUE), rank(x, ties.method = "min"))
})

test_that("when `na_propagate = FALSE`, all NA (or NaN) values get the same rank", {
  # this is in contrast to rank(), which treats all NA (NaN) as different
  x <- c(1, NA, 3, NaN, NA, 1, NaN)

  expect_identical(vec_rank(x, na_value = "largest"), c(1L, 4L, 3L, 4L, 4L, 1L, 4L))
  expect_identical(vec_rank(x, na_value = "smallest"), c(5L, 1L, 7L, 1L, 1L, 5L, 1L))

  # If distinct, NaN are always ranked between real numbers and NA_real_
  expect_identical(vec_rank(x, na_value = "largest", nan_distinct = TRUE), c(1L, 6L, 3L, 4L, 6L, 1L, 4L))
  expect_identical(vec_rank(x, na_value = "smallest", nan_distinct = TRUE), c(5L, 1L, 7L, 3L, 1L, 5L, 3L))
})

test_that("ranks character vectors in the C locale", {
  x <- c("B", "b", "a")
  expect_identical(vec_rank(x), c(1L, 3L, 2L))
})

test_that("works with data frames", {
  df <- data_frame(
    x = c(1, 2, 1, 2, 2),
    y = c(2, 2, 1, 2, 5)
  )

  expect_identical(vec_rank(df, ties = "min"), c(2L, 3L, 1L, 3L, 5L))
  expect_identical(vec_rank(df, ties = "sequential"), c(2L, 3L, 1L, 4L, 5L))
})

test_that("can control the direction per column", {
  df <- data_frame(
    x = c(1, 2, 1, 2, 2),
    y = c(2, 2, 1, 2, 5)
  )

  df2 <- df
  df2$y <- -df2$y

  expect_identical(
    vec_rank(df, direction = c("asc", "desc")),
    vec_rank(df2, direction = "asc")
  )
})

test_that("completely missing rows can propagate NA", {
  df <- data_frame(
    x = c(1, NA, NA),
    y = c(NA, NA, 1)
  )

  expect_identical(vec_rank(df, na_propagate = TRUE), c(1L, NA, 2L))
  expect_identical(vec_rank(df, na_propagate = TRUE, direction = "desc"), c(2L, NA, 1L))
})

test_that("partially missing rows are controlled by `na_value`", {
  df <- data_frame(
    x = c(1, 1, NA, NA, NA),
    y = c(3, NA, NA, 2, 1)
  )

  expect_identical(
    vec_rank(df, na_value = c("largest", "smallest")),
    c(2L, 1L, 3L, 5L, 4L)
  )
  expect_identical(
    vec_rank(df, na_value = c("largest", "smallest"), na_propagate = TRUE),
    c(2L, 1L, NA, 4L, 3L)
  )

  expect_identical(
    vec_rank(df, na_value = c("largest", "smallest"), direction = "desc"),
    c(4L, 5L, 3L, 1L, 2L)
  )
  expect_identical(
    vec_rank(df, na_value = c("largest", "smallest"), na_propagate = TRUE, direction = "desc"),
    c(3L, 4L, NA, 1L, 2L)
  )
})

test_that("`x` must be a vector", {
  expect_error(vec_rank(identity), class = "vctrs_error_scalar_type")
})

test_that("`ties` is validated", {
  expect_error(vec_rank(1, ties = "foo"), "must be one of")
  expect_error(vec_rank(1, ties = 1), "must be a character")
})

test_that("`na_propagate` is validated", {
  expect_error(vec_rank(1, na_propagate = NA), "must be a logical value")
  expect_error(vec_rank(1, na_propagate = c(TRUE, FALSE)), "must be a logical value")
  expect_error(vec_rank(1, na_propagate = "foo"), "must be a logical value")
})
