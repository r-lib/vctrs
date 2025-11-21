test_that("can rank with different types of `ties`", {
  x <- c(2L, 5L, 1L, 1L, 2L)

  expect_identical(vec_rank(x, ties = "min"), rank(x, ties.method = "min"))
  expect_identical(vec_rank(x, ties = "max"), rank(x, ties.method = "max"))
  expect_identical(
    vec_rank(x, ties = "sequential"),
    rank(x, ties.method = "first")
  )
  expect_identical(vec_rank(x, ties = "dense"), c(2L, 3L, 1L, 1L, 2L))
})

test_that("can rank in descending order", {
  x <- c(2L, 5L, 1L, 1L, 2L)

  expect_identical(
    vec_rank(x, ties = "min", direction = "desc"),
    rank(-x, ties.method = "min")
  )
  expect_identical(
    vec_rank(x, ties = "max", direction = "desc"),
    rank(-x, ties.method = "max")
  )
  expect_identical(
    vec_rank(x, ties = "sequential", direction = "desc"),
    rank(-x, ties.method = "first")
  )
  expect_identical(
    vec_rank(x, ties = "dense", direction = "desc"),
    c(2L, 1L, 3L, 3L, 2L)
  )
})

test_that("can rank incomplete values with `NA`", {
  x <- c(2, NA, 4, NaN, 4, 2, NA)

  expect_identical(
    vec_rank(x, ties = "min", incomplete = "na"),
    rank(x, ties.method = "min", na.last = "keep")
  )
  expect_identical(
    vec_rank(x, ties = "max", incomplete = "na"),
    rank(x, ties.method = "max", na.last = "keep")
  )
  expect_identical(
    vec_rank(x, ties = "sequential", incomplete = "na"),
    rank(x, ties.method = "first", na.last = "keep")
  )
  expect_identical(
    vec_rank(x, ties = "dense", incomplete = "na"),
    c(1L, NA, 2L, NA, 2L, 1L, NA)
  )

  # NaN are treated as missing, regardless of whether or not they are distinct from NA_real_
  expect_identical(
    vec_rank(x, ties = "min", incomplete = "na", nan_distinct = TRUE),
    vec_rank(x, ties = "min", incomplete = "na", nan_distinct = FALSE)
  )
})

test_that("works correctly when `incomplete = 'na'` with no missing values", {
  x <- c(1, 2, 1, 5, 2)
  expect_identical(vec_rank(x, incomplete = "na"), rank(x, ties.method = "min"))
})

test_that("when ranking incomplete values, all NA (or NaN) values get the same rank", {
  # this is in contrast to rank(), which treats all NA (NaN) as different
  x <- c(1, NA, 3, NaN, NA, 1, NaN)

  expect_identical(
    vec_rank(x, na_value = "largest"),
    c(1L, 4L, 3L, 4L, 4L, 1L, 4L)
  )
  expect_identical(
    vec_rank(x, na_value = "smallest"),
    c(5L, 1L, 7L, 1L, 1L, 5L, 1L)
  )

  # If distinct, NaN are always ranked between real numbers and NA_real_
  expect_identical(
    vec_rank(x, na_value = "largest", nan_distinct = TRUE),
    c(1L, 6L, 3L, 4L, 6L, 1L, 4L)
  )
  expect_identical(
    vec_rank(x, na_value = "smallest", nan_distinct = TRUE),
    c(5L, 1L, 7L, 3L, 1L, 5L, 3L)
  )
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

test_that("works with data frames with 0 columns and >0 rows (#1863)", {
  # All rows are treated as being from the same group
  df <- data_frame(.size = 5)

  expect_identical(vec_rank(df, ties = "min"), c(1L, 1L, 1L, 1L, 1L))
  expect_identical(vec_rank(df, ties = "sequential"), c(1L, 2L, 3L, 4L, 5L))
  expect_identical(
    vec_rank(df, ties = "sequential", direction = "desc"),
    c(1L, 2L, 3L, 4L, 5L)
  )
})

test_that("works with data frames with 0 columns and 0 rows (#1863)", {
  df <- data_frame(.size = 0)

  expect_identical(vec_rank(df, ties = "min"), integer())
  expect_identical(vec_rank(df, ties = "sequential"), integer())
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

test_that("incompleteness is respected in data frames and rcrds", {
  df <- data_frame(
    x = c(1, NA, NA, 1),
    y = c(NA, NA, 1, 1)
  )

  expect_identical(vec_rank(df, incomplete = "na"), c(NA, NA, NA, 1L))
  expect_identical(
    vec_rank(df, incomplete = "na", direction = "desc"),
    c(NA, NA, NA, 1L)
  )

  x <- new_rcrd(list(
    x = c(1, 1, NA, NA, 1),
    y = c(1, NA, 1, NA, 1)
  ))

  expect_identical(vec_rank(x, incomplete = "na"), c(1L, NA, NA, NA, 1L))
})

test_that("can control `na_value` per column", {
  df <- data_frame(
    x = c(1, 1, NA, NA, NA),
    y = c(3, NA, NA, 2, 1)
  )

  expect_identical(
    vec_rank(df, na_value = c("largest", "smallest")),
    c(2L, 1L, 3L, 5L, 4L)
  )
  expect_identical(
    vec_rank(df, na_value = c("largest", "smallest"), direction = "desc"),
    c(4L, 5L, 3L, 1L, 2L)
  )

  # But `incomplete = "na"` overrules it
  expect_identical(
    vec_rank(df, na_value = c("largest", "smallest"), incomplete = "na"),
    c(1L, NA, NA, NA, NA)
  )
  expect_identical(
    vec_rank(
      df,
      na_value = c("largest", "smallest"),
      incomplete = "na",
      direction = "desc"
    ),
    c(1L, NA, NA, NA, NA)
  )
})

test_that("`x` must be a vector", {
  expect_error(vec_rank(identity), class = "vctrs_error_scalar_type")
})

test_that("`x` must not be `NULL` (#1823, #1967)", {
  expect_snapshot(error = TRUE, {
    vec_rank(NULL)
  })
  expect_snapshot(error = TRUE, {
    vec_rank(NULL, incomplete = "na")
  })
  expect_snapshot(error = TRUE, {
    vec_rank(NULL, ties = "sequential", incomplete = "na")
  })
})

test_that("`ties` is validated", {
  expect_snapshot(error = TRUE, vec_rank(1, ties = "foo"))
  expect_snapshot(error = TRUE, vec_rank(1, ties = 1))
})

test_that("`incomplete` is validated", {
  expect_snapshot(error = TRUE, vec_rank(1, incomplete = NA))
  expect_snapshot(error = TRUE, vec_rank(1, incomplete = c(TRUE, FALSE)))
  expect_snapshot(error = TRUE, vec_rank(1, incomplete = "foo"))
})
