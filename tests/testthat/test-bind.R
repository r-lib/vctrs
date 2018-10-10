context("test-bind")

# rows --------------------------------------------------------------------

test_that("empty inputs return an empty data frame", {
  expect_equal(vec_rbind(), data_frame())
  expect_equal(vec_rbind(NULL, NULL), data_frame())
})

test_that("NULL is idempotent", {
  df <- data_frame(x = 1)
  expect_equal(vec_rbind(df, NULL), df)
})

test_that("output is tibble if any input is tibble", {
  df <- data_frame(x = 1)
  dt <- tibble::tibble(x = 1)

  expect_s3_class(vec_rbind(dt), "tbl_df")
  expect_s3_class(vec_rbind(dt, df), "tbl_df")
  expect_s3_class(vec_rbind(df, dt), "tbl_df")
})

test_that("type of column is common type of individual columns", {
  x_int <- data_frame(x = 1L)
  x_dbl <- data_frame(x = 2.5)
  x_chr <- data_frame(x = "a")

  expect_equal(vec_rbind(x_int, x_int), data_frame(x = c(1L, 1L)))
  expect_equal(vec_rbind(x_int, x_dbl), data_frame(x = c(1, 2.5)))

  expect_error(vec_rbind(x_int, x_chr), class = "error_incompatible_type")
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

test_that("matrix becomes data frame", {
  x <- matrix(1:4, nrow = 2)
  expect_equal(vec_rbind(x), data.frame(V1 = 1:2, V2 = 3:4))
})


# cols --------------------------------------------------------------------

test_that("empty inputs give data frame", {
  expect_equal(vec_cbind(), data_frame())
  expect_equal(vec_cbind(NULL), data_frame())
})

test_that("NULL is idempotent", {
  df <- data_frame(x = 1)
  expect_equal(vec_cbind(df, NULL), df)
})

test_that("outer names are respected", {
  expect_named(vec_cbind(x = 1, y = 4), c("x", "y"))
  expect_named(vec_cbind(a = data.frame(x = 1)), "a..x")
})

test_that("inner names are respected", {
  expect_named(vec_cbind(data_frame(x = 1), data_frame(y = 1)), c("x", "y"))
})

test_that("nameless vectors get tidy defaults", {
  expect_named(vec_cbind(1:2, 1), c("..1", "..2"))
})

test_that("matrix becomes data frame", {
  x <- matrix(1:4, nrow = 2)
  expect_equal(vec_cbind(x), data.frame(V1 = 1:2, V2 = 3:4))

  # respecting outer names
  expect_equal(vec_cbind(x = x), data.frame(x1 = 1:2, x2 = 3:4))
})


test_that("duplicate names are de-deduplicated", {
  expect_named(vec_cbind(x = 1, x = 1), c("x..1", "x..2"))
  expect_named(vec_cbind(data.frame(x = 1), data.frame(x = 1)), c("x..1", "x..2"))
})

test_that("rows recycled to longest", {
  df <- data.frame(x = 1:3)

  expect_dim(vec_cbind(df), c(3, 1))
  expect_dim(vec_cbind(df, NULL), c(3, 1))
  expect_dim(vec_cbind(df, y = 1), c(3, 2))
  expect_dim(vec_cbind(data.frame(x = 1), y = 1:3), c(3, 2))

  expect_dim(
    vec_cbind(
      x = data.frame(a = 1, b = 2),
      y = 1:3
    ),
    c(3, 3)
  )
})

test_that("output is tibble if any input is tibble", {
  df <- data.frame(x = 1)
  dt <- tibble::tibble(y = 2)

  expect_s3_class(vec_cbind(dt), "tbl_df")
  expect_s3_class(vec_cbind(df, dt), "tbl_df")
  expect_s3_class(vec_cbind(dt, df), "tbl_df")
})

test_that("can override default .nrow", {
  expect_dim(vec_cbind(1, .size = 3), c(3, 1))
})

test_that("can't violate recycling rules", {
  expect_error(vec_cbind(1:2, .size = 3), "Incompatible")
})
