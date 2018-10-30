context("test-type-data-frame")

# printing ----------------------------------------------------------------

test_that("data frames print nicely", {
  expect_equal(vec_ptype_abbr(mtcars), "df[,11]")

  expect_known_output(
    file = test_path("test-type-data-frame.txt"),
    {
      cat("mtcars:\n")
      vec_ptype(mtcars)
      cat("\n")
      cat("iris:\n")
      vec_ptype(iris)
    }
  )
})

test_that("embedded data frames print nicely", {
  df <- data.frame(x = 1:3)
  df$a <- data.frame(a = 1:3, b = letters[1:3])
  df$b <- list_of(1, 2, 3)
  df$c <- as_list_of(split(data.frame(x = 1:3, y = letters[1:3]), 1:3))

  expect_known_output(
    file = test_path("test-type-data-frame-embedded.txt"),
    {
      vec_ptype(df)
    }
  )
})

# coercing ----------------------------------------------------------------

test_that("data frame only combines with other data frames or NULL", {
  dt <- data.frame(x = 1)
  expect_equal(vec_type_common(dt, NULL), vec_type(dt))
  expect_error(vec_type_common(dt, 1:10), class = "error_incompatible_type")
})

test_that("data frame takes max of individual variables", {
  dt1 <- data.frame(x = FALSE, y = 1L)
  dt2 <- data.frame(x = 1.5, y = 1.5)

  expect_equal(vec_type_common(dt1, dt2), vec_type_common(dt2))
})

test_that("data frame combines variables", {
  dt1 <- data.frame(x = 1)
  dt2 <- data.frame(y = 1)

  dt3 <- max(dt1, dt2)
  expect_equal(
    vec_type_common(dt1, dt2),
    vec_type_common(data.frame(x = double(), y = double()))
  )
})

# casting -----------------------------------------------------------------

test_that("safe casts work as expected", {
  df <- data.frame(x = 1, y = 0)

  expect_equal(vec_cast(NULL, df), NULL)
  expect_equal(vec_cast(df, df), df)

  expect_equal(vec_cast(data.frame(x = TRUE, y = FALSE), df), df)
})

test_that("warn about lossy coercions", {
  df1 <- data.frame(x = 1, y = 1)
  df2 <- data.frame(x = c("a", 1), stringsAsFactors = FALSE)

  expect_condition(vec_cast(df1, df1[1]), class = "warning_lossy_cast")
  expect_condition(vec_cast(df2, df1), class = "warning_lossy_cast")
})

test_that("invalid cast generates error", {
  expect_error(vec_cast(1L, data.frame()), class = "error_incompatible_cast")
})

test_that("column order matches type", {
  df1 <- data.frame(x = 1, y = "a")
  df2 <- data.frame(x = TRUE, z = 3)

  df3 <- vec_cast(df2, vec_type_common(df1, df2))
  expect_named(df3, c("x", "y", "z"))
})

test_that("casts preserve outer class", {
  df <- data.frame(x = 1)
  dt <- tibble::tibble(x = 1)

  expect_s3_class(vec_cast(df, dt), "tbl_df")
  expect_s3_class(vec_cast(dt, df), "data.frame")
})

test_that("restore generates correct row/col names", {
  df1 <- data.frame(x = 1:4, y = 1:4, z = 1:4)
  df2 <- vec_restore(lapply(df1[1:3], `[`, 1:2), df1)

  expect_named(df2, c("x", "y", "z"))
  expect_equal(.row_names_info(df2), -2)
})
