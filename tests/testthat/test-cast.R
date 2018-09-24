context("test-cast")

# NULL --------------------------------------------------------------------

test_that("NULL is idempotent", {
  expect_equal(vec_cast(NULL, NULL), NULL)
  expect_equal(vec_cast(list(1:3), NULL), list(1:3))
})

# Default --------------------------------------------------------------------

test_that("new classes are uncoercible by default", {
  x <- structure(1:10, class = "vctrs_nonexistant")
  expect_error(vec_cast(1, x), class = "error_incompatible_cast")
  expect_error(vec_cast(x, 1), class = "error_incompatible_cast")
})

# Logical -----------------------------------------------------------------

test_that("safe casts work as expeced", {
  expect_equal(vec_cast(NULL, logical()), NULL)
  expect_equal(vec_cast(TRUE, logical()), TRUE)
  expect_equal(vec_cast(1L, logical()), TRUE)
  expect_equal(vec_cast(1, logical()), TRUE)
  expect_equal(vec_cast("TRUE", logical()), TRUE)
  expect_equal(vec_cast(list(1), logical()), TRUE)
})

test_that("lossy casts generate warning", {
  expect_condition(vec_cast(2L, logical()), class = "warning_lossy_cast")
  expect_condition(vec_cast(2, logical()), class = "warning_lossy_cast")
  expect_condition(vec_cast("x", logical()), class = "warning_lossy_cast")
  expect_condition(vec_cast(list(c(TRUE, FALSE)), logical()), class = "warning_lossy_cast")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), logical()), class = "error_incompatible_cast")
})

test_that("dimensionality matches output" ,{
  x <- matrix(1, nrow = 2, ncol = 2)
  expect_equal(vec_cast(x, logical()), rep(TRUE, 4))

  x1 <- matrix(TRUE, nrow = 1, ncol = 1)
  x2 <- matrix(1, nrow = 0, ncol = 2)
  expect_dim(vec_cast(x1, x2), c(1, 2))
})

# Integer -----------------------------------------------------------------

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, integer()), NULL)
  expect_equal(vec_cast(TRUE, integer()), 1L)
  expect_equal(vec_cast(1L, integer()), 1L)
  expect_equal(vec_cast(1, integer()), 1L)
  expect_equal(vec_cast("1", integer()), 1L)
  expect_equal(vec_cast(list(1L), integer()), 1L)
})

test_that("lossy casts generate warning", {
  expect_condition(vec_cast(2.5, integer()), class = "warning_lossy_cast")
  expect_condition(vec_cast("2.5", integer()), class = "warning_lossy_cast")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), integer()), class = "error_incompatible_cast")
})

# Double ------------------------------------------------------------------

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, double()), NULL)
  expect_equal(vec_cast(TRUE, double()), 1L)
  expect_equal(vec_cast(1.5, double()), 1.5)
  expect_equal(vec_cast(1.5, double()), 1.5)
  expect_equal(vec_cast("1.5", double()), 1.5)
  expect_equal(vec_cast(list(1.5), double()), 1.5)
})

test_that("lossy casts generate warning", {
  expect_condition(vec_cast("x", double()), class = "warning_lossy_cast")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), double()), class = "error_incompatible_cast")
})

# Character ---------------------------------------------------------------

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, character()), NULL)
  expect_equal(vec_cast(TRUE, character()), "TRUE")
  expect_equal(vec_cast(list("x"), character()), "x")
})

test_that("difftime gets special treatment", {
  dt1 <- as.difftime(600, units = "secs")

  expect_equal(vec_cast(dt1, character()), "600 secs")
})

# Lists  ------------------------------------------------------------------

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, list()), NULL)
  expect_equal(vec_cast(1:2, list()), list(1L, 2L))
  expect_equal(vec_cast(list(1L, 2L), list()), list(1L, 2L))
})


# data frames -------------------------------------------------------------

test_that("safe casts work as expected", {
  df <- data.frame(x = 1, y = 0)

  expect_equal(vec_cast(NULL, df), NULL)
  expect_equal(vec_cast(df, df), df)

  expect_equal(vec_cast(data.frame(x = TRUE, y = FALSE), df), df)
})

test_that("warn about lossy coercions", {
  df1 <- data.frame(x = 1, y = 1)
  df2 <- data.frame(x = "a", stringsAsFactors = FALSE)

  expect_condition(vec_cast(df1, df1[1]), class = "warning_lossy_cast")
  expect_condition(vec_cast(df2, df1), class = "warning_lossy_cast")
})

test_that("invalid cast generates error", {
  expect_error(vec_cast(1L, data.frame()), class = "error_incompatible_cast")
})

test_that("column order matches type", {
  df1 <- data.frame(x = 1, y = "a")
  df2 <- data.frame(x = TRUE, z = 3)

  df3 <- vec_cast(df2, vec_ptype(df1, df2)[[1]])
  expect_named(df3, c("x", "y", "z"))
})

test_that("casts preserve outer class", {
  df <- data.frame(x = 1)
  dt <- tibble::tibble(x = 1)

  expect_s3_class(vec_cast(df, dt), "tbl_df")
  expect_s3_class(vec_cast(dt, df), "data.frame")
})

test_that("recast generates correct row/col names", {
  df1 <- data.frame(x = 1:4, y = 1:4, z = 1:4)
  df2 <- vec_restore(lapply(df1[1:3], `[`, 1:2), df1)

  expect_named(df2, c("x", "y", "z"))
  expect_equal(.row_names_info(df2), -2)
})

test_that("recast makes tibbles", {
  df1 <- tibble::tibble(x = 1:4)
  df2 <- vec_restore(vec_data(df1), df1)

  expect_s3_class(df1, "tbl_df")
})
