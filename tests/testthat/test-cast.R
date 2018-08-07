context("test-cast")

# NULL --------------------------------------------------------------------

test_that("NULL is idempotent", {
  expect_equal(vec_cast(NULL, NULL), NULL)
  expect_equal(vec_cast(list(1:3), NULL), list(1:3))
})

# Default --------------------------------------------------------------------

test_that("new classes are uncoercible by default", {
  x <- structure(1:10, class = "vctrs_nonexistant")
  expect_error(vec_cast(1, x), class = "error_no_cast")
  expect_error(vec_cast(x, 1), class = "error_no_cast")
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
  expect_condition(vec_cast(2L, logical()), class = "warning_cast_lossy")
  expect_condition(vec_cast(2, logical()), class = "warning_cast_lossy")
  expect_condition(vec_cast("x", logical()), class = "warning_cast_lossy")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), logical()), class = "error_no_cast")
})

test_that("dimensionality is preserved" ,{
  x <- matrix(1, nrow = 2, ncol = 2)
  expect_dim(vec_cast(x, logical()), c(2, 2))
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
  expect_condition(vec_cast(2.5, integer()), class = "warning_cast_lossy")
  expect_condition(vec_cast("2.5", integer()), class = "warning_cast_lossy")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), integer()), class = "error_no_cast")
})

test_that("dimensionality is preserved" ,{
  x <- matrix(TRUE, nrow = 2, ncol = 2)
  expect_dim(vec_cast(x, integer()), c(2, 2))
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
  expect_condition(vec_cast("x", double()), class = "warning_cast_lossy")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), double()), class = "error_no_cast")
})

test_that("dimensionality is preserved" ,{
  x <- matrix(TRUE, nrow = 2, ncol = 2)
  expect_dim(vec_cast(x, double()), c(2, 2))
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

test_that("dimensionality is preserved" ,{
  x <- matrix(TRUE, nrow = 2, ncol = 2)
  expect_dim(vec_cast(x, character()), c(2, 2))
})


# Lists  ------------------------------------------------------------------

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, list()), NULL)
  expect_equal(vec_cast(1:2, list()), list(1L, 2L))
})

test_that("dimensionality is preserved" ,{
  x <- matrix(TRUE, nrow = 2, ncol = 2)
  expect_dim(vec_cast(x, list()), c(2, 2))
})

# Factors -----------------------------------------------------------------

test_that("safe casts work as expected", {
  fa <- factor("a")
  fab <- factor(c("a", "b"))

  expect_equal(vec_cast(NULL, fa), NULL)

  expect_equal(vec_cast(fa, fa), fa)
  expect_equal(vec_cast(fa, fab), fab[1])
  expect_equal(vec_cast("a", fab), fab[1])

  expect_equal(vec_cast("a", factor()), fa)
  expect_equal(vec_cast(fa, factor()), fa)

  expect_equal(vec_cast(list("a", "b"), fab), fab)
})

test_that("lossy casts generate warning", {
  fa <- factor("a")
  fb <- factor("b")

  expect_condition(vec_cast(fa, fb), class = "warning_cast_lossy")
  expect_condition(vec_cast("a", fb), class = "warning_cast_lossy")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(double(), factor("a")), class = "error_no_cast")
})

test_that("orderedness of factor is preserved", {
  fct <- factor("a")
  ord <- ordered("a")

  expect_equal(vec_cast(fct, ord), ord)
  expect_equal(vec_cast("a", ord), ord)
})

# Dates -------------------------------------------------------------------

test_that("safe casts work as expected", {
  date <- as.Date("2018-01-01")
  type_date <- as_vec_type(Sys.Date())

  expect_equal(vec_cast(NULL, date), NULL)
  expect_equal(vec_cast(17532, date), date)
  expect_equal(vec_cast("2018-01-01", date), date)
  expect_equal(vec_cast(date, date), date)
  expect_equal(vec_cast(as.POSIXct(date), date), date)
  expect_equal(vec_cast(list(date), date), date)
})

test_that("lossy casts generate warning", {
  date <- as.Date("2018-01-01")
  datetime <- as.POSIXct(date) + 3600

  expect_condition(vec_cast(datetime, date), class = "warning_cast_lossy")
})

test_that("invalid casts generate error", {
  date <- as.Date("2018-01-01")
  expect_error(vec_cast(integer(), date), class = "error_no_cast")
})


# Date-times --------------------------------------------------------------

test_that("safe casts work as expected", {
  datetime <- as.POSIXct("1970-02-01", tz = "UTC")

  expect_equal(vec_cast(NULL, datetime), NULL)
  expect_equal(vec_cast(2678400, datetime), datetime)
  expect_equal(vec_cast("1970-02-01", datetime), datetime)
  expect_equal(vec_cast(datetime, datetime), datetime)
  expect_equal(vec_cast(as.Date(datetime), datetime), datetime)
  expect_equal(vec_cast(list(datetime), datetime), datetime)
})

test_that("invalid casts generate error", {
  datetime <- as.POSIXct("1970-02-01", tz = "UTC")
  expect_error(vec_cast(integer(), datetime), class = "error_no_cast")
})

test_that("dates become midnight in date-time tzone", {
  date1 <- as.Date("2010-01-01")
  datetime <- as.POSIXct(character(), tz = "Pacific/Auckland")

  date2 <- vec_cast(date1, datetime)
  expect_equal(attr(date2, "tzone"), "Pacific/Auckland")
  expect_equal(format(date2, "%H:%M"), "00:00")
})

# difftime ----------------------------------------------------------------

test_that("safe casts work as expected", {
  dt1 <- as.difftime(600, units = "secs")
  dt2 <- as.difftime(10, units = "mins")

  expect_equal(vec_cast(NULL, dt1), NULL)
  expect_equal(vec_cast(600, dt1), dt1)
  expect_equal(vec_cast(dt1, dt1), dt1)
  expect_equal(vec_cast(dt1, dt2), dt2)
  expect_equal(vec_cast(list(dt1), dt1), dt1)
})

test_that("invalid casts generate error", {
  dt <- as.difftime(600, units = "secs")
  expect_error(vec_cast(integer(), dt), class = "error_no_cast")
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

  expect_condition(vec_cast(df1, df1[1]), class = "warning_cast_lossy_dataframe")
  expect_condition(vec_cast(df2, df1), class = "warning_cast_lossy_vector")
})

test_that("invalid cast generates error", {
  expect_error(vec_cast(1L, data.frame()), class = "error_no_cast")
})

test_that("column order matches type", {
  df1 <- data.frame(x = 1, y = "a")
  df2 <- data.frame(x = TRUE, z = 3)

  df3 <- vec_cast(df2, max(vec_type(df1), vec_type(df2))$prototype)
  expect_named(df3, c("x", "y", "z"))
})

test_that("casts preserve outer class", {
  df <- data.frame(x = 1)
  dt <- tibble::tibble(x = 1)

  expect_s3_class(vec_cast(df, dt), "tbl_df")
  expect_s3_class(vec_cast(dt, df), "data.frame")
})
