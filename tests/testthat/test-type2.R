context("test-type-coerce")

# High-level regression + symmetry tests ---------------------------------------------

test_that("base coercions are symmetric and unchanging", {
  types <- list(NULL, logical(), integer(), double(), character(), list())
  mat <- maxtype_mat(types)

  expect_true(isSymmetric(mat))
  expect_known_output(mat, test_path("test-type-coerce-base.txt"), print = TRUE)
})

test_that("datetime coercions are symmetric and unchanging", {
  types <- list(
    NULL,
    Sys.Date(),
    Sys.time(),
    difftime(Sys.time() + 1000, Sys.time()),
    difftime(Sys.time() + 1, Sys.time())
  )
  mat <- maxtype_mat(types)

  expect_true(isSymmetric(mat))
  expect_known_output(
    mat,
    test_path("test-type-coerce-date-time.txt"),
    print = TRUE,
    width = 200
  )
})

test_that("factor/character coercions are symmetric and unnchanging", {
  types <- list(
    NULL,
    ordered(character()),
    factor(),
    character()
  )
  mat <- maxtype_mat(types)
  expect_true(isSymmetric(mat))

  expect_known_output(
    mat,
    print = TRUE,
    test_path("test-type-coerce-factor.txt"),
    width = 200
  )
})

test_that("list coercions are symmetric and unchanging", {
  types <- list(
    NULL,
    list(),
    list_of(.type = integer()),
    list_of(.type = double()),
    list_of(.type = character())
  )
  mat <- maxtype_mat(types)

  expect_true(isSymmetric(mat))
  expect_known_output(
    mat,
    test_path("test-type-coerce-lists.txt"),
    print = TRUE,
    width = 200
  )
})

test_that("new classes are uncoercible by default", {
  x <- structure(1:10, class = "vctrs_nonexistant")
  expect_error(vec_type2(1, x), class = "error_no_max_type")
  expect_error(vec_type2(x, 1), class = "error_no_max_type")
})


# Date times --------------------------------------------------------------

test_that("tz comes from first non-empty", {
  # On the assumption that if you've set the time zone explicitly it
  # should win

  x <- as.POSIXct("2020-01-01")
  y <- as.POSIXct("2020-01-01", tz = "America/New_York")

  expect_equal(vec_type2(x, y), y[0])
  expect_equal(vec_type2(y, x), y[0])

  z <- as.POSIXct("2020-01-01", tz = "Pacific/Auckland")
  expect_equal(vec_type2(y, z), y[0])
  expect_equal(vec_type2(z, y), z[0])
})

# Factors -----------------------------------------------------------------

test_that("factors level are unioned", {
  # This is technically incorrect, but because of R's existing behaviour
  # anything else will cause substantial friction.
  fa <- vec_type(factor(levels = "a"))
  fb <- vec_type(factor(levels = "b"))

  expect_equal(max(fa, fb), vec_type(factor(levels = c("a", "b"))))
  expect_equal(max(fb, fa), vec_type(factor(levels = c("b", "a"))))
})

# Data frame --------------------------------------------------------------

test_that("data frame only combines with other data frames or NULL", {
  dt <- vec_type(data.frame(x = 1))

  expect_equal(max(dt, NULL), dt)
  expect_error(max(dt, 1:10), class = "error_no_max_type")
})

test_that("data frame takes max of individual variables", {
  dt1 <- vec_type(data.frame(x = FALSE, y = 1L))
  dt2 <- vec_type(data.frame(x = 1.5, y = 1.5))

  expect_equal(max(dt1, dt2), dt2)
})

test_that("data frame combines variables", {
  dt1 <- vec_type(data.frame(x = 1))
  dt2 <- vec_type(data.frame(y = 1))

  dt3 <- max(dt1, dt2)
  expect_equal(dt3$prototype, data.frame(x = double(), y = double()))
})

test_that("tibble beats data frame", {
  df <- vec_type(data_frame())
  dt <- vec_type(tibble::tibble())

  expect_s3_class(max(dt, df)$prototype, "tbl_df")
  expect_s3_class(max(dt, df)$prototype, "tbl_df")
})
