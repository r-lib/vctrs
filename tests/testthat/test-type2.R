context("test-type-coerce")

# High-level regression + symmetry tests ---------------------------------------------

test_that("base coercions are symmetric and unchanging", {
  types <- list(unknown(), logical(), integer(), double(), character(), list())
  mat <- maxtype_mat(types)

  expect_true(isSymmetric(mat))
  expect_known_output(mat, test_path("test-type-coerce-base.txt"), print = TRUE)
})

test_that("datetime coercions are symmetric and unchanging", {
  types <- list(
    unknown(),
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
    unknown(),
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
    unknown(),
    list(),
    list_of(.ptype = integer()),
    list_of(.ptype = double()),
    list_of(.ptype = character())
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
  expect_error(vec_type2(1, x), class = "error_incompatible_type")
  expect_error(vec_type2(x, 1), class = "error_incompatible_type")
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
  fa <- vec_ptype(factor(levels = "a"))
  fb <- vec_ptype(factor(levels = "b"))

  expect_equal(vec_ptype(fa, fb), vec_ptype(factor(levels = c("a", "b"))))
  expect_equal(vec_ptype(fb, fa), vec_ptype(factor(levels = c("b", "a"))))
})

# Data frame --------------------------------------------------------------

test_that("data frame only combines with other data frames or NULL", {
  dt <- data.frame(x = 1)
  expect_equal(vec_ptype(dt, NULL), vec_ptype(dt))
  expect_error(vec_ptype(dt, 1:10), class = "error_incompatible_type")
})

test_that("data frame takes max of individual variables", {
  dt1 <- data.frame(x = FALSE, y = 1L)
  dt2 <- data.frame(x = 1.5, y = 1.5)

  expect_equal(vec_ptype(dt1, dt2), vec_ptype(dt2))
})

test_that("data frame combines variables", {
  dt1 <- data.frame(x = 1)
  dt2 <- data.frame(y = 1)

  dt3 <- max(dt1, dt2)
  expect_equal(
    vec_ptype(dt1, dt2),
    vec_ptype(data.frame(x = double(), y = double()))
  )
})

test_that("tibble beats data frame", {
  df <- vec_ptype(data_frame())
  dt <- vec_ptype(tibble::tibble())

  expect_s3_class(vec_ptype(dt, df)[[1]], "tbl_df")
  expect_s3_class(vec_ptype(dt, df)[[1]], "tbl_df")
})
