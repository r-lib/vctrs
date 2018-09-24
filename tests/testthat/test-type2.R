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
    new_date(),
    new_datetime(),
    new_datetime(tzone = "US/Central"),
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
