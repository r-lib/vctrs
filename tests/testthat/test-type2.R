context("test-type-coerce")

# High-level regression + symmetry tests ---------------------------------------------

test_that("base coercions are symmetric and unchanging", {
  types <- list(
    logical(),
    integer(),
    double(),
    character(),
    list()
  )
  mat <- maxtype_mat(types)

  expect_true(isSymmetric(mat))
  expect_known_output(mat, test_path("test-type-coerce-base.txt"), print = TRUE)
})

test_that("datetime coercions are symmetric and unchanging", {
  types <- list(
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
