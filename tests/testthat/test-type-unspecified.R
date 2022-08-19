
test_that("unknown type is idempotent", {
  types <- list(
    unspecified(),
    logical(),
    integer(),
    double(),
    character(),
    list(),
    new_list_of(ptype = integer()),
    new_factor(),
    new_ordered(),
    new_date(),
    new_datetime(),
    new_duration(),
    vec_ptype(matrix(1:4, 2)),
    vec_ptype(array(1:5, c(1, 5)))
  )

  lhs <- map(types, vec_ptype2, x = unspecified())
  expect_identical(types, lhs)

  lhs <- map(types, vec_ptype2, x = NA)
  expect_identical(types, lhs)

  rhs <- map(types, vec_ptype2, y = unspecified())
  expect_identical(types, rhs)

  rhs <- map(types, vec_ptype2, y = NA)
  expect_identical(types, rhs)
})

test_that("common type of unspecified and NULL is unspecified", {
  expect_identical(vec_ptype2(unspecified(), NULL), unspecified())
  expect_identical(vec_ptype2(NULL, unspecified()), unspecified())

  expect_identical(vec_ptype2(NA, NULL), unspecified())
  expect_identical(vec_ptype2(NULL, NA), unspecified())
})

test_that("cannot take the common type of unspecified and a scalar list", {
  expect_error(vec_ptype2(unspecified(), foobar()), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(foobar(), unspecified()), class = "vctrs_error_scalar_type")
})

test_that("subsetting works", {
  expect_identical(unspecified(4)[2:3], unspecified(2))
})

test_that("has useful print method", {
  expect_snapshot(unspecified())
})

test_that("can finalise data frame containing unspecified columns", {
  df <- data.frame(y = NA, x = c(1, 2, NA))

  ptype <- vec_ptype(df)
  expect_identical(ptype$y, unspecified())

  finalised <- vec_ptype_finalise(ptype)
  expect_identical(finalised$y, lgl())

  common <- vec_ptype_common(df, df)
  expect_identical(common$y, lgl())
})

test_that("can cast to common type data frame containing unspecified columns", {
  df <- data.frame(y = NA, x = c(1, 2, NA))
  expect_identical(vec_cast_common(df, df), list(df, df))
})

test_that("unspecified vectors are always unspecified (#222)", {
  expect_true(is_unspecified(unspecified()))
  expect_true(is_unspecified(unspecified(1)))
})

test_that("S3 vectors and shaped vectors are never unspecified", {
  expect_false(is_unspecified(foobar(NA)))
  expect_false(is_unspecified(foobar(lgl(NA, NA))))
  expect_false(is_unspecified(matrix(NA, 2)))
})

test_that("can finalise lengthy unspecified vectors", {
  expect_identical(vec_ptype_finalise(unspecified(3)), rep(NA, 3))
  expect_identical(ununspecify(unspecified(3)), rep(NA, 3))
})

test_that("unspecified() validates input", {
  expect_identical(unspecified(1), unspecified(1L))
  expect_error(unspecified(1:3), "must be a single number")
})

test_that("tibble::type_sum() knows about unspecified", {
  expect_identical(tibble::type_sum(unspecified(3)), "???")
})

test_that("casting to a scalar type errors", {
  expect_error(vec_cast(NA, quote(x)), class = "vctrs_error_scalar_type")
  expect_error(vec_cast(unspecified(1), quote(x)), class = "vctrs_error_scalar_type")
})

test_that("monitoring test - can cast to unspecified from unspecified", {
  expect_identical(vec_cast(NA, unspecified()), unspecified(1))
  expect_identical(vec_cast(unspecified(1), unspecified()), unspecified(1))
})

test_that("monitoring test - casting unspecified input to NA unspecified results in NA vector", {
  expect_identical(vec_cast(unspecified(1), NA), NA)
  expect_identical(vec_cast(NA, NA), NA)
})
