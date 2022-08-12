
test_that("vec_as_subscript() coerces unspecified vectors", {
  expect_identical(
    vec_as_subscript(NA),
    NA
  )
  expect_identical(
    vec_as_subscript(NA, logical = "error"),
    na_int
  )
  expect_identical(
    vec_as_subscript(NA, logical = "error", numeric = "error"),
    na_chr
  )
})

test_that("vec_as_subscript() coerces subtypes and supertypes", {
  expect_identical(vec_as_subscript(factor("foo")), "foo")

  with_lgl_subtype({
    expect_identical(vec_as_subscript(new_lgl_subtype(TRUE)), TRUE)
  })
  with_lgl_supertype({
    expect_identical(vec_as_subscript(new_lgl_supertype(TRUE)), TRUE)
  })
})

test_that("vec_as_subscript() handles NULL", {
  expect_identical(vec_as_subscript(NULL), int())
  expect_error(
    vec_as_subscript(NULL, numeric = "error"),
    class = "vctrs_error_subscript_type"
  )
})

test_that("vec_as_subscript() handles symbols", {
  expect_identical(vec_as_subscript(quote(foo)), "foo")
  expect_identical(vec_as_subscript(quote(`<U+5E78>`)), "\u5e78")
  expect_error(
    vec_as_subscript(quote(foo), character = "error"),
    class = "vctrs_error_subscript_type"
  )
})

test_that("can customise subscript errors", {
  expect_snapshot({
    (expect_error(
      with_tibble_cols(vec_as_subscript(env())),
      class = "vctrs_error_subscript_type"
    ))
  })

  expect_snapshot({
    (expect_error(
      with_dm_tables(vec_as_subscript(env())),
      class = "vctrs_error_subscript_type"
    ))
  })
})

test_that("vec_as_subscript() checks dimensionality", {
  expect_snapshot({
    (expect_error(vec_as_subscript(matrix(TRUE, nrow = 1)), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_subscript(array(TRUE, dim = c(1, 1, 1))), class = "vctrs_error_subscript_type"))
    (expect_error(with_tibble_rows(vec_as_subscript(matrix(TRUE, nrow = 1))), class = "vctrs_error_subscript_type"))
  })
})

test_that("vec_as_subscript() works with vectors of dimensionality 1", {
  arr <- array(TRUE, dim = 1)
  expect_identical(vec_as_subscript(arr), arr)
})

test_that("vec_as_subscript() forbids subscript types", {
  expect_snapshot(error = TRUE, vec_as_subscript(1L, logical = "error", numeric = "error"))
  expect_snapshot(error = TRUE, vec_as_subscript("foo", logical = "error", character = "error"))
  expect_snapshot(error = TRUE, vec_as_subscript(TRUE, logical = "error"))
  expect_snapshot(error = TRUE, vec_as_subscript("foo", character = "error"))
  expect_snapshot(error = TRUE, vec_as_subscript(NULL, numeric = "error"))
  expect_snapshot(error = TRUE, vec_as_subscript(quote(foo), character = "error"))
})

test_that("vec_as_subscript2() forbids subscript types", {
  expect_snapshot(error = TRUE, vec_as_subscript2(1L, numeric = "error", logical = "error"))
  expect_snapshot(error = TRUE, vec_as_subscript2("foo", character = "error", logical = "error"))
  expect_snapshot(error = TRUE, vec_as_subscript2(TRUE, logical = "error"))
})

test_that("vec_as_subscript2() retains the call when throwing vec_as_subscript() errors (#1605)", {
  expect_snapshot(error = TRUE, vec_as_subscript2(1L, numeric = "error", call = call("foo")))
  expect_snapshot(error = TRUE, vec_as_subscript2(1.5, call = call("foo")))
})

test_that("vec_as_subscript2() retains the call when erroring on logical input (#1605)", {
  expect_snapshot(error = TRUE, vec_as_subscript2(TRUE, call = call("foo")))
})

test_that("vec_as_subscript() evaluates arg lazily", {
  expect_silent(vec_as_subscript(1L, arg = print("oof")))
  expect_silent(vec_as_subscript_result(1L, arg = print("oof"), NULL, logical = "error", numeric = "cast", character = "error"))
})

test_that("vec_as_subscript2() evaluates arg lazily", {
  expect_silent(vec_as_subscript2(1L, arg = print("oof")))
  expect_silent(vec_as_subscript2_result(1L, arg = print("oof"), NULL, logical = "error", numeric = "cast", character = "error"))
})
