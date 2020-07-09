
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
  verify_errors({
    expect_error(
      with_tibble_cols(vec_as_subscript(env())),
      class = "vctrs_error_subscript_type"
    )
  })
})

test_that("vec_as_subscript() checks dimensionality", {
  verify_errors({
    expect_error(vec_as_subscript(matrix(TRUE, nrow = 1)), class = "vctrs_error_subscript_type")
    expect_error(vec_as_subscript(array(TRUE, dim = c(1, 1, 1))), class = "vctrs_error_subscript_type")
    expect_error(with_tibble_rows(vec_as_subscript(matrix(TRUE, nrow = 1))), class = "vctrs_error_subscript_type")
  })
})

test_that("vec_as_subscript() works with vectors of dimensionality 1", {
  arr <- array(TRUE, dim = 1)
  expect_identical(vec_as_subscript(arr), arr)
})

test_that("subscript functions have informative error messages", {
  verify_output(test_path("error", "test-subscript.txt"), {
    "# vec_as_subscript() forbids subscript types"
    vec_as_subscript(1L, logical = "error", numeric = "error")
    vec_as_subscript("foo", logical = "error", character = "error")
    vec_as_subscript(TRUE, logical = "error")
    vec_as_subscript("foo", character = "error")
    vec_as_subscript(NULL, numeric = "error")
    vec_as_subscript(quote(foo), character = "error")

    "# vec_as_subscript2() forbids subscript types"
    vec_as_subscript2(1L, numeric = "error", logical = "error")
    vec_as_subscript2("foo", character = "error", logical = "error")
    vec_as_subscript2(TRUE, logical = "error")

    "# can customise subscript errors"
    with_tibble_cols(vec_as_subscript(env()))
    with_dm_tables(vec_as_subscript(env()))

    "# vec_as_subscript() checks dimensionality"
    vec_as_subscript(matrix(TRUE, nrow = 1))
    vec_as_subscript(array(TRUE, dim = c(1, 1, 1)))
    with_tibble_rows(vec_as_subscript(matrix(TRUE, nrow = 1)))
  })
})
