context("test-type-dim")

arr_lgl <- function(...) {
  array(logical(), c(...))
}
arr_chr <- function(...) {
  array(character(), c(...))
}

test_that("vectors have NULL dims", {
  expect_equal(dim_common(integer(), logical()), NULL)
})

test_that("matrices and array expand vectors", {
  expect_equal(dim_common(arr_lgl(0, 2), logical()), c(0, 2))
  expect_equal(dim_common(logical(), arr_lgl(0, 2)), c(0, 2))
})

test_that("non-zero dims must be equal", {
  expect_error(
    dim_common(arr_lgl(0, 2), arr_lgl(0, 3)),
    class = "error_incompatible_type"
  )
})

test_that("S3 vectors can't have dims", {
  expect_error(
    dim_common(arr_lgl(0, 2), factor()),
    class = "error_incompatible_type"
  )
})
