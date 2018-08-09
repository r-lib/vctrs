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

test_that("equal dimensions are preserved", {
  expect_equal(dim_common(arr_lgl(0, 2, 3), arr_lgl(0, 2, 3)), c(0, 2, 3))
})

test_that("dimesionality must be equal", {
  expect_error(
    dim_common(arr_lgl(0, 2), arr_lgl(0, 3, 1)),
    class = "error_incompatible_type"
  )
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


# recycling ---------------------------------------------------------------

test_that("leaves x as is if to is null or an object", {
  x <- 1:10

  expect_equal(shape_recycle(x, factor()), x)
  expect_equal(shape_recycle(x, NULL), x)
})

test_that("can always recycle down to a vector", {
  x <- 1:4

  expect_equal(shape_recycle(x, ones(0)), x)
  expect_equal(shape_recycle(matrix(x, nrow = 2), ones(0)), x)
})

test_that("dimensions must be equal", {
  expect_error(
    shape_recycle(ones(0, 2), ones(0, 3, 4)),
    class = "error_incompatible_cast"
  )
})

test_that("can recycle along dimensions of length 1", {
  x1 <- matrix(1:2, nrow = 1)
  x2 <- matrix(1:2, ncol = 1)
  to <- array(dim = c(2, 2))

  expect_equal(shape_recycle(x1, to), rbind(x1, x1))
  expect_equal(shape_recycle(x2, to), cbind(x2, x2))
})

test_that("can't recycle along non-matching dimensions", {
  expect_error(
    shape_recycle(ones(1, 2), ones(1, 3)),
    class = "error_incompatible_cast"
  )
})
