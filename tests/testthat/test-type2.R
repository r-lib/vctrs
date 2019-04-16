context("test-type-coerce")

test_that("base coercions are symmetric and unchanging", {
  types <- list(
    logical(),
    integer(),
    double(),
    character(),
    raw(),
    list()
  )
  mat <- maxtype_mat(types)

  expect_true(isSymmetric(mat))
  expect_known_output(mat, test_path("test-type2.txt"), print = TRUE)
})

test_that("new classes are uncoercible by default", {
  x <- structure(1:10, class = "vctrs_nonexistant")
  expect_error(vec_type2(1, x), class = "vctrs_error_incompatible_type")
  expect_error(vec_type2(x, 1), class = "vctrs_error_incompatible_type")
})

test_that("vec_typeof2() returns common type", {
  nms <- names(empty_types)

  for (i in seq_along(empty_types)) {
    this <- nms[[i]]

    for (j in seq_along(empty_types)) {
      that <- nms[[j]]

      if (i <= j) {
        exp <- paste0("vctrs_type2_", this, "_", that)
      } else {
        exp <- paste0("vctrs_type2_", that, "_", this)
      }
      out <- vec_typeof2(empty_types[[this]], empty_types[[that]])

      expect_identical(out, exp)
    }
  }
})

test_that("vec_type2() dispatches when inputs have shape", {
  expect_identical(dim(vec_type2(int(), matrix(nrow = 3, ncol = 4))), c(0L, 4L))
  expect_identical(dim(vec_type2(matrix("", nrow = 3), c("", "", ""))), c(0L, 1L))
})

test_that("vec_type2() requires vectors", {
  expect_error(vec_type2(NULL, quote(name)), class = "vctrs_error_scalar_type")
  expect_error(vec_type2(NA, quote(name)), class = "vctrs_error_scalar_type")
  expect_error(vec_type2(list(), quote(name)), class = "vctrs_error_scalar_type")
  expect_error(vec_type2(quote(name), NULL), class = "vctrs_error_scalar_type")
  expect_error(vec_type2(quote(name), NA), class = "vctrs_error_scalar_type")
  expect_error(vec_type2(quote(name), list()), class = "vctrs_error_scalar_type")
  expect_error(vec_type2(quote(name), quote(name)), class = "vctrs_error_scalar_type")
})

test_that("stop_incompatible_type() checks for scalars", {
  expect_error(stop_incompatible_type(NA, foobar()), class = "vctrs_error_scalar_type")
  expect_error(vec_type2(NA, foobar()), class = "vctrs_error_scalar_type")
  expect_error(vec_type2(foobar(), list()), class = "vctrs_error_scalar_type")
})
