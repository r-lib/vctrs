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

test_that("vec_dispatch_typeof() returns common type", {
  nms <- names(empty_types)

  for (i in seq_along(empty_types)) {
    this <- nms[[i]]

    for (j in seq_along(empty_types)) {
      that <- nms[[j]]

      if (i <= j) {
        exp <- paste0("vctrs_dispatch_", this, "_", that)
      } else {
        exp <- paste0("vctrs_dispatch_", that, "_", this)
      }
      out <- vec_dispatch_typeof(empty_types[[this]], empty_types[[that]])

      expect_identical(out, exp)
    }
  }
})

test_that("vec_type2() dispatches when inputs have shape", {
  expect_identical(dim(vec_type2(int(), matrix(nrow = 3, ncol = 4))), c(0L, 4L))
  expect_identical(dim(vec_type2(matrix("", nrow = 3), c("", "", ""))), c(0L, 1L))
})
