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
  expect_error(vec_ptype2(1, x), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(x, 1), class = "vctrs_error_incompatible_type")
})

test_that("vec_typeof2() returns common type", {
  nms <- names(base_empty_types)

  for (i in seq_along(base_empty_types)) {
    this <- nms[[i]]

    for (j in seq_along(base_empty_types)) {
      that <- nms[[j]]

      if (i <= j) {
        exp <- paste0("vctrs_type2_", this, "_", that)
      } else {
        exp <- paste0("vctrs_type2_", that, "_", this)
      }
      out <- vec_typeof2(base_empty_types[[this]], base_empty_types[[that]])

      expect_identical(out, exp)
    }
  }
})

test_that("vec_ptype2() dispatches when inputs have shape", {
  expect_identical(dim(vec_ptype2(int(), matrix(nrow = 3, ncol = 4))), c(0L, 4L))
  expect_identical(dim(vec_ptype2(matrix("", nrow = 3), c("", "", ""))), c(0L, 1L))
})

test_that("vec_ptype2() requires vectors", {
  expect_error(vec_ptype2(NULL, quote(name)), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(NA, quote(name)), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(list(), quote(name)), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(quote(name), NULL), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(quote(name), NA), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(quote(name), list()), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(quote(name), quote(name)), class = "vctrs_error_scalar_type")
})

test_that("vec_ptype2() forwards argument tag", {
  expect_error(vec_ptype2(quote(name), list(), x_arg = "foo"), "`foo`", class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(list(), quote(name), y_arg = "foo"), "`foo`", class = "vctrs_error_scalar_type")
})

test_that("stop_incompatible_type() checks for scalars", {
  expect_error(stop_incompatible_type(NA, foobar()), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype_common(NA, foobar()), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype_common(foobar(), list()), class = "vctrs_error_scalar_type")
})

test_that("vec_ptype2() methods forward args to stop_incompatible_type()", {
  expect_args(new_hidden(), NA, x_arg = "foo", y_arg = "bar")
  expect_args(lgl(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(int(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(dbl(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(chr(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(list(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(new_rcrd(list(x = NA)), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(data.frame(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(Sys.Date(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(as.difftime(1, units = "hours"), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(factor(), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(ordered(""), new_hidden(), x_arg = "foo", y_arg = "bar")
  expect_args(ordered(""), factor(), x_arg = "foo", y_arg = "bar")
  expect_args(bit64::as.integer64(1), new_hidden(), x_arg = "foo", y_arg = "bar")
})

test_that("vec_ptype2() data frame methods builds argument tags", {
  expect_known_output(file = test_path("test-type2-error-messages.txt"), {
    cat_line("Bare objects:")
    try2(vec_ptype2("foo", 10))

    cat_line("Nested dataframes:")
    df1 <- tibble(x = tibble(y = tibble(z = 1)))
    df2 <- tibble(x = tibble(y = tibble(z = "a")))
    try2(vec_ptype2(df1, df2))
  })
})

test_that("stop_incompatible_type() can be called without argument tags", {
  expect_error(stop_incompatible_type(1, 2, x_arg = "", y_arg = ""), "<double> and <double>", class = "vctrs_error_incompatible_type")
})

test_that("vec_ptype2() returns empty prototype when other input is NULL", {
  expect_identical(vec_ptype2(1:5, NULL), int())
  expect_identical(vec_ptype2(NULL, 1:5), int())
})
