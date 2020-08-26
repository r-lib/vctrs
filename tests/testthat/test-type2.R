
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

test_that("vec_typeof2_s3() returns common type", {
  all_base_empty_types <- c(base_empty_types, base_s3_empty_types)

  nms_s3 <- names(base_s3_empty_types)
  nms <- names(all_base_empty_types)

  for (i in seq_along(all_base_empty_types)) {
    this <- nms[[i]]

    for (j in seq_along(all_base_empty_types)) {
      that <- nms[[j]]

      # Skip when we have two non s3 objects
      if (!(this %in% nms_s3) & !(that %in% nms_s3)) {
        next
      }

      if (i <= j) {
        exp <- paste0("vctrs_type2_s3_", this, "_", that)
      } else {
        exp <- paste0("vctrs_type2_s3_", that, "_", this)
      }
      out <- vec_typeof2_s3(all_base_empty_types[[this]], all_base_empty_types[[that]])

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

test_that("vec_ptype2() with unspecified requires vectors", {
  expect_error(vec_ptype2(unspecified(), quote(name)), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(quote(name), unspecified()), class = "vctrs_error_scalar_type")
})

test_that("vec_ptype2() forwards argument tag", {
  expect_error(vec_ptype2(quote(name), list(), x_arg = "foo"), "`foo`", class = "vctrs_error_scalar_type")
  expect_error(vec_ptype2(list(), quote(name), y_arg = "foo"), "`foo`", class = "vctrs_error_scalar_type")
})

test_that("stop_incompatible_type() checks for scalars", {
  expect_error(stop_incompatible_type(NA, foobar(), x_arg = "x", y_arg = "y"), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype_common(NA, foobar()), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype_common(foobar(), list()), class = "vctrs_error_scalar_type")
})

test_that("vec_ptype2() methods forward args to stop_incompatible_type()", {
  expect_args(new_hidden(), lgl(), x_arg = "foo", y_arg = "bar")
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

test_that("Subclasses of data.frame dispatch to `vec_ptype2()` methods", {
  local_methods(
    vec_ptype2.quuxframe = function(x, y, ...) UseMethod("vec_ptype2.quuxframe"),
    vec_ptype2.quuxframe.data.frame = function(x, y, ...) "dispatched!",
    vec_ptype2.data.frame.quuxframe = function(x, y, ...) "dispatched!"
  )

  quux <- structure(data.frame(), class = c("quuxframe", "data.frame"))

  expect_identical(vec_ptype2(quux, mtcars), "dispatched!")
  expect_identical(vec_ptype2(mtcars, quux), "dispatched!")

  quux <- structure(data.frame(), class = c("quuxframe", "tbl_df", "data.frame"))

  expect_identical(vec_ptype2(quux, mtcars), "dispatched!")
  expect_identical(vec_ptype2(mtcars, quux), "dispatched!")
})

test_that("Subclasses of `tbl_df` do not have `tbl_df` common type (#481)", {
  quux <- tibble()
  quux <- foobar(quux)

  expect_incompatible_df(
    vec_ptype_common(quux, tibble()),
    tibble()
  )
  expect_incompatible_df(
    vec_ptype_common(tibble(), quux),
    tibble()
  )

  expect_df_fallback_warning(
    expect_identical(
      vec_ptype_common_df_fallback(quux, tibble()),
      tibble()
    )
  )
  expect_df_fallback_warning(
    expect_identical(
      vec_ptype_common_df_fallback(tibble(), quux),
      tibble()
    )
  )
})

test_that("Column name encodings are handled correctly in the common type (#553)", {
  encs <- encodings()

  data <- list(chr())

  df_utf8 <- tibble::as_tibble(set_names(data, encs$utf8))
  df_unknown <- tibble::as_tibble(set_names(data, encs$unknown))

  expect_identical(vec_ptype2(df_utf8, df_unknown), df_utf8)
})

test_that("vec_is_subtype() determines subtyping relationship", {
  expect_true(vec_is_subtype(lgl(), int()))
  expect_false(vec_is_subtype(int(), lgl()))

  expect_false(vec_is_subtype(lgl(), chr()))
  expect_false(vec_is_subtype(chr(), lgl()))

  local_methods(
    vec_ptype2.vctrs_foobar = function(x, y, ...) UseMethod("vec_ptype2.vctrs_foobar"),
    vec_ptype2.vctrs_foobar.logical = function(x, y, ...) logical(),
    vec_ptype2.logical.vctrs_foobar = function(x, y, ...) logical()
  )
  expect_true(vec_is_subtype(foobar(TRUE), lgl()))
  expect_false(vec_is_subtype(lgl(), foobar(TRUE)))
})

test_that("can override scalar vector error message for base scalar types", {
  verify_errors({
    expect_error(vec_ptype2(NULL, quote(x), y_arg = "foo"), class = "vctrs_error_scalar_type")
    expect_error(vec_ptype2(quote(x), NULL, x_arg = "foo"), class = "vctrs_error_scalar_type")
  })
})

test_that("can override scalar vector error message for S3 types", {
  verify_errors({
    expect_error(vec_ptype2(NULL, foobar(), y_arg = "foo"), class = "vctrs_error_scalar_type")
    expect_error(vec_ptype2(foobar(), NULL, x_arg = "foo"), class = "vctrs_error_scalar_type")
  })
})

test_that("ptype2 and cast errors when same class fallback is impossible are informative", {
  verify_errors({
    expect_error(
      vec_cast(foobar(1, bar = TRUE), foobar(2, baz = TRUE)),
      class = "vctrs_error_incompatible_type"
    )
    expect_error(
      vec_ptype2(foobar(1, bar = TRUE), foobar(2, baz = TRUE)),
      class = "vctrs_error_incompatible_type"
    )

    "Incompatible attributes bullets are not show when methods are implemented"
    with_foobar_cast <- function(expr ) {
      with_methods(
        vec_cast.vctrs_foobar = function(...) NULL,
        vec_cast.vctrs_foobar.vctrs_foobar = function(x, to, ...) vec_default_cast(x, to, ...),
        expr
      )
    }
    with_foobar_ptype2 <- function(expr ) {
      with_methods(
        vec_ptype2.vctrs_foobar = function(...) NULL,
        vec_ptype2.vctrs_foobar.vctrs_foobar = function(x, y, ...) vec_default_ptype2(x, y, ...),
        expr
      )
    }
    expect_error(
      with_foobar_cast(vec_cast(foobar(1, bar = TRUE), foobar(2, baz = TRUE))),
      class = "vctrs_error_incompatible_type"
    )
    expect_error(
      with_foobar_ptype2(vec_ptype2(foobar(1, bar = TRUE), foobar(2, baz = TRUE))),
       class = "vctrs_error_incompatible_type"
    )
  })
})

test_that("common type errors don't mention columns if they are compatible", {
  verify_errors({
    df <- data.frame(x = 1, y = "")
    foo <- structure(df, class = c("vctrs_foo", "data.frame"))
    bar <- structure(df, class = c("vctrs_bar", "data.frame"))
    expect_error(
      vec_cast_no_fallback(foo, bar),
      class = "vctrs_error_incompatible_type"
    )
  })
})

test_that("common type warnings for data frames take attributes into account", {
  verify_errors({
    foobar_bud <- foobar(mtcars, bud = TRUE)
    foobar_boo <- foobar(mtcars, boo = TRUE)
    expect_df_fallback_warning(vec_ptype2_fallback(foobar_bud, foobar_boo))

    "For reference, warning for incompatible classes"
    expect_df_fallback_warning(vec_ptype2_fallback(foobar(mtcars), foobaz(mtcars)))

    "For reference, error when fallback is disabled"
    expect_error(
      vec_ptype2_no_fallback(foobar(mtcars), foobaz(mtcars)),
      class = "vctrs_error_incompatible_type"
    )
  })
})

test_that("vec_ptype2() methods get prototypes", {
  x <- NULL
  y <- NULL

  local_methods(vec_ptype2.vctrs_foobar.vctrs_foobar = function(x, y, ...) {
    x <<- x
    y <<- y
    NULL
  })

  vec_ptype2(foobar(1:3), foobar(letters))
  expect_identical(x, foobar(int()))
  expect_identical(y, foobar(chr()))

  vec_ptype2(foobar(mtcars), foobar(iris))
  expect_identical(x, foobar(mtcars[0, , drop = FALSE]))
  expect_identical(y, foobar(iris[0, , drop = FALSE]))
})

test_that("vec_ptype2() allows vec_ptype() to return another type", {
  out <- with_methods(
    vec_restore.vctrs_foobar = function(x, to, ...) unstructure(x),
    vec_ptype2(foobar(1), foobar(2))
  )
  expect_identical(out, dbl())
})

test_that("vec_ptype2() errors have informative output", {
  verify_output(test_path("error", "test-type2.txt"), {
    "# can override scalar vector error message for base scalar types"
    vec_ptype2(NULL, quote(x), y_arg = "foo")
    vec_ptype2(quote(x), NULL, x_arg = "foo")

    "# can override scalar vector error message for S3 types"
    vec_ptype2(NULL, foobar(), y_arg = "foo")
    vec_ptype2(foobar(), NULL, x_arg = "foo")

    "# ptype2 and cast errors when same class fallback is impossible are informative"
    vec_cast(foobar(1, bar = TRUE), foobar(2, baz = TRUE))
    vec_ptype2(foobar(1, bar = TRUE), foobar(2, baz = TRUE))

    "Incompatible attributes bullets are not show when methods are implemented"
    with_foobar_cast <- function(expr ) {
      with_methods(
        vec_cast.vctrs_foobar = function(...) NULL,
        vec_cast.vctrs_foobar.vctrs_foobar = function(x, to, ...) vec_default_cast(x, to, ...),
        expr
      )
    }
    with_foobar_ptype2 <- function(expr ) {
      with_methods(
        vec_ptype2.vctrs_foobar = function(...) NULL,
        vec_ptype2.vctrs_foobar.vctrs_foobar = function(x, y, ...) vec_default_ptype2(x, y, ...),
        expr
      )
    }
    with_foobar_cast(vec_cast(foobar(1, bar = TRUE), foobar(2, baz = TRUE)))
    with_foobar_ptype2(vec_ptype2(foobar(1, bar = TRUE), foobar(2, baz = TRUE)))

    "# common type errors don't mention columns if they are compatible"
    df <- data.frame(x = 1, y = "")
    foo <- structure(df, class = c("vctrs_foo", "data.frame"))
    bar <- structure(df, class = c("vctrs_bar", "data.frame"))
    vec_cast_no_fallback(foo, bar)

    "# common type warnings for data frames take attributes into account"
    foobar_bud <- foobar(mtcars, bud = TRUE)
    foobar_boo <- foobar(mtcars, boo = TRUE)
    vec_ptype2_fallback(foobar_bud, foobar_boo)

    "For reference, warning for incompatible classes"
    vec_ptype2_fallback(foobar(mtcars), foobaz(mtcars))

    "For reference, error when fallback is disabled"
    vec_ptype2_no_fallback(foobar(mtcars), foobaz(mtcars))
  })
})
