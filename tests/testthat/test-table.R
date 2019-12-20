
test_that("can slice table", {
  expect_identical(tbl_slice(mtcars, 3), mtcars[3])
  expect_identical(tbl_slice(mtcars, 2:3), mtcars[2:3])
  expect_identical(tbl_slice(mtcars, c("cyl", "vs")), mtcars[c("cyl", "vs")])
  ind <- c(rep(TRUE, 5), rep(FALSE, 6))
  expect_identical(tbl_slice(mtcars, ind), mtcars[ind])
})

test_that("tbl_slice() uses the proxy", {
  class <- NULL

  local_methods(
    vec_proxy.vctrs_foobar = function(x, ...) {
      x[[1]] <- new_data_frame(list(x = x[[1]]), class = "foo")
      x
    },
    vec_restore.vctrs_foobar = function(x, to, ...) {
      class <<- class(x[[1]])
      tbl_foobar(x)
    }
  )

  tbl_slice(tbl_foobar(mtcars), 1:3)
  expect_identical(class, c("foo", "data.frame"))
})

test_that("tbl_slice() uses number of columns to check bounds", {
  tbl_slice(vec_slice(mtcars, 1:3), 1:4)
})

test_that("tbl_slice() repairs names", {
  expect_identical(
    tbl_slice(mtcars, c(1, 2, 1)),
    set_names(mtcars[c(1, 2, 1)], c("mpg...1", "cyl", "mpg...3"))
  )
})

test_that("tbl_size() returns number of columns", {
  expect_identical(tbl_size(mtcars), ncol(mtcars))
})

test_that("can take the table prototype", {
  expect_identical(tbl_ptype(mtcars), mtcars_tbl_ptype)
})

test_that("data frames are tabular", {
  expect_true(tbl_is(mtcars))
  expect_error_free(tbl_assert(mtcars))
})

test_that("vectors are not tabular", {
  expect_false(tbl_is(NA))
  expect_false(tbl_is(1:3))
  expect_error(tbl_assert(NA), "must be a data frame")
  expect_error(tbl_assert(1:3), "must be a data frame")
})

test_that("can cast to tabular type", {
  expect_identical(tbl_cast(mtcars, mtcars), mtcars)
  expect_identical(tbl_cast(mtcars[1:3], mtcars), mtcars[1:3])

  sub_df <- structure(mtcars, class = c("df_subclass", "data.frame"))
  expect_identical(tbl_cast(sub_df, mtcars), mtcars)
  expect_error(tbl_cast(mtcars, sub_df), class = "vctrs_error_incompatible_cast")
})

test_that("can cast `NULL` to tabular type", {
  expect_null(tbl_cast(NULL, mtcars))
  expect_identical(tbl_cast(mtcars, NULL), mtcars)
})

test_that("common tabular type of data frames is `data.frame()`", {
  expect_identical(tbl_ptype2(mtcars[1:3], mtcars[4:7]), data.frame())
  expect_identical(tbl_ptype2(mtcars[4:7], mtcars[1:3]), data.frame())
})

test_that("can take the common tabular type with `NULL`", {
  expect_identical(tbl_ptype2(NULL, mtcars), mtcars_tbl_ptype)
  expect_identical(tbl_ptype2(mtcars, NULL), mtcars_tbl_ptype)
  expect_error(tbl_ptype2(NULL, 1:3), "must be a data frame")
  expect_error(tbl_ptype2(1:3, NULL), "must be a data frame")
})

test_that("data frames don't have common type with vectors", {
  expect_error(
    tbl_ptype2(1:3, mtcars[4:6]),
    "must be a data frame"
  )
  expect_error(
    tbl_ptype2(mtcars[4:6], 1:3),
    "must be a data frame"
  )

  mtx <- as.matrix(mtcars)
  expect_error(
    tbl_ptype2(mtx, mtcars[4:6]),
    "must be a data frame"
  )
  expect_error(
    tbl_ptype2(mtcars[4:6], mtx),
    "must be a data frame"
  )
})

test_that("data frames don't have common type with subclasses of data frames", {
  sub_df <- structure(mtcars, class = c("df_subclass", "data.frame"))
  expect_error(
    tbl_ptype2(sub_df, mtcars[4:6]),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    tbl_ptype2(mtcars[4:6], sub_df),
    class = "vctrs_error_incompatible_type"
  )
})
