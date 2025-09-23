test_that("tibble beats data frame", {
  df <- new_data_frame()
  dt <- tibble::tibble()

  expect_s3_class(vec_ptype_common(dt, df), "tbl_df")
  expect_s3_class(vec_ptype_common(df, dt), "tbl_df")
})

test_that("can cast tibble to df and vice versa", {
  df <- new_data_frame()
  tib <- tibble::tibble()

  expect_identical(vec_cast(df, tib), tib)
  expect_identical(vec_cast(tib, df), df)
  expect_identical(vec_cast(tib, tib), tib)
})

test_that("can't cast vector to tibble", {
  dt <- tibble::tibble()
  v <- logical()

  expect_snapshot({
    local_error_call(call("my_function"))
    (expect_error(vec_ptype2(v, dt), class = "vctrs_error_incompatible_type"))
    (expect_error(vec_ptype2(dt, v), class = "vctrs_error_incompatible_type"))
    (expect_error(vec_cast(v, dt), class = "vctrs_error_incompatible_type"))
  })
})

test_that("casting to and from tibble preserves row names", {
  out <- vec_cast(mtcars, tibble::as_tibble(mtcars))
  expect_identical(row.names(out), row.names(mtcars))

  out <- vec_cast(out, unrownames(mtcars))
  expect_identical(row.names(out), row.names(mtcars))
})

test_that("no common type between list and tibble", {
  dt <- tibble::tibble()
  l <- list()

  expect_error(vec_ptype2(l, dt), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(dt, l), class = "vctrs_error_incompatible_type")
})

test_that("vec_restore restores tibbles", {
  df1 <- tibble::tibble(x = 1:4)
  df2 <- vec_restore(vec_data(df1), df1)

  expect_s3_class(df2, "tbl_df")
})

test_that("the type of a tibble with an unspecified column retains unspecifiedness", {
  df1 <- tibble::tibble(x = 1, y = NA)
  df2 <- tibble::tibble(x = 1, y = unspecified(1))
  expect <- tibble::tibble(x = numeric(), y = unspecified())

  expect_identical(vec_ptype(df1), expect)
  expect_identical(vec_ptype(df2), expect)
})

test_that("vec_ptype_finalise() works recursively over tibbles", {
  df <- tibble(x = numeric(), y = unspecified())
  expect <- tibble(x = numeric(), y = logical())

  expect_identical(vec_ptype_finalise(df), expect)
})

test_that("vec_ptype_finalise() can handle tibble df columns", {
  df <- tibble(x = numeric(), y = tibble(z = unspecified()))
  expect <- tibble(x = numeric(), y = tibble(z = logical()))

  expect_identical(vec_ptype_finalise(df), expect)
})

test_that("can use ptype2 and cast with tibble that has incorrect class vector", {
  tib1 <- structure(data.frame(x = 1), class = c("tbl_df", "data.frame"))
  tib2 <- structure(data.frame(y = 2), class = c("tbl_df", "data.frame"))
  exp <- structure(
    data.frame(x = dbl(), y = dbl()),
    class = c("tbl_df", "data.frame")
  )

  requireNamespace("tibble")

  expect_identical(
    vec_ptype_common(tib1, tib2),
    exp
  )
  expect_identical(
    vec_ptype_common(tib1, data.frame(y = 2)),
    tibble::new_tibble(exp, nrow = nrow(exp))
  )
  expect_identical(
    vec_ptype_common(data.frame(x = 1), tib2),
    tibble::new_tibble(exp, nrow = nrow(exp))
  )
  expect_identical(
    vec_cast(tib1, tib1),
    tib1
  )

  expect_snapshot({
    local_error_call(call("my_function"))
    (expect_error(
      vec_cast(tib1, tib2),
      class = "vctrs_error_cast"
    ))
    (expect_error(
      vec_cast(tib1, data.frame(y = 2)),
      class = "vctrs_error_cast"
    ))
    (expect_error(
      vec_cast(data.frame(x = 1), tib2),
      class = "vctrs_error_cast"
    ))
  })
})
