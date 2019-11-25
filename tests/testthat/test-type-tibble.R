context("test-type-tibble")

test_that("tibble beats data frame", {
  df <- new_data_frame()
  dt <- tibble::tibble()

  expect_s3_class(vec_ptype_common(dt, df), "tbl_df")
  expect_s3_class(vec_ptype_common(df, dt), "tbl_df")
})

test_that("can cast tibble to df and vice versa", {
  df <- new_data_frame()
  dt <- tibble::tibble()

  expect_equal(vec_cast(df, dt), dt)
  expect_equal(vec_cast(dt, df), df)
})

test_that("can't cast vector to tibble", {
  dt <- tibble::tibble()
  v <- logical()

  expect_error(vec_ptype2(v, dt), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(dt, v), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(v, dt), class = "vctrs_error_incompatible_cast")
})

test_that("can't cast list to tibble", {
  dt <- tibble::tibble()
  l <- list()

  expect_error(vec_ptype2(l, dt), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(dt, l), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(l, dt), class = "vctrs_error_incompatible_cast")
})

test_that("vec_restore restores tibbles", {
  df1 <- tibble::tibble(x = 1:4)
  df2 <- vec_restore(vec_data(df1), df1)

  expect_s3_class(df2, "tbl_df")
})

test_that("tibbles have common tabular type with data frames", {
  expect_identical(tbl_ptype2(tibble::tibble(x = 1), mtcars), tibble::tibble())
  expect_identical(tbl_ptype2(mtcars, tibble::tibble(x = 1)), tibble::tibble())
})


# grouped_df ---------------------------------------------------------

test_that("common type of grouped-df and df is df", {
  gdf <- dplyr::group_by(mtcars, cyl)

  expect_grouped_cyl <- function(x) {
    expect_is(x, "grouped_df")
    expect_identical(dplyr::group_vars(x), "cyl")
    expect_named(x, names(mtcars))
  }

  expect_grouped_cyl(vec_ptype_common(gdf, mtcars[1:3]))
  expect_grouped_cyl(vec_ptype_common(mtcars[1:3], gdf))
  expect_grouped_cyl(vec_ptype_common(gdf, mtcars["drat"]))
})

test_that("common type of two grouped-df takes union of groups", {
  gdf1 <- dplyr::group_by(mtcars, cyl)
  gdf2 <- dplyr::group_by(mtcars, vs, am)

  expect_grouped <- function(x, groups) {
    expect_is(x, "grouped_df")
    expect_identical(dplyr::group_vars(x), groups)
    expect_named(x, names(mtcars))
  }

  expect_grouped(vec_ptype_common(gdf1, gdf1), "cyl")
  expect_grouped(vec_ptype_common(gdf1, gdf2), c("cyl", "vs", "am"))
  expect_grouped(vec_ptype_common(gdf2, gdf1), c("vs", "am", "cyl"))
})

test_that("groups are recomputed after restoration", {
  gdf <- dplyr::group_by(mtcars, cyl)
  out <- vec_restore(vec_proxy(gdf), to = gdf)
  expect_identical(
    unstructure(dplyr::group_data(gdf)),
    unstructure(dplyr::group_data(out))
  )
})
