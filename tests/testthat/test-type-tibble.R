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
  expect_grouped(vec_ptype_common(gdf, mtcars[1:3]), "cyl")
  expect_grouped(vec_ptype_common(mtcars[1:3], gdf), "cyl")
  expect_grouped(vec_ptype_common(gdf, mtcars["drat"]), "cyl")
})

test_that("common type of two grouped-df takes union of groups", {
  gdf1 <- dplyr::group_by(mtcars, cyl)
  gdf2 <- dplyr::group_by(mtcars, vs, am)
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

test_that("can cast data frame to grouped-df", {
  expect_equal(
    vec_cast(mtcars[1:3], dplyr::group_by(mtcars, cyl)),
    dplyr::group_by(mtcars[1:3], cyl)
  )
})

test_that("can rbind grouped-dfs", {
  gdf <- dplyr::group_by(mtcars, cyl)
  exp <- dplyr::group_by(vec_rbind(mtcars, mtcars), cyl)
  exp_data <- unstructure(dplyr::group_data(exp))

  out <- vec_rbind(gdf, gdf)
  expect_grouped(out, "cyl")
  expect_identical(unstructure(dplyr::group_data(out)), exp_data)

  out <- vec_rbind(gdf, mtcars)
  expect_grouped(out, "cyl")
  expect_identical(unstructure(dplyr::group_data(out)), exp_data)

  out <- vec_rbind(mtcars, gdf)
  expect_grouped(out, "cyl")
  expect_identical(unstructure(dplyr::group_data(out)), exp_data)

  gdf2 <- dplyr::group_by(mtcars, vs, am)
  out <- vec_rbind(gdf2, mtcars, gdf)
  exp_data <- dplyr::group_by(vec_rbind(mtcars, mtcars, mtcars), cyl, vs, am)
  exp_data <- unstructure(dplyr::group_data(exp_data))
  expect_grouped(out, c("cyl", "vs", "am"))
  expect_identical(unstructure(dplyr::group_data(out)), exp_data)
})
