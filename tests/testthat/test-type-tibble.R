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

test_that("common type of dynamic gdf and df is dynamic gdf", {
  gdf <- dplyr::group_by(mtcars, cyl)
  expect_dynamically_grouped(vec_ptype_common(gdf, mtcars[1:3]), "cyl")
  expect_dynamically_grouped(vec_ptype_common(mtcars[1:3], gdf), "cyl")
  expect_dynamically_grouped(vec_ptype_common(gdf, mtcars["drat"]), "cyl")
})

test_that("common type of dynamic gdf and tib is dynamic gdf", {
  gdf <- dplyr::group_by(mtcars, cyl)
  tib <- dplyr::as_tibble(mtcars)
  expect_dynamically_grouped(vec_ptype_common(gdf, tib[1:3]), "cyl")
  expect_dynamically_grouped(vec_ptype_common(tib[1:3], gdf), "cyl")
  expect_dynamically_grouped(vec_ptype_common(gdf, tib["drat"]), "cyl")
})

test_that("common type of two dynamic gdfs takes union of groups", {
  gdf1 <- dplyr::group_by(mtcars, cyl)
  gdf2 <- dplyr::group_by(mtcars, vs, am)
  expect_dynamically_grouped(vec_ptype_common(gdf1, gdf1), "cyl")
  expect_dynamically_grouped(vec_ptype_common(gdf1, gdf2), c("cyl", "vs", "am"))
  expect_dynamically_grouped(vec_ptype_common(gdf2, gdf1), c("vs", "am", "cyl"))
})

test_that("common type of static gdf with df", {
  gdf <- dplyr::group_by(mtcars, cyl, am, .drop = FALSE)
  tib <- tibble::as_tibble(mtcars)

  gdata1 <- dplyr::group_data(vec_ptype_common(gdf, mtcars))
  gdata2 <- dplyr::group_data(vec_ptype_common(gdf, tib))
  gdata3 <- dplyr::group_data(vec_ptype_common(mtcars, gdf))
  gdata4 <- dplyr::group_data(vec_ptype_common(tib, gdf))

  exp <- dplyr::group_data(gdf)
  exp$.rows <- rep(list(int()), nrow(exp))

  expect_identical(gdata1, exp)
  expect_identical(gdata2, exp)
  expect_identical(gdata3, exp)
  expect_identical(gdata4, exp)
})

test_that("TODO: common type of static and dynamic gdf is still unimplemented", {
  static <- dplyr::group_by(mtcars, cyl, am, .drop = FALSE)
  dynamic <- dplyr::group_by(mtcars, am, cyl)
  expect_error(vec_ptype_common(static, dynamic), "unimplemented")
})

test_that("TODO: common type of two static gdfs is still unimplemented", {
  gdf1 <- dplyr::group_by(mtcars, cyl, am, .drop = FALSE)
  gdf2 <- dplyr::group_by(mtcars, am, cyl, .drop = FALSE)
  expect_error(vec_ptype_common(gdf1, gdf1), "unimplemented")
})

test_that("static groups are proxied and restored", {
  static <- dplyr::group_by(mtcars, cyl, am, .drop = FALSE)

  proxy <- vec_proxy(static)
  proxy <- proxy[1:3, c("cyl", "am", "disp", "vctrs::virtual_cols")]

  expect_identical(
    proxy$`vctrs::virtual_cols`,
    data_frame(`dplyr::grouped_df_groups` = c(4L, 4L, 2L))
  )

  out <- vec_restore(proxy, static)

  gdata <- dplyr::group_data(out)
  gtable <- gdata[-length(gdata)]

  expect_identical(gtable, group_table(static))
  expect_identical(
    gdata$.rows,
    vec_match_all(gtable, mtcars[1:3, c("cyl", "am")])
  )
})

test_that("handles multiple id columns in the static gdf proxy", {
  static <- dplyr::group_by(mtcars, cyl, am, .drop = FALSE)

  proxy <- vec_proxy(static)
  proxy <- proxy[, c(2, 9, 12)]
  proxy$`vctrs::virtual_cols` <- new_data_frame(rep(proxy$`vctrs::virtual_cols`, 2))

  # Allowed because they are congruent
  expect_identical(
    dplyr::group_data(vec_restore(proxy, static)),
    dplyr::group_data(static)
  )

  proxy$`vctrs::virtual_cols`[[1]][[4]] <- 100L
  expect_error(
    vec_restore(proxy, static),
    "incongruent"
  )
})

test_that("dynamic groups are recomputed after restoration", {
  gdf <- dplyr::group_by(mtcars, cyl)
  out <- vec_restore(vec_proxy(gdf), to = gdf)
  expect_identical(
    unstructure(dplyr::group_data(gdf)),
    unstructure(dplyr::group_data(out))
  )
})

test_that("drop is restored", {
  df_true <- dplyr::group_by(mtcars, cyl, .drop = TRUE)
  proxy_true <- vec_proxy(df_true)
  drop_true <- vec_restore(proxy_true, df_true)

  df_false <- dplyr::group_by(mtcars, cyl, .drop = FALSE)
  proxy_false <- vec_proxy(df_false)
  drop_false <- vec_restore(proxy_false, df_false)

  expect_true(dplyr::group_by_drop_default(drop_true))
  expect_false(dplyr::group_by_drop_default(drop_false))
})

test_that("can cast df to dynamic gdf", {
  gdf <- dplyr::group_by(mtcars, cyl)
  expect_equal(
    vec_cast(mtcars[1:3], gdf),
    dplyr::group_by(vec_cast(mtcars[1:3], mtcars), cyl)
  )
  expect_equal(
    vec_cast(dplyr::group_by(mtcars[8:10], vs, am), gdf),
    dplyr::group_by(vec_cast(mtcars[8:10], mtcars), cyl)
  )

  expect_true(dplyr::group_by_drop_default(vec_cast(mtcars, gdf)))
})

test_that("can cast df to static gdf", {
  static <- dplyr::group_by(mtcars, cyl, .drop = FALSE)
  out <- vec_cast(mtcars, static)
  expect_false(dplyr::group_by_drop_default(out))
})

test_that("can cast dynamic gdf to static gdf", {
  static <- dplyr::group_by(mtcars, cyl, .drop = FALSE)
  dynamic <- dplyr::group_by(mtcars, vs, am, .drop = TRUE)

  out <- vec_cast(dynamic, static)
  expect_false(dplyr::group_by_drop_default(out))
  expect_identical(dplyr::group_data(out), dplyr::group_data(static))
})

test_that("can cast static gdf to static gdf", {
  static_x <- dplyr::group_by(mtcars, cyl, .drop = FALSE)
  static_to <- dplyr::group_by(mtcars, vs, am, .drop = FALSE)

  out <- vec_cast(static_x, static_to)
  expect_false(dplyr::group_by_drop_default(out))
  expect_identical(dplyr::group_data(out), dplyr::group_data(static_to))
})

test_that("can cast static gdf to static gdf with different sizes", {
  static_x <- dplyr::group_by(mtcars[1:15, ], cyl, .drop = FALSE)
  static_to <- dplyr::group_by(mtcars, vs, am, .drop = FALSE)
  gdata_to <- dplyr::group_data(static_to)
  gtable_to <- gdata_to[-length(gdata_to)]

  out <- vec_cast(static_x, static_to)
  gdata_out <- dplyr::group_data(out)
  gtable_out <- gdata_out[-length(gdata_out)]

  expect_identical(gtable_out, gtable_to)
  expect_identical(
    gdata_out$.rows,
    vec_match_all(gtable_to, static_x[names(gtable_to)])
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

test_that("can cbind grouped-dfs", {
  skip("TODO")
  gdf <- dplyr::group_by(mtcars, cyl)
  exp <- dplyr::group_by(vec_cbind(mtcars, mtcars), cyl...2, cyl...13)
  exp_data <- unstructure(dplyr::group_data(exp))

  expect_cbinded <- function(out, groups) {
    repaired_names <- vec_as_names(rep(names(mtcars), 2), repair = "unique")
    expect_grouped(out, groups, names = repaired_names)
  }
  expect_cbinded(vec_cbind(gdf, gdf), groups = c("cyl...2", "cyl...13"))
  expect_cbinded(vec_cbind(gdf, mtcars), groups = c("cyl...2"))
  expect_cbinded(vec_cbind(mtcars, gdf), groups = c("cyl...13"))

  out <- vec_cbind(
    dplyr::group_by(mtcars[1:3], cyl),
    dplyr::group_by(mtcars, am, vs)
  )
  expect_named(out, names(vec_cbind(mtcars[1:3], mtcars)))
  expect_identical(dplyr::group_vars(out), c("cyl...2", "vs", "am"))

  gdf1 <- dplyr::group_by(mtcars[1:3], cyl)
  gdf2 <- dplyr::group_by(mtcars[8:10], vs, am)
  out <- vec_cbind(
    gdf1,
    mtcars[4:6],
    gdf2
  )
  expect_identical(dplyr::group_vars(out), c("cyl", "vs", "am"))
  expect_named(out, names(mtcars)[c(1:3, 4:6, 8:10)])
})

test_that("can concatenate grouped-dfs", {
  out <- vec_c(
    dplyr::group_by(mtcars, cyl),
    mtcars,
    dplyr::group_by(mtcars, cyl, am)
  )
  expect_is(out, "grouped_df")
  expect_identical(dplyr::group_vars(out), c("cyl", "am"))
})

test_that("can slice grouped-dfs", {
  out <- vec_slice(dplyr::group_by(mtcars, cyl), 0)
  expect_identical(dplyr::group_vars(out), "cyl")
})

test_that("grouped columns are equal to ungrouped ones", {
  gdf <- dplyr::group_by(mtcars, cyl)
  expect_identical(vec_equal(gdf, mtcars), rep(TRUE, nrow(mtcars)))
})
