
test_that("grouped-df is proxied and restored", {
  gdf <- dplyr::group_by(mtcars, cyl)

  expect_identical(vec_proxy(gdf), gdf)
  expect_identical(vec_restore(mtcars, gdf), gdf)

  expect_identical(vec_ptype(gdf), gdf[0, ])

  # Taking the prototype with `[` because of tidyverse/dplyr#5040
  gdf <- dplyr::group_by(mtcars, cyl, am, vs)
  expect_identical(gdf[0, ], vec_ptype(gdf))

  expect_error(
    vec_ptype(dplyr::group_by(mtcars, cyl, .drop = FALSE)),
    "unsupported in vctrs"
  )
})

test_that("can take the common type of grouped tibbles and tibbles", {
  gdf <- dplyr::group_by(mtcars, cyl)
  expect_identical(vec_ptype2(gdf, data.frame()), vec_ptype(gdf))
  expect_identical(vec_ptype2(data.frame(), gdf), vec_ptype(gdf))
  expect_identical(vec_ptype2(gdf, tibble()), vec_ptype(gdf))
  expect_identical(vec_ptype2(tibble(), gdf), vec_ptype(gdf))
})

test_that("the common type of grouped tibbles includes the union of grouping variables", {
  gdf1 <- dplyr::group_by(mtcars, cyl)
  gdf2 <- dplyr::group_by(mtcars, am, vs)
  expect_identical(
    # Taking the prototype again because of tidyverse/dplyr#5040
    vec_ptype(vec_ptype2(gdf1, gdf2)),
    vec_ptype(dplyr::group_by(mtcars, cyl, am, vs))
  )
})

test_that("can cast to and from `grouped_df`", {
  gdf <- dplyr::group_by(mtcars, cyl)
  input <- mtcars[10]
  cast_gdf <- dplyr::group_by(vec_cast(mtcars[10], mtcars), cyl)

  expect_error(
    vec_cast(input, dplyr::group_by(mtcars["cyl"], cyl)),
    class = "vctrs_error_cast_lossy"
  )

  expect_identical(
    vec_cast(input, gdf),
    cast_gdf
  )
  expect_identical(
    vec_cast(gdf, mtcars),
    unrownames(mtcars)
  )

  expect_identical(
    vec_cast(tibble::as_tibble(input), gdf),
    unrownames(cast_gdf)
  )
  tib <- tibble::as_tibble(mtcars)
  expect_identical(
    unrownames(vec_cast(gdf, tib)),
    tib
  )
})

test_that("casting to `grouped_df` doesn't require grouping variables", {
  expect_identical(
    vec_cast(mtcars[10], dplyr::group_by(mtcars, cyl)),
    dplyr::group_by(vec_cast(mtcars[10], mtcars), cyl)
  )
})

test_that("can cbind grouped data frames", {
  gdf <- dplyr::group_by(mtcars[-10], cyl)
  df <- unrownames(mtcars)[10]

  expect_identical(
    vec_cbind(gdf, df),
    tibble::as_tibble(mtcars)[c(1:9, 11, 10)]
  )
})
