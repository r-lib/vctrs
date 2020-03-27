
# `grouped_df` -------------------------------------------------------

test_that("grouped-df is proxied and restored", {
  gdf <- dplyr::group_by(mtcars, cyl)

  expect_identical(vec_proxy(gdf), gdf)
  expect_identical(vec_restore(mtcars, gdf), gdf)

  expect_identical(vec_ptype(gdf), gdf[0, ])

  gdf <- dplyr::group_by(mtcars, cyl, am, vs)
  expect_identical(gdf[0, ], vec_ptype(gdf))

  out <- vec_ptype(dplyr::group_by(mtcars, cyl, .drop = FALSE))
  expect_drop(out, FALSE)
})

test_that("can take the common type of grouped tibbles and tibbles", {
  gdf <- dplyr::group_by(mtcars, cyl)
  expect_identical(vec_ptype2(gdf, data.frame()), vec_ptype(gdf))
  expect_identical(vec_ptype2(data.frame(), gdf), vec_ptype(gdf))
  expect_identical(vec_ptype2(gdf, tibble()), vec_ptype(gdf))
  expect_identical(vec_ptype2(tibble(), gdf), vec_ptype(gdf))

  gdf_nodrop <- dplyr::group_by(mtcars, cyl, .drop = FALSE)
  expect_drop(vec_ptype2(gdf, gdf_nodrop), FALSE)
  expect_drop(vec_ptype2(gdf_nodrop, gdf), FALSE)
  expect_drop(vec_ptype2(gdf_nodrop, mtcars), FALSE)
  expect_drop(vec_ptype2(mtcars, gdf_nodrop), FALSE)
})

test_that("the common type of grouped tibbles includes the union of grouping variables", {
  gdf1 <- dplyr::group_by(mtcars, cyl)
  gdf2 <- dplyr::group_by(mtcars, am, vs)
  expect_identical(
    vec_ptype2(gdf1, gdf2),
    vec_ptype(dplyr::group_by(mtcars, cyl, am, vs))
  )
})

test_that("can cast to and from `grouped_df`", {
  gdf <- dplyr::group_by(unrownames(mtcars), cyl)
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

test_that("casting to `grouped_df` handles `drop`", {
  gdf_nodrop <- dplyr::group_by(mtcars, cyl, .drop = FALSE)
  expect_identical(vec_cast(mtcars, gdf_nodrop), gdf_nodrop)
})

test_that("can cbind grouped data frames", {
  gdf <- dplyr::group_by(mtcars[-10], cyl)
  df <- unrownames(mtcars)[10]

  expect_identical(
    unrownames(vec_cbind(gdf, df)),
    tibble::as_tibble(mtcars)[c(1:9, 11, 10)]
  )

  gdf1 <- dplyr::group_by(mtcars[2], cyl)
  gdf2 <- dplyr::group_by(mtcars[8:9], vs, am)
  expect_identical(
    unrownames(vec_cbind(gdf1, gdf2)),
    tibble::as_tibble(mtcars)[c(2, 8, 9)]
  )
})


# `rowwise` ----------------------------------------------------------

test_that("rowwise can be proxied and restored", {
  rww <- dplyr::rowwise(unrownames(mtcars))

  expect_identical(vec_proxy(rww), rww)
  expect_identical(vec_restore(unrownames(mtcars), rww), rww)

  expect_identical(vec_ptype(rww), rww[0, ])
})

test_that("can take the common type of rowwise tibbles and tibbles", {
  rww <- dplyr::rowwise(mtcars)
  expect_identical(vec_ptype2(rww, data.frame()), vec_ptype(rww))
  expect_identical(vec_ptype2(data.frame(), rww), vec_ptype(rww))
  expect_identical(vec_ptype2(rww, tibble()), vec_ptype(rww))
  expect_identical(vec_ptype2(tibble(), rww), vec_ptype(rww))
})

test_that("can cast to and from `rowwise_df`", {
  rww <- unrownames(dplyr::rowwise(mtcars))
  input <- mtcars[10]
  cast_rww <- dplyr::rowwise(vec_cast(mtcars[10], mtcars))

  expect_error(
    vec_cast(input, dplyr::rowwise(mtcars["cyl"])),
    class = "vctrs_error_cast_lossy"
  )

  expect_identical(
    vec_cast(input, rww),
    cast_rww
  )
  expect_identical(
    vec_cast(rww, mtcars),
    unrownames(mtcars)
  )

  expect_identical(
    vec_cast(tibble::as_tibble(input), rww),
    unrownames(cast_rww)
  )
  tib <- tibble::as_tibble(mtcars)
  expect_identical(
    unrownames(vec_cast(rww, tib)),
    tib
  )
})

test_that("can cbind rowwise data frames", {
  df <- unrownames(mtcars)
  rww <- dplyr::rowwise(df[-2])
  gdf <- dplyr::group_by(df[2], cyl)

  exp <- dplyr::rowwise(df[c(1, 3:11, 2)])
  expect_identical(vec_cbind(rww, df[2]), exp)

  # Suboptimal
  expect_identical(vec_cbind(rww, gdf), exp)
})

test_that("no common type between rowwise and grouped data frames", {
  expect_error(
    vec_ptype2(dplyr::rowwise(mtcars), dplyr::group_by(mtcars, cyl)),
    class = "vctrs_error_incompatible_type"
  )
})
