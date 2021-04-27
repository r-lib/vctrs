
# `grouped_df` -------------------------------------------------------

bare_mtcars <- unrownames(mtcars)

test_that("grouped-df is proxied and restored", {
  skip("until vec_order_radix -> vec_order")

  gdf <- dplyr::group_by(bare_mtcars, cyl)

  expect_identical(vec_proxy(gdf), gdf)
  expect_identical(vec_restore(bare_mtcars, gdf), gdf)

  expect_identical(vec_ptype(gdf), gdf[0, ])

  gdf <- dplyr::group_by(bare_mtcars, cyl, am, vs)
  expect_identical(gdf[0, ], vec_ptype(gdf))

  out <- vec_ptype(dplyr::group_by(bare_mtcars, cyl, .drop = FALSE))
  expect_drop(out, FALSE)
})

test_that("can take the common type of grouped tibbles and tibbles", {
  skip("until vec_order_radix -> vec_order")

  gdf <- dplyr::group_by(bare_mtcars, cyl)
  expect_identical(vec_ptype2(gdf, data.frame()), vec_ptype(gdf))
  expect_identical(vec_ptype2(data.frame(), gdf), vec_ptype(gdf))
  expect_identical(vec_ptype2(gdf, tibble()), vec_ptype(gdf))
  expect_identical(vec_ptype2(tibble(), gdf), vec_ptype(gdf))

  gdf_nodrop <- dplyr::group_by(bare_mtcars, cyl, .drop = FALSE)
  expect_drop(vec_ptype2(gdf, gdf_nodrop), FALSE)
  expect_drop(vec_ptype2(gdf_nodrop, gdf), FALSE)
  expect_drop(vec_ptype2(gdf_nodrop, bare_mtcars), FALSE)
  expect_drop(vec_ptype2(bare_mtcars, gdf_nodrop), FALSE)
})

test_that("the common type of grouped tibbles includes the union of grouping variables", {
  skip("until vec_order_radix -> vec_order")

  gdf1 <- dplyr::group_by(bare_mtcars, cyl)
  gdf2 <- dplyr::group_by(bare_mtcars, am, vs)
  expect_identical(
    vec_ptype2(gdf1, gdf2),
    vec_ptype(dplyr::group_by(bare_mtcars, cyl, am, vs))
  )
})

test_that("can cast to and from `grouped_df`", {
  skip("until vec_order_radix -> vec_order")

  gdf <- dplyr::group_by(unrownames(bare_mtcars), cyl)
  input <- bare_mtcars[10]
  cast_gdf <- dplyr::group_by(vec_cast(bare_mtcars[10], bare_mtcars), cyl)

  expect_error(
    vec_cast(input, dplyr::group_by(bare_mtcars["cyl"], cyl)),
    class = "vctrs_error_cast_lossy"
  )

  expect_identical(
    vec_cast(input, gdf),
    cast_gdf
  )
  expect_identical(
    vec_cast(gdf, bare_mtcars),
    unrownames(bare_mtcars)
  )

  expect_identical(
    vec_cast(tibble::as_tibble(input), gdf),
    unrownames(cast_gdf)
  )
  tib <- tibble::as_tibble(bare_mtcars)
  expect_identical(
    unrownames(vec_cast(gdf, tib)),
    tib
  )
})

test_that("casting to `grouped_df` doesn't require grouping variables", {
  skip("until vec_order_radix -> vec_order")

  expect_identical(
    vec_cast(bare_mtcars[10], dplyr::group_by(bare_mtcars, cyl)),
    dplyr::group_by(vec_cast(bare_mtcars[10], bare_mtcars), cyl)
  )
})

test_that("casting to `grouped_df` handles `drop`", {
  skip("until vec_order_radix -> vec_order")

  gdf_nodrop <- dplyr::group_by(bare_mtcars, cyl, .drop = FALSE)
  expect_identical(vec_cast(bare_mtcars, gdf_nodrop), gdf_nodrop)
})

test_that("can cbind grouped data frames", {
  skip("until vec_order_radix -> vec_order")

  gdf <- dplyr::group_by(bare_mtcars[-10], cyl)
  df <- unrownames(bare_mtcars)[10]

  expect_identical(
    unrownames(vec_cbind(gdf, df)),
    tibble::as_tibble(bare_mtcars)[c(1:9, 11, 10)]
  )

  gdf1 <- dplyr::group_by(bare_mtcars[2], cyl)
  gdf2 <- dplyr::group_by(bare_mtcars[8:9], vs, am)
  expect_identical(
    unrownames(vec_cbind(gdf1, gdf2)),
    tibble::as_tibble(bare_mtcars)[c(2, 8, 9)]
  )
})


# `rowwise` ----------------------------------------------------------

test_that("rowwise can be proxied and restored", {
  rww <- dplyr::rowwise(unrownames(bare_mtcars))

  expect_identical(vec_proxy(rww), rww)
  expect_identical(vec_restore(unrownames(bare_mtcars), rww), rww)

  expect_identical(vec_ptype(rww), rww[0, ])
})

test_that("can take the common type of rowwise tibbles and tibbles", {
  rww <- dplyr::rowwise(bare_mtcars)
  expect_identical(vec_ptype2(rww, data.frame()), vec_ptype(rww))
  expect_identical(vec_ptype2(data.frame(), rww), vec_ptype(rww))
  expect_identical(vec_ptype2(rww, tibble()), vec_ptype(rww))
  expect_identical(vec_ptype2(tibble(), rww), vec_ptype(rww))
})

test_that("can cast to and from `rowwise_df`", {
  rww <- unrownames(dplyr::rowwise(bare_mtcars))
  input <- bare_mtcars[10]
  cast_rww <- dplyr::rowwise(vec_cast(bare_mtcars[10], bare_mtcars))

  expect_error(
    vec_cast(input, dplyr::rowwise(bare_mtcars["cyl"])),
    class = "vctrs_error_cast_lossy"
  )

  expect_identical(
    vec_cast(input, rww),
    cast_rww
  )
  expect_identical(
    vec_cast(rww, bare_mtcars),
    unrownames(bare_mtcars)
  )

  expect_identical(
    vec_cast(tibble::as_tibble(input), rww),
    unrownames(cast_rww)
  )
  tib <- tibble::as_tibble(bare_mtcars)
  expect_identical(
    unrownames(vec_cast(rww, tib)),
    tib
  )
})

test_that("can cbind rowwise data frames", {
  skip("until vec_order_radix -> vec_order")

  df <- unrownames(bare_mtcars)
  rww <- dplyr::rowwise(df[-2])
  gdf <- dplyr::group_by(df[2], cyl)

  exp <- dplyr::rowwise(df[c(1, 3:11, 2)])
  expect_identical(vec_cbind(rww, df[2]), exp)

  # Suboptimal
  expect_identical(vec_cbind(rww, gdf), exp)
})

test_that("no common type between rowwise and grouped data frames", {
  skip("until vec_order_radix -> vec_order")

  expect_df_fallback_warning(
    out <- vec_ptype_common_df_fallback(dplyr::rowwise(bare_mtcars), dplyr::group_by(bare_mtcars, cyl))
  )
  expect_identical(out, tibble::as_tibble(bare_mtcars[0, ]))
})
