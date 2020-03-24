
test_that("grouped-df is proxied and restored", {
  gdf <- dplyr::group_by(mtcars, cyl)

  expect_identical(vec_proxy(gdf), gdf)
  expect_identical(vec_restore(mtcars, gdf), gdf)

  expect_identical(vec_ptype(gdf), gdf[0, ])

  expect_error(
    vec_ptype(dplyr::group_by(mtcars, cyl, .drop = FALSE)),
    "unsupported in vctrs"
  )
})
