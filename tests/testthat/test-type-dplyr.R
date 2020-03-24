
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
    vec_ptype2(gdf1, gdf2),
    vec_ptype(dplyr::group_by(mtcars, cyl, am, vs))
  )
})
