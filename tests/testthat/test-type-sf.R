# Never run on CRAN, even if they have sf, because we don't regularly
# check these on CI and we don't want a change in sf to force a CRAN
# failure for vctrs.
skip_on_cran()

# Need recent version to work around restore bug for sfc lists and changes
# to `c.sfc()`
skip_if_not_installed("sf", "1.0-11")

test_that("sf has a ptype2 method", {
  testthat_import_from_sf()

  sfc1 = st_sfc(st_point(1:2), st_point(3:4))
  sfc2 = st_sfc(st_linestring(matrix(1:4, 2)))

  sf1 = st_sf(x = c(TRUE, FALSE), geo1 = sfc1)
  sf2 = st_sf(y = "", geo2 = sfc2, x = 0, stringsAsFactors = FALSE)

  # <sf> + <sf> = <sf>
  out = vctrs::vec_ptype2(sf1, sf2)
  exp = st_sf(
    x = double(),
    y = character(),
    geo1 = sfc1[0],
    geo2 = sfc2[0],
    stringsAsFactors = FALSE
  )
  expect_identical(out, exp)

  # <sf> + <df> = <df>
  out = vctrs::vec_ptype2(sf1, new_data_frame(sf2))
  exp = data_frame(
    x = double(),
    geo1 = sfc1[0],
    y = character(),
    geo2 = sfc2[0]
  )
  expect_identical(out, exp)

  # <df> + <sf> = <df>
  out = vctrs::vec_ptype2(new_data_frame(sf1), sf2)
  exp = data_frame(
    x = double(),
    geo1 = sfc1[0],
    y = character(),
    geo2 = sfc2[0]
  )
  expect_identical(out, exp)

  # It works interactively, but not in `devtools::test_active_file()`.
  # Something about lazy registration of S3 methods not making it into tibble?
  #
  #   # <sf> + <tib> = <tib>
  #   out = vctrs::vec_ptype2(sf1, tibble::new_tibble(sf2))
  #   exp = tibble::tibble(
  #     x = double(),
  #     geo1 = sfc1[0],
  #     y = character(),
  #     geo2 = sfc2[0]
  #   )
  #   expect_identical(out, exp)
  #
  #   # <tib> + <sf> = <tib>
  #   out = vctrs::vec_ptype2(tibble::new_tibble(sf1), sf2)
  #   exp = tibble::tibble(
  #     x = double(),
  #     geo1 = sfc1[0],
  #     y = character(),
  #     geo2 = sfc2[0]
  #   )
  #   expect_identical(out, exp)
})

test_that("sf has a cast method", {
  testthat_import_from_sf()

  sfc1 = st_sfc(st_point(1:2), st_point(3:4))
  sfc2 = st_sfc(st_linestring(matrix(1:4, 2)))

  sf1 = st_sf(x = c(TRUE, FALSE), geo1 = sfc1)
  sf2 = st_sf(y = "", geo2 = sfc2, x = 0, stringsAsFactors = FALSE)

  # Incompatible attributes
  expect_snapshot(error = TRUE, {
    vctrs::vec_cast(sf1, sf2)
  })
  expect_snapshot(error = TRUE, {
    vctrs::vec_cast(sf2, sf1)
  })

  common = vctrs::vec_ptype2(sf1, sf2)

  # From `vec_cast_sf_sf`
  out = vctrs::vec_cast(sf1, common)
  exp = st_sf(
    x = c(1, 0),
    y = character(2)[NA],
    geo1 = sfc1,
    geo2 = sfc2[c(NA, NA) + 0L],
    stringsAsFactors = FALSE
  )
  expect_identical(out, exp)

  # From `vec_cast_sf_sf`
  out = vctrs::vec_cast(sf2, common)
  exp = st_sf(
    x = 0,
    y = "",
    geo1 = sfc1[NA + 0L],
    geo2 = sfc2,
    stringsAsFactors = FALSE
  )
  expect_identical(out, exp)

  # Explicit opt out of data.frame -> sf, avoids `vec_default_cast()`
  expect_snapshot(error = TRUE, {
    vctrs::vec_cast(new_data_frame(sf1), common)
  })
  # Explicit opt out of tibble -> sf, avoids `vec_default_cast()`
  expect_snapshot(error = TRUE, {
    vctrs::vec_cast(tibble::new_tibble(sf1), common)
  })
})

# https://github.com/r-lib/vctrs/issues/1136
test_that("can combine sf data frames", {
  testthat_import_from_sf()
  testthat_import_from("dplyr", "bind_rows")

  sfc1 = st_sfc(st_point(1:2), st_point(3:4))
  sfc2 = st_sfc(st_linestring(matrix(1:4, 2)))

  sf1 = st_sf(x = c(TRUE, FALSE), geo1 = sfc1)
  sf2 = st_sf(y = "", geo2 = sfc2, x = 0, stringsAsFactors = FALSE)

  exp = st_sf(
    x = c(1, 0, 0),
    geo1 = sfc1[c(1, 2, NA)],
    y = c(NA, NA, ""),
    geo2 = sfc2[c(NA, NA, 1)]
  )
  expect_identical(vctrs::vec_rbind(sf1, sf2), exp)
  expect_identical(bind_rows(sf1, sf2), exp)

  exp = st_sf(
    y = c("", NA, NA, ""),
    x = c(0, 1, 0, 0),
    geo2 = sfc2[c(1, NA, NA, 1)],
    geo1 = sfc1[c(NA, 1, 2, NA)]
  )
  expect_identical(vctrs::vec_rbind(sf2, sf1, sf2), exp)
  expect_identical(bind_rows(sf2, sf1, sf2), exp)
})

test_that("can combine sf and tibble", {
  testthat_import_from_sf()

  sfc1 = st_sfc(st_point(1:2), st_point(3:4))
  sfc2 = st_sfc(st_linestring(matrix(1:4, 2)))

  sf1 = st_sf(x = c(TRUE, FALSE), geo1 = sfc1)
  sf2 = st_sf(y = "", geo2 = sfc2, x = 0, stringsAsFactors = FALSE)

  out = vctrs::vec_rbind(sf2, data.frame(x = 1))
  exp = data_frame(
    y = c("", NA),
    x = c(0, 1),
    geo2 = sfc2[c(1, NA)]
  )
  expect_identical(out, exp)

  # It works interactively, but not in `devtools::test_active_file()`.
  # Something about lazy registration of S3 methods not making it into tibble?
  #
  #   out = vctrs::vec_rbind(sf2, tibble::tibble(x = 1))
  #   exp = tibble::tibble(
  #     y = c("", NA),
  #     x = c(0, 1),
  #     geo2 = sfc2[c(1, NA)]
  #   )
  #   expect_identical(out, exp)
  #
  #   out = vctrs::vec_rbind(tibble::tibble(x = 1), sf2)
  #   exp = tibble::tibble(
  #     x = c(1, 0),
  #     y = c(NA, ""),
  #     geo2 = sfc2[c(NA, 1)]
  #   )
  #   expect_identical(out, exp)
})

# https://github.com/r-spatial/sf/issues/1390
test_that("can combine sfc lists", {
  testthat_import_from_sf()

  ls <- st_linestring(matrix(1:3, ncol = 3))

  sfc <- st_sfc(ls)
  expect_identical(
    vec_c(sfc, sfc),
    c(sfc, sfc)
  )

  sf <- st_as_sf(data.frame(id = 1, geometry = sfc))

  expect_identical(
    vec_rbind(sf, sf),
    rbind(sf, sf)
  )
  expect_identical(
    vec_rbind(sf, sf, sf),
    rbind(sf, sf, sf)
  )
})

test_that("can combine sfc lists with unspecified chunks", {
  testthat_import_from_sf()

  point <- st_point(1:2)
  out <- vec_c(c(NA, NA), st_sfc(point), NA)
  expect_identical(out, st_sfc(point)[c(NA, NA, 1, NA)])

  multipoint <- st_multipoint(matrix(1:4, 2))
  x <- st_sfc(point)
  y <- st_sfc(multipoint, multipoint)
  out <- vec_rbind(
    data_frame(x = x),
    data_frame(y = y)
  )
  expect_identical(
    out,
    data_frame(
      x = st_sfc(point)[c(1, NA, NA)],
      y = st_sfc(multipoint)[c(NA, 1, 1)]
    )
  )
})

test_that("`n_empty` attribute of `sfc` vectors is restored", {
  testthat_import_from_sf()

  pt1 = st_sfc(st_point(c(NA_real_, NA_real_)))
  pt2 = st_sfc(st_point(0:1))

  x = c(pt1, pt2)
  expect_identical(attr(vctrs::vec_slice(x, 1), "n_empty"), 1L)
  expect_identical(attr(vctrs::vec_slice(x, 2), "n_empty"), 0L)

  combined = vctrs::vec_c(pt1, pt2, pt1)
  expect_length(combined, 3)
  expect_identical(attr(combined, "n_empty"), 2L)
})

test_that("bbox attributes of `sfc` vectors are restored", {
  testthat_import_from_sf()

  pt1 = st_sfc(st_point(c(1L, 2L)))
  pt2 = st_sfc(st_point(c(10L, 20L)))

  x = c(pt1, pt2)
  expect_identical(st_bbox(vctrs::vec_slice(x, 1)), st_bbox(pt1))
  expect_identical(st_bbox(vctrs::vec_slice(x, 2)), st_bbox(pt2))

  combined = vctrs::vec_c(pt1, pt2)
  expect_identical(st_bbox(x), st_bbox(combined))
})

test_that("`precision` and `crs` attributes of `sfc` vectors are restored", {
  testthat_import_from_sf()
  x = st_sfc(st_point(c(pi, pi)), precision = 1e-4, crs = 3857)
  out = vctrs::vec_slice(x, 1)
  expect_identical(st_precision(x), st_precision(out))
  expect_identical(st_crs(x), st_crs(out))
})

test_that("`precision` attributes of `sfc` vectors must be the same", {
  testthat_import_from_sf()

  x = st_sfc(st_point(c(pi, pi)), precision = 1e-4, crs = 3857)
  y = st_sfc(st_point(c(0, 0)), precision = 1e-4, crs = 3857)

  out = vctrs::vec_c(x, y)
  expect_identical(st_precision(x), st_precision(out))

  # sf implements `vec_proxy.sfc()` but has incorrectly implemented
  # `sf:::vec_ptype2.sfc.sfc` rather than
  # `sf:::vec_ptype2.sfc_GEOMETRY.sfc_GEOMETRY` or
  # `sf:::vec_ptype2.sfc_POINT.sfc_POINT`. If they implement those correctly,
  # this will likely throw an sf specific error, because it looks like the
  # intent of `sf:::vec_ptype2.sfc.sfc` is to error on different precisions.
  y = st_sfc(st_point(c(0, 0)), precision = 1e-2, crs = 3857)
  expect_snapshot(error = TRUE, {
    vctrs::vec_c(x, y)
  })
})

test_that("`crs` attributes of `sfc` vectors must be the same", {
  testthat_import_from_sf()

  x = st_sfc(st_point(c(pi, pi)), precision = 1e-4, crs = 3857)
  y = st_sfc(st_point(c(0, 0)), precision = 1e-4, crs = 3857)

  out = vctrs::vec_c(x, y)
  expect_identical(st_crs(x), st_crs(out))

  # sf implements `vec_proxy.sfc()` but has incorrectly implemented
  # `sf:::vec_ptype2.sfc.sfc` rather than
  # `sf:::vec_ptype2.sfc_GEOMETRY.sfc_GEOMETRY` or
  # `sf:::vec_ptype2.sfc_POINT.sfc_POINT`. If they implement those correctly,
  # this will likely throw an sf specific error, because it looks like the
  # intent of `sf:::vec_ptype2.sfc.sfc` is to error on different crs.
  y = st_sfc(st_point(c(0, 0)), precision = 1e-4, crs = 4326)
  expect_snapshot(error = TRUE, {
    vctrs::vec_c(x, y)
  })
})

test_that("`vec_locate_matches()` works with `sfc` vectors", {
  testthat_import_from_sf()

  x <- c(
    st_sfc(st_point(c(0, 0))),
    st_sfc(st_point(c(0, 1))),
    st_sfc(st_point(c(2, 1))),
    st_sfc(c(st_point(c(0, 1)), st_point(c(0, 1))))
  )

  y <- c(
    st_sfc(c(st_point(c(0, 1)), st_point(c(0, 1)))),
    st_sfc(st_point(c(0, 0))),
    st_sfc(st_point(c(0, 3))),
    st_sfc(st_point(c(0, 0))),
    st_sfc(st_point(c(0, 1)))
  )

  out <- vec_locate_matches(x, y)
  expect_identical(out$needles, c(1L, 1L, 2L, 3L, 4L))
  expect_identical(out$haystack, c(2L, 4L, 5L, NA, 1L))
})

test_that("`vec_rbind()` doesn't leak common type fallbacks (#1331)", {
  testthat_import_from_sf()

  sf = st_sf(id = 1:2, geo = st_sfc(st_point(c(1, 1)), st_point(c(2, 2))))

  expect_identical(
    vec_rbind(sf, sf),
    st_sf(id = rep(1:2, 2), geo = rep(sf$geo, 2))
  )
  expect_identical(
    vec_rbind(sf, sf, .names_to = "id"),
    st_sf(id = rep(1:2, each = 2), geo = rep(sf$geo, 2))
  )
})

test_that("vec_ptype() calls vec_restore(), which correctly recomputes column ordering", {
  testthat_import_from_sf()

  geometry <- st_sfc(
    st_point(c(1, 2)),
    st_point(c(3, 4))
  )
  df <- data.frame(before = 1:2, geometry = geometry)
  df <- st_as_sf(df)

  # Note how we add a column after the `geometry` column.
  # sf will relocate this column when `st_as_sfc()` is called via `vec_ptype()`.
  df$after <- 3:4

  exp <- st_as_sf(data.frame(
    before = 1:2,
    after = 3:4,
    geometry = geometry
  ))

  # Because there is only 1 input, `vec_ptype()` is called rather than
  # `vec_ptype2()`. This calls `df_map()` and then does a `vec_restore()` on the
  # way out, which hits `vec_restore_sf()`. The problem is that `df_map()`
  # partially restores the data frame by copying over all attributes from `to`
  # before calling `vec_restore_sf()`, which we don't want. So we have to be
  # very careful in `vec_restore_sf()` to reclear all attributes back to a bare
  # data frame before calling `st_as_sf()`, otherwise it will dispatch to the
  # wrong S3 method.
  expect_identical(
    vec_c(df),
    exp
  )
})

test_that("vec_restore() doesn't modify non-GEOMETRYCOLLECTION empty types", {
  testthat_import_from_sf()

  x <- st_sfc(
    st_point(c(1, 2)),
    st_point(),
    st_linestring(matrix(c(3, 4), ncol = 2)),
    st_linestring()
  )

  # `x` is a mixed type `sfc_GEOMETRY`, so is a candidate for
  # `GEOMETRYCOLLECTION EMPTY` replacement. Check that `POINT EMPTY` and
  # `LINESTRING EMPTY` aren't replaced.
  proxy <- vec_proxy(x)
  out <- vec_restore(proxy, x)

  expect_identical(x, out)
})
