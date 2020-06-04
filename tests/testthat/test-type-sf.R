
# Avoids adding `sf` to Suggests
testthat_import_from("sf", c(
  "st_sfc",
  "st_point",
  "st_bbox",
  "st_precision",
  "st_crs",
  "st_linestring",
  "st_as_sf",
  "st_multipoint"
))

# https://github.com/r-spatial/sf/issues/1390
test_that("can combine sfc lists", {
  ls <- st_linestring(matrix(1:3, ncol = 3))

  sfc <- st_sfc(ls)
  expect_identical(vec_c(sfc, sfc), c(sfc, sfc))

  sf <- st_as_sf(data.frame(id = 1, geometry = sfc))
  expect_identical(vec_rbind(sf, sf), rbind(sf, sf))
})

test_that("can combine sfc lists with unspecified chunks", {
  point <- st_point(1:2)
  out <- vec_c(c(NA, NA), st_sfc(point), NA)
  expect_identical(out, st_sfc(NA, NA, point, NA))

  multipoint <- st_multipoint(matrix(1:4, 2))
  x <- st_sfc(point)
  y <- st_sfc(multipoint, multipoint)
  out <- vec_rbind(
    data_frame(x = x),
    data_frame(y = y)
  )
  expect_identical(out, data_frame(
    x = st_sfc(point, NA, NA),
    y = st_sfc(NA, multipoint, multipoint)
  ))
})

test_that("`n_empty` attribute of `sfc` vectors is restored", {
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
	pt1 = st_sfc(st_point(c(1L, 2L)))
	pt2 = st_sfc(st_point(c(10L, 20L)))

	x = c(pt1, pt2)
	expect_identical(st_bbox(vctrs::vec_slice(x, 1)), st_bbox(pt1))
	expect_identical(st_bbox(vctrs::vec_slice(x, 2)), st_bbox(pt2))

	combined = vctrs::vec_c(pt1, pt2)
	expect_identical(st_bbox(x), st_bbox(combined))
})

test_that("`precision` and `crs` attributes of `sfc` vectors are restored", {
	x = st_sfc(st_point(c(pi, pi)), precision = 1e-4, crs = 3857)
	out = vctrs::vec_slice(x, 1)
	expect_identical(st_precision(x), st_precision(out))
	expect_identical(st_crs(x), st_crs(out))
})

test_that("`precision` and `crs` attributes of `sfc` vectors are combined", {
	x = st_sfc(st_point(c(pi, pi)), precision = 1e-4, crs = 3857)
	y = st_sfc(st_point(c(0, 0)), precision = 1e-4, crs = 3857)

	out = vctrs::vec_c(x, y)
	expect_identical(st_precision(x), st_precision(out))
	expect_identical(st_crs(x), st_crs(out))

        # These used to be errors before we fell back to c()
	y = st_sfc(st_point(c(0, 0)), precision = 1e-2, crs = 3857)
	expect_identical(vctrs::vec_c(x, y), c(x, y))
	# expect_error(vctrs::vec_c(x, y), "precisions not equal")

	y = st_sfc(st_point(c(0, 0)), precision = 1e-4, crs = 4326)
	expect_identical(vctrs::vec_c(x, y), c(x, y))
	# expect_error(vctrs::vec_c(x, y), "coordinate reference systems not equal")
})
