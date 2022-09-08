
# Avoids adding `sf` to Suggests
testthat_import_from("sf", c(
  "st_sf",
  "st_sfc",
  "st_point",
  "st_bbox",
  "st_precision",
  "st_crs",
  "st_linestring",
  "st_as_sf",
  "st_multipoint"
))

# Need recent version to work around restore bug for sfc lists
skip_if_not_installed("sf", "0.9-4")

test_that("sf has a ptype2 method", {
	sfc1 = st_sfc(st_point(1:2), st_point(3:4))
	sfc2 = st_sfc(st_linestring(matrix(1:4, 2)))

	sf1 = st_sf(x = c(TRUE, FALSE), geo1 = sfc1)
	sf2 = st_sf(y = "", geo2 = sfc2, x = 0, stringsAsFactors = FALSE)

	out = vctrs::vec_ptype2(sf1, sf2)
	exp = st_sf(
		x = double(),
		y = character(),
		geo1 = sfc1[0],
		geo2 = sfc2[0],
		stringsAsFactors = FALSE
	)
	expect_identical(out, exp)

	out = vctrs::vec_ptype2(sf1, new_data_frame(sf2))
	expect_identical(out, exp)

	out = vctrs::vec_ptype2(new_data_frame(sf1), sf2)
	exp_rhs = st_sf(
		x = double(),
		y = character(),
		geo1 = sfc1[0],
		geo2 = sfc2[0],
		stringsAsFactors = FALSE,
		sf_column_name = "geo2"
	)
	expect_identical(out, exp_rhs)
})

test_that("sf has a cast method", {
	sfc1 = st_sfc(st_point(1:2), st_point(3:4))
	sfc2 = st_sfc(st_linestring(matrix(1:4, 2)))

	sf1 = st_sf(x = c(TRUE, FALSE), geo1 = sfc1)
	sf2 = st_sf(y = "", geo2 = sfc2, x = 0, stringsAsFactors = FALSE)

	expect_error(
		vctrs::vec_cast(sf1, sf2),
		class = "vctrs_error_cast_lossy"
	)
	expect_error(
		vctrs::vec_cast(sf2, sf1),
		class = "vctrs_error_cast_lossy"
	)

	common = vec_ptype2(sf1, sf2)

	out = vctrs::vec_cast(sf1, common)
	exp = st_sf(
		x = c(1, 0),
		y = character(2)[NA],
		geo1 = sfc1,
		geo2 = sfc2[c(NA, NA) + 0L],
		stringsAsFactors = FALSE
	)
	expect_identical(out, exp)

	out = vctrs::vec_cast(new_data_frame(sf1), common)
	expect_identical(out, exp)

	out = vctrs::vec_cast(sf1, new_data_frame(common))
	expect_identical(out, new_data_frame(exp))

	out = vctrs::vec_cast(sf2, common)
	exp = st_sf(
		x = 0,
		y = "",
		geo1 = sfc1[NA + 0L],
		geo2 = sfc2,
		stringsAsFactors = FALSE
	)
	expect_identical(out, exp)
})

# https://github.com/r-lib/vctrs/issues/1136
test_that("can combine sf data frames", {
	testthat_import_from("dplyr", "bind_rows")

	sfc1 = st_sfc(st_point(1:2), st_point(3:4))
	sfc2 = st_sfc(st_linestring(matrix(1:4, 2)))

	sf1 = st_sf(x = c(TRUE, FALSE), geo1 = sfc1)
	sf2 = st_sf(y = "", geo2 = sfc2, x = 0, stringsAsFactors = FALSE)

	# FIXME: Currently `vec_rbind()` returns a data frame because we
	# are temporarily working around bugs due to bad interaction of
	# different fallbacks. `bind_rows()` returns an `sf` data frame as
	# expected because of `dplyr_reconstruct()`.
	exp = data_frame(
		x = c(1, 0, 0),
		geo1 = sfc1[c(1:2, NA)],
		y = c(NA, NA, ""),
		geo2 = sfc2[c(NA, NA, 1)]
	)
	expect_identical(vctrs::vec_rbind(sf1, sf2), exp)
	expect_identical(bind_rows(sf1, sf2), st_as_sf(exp))

	exp = data_frame(
		y = c("", NA, NA, ""),
		x = c(0, 1, 0, 0),
		geo2 = sfc2[c(1, NA, NA, 1)],
		geo1 = sfc1[c(NA, 1:2, NA)]
	)
	expect_identical(vctrs::vec_rbind(sf2, sf1, sf2), exp)
	expect_identical(bind_rows(sf2, sf1, sf2), st_as_sf(exp))
})

test_that("can combine sf and tibble", {
	sfc1 = st_sfc(st_point(1:2), st_point(3:4))
	sfc2 = st_sfc(st_linestring(matrix(1:4, 2)))

	sf1 = st_sf(x = c(TRUE, FALSE), geo1 = sfc1)
	sf2 = st_sf(y = "", geo2 = sfc2, x = 0, stringsAsFactors = FALSE)

	out = vctrs::vec_rbind(sf2, data.frame(x = 1))
	exp = data_frame(
		y = c("", NA),
		x = c(0, 1),
		geo2 = sfc2[c(1L, NA)]
	)
	expect_identical(out, exp)

	out = vctrs::vec_rbind(sf2, tibble::tibble(x = 1))
	expect_identical(out, exp)

	out = vctrs::vec_rbind(tibble::tibble(x = 1), sf2)
	exp = data_frame(
		x = c(1, 0),
		y = c(NA, ""),
		geo2 = sfc2[c(NA, 1L)]
	)
	expect_identical(out, exp)
})

# https://github.com/r-spatial/sf/issues/1390
test_that("can combine sfc lists", {
  ls <- st_linestring(matrix(1:3, ncol = 3))

  sfc <- st_sfc(ls)
  expect_identical(vec_c(sfc, sfc), c(sfc, sfc))

  sf <- st_as_sf(data.frame(id = 1, geometry = sfc))

  # Currently returns a bare data frame because of the workaround for
  # the `c()` fallback sentinels
  expect_identical(vec_rbind(sf, sf), new_data_frame(rbind(sf, sf)))
  expect_identical(vec_rbind(sf, sf, sf), new_data_frame(rbind(sf, sf, sf)))
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

test_that("`vec_locate_matches()` works with `sfc` vectors", {
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
  expect_identical(out$needles,  c(1L, 1L, 2L, 3L, 4L))
  expect_identical(out$haystack, c(2L, 4L, 5L, NA, 1L))
})

test_that("`vec_rbind()` doesn't leak common type fallbacks (#1331)", {
	sf = st_sf(id = 1:2, geo = st_sfc(st_point(c(1, 1)), st_point(c(2, 2))))

	expect_equal(
		vec_rbind(sf, sf),
		data_frame(id = rep(1:2, 2), geo = rep(sf$geo, 2))
	)

	expect_equal(
		vec_rbind(sf, sf, .names_to = "id"),
		data_frame(id = rep(1:2, each = 2), geo = rep(sf$geo, 2))
	)
})


# Local Variables:
# indent-tabs-mode: t
# ess-indent-offset: 4
# tab-width: 4
# End:
