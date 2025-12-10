test_that("`numeric_version` is a vector (#723)", {
  x <- numeric_version("0.1.0")
  y <- numeric_version("0.2.0")
  z <- c(x, y)

  expect_true(vec_is(x))

  expect_true(vec_equal(x, x))
  expect_false(vec_equal(x, y))
  expect_identical(vec_equal(y, z), c(FALSE, TRUE))

  expect_identical(vec_unique(z), z)
  expect_identical(vec_unique(c(y, z, x)), z[2:1])
})

test_that("`numeric_version` falls back to base methods", {
  x <- utils::packageVersion("rlang")
  y <- utils::packageVersion("vctrs")
  z <- c(x, y)

  # `z` is a `list-of`-like type but slicing 1 element returns the
  # atomic type. To implement this in vctrs we'd need to provide a way
  # of customising the "wrapper" type for size > 1 vectors.
  expect_identical(vec_slice(z, 1:2), z)
  expect_identical(vec_slice(z, 1), x)
  expect_identical(vec_slice(z, 2), y)

  expect_identical(vec_c(x, y), z)
})

test_that("`numeric_version` has an equality, comparison, and order proxy", {
  numeric_row <- function(...) {
    out <- list2(...)
    out <- map(out, as.integer)
    names(out) <- paste0("...", seq_len(8L))
    new_data_frame(out, n = 1L)
  }

  x <- numeric_version(c("1.2-3", "1.21.1", "3", "2.21.0.9000", "0.5.01"))

  expect <- vec_rbind(
    numeric_row(1, 2, 3, 0, 0, 0, 0, 0),
    numeric_row(1, 21, 1, 0, 0, 0, 0, 0),
    numeric_row(3, 0, 0, 0, 0, 0, 0, 0),
    numeric_row(2, 21, 0, 9000, 0, 0, 0, 0),
    numeric_row(0, 5, 1, 0, 0, 0, 0, 0)
  )

  expect_identical(vec_proxy_equal(x), expect)
  expect_identical(vec_proxy_compare(x), expect)
  expect_identical(vec_proxy_order(x), expect)
})

test_that("`numeric_version` proxy works with empty vectors", {
  x <- numeric_version(character())

  expect <- vec_rep(list(integer()), times = 8L)
  names(expect) <- paste0("...", seq_len(8L))
  expect <- new_data_frame(expect, n = 0L)

  expect_identical(vec_proxy_equal(x), expect)
})

test_that("`numeric_version` proxy handles pseudo-`NA`", {
  numeric_row <- function(...) {
    out <- list2(...)
    out <- map(out, as.integer)
    names(out) <- paste0("...", seq_len(8L))
    new_data_frame(out, n = 1L)
  }

  x <- numeric_version(c("1_1", "1.2", NA), strict = FALSE)

  expect <- vec_rbind(
    numeric_row(NA, NA, NA, NA, NA, NA, NA, NA),
    numeric_row(1, 2, 0, 0, 0, 0, 0, 0),
    numeric_row(NA, NA, NA, NA, NA, NA, NA, NA)
  )

  expect_identical(vec_proxy_equal(x), expect)
  expect_identical(vec_proxy_compare(x), expect)
  expect_identical(vec_proxy_order(x), expect)
})

test_that("`numeric_version` works with functions using the equality proxy", {
  x <- numeric_version(
    c("1.2-3", "1.21.1", "1_1", "0.5", "1.3"),
    strict = FALSE
  )
  y <- numeric_version(
    c("1.21.1", "1.21.1", "1_2", "0.05", "1_3"),
    strict = FALSE
  )

  expect_identical(vec_unique(x), x)
  expect_identical(vec_unique(y), y[c(1, 3, 4)])

  expect_identical(vec_detect_missing(y), c(FALSE, FALSE, TRUE, FALSE, TRUE))

  expect_identical(vec_equal(x, y), c(FALSE, TRUE, NA, TRUE, NA))
  expect_identical(
    vec_equal(x, y, na_equal = TRUE),
    c(FALSE, TRUE, TRUE, TRUE, FALSE)
  )
})

test_that("`numeric_version` works with functions using the comparison proxy", {
  x <- numeric_version(
    c("1.2-3", "1.21.1", "1_1", "0.5", "1.3"),
    strict = FALSE
  )
  y <- numeric_version(
    c("1.21.1", "1.21.1", "1_2", "0.05", "1_3"),
    strict = FALSE
  )

  expect_identical(vec_compare(x, y), c(-1L, 0L, NA, 0L, NA))
  expect_identical(vec_compare(x, y, na_equal = TRUE), c(-1L, 0L, 0L, 0L, 1L))

  # Specifically related to base R taking a joint proxy in `Ops.numeric_version`
  x <- numeric_version("3.3")
  y <- numeric_version("3.21")
  # `.encode_numeric_version(x) < .encode_numeric_version(y)` == FALSE
  # `x < y` == TRUE
  expect_identical(vec_compare(x, y), -1L)
})

test_that("`numeric_version` works with functions using the order proxy (tidyverse/dplyr#6680)", {
  x <- numeric_version(
    c("1.2-3", "1.21.1", "1_1", "0.5", "1.30"),
    strict = FALSE
  )
  y <- numeric_version(
    c("1.21.1", "1.21.1", "1_2", "0.05", "1_3"),
    strict = FALSE
  )

  expect_identical(vec_order(y), c(4L, 1L, 2L, 3L, 5L))
  expect_identical(vec_order_radix(y), c(4L, 1L, 2L, 3L, 5L))

  expect_identical(vec_order(y, na_value = "smallest"), c(3L, 5L, 4L, 1L, 2L))
  expect_identical(
    vec_order_radix(y, na_value = "smallest"),
    c(3L, 5L, 4L, 1L, 2L)
  )

  expect_identical(
    vec_locate_matches(x, y),
    data_frame(
      needles = c(1L, 2L, 2L, 3L, 3L, 4L, 5L),
      haystack = c(NA, 1L, 2L, 3L, 5L, 4L, NA)
    )
  )
  expect_identical(
    vec_locate_matches(x, y, condition = "<"),
    data_frame(
      needles = c(1L, 1L, 2L, 3L, 4L, 4L, 5L),
      haystack = c(1L, 2L, NA, NA, 1L, 2L, NA)
    )
  )
})

test_that("`numeric_version` proxy can handle at most 8 components", {
  x <- numeric_version("1.2.3.4.5.6.7.8")
  expect_silent(vec_proxy_equal(x))

  x <- numeric_version("1.2.3.4.5.6.7.8.9")
  expect_snapshot(error = TRUE, {
    vec_proxy_equal(x)
  })
})

test_that("`numeric_version` can compare against components with 8 components", {
  x <- numeric_version("2.3.4.5.6.7.8.9")
  y <- c(x, numeric_version(c("1.1", "11.2", "2.1")))

  expect_identical(vec_compare(x, y), c(0L, 1L, -1L, 1L))
})

test_that("`package_version` and `R_system_version` use the `numeric_version` proxy", {
  x <- numeric_version("1.5.6")
  y <- package_version("1.5.6")
  z <- R_system_version("1.5.6")

  expect_identical(vec_proxy_equal(y), vec_proxy_equal(x))
  expect_identical(vec_proxy_equal(z), vec_proxy_equal(x))
})

test_that("`omit` class is numeric (#1160)", {
  x <- c(NA, 1:3, NA)
  omit <- attr(na.omit(x), "na.action")

  expect_identical(
    vec_ptype_common(omit, omit),
    structure(int(), class = "omit")
  )
  expect_identical(vec_ptype_common(1.5, omit), dbl())
  expect_identical(vec_ptype_common(omit, 1L), int())

  expect_identical(vec_cast_common(omit, omit), list(omit, omit))
  expect_identical(vec_cast_common(omit, 1L), list(unstructure(omit), 1L))
  expect_identical(
    vec_cast_common(1.5, omit),
    list(1.5, unstructure(as.double(omit)))
  )

  expect_error(vec_cast(1L, omit), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(1.0, omit), class = "vctrs_error_incompatible_type")

  expect_identical(vec_slice(omit, 1), structure(1L, class = "omit"))
  expect_identical(
    vec_c(omit, omit),
    structure(c(1L, 5L, 1L, 5L), class = "omit")
  )
  expect_identical(vec_c(omit, omit, 10L), c(1L, 5L, 1L, 5L, 10L))

  expect_identical(vec_slice(x, omit), x[omit])
})

test_that("`exclude` class is numeric (#1160)", {
  x <- c(NA, 1:3, NA)
  exc <- attr(na.exclude(x), "na.action")

  expect_identical(
    vec_ptype_common(exc, exc),
    structure(int(), class = "exclude")
  )
  expect_identical(vec_ptype_common(1.5, exc), dbl())
  expect_identical(vec_ptype_common(exc, 1L), int())

  expect_identical(vec_cast_common(exc, exc), list(exc, exc))
  expect_identical(vec_cast_common(exc, 1L), list(unstructure(exc), 1L))
  expect_identical(
    vec_cast_common(1.5, exc),
    list(1.5, unstructure(as.double(exc)))
  )

  expect_error(vec_cast(1L, exc), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(1.0, exc), class = "vctrs_error_incompatible_type")

  expect_identical(vec_slice(exc, 1), structure(1L, class = "exclude"))
  expect_identical(
    vec_c(exc, exc),
    structure(c(1L, 5L, 1L, 5L), class = "exclude")
  )
  expect_identical(vec_c(exc, exc, 10L), c(1L, 5L, 1L, 5L, 10L))

  expect_identical(vec_slice(x, exc), x[exc])
})
