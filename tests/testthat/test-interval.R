# ------------------------------------------------------------------------------
# vec_interval_groups()

test_that("can compute groups", {
  x <- data_frame(
    start = c(1L, 9L,  2L, 2L, 10L),
    end = c(5L, 11L, 6L, 8L, 12L)
  )

  expect_identical(
    vec_interval_groups(x$start, x$end),
    data_frame(start = c(1L, 9L), end = c(8L, 12L))
  )
})

test_that("can group with size one input", {
  x <- data_frame(start = 1L, end = 2L)

  expect_identical(
    vec_interval_groups(x$start, x$end),
    x
  )
})

test_that("can group with size zero input", {
  x <- data_frame(start = integer(), end = integer())

  expect_identical(
    vec_interval_groups(x$start, x$end),
    x
  )
})

test_that("missing intervals are retained", {
  x <- data_frame(start = NA, end = NA)

  expect_identical(
    vec_interval_groups(x$start, x$end),
    x
  )

  x <- data_frame(start = c(NA, NA), end = c(NA, NA))

  expect_identical(
    vec_interval_groups(x$start, x$end),
    x[1,]
  )

  x <- data_frame(start = c(3, NA, 2, NA), end = c(5, NA, 3, NA))

  expect_identical(
    vec_interval_groups(x$start, x$end),
    data_frame(start = c(2, NA), end = c(5, NA))
  )
})

test_that("missing intervals can be dropped", {
  x <- data_frame(start = NA, end = NA)

  expect_identical(
    vec_interval_groups(x$start, x$end, missing = "drop"),
    data_frame(start = logical(), end = logical())
  )

  x <- data_frame(start = c(NA, NA), end = c(NA, NA))

  expect_identical(
    vec_interval_groups(x$start, x$end, missing = "drop"),
    data_frame(start = logical(), end = logical())
  )

  x <- data_frame(start = c(3, NA, 2, NA), end = c(5, NA, 3, NA))

  expect_identical(
    vec_interval_groups(x$start, x$end, missing = "drop"),
    data_frame(start = 2, end = 5)
  )
})

test_that("max endpoint is retained even if it isn't the last in the group", {
  # 10 is max end of first group, but 5 is last value in that group
  x <- data_frame(start = c(1L, 2L, 12L), end = c(10L, 5L, 15L))

  expect_identical(
    vec_interval_groups(x$start, x$end),
    data_frame(start = c(1L, 12L), end = c(10L, 15L))
  )
})

# ------------------------------------------------------------------------------
# vec_interval_locate_groups()

test_that("can locate groups", {
  x <- data_frame(
    start = c(1L, 9L,  2L, 2L, 10L),
    end = c(5L, 11L, 6L, 8L, 12L)
  )

  out <- vec_interval_locate_groups(x$start, x$end)

  expect_identical(
    out$key,
    data_frame(start = c(1L, 9L), end = c(8L, 12L))
  )

  expect_identical(
    out$loc,
    list(c(1L, 3L, 4L), c(2L, 5L))
  )
})

test_that("can locate groups with size one input", {
  expect_identical(
    vec_interval_locate_groups(1L, 2L),
    data_frame(
      key = data_frame(start = 1L, end = 2L),
      loc = list(1L)
    )
  )
})

test_that("can locate groups with size zero input", {
  expect_identical(
    vec_interval_locate_groups(integer(), integer()),
    data_frame(
      key = data_frame(start = integer(), end = integer()),
      loc = list()
    )
  )
})

test_that("locations are ordered by both `start` and `end`", {
  x <- data_frame(start = c(4L, 4L, 1L), end = c(6L, 5L, 2L))

  out <- vec_interval_locate_groups(x$start, x$end)

  # Ties of `start = 4` are broken by `end` values and reordered
  expect_identical(
    out$loc,
    list(3L, c(2L, 1L))
  )

  # So this orders `x`
  expect_identical(
    vec_slice(x, unlist(out$loc)),
    vec_sort(x)
  )
})

test_that("missing intervals are retained", {
  x <- data_frame(start = NA, end = NA)

  out <- vec_interval_locate_groups(x$start, x$end)

  expect_identical(
    out$key,
    data_frame(start = NA, end = NA)
  )
  expect_identical(
    out$loc,
    list(1L)
  )

  x <- data_frame(start = c(3, NA, 2, NA), end = c(5, NA, 3, NA))

  out <- vec_interval_locate_groups(x$start, x$end)

  expect_identical(
    out$key,
    data_frame(start = c(2, NA), end = c(5, NA))
  )
  expect_identical(
    out$loc,
    list(c(3L, 1L), c(2L, 4L)),
  )
})

test_that("missing intervals can be dropped", {
  x <- data_frame(start = NA, end = NA)

  out <- vec_interval_locate_groups(x$start, x$end, missing = "drop")

  expect_identical(
    out$key,
    data_frame(start = logical(), end = logical())
  )
  expect_identical(
    out$loc,
    list()
  )

  x <- data_frame(start = c(3, NA, 2, NA), end = c(5, NA, 3, NA))

  out <- vec_interval_locate_groups(x$start, x$end, missing = "drop")

  expect_identical(
    out$key,
    data_frame(start = 2, end = 5)
  )
  expect_identical(
    out$loc,
    list(c(3L, 1L)),
  )
})

test_that("treats NA and NaN as equivalent with doubles", {
  x <- data_frame(start = c(NA, NaN, NA, NaN), end = c(NA, NA, NaN, NaN))

  out <- vec_interval_locate_groups(x$start, x$end)

  expect_identical(
    out$key,
    data_frame(start = NA_real_, end = NaN)
  )
  expect_identical(
    out$loc,
    list(1:4),
  )

  out <- vec_interval_locate_groups(x$start, x$end, missing = "drop")

  expect_identical(
    out$key,
    data_frame(start = double(), end = double())
  )
  expect_identical(
    out$loc,
    list(),
  )
})

test_that("recognizes missing rows in data frames", {
  start <- data_frame(year = c(2019, NA, NA, 2019, 2019), month = c(12, NA, NA, 12, 12))
  end <- data_frame(year = c(2020, NA, NA, 2020, 2020), month = c(2, NA, NA, 11, 12))
  x <- data_frame(start = start, end = end)

  out <- vec_interval_locate_groups(x$start, x$end)

  expect_start <- data_frame(year = c(2019, NA), month = c(12, NA))
  expect_end <- data_frame(year = c(2020, NA), month = c(12, NA))
  expect <- data_frame(start = expect_start, end = expect_end)

  expect_identical(out$key, expect)
  expect_identical(out$loc, list(c(1L, 4L, 5L), c(2L, 3L)))
})

test_that("works on various types", {
  x <- data_frame(start = c(1.5, 2, 3.1, NA), end = c(1.7, 3.2, 4.5, NA))

  out <- vec_interval_locate_groups(x$start, x$end)
  expect_identical(out$key, data_frame(start = c(1.5, 2, NA), end = c(1.7, 4.5, NA)))
  expect_identical(out$loc, list(1L, 2:3, 4L))

  out <- vec_interval_locate_groups(x$start, x$end, missing = "drop")
  expect_identical(out$key, data_frame(start = c(1.5, 2), end = c(1.7, 4.5)))
  expect_identical(out$loc, list(1L, 2:3))

  x <- data_frame(start = c("a", "c", "f", NA), end = c("b", "g", "h", NA))

  out <- vec_interval_locate_groups(x$start, x$end)
  expect_identical(out$key, data_frame(start = c("a", "c", NA), end = c("b", "h", NA)))
  expect_identical(out$loc, list(1L, 2:3, 4L))

  out <- vec_interval_locate_groups(x$start, x$end, missing = "drop")
  expect_identical(out$key, data_frame(start = c("a", "c"), end = c("b", "h")))
  expect_identical(out$loc, list(1L, 2:3))
})

test_that("can keep abutting intervals separate", {
  # after
  x <- data_frame(start = c(1L, 2L, 0L), end = c(2L, 3L, 2L))

  out <- vec_interval_locate_groups(x$start, x$end, abutting = FALSE)

  expect_identical(out$key, data_frame(start = c(0L, 2L), end = c(2L, 3L)))
  expect_identical(out$loc, list(c(3L, 1L), 2L))

  # before
  x <- data_frame(start = c(1L, 0L), end = c(2L, 1L))

  out <- vec_interval_locate_groups(x$start, x$end, abutting = FALSE)

  expect_identical(out$key, data_frame(start = c(0L, 1L), end = c(1L, 2L)))
  expect_identical(out$loc, list(2L, 1L))

  # both
  x <- data_frame(start = c(1L, 0L, 2L), end = c(2L, 1L, 3L))

  out <- vec_interval_locate_groups(x$start, x$end, abutting = FALSE)

  expect_identical(out$key, data_frame(start = c(0L, 1L, 2L), end = c(1L, 2L, 3L)))
  expect_identical(out$loc, list(2L, 1L, 3L))
})

test_that("`missing` is validated", {
  expect_snapshot((expect_error(vec_interval_locate_groups(1, 2, missing = "s"))))
  expect_snapshot((expect_error(vec_interval_locate_groups(1, 2, missing = c("group", "drop")))))
})

test_that("common type is taken", {
  expect_snapshot((expect_error(vec_interval_locate_groups(1, "x"))))
})

# ------------------------------------------------------------------------------
# vec_interval_complement()

test_that("computes the complement", {
  x <- data_frame(
    start = c(6L, 1L, 2L, 12L),
    end = c(9L, 3L, 4L, 14L)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end),
    data_frame(start = c(4L, 9L), end = c(6L, 12L))
  )
})

test_that("treats intervals as half-open like [a, b)", {
  x <- data_frame(
    start = c(1L, 5L),
    end = c(4L, 6L)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end),
    data_frame(start = 4L, end = 5L)
  )
})

test_that("`[a, b)` and `[b, c)` result in no complement values", {
  x <- data_frame(
    start = c(1L, 5L),
    end = c(5L, 6L)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end),
    data_frame(start = integer(), end = integer())
  )
})

test_that("works with `lower == upper`", {
  x <- data_frame(
    start = c(1L, 2L, 12L, NA),
    end = c(10L, 5L, 15L, NA)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, lower = 10L, upper = 10L),
    data_frame(start = integer(), end = integer())
  )
  expect_identical(
    vec_interval_complement(x$start, x$end, lower = -1L, upper = -1L),
    data_frame(start = integer(), end = integer())
  )
  expect_identical(
    vec_interval_complement(x$start, x$end, lower = 20L, upper = 20L),
    data_frame(start = integer(), end = integer())
  )
})

test_that("works with `lower` before any values", {
  x <- data_frame(
    start = c(1L, 2L, 12L, NA),
    end = c(10L, 5L, 15L, NA)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, lower = -1L),
    data_frame(start = c(-1L, 10L), end = c(1L, 12L))
  )
})

test_that("works if both `lower` and `upper` are before any values", {
  x <- data_frame(
    start = c(2L, 1L, 12L, NA),
    end = c(5L, 10L, 15L, NA)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, lower = -5L, upper = -2L),
    data_frame(start = -5L, end = -2L)
  )
})

test_that("works with `upper` after any values", {
  x <- data_frame(
    start = c(2L, 1L, 13L, 12L, NA),
    end = c(5L, 10L, 17L, 15L, NA)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, upper = 20L),
    data_frame(start = c(10L, 17L), end = c(12L, 20L))
  )
})

test_that("works if both `lower` and `upper` are after any values", {
  x <- data_frame(
    start = c(2L, 1L, 12L, NA),
    end = c(5L, 10L, 15L, NA)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, lower = 17L, upper = 19L),
    data_frame(start = 17L, end = 19L)
  )
})

test_that("works with only NA and `lower`", {
  x <- data_frame(start = NA_integer_, end = NA_integer_)
  expect_identical(vec_interval_complement(x$start, x$end, lower = 5L), data_frame(start = integer(), end = integer()))
})

test_that("works with only NA and `upper`", {
  x <- data_frame(start = NA_integer_, end = NA_integer_)
  expect_identical(vec_interval_complement(x$start, x$end, upper = 5L), data_frame(start = integer(), end = integer()))
})

test_that("works with only NA and both `lower` and `upper`", {
  x <- data_frame(start = NA_integer_, end = NA_integer_)
  expect_identical(vec_interval_complement(x$start, x$end, lower = 2L, upper = 5L), data_frame(start = 2L, end = 5L))
})

test_that("works with `lower` that is on the max set value", {
  x <- data_frame(
    start = c(1L, 12L),
    end = c(9L, 13L)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, lower = 9L),
    data_frame(start = 9L, end = 12L)
  )
})

test_that("works with `upper` that is on the max set value", {
  x <- data_frame(
    start = c(-5L, 1L, 2L, 12L),
    end = c(0L, 10L, 5L, 15L)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, upper = 10L),
    data_frame(start = 0L, end = 1L)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, lower = 10L, upper = 10L),
    data_frame(start = integer(), end = integer())
  )
})

test_that("size zero case generally returns nothing", {
  expect_identical(
    vec_interval_complement(integer(), integer()),
    data_frame(start = integer(), end = integer())
  )

  expect_identical(
    vec_interval_complement(integer(), integer(), lower = 5L),
    data_frame(start = integer(), end = integer())
  )

  expect_identical(
    vec_interval_complement(integer(), integer(), upper = 5L),
    data_frame(start = integer(), end = integer())
  )
})

test_that("size zero case with both `lower` and `upper` returns an interval", {
  expect_identical(
    vec_interval_complement(integer(), integer(), lower = 5L, upper = 10L),
    data_frame(start = 5L, end = 10L)
  )
})

test_that("size zero case with `lower == upper` doesn't return anything", {
  expect_identical(
    vec_interval_complement(integer(), integer(), lower = 5L, upper = 5L),
    data_frame(start = integer(), end = integer())
  )
})

test_that("works when `lower` is contained in an interval", {
  expect_identical(
    vec_interval_complement(c(-5, 1, 10), c(-3, 5, 15), lower = 3),
    data_frame(start = 5, end = 10)
  )
})

test_that("works when `lower` is in a gap between intervals", {
  expect_identical(
    vec_interval_complement(c(-5, 1, 10), c(-3, 5, 15), lower = 7),
    data_frame(start = 7, end = 10)
  )
})

test_that("works when `upper` is in a gap between intervals", {
  expect_identical(
    vec_interval_complement(c(-5, 1, 10), c(-3, 5, 15), upper = 7),
    data_frame(start = c(-3, 5), end = c(1, 7))
  )
})

test_that("works when `lower` and `upper` are in a gap between intervals", {
  expect_identical(
    vec_interval_complement(c(-5, 1, 10), c(-3, 5, 15), lower = 6, upper = 7),
    data_frame(start = 6, end = 7)
  )
  expect_identical(
    vec_interval_complement(c(-5, 1, 10), c(-3, 5, 15), lower = 7, upper = 7),
    data_frame(start = double(), end = double())
  )
})

test_that("works when `lower` and `upper` have an interval between them", {
  expect_identical(
    vec_interval_complement(c(-5, 1, 10), c(-3, 5, 15), lower = 0, upper = 7),
    data_frame(start = c(0, 5), end = c(1, 7))
  )
  expect_identical(
    vec_interval_complement(c(-5, 1, 10), c(-3, 5, 15), lower = -6, upper = 7),
    data_frame(start = c(-6, -3, 5), end = c(-5, 1, 7))
  )
})

test_that("allow `lower > upper` which returns an empty interval", {
  x <- data_frame(start = c(1, 2), end = c(5, 12))
  expect_identical(
    vec_interval_complement(x$start, x$end, lower = 10, upper = 9),
    data_frame(start = double(), end = double())
  )
})

test_that("complement works when `lower` and `upper` are in the same interval", {
  x <- data_frame(start = 1, end = 5)

  expect_identical(
    vec_interval_complement(x$start, x$end, lower = 2, upper = 4),
    data_frame(start = double(), end = double())
  )
})

test_that("`lower` and `upper` can't contain missing values", {
  expect_snapshot({
    (expect_error(vec_interval_complement(1, 2, lower = NA)))
    (expect_error(vec_interval_complement(1, 2, upper = NA)))

    start <- data_frame(x = 1, y = 2)
    end <- data_frame(x = 1, y = 3)
    (expect_error(vec_interval_complement(start, end, lower = data_frame(x = 1, y = NA))))
    (expect_error(vec_interval_complement(start, end, upper = data_frame(x = 1, y = NA))))
  })
})
