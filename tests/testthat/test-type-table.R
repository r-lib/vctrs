context("test-type-table")

# Print -------------------------------------------------------------------

test_that("ptype print methods are descriptive", {
  tab1 <- new_table()
  tab2 <- new_table(dim = c(0L, 1L, 2L, 1L))

  expect_equal(vec_ptype_abbr(tab1), "table")
  expect_equal(vec_ptype_abbr(tab2), "table")

  expect_equal(vec_ptype_full(tab1), "table")
  expect_equal(vec_ptype_full(tab2), "table[,1,2,1]")
})

# Coercion ----------------------------------------------------------------

test_that("can find a common type among tables with identical dimensions", {
  tab1 <- new_table()
  tab2 <- new_table(1:2, dim = c(1L, 2L, 1L))

  expect_identical(vec_ptype2(tab1, tab1), new_table())
  expect_identical(vec_ptype2(tab2, tab2), new_table(dim = c(0L, 2L, 1L)))
})

test_that("size is not considered in the ptype", {
  x <- new_table(1:2, dim = 2L)
  y <- new_table(1:3, dim = 3L)

  expect_identical(vec_ptype2(x, y), new_table())
})

test_that("can broadcast table shapes", {
  x <- new_table(dim = c(0L, 1L))
  y <- new_table(dim = c(0L, 2L))

  expect_identical(vec_ptype2(x, y), new_table(dim = c(0L, 2L)))

  x <- new_table(dim = c(0L, 1L, 3L))
  y <- new_table(dim = c(0L, 2L, 1L))

  expect_identical(vec_ptype2(x, y), new_table(dim = c(0L, 2L, 3L)))
})

test_that("implicit axes are broadcast", {
  x <- new_table(dim = c(0L, 2L))
  y <- new_table(dim = c(0L, 1L, 3L))

  expect_identical(vec_ptype2(x, y), new_table(dim = c(0L, 2L, 3L)))
})

test_that("errors on non-broadcastable dimensions", {
  x <- new_table(dim = c(0L, 2L))
  y <- new_table(dim = c(0L, 3L))

  expect_error(vec_ptype2(x, y), class = "vctrs_error_incompatible_type")
})

test_that("errors on non-tables", {
  expect_error(vec_ptype2(new_table(), 1), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(new_table(), 1L), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(new_table(), "1"), class = "vctrs_error_incompatible_type")

  expect_error(vec_ptype2(1, new_table()), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(1L, new_table()), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2("1", new_table()), class = "vctrs_error_incompatible_type")
})

test_that("inheritance is not allowed", {
  x <- new_table()

  y <- x
  class(y) <- c("footable", class(x))

  expect_error(vec_ptype2(x, y), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(y, x), class = "vctrs_error_incompatible_type")
})

test_that("common types have symmetry when mixed with unspecified input", {
  x <- new_table()

  expect_identical(vec_ptype2(x, NA), new_table())
  expect_identical(vec_ptype2(NA, x), new_table())

  x <- new_table(dim = c(0L, 2L))

  expect_identical(vec_ptype2(x, NA), new_table(dim = c(0L, 2L)))
  expect_identical(vec_ptype2(NA, x), new_table(dim = c(0L, 2L)))
})

# Casting -----------------------------------------------------------------

test_that("can cast to an identically shaped table", {
  x <- new_table(1:5, dim = 5L)
  y <- new_table(1:8, dim = c(2L, 2L, 2L))

  expect_identical(vec_cast(x, x), x)
  expect_identical(vec_cast(y, y), y)
})

test_that("can broadcast table shapes", {
  # We test only the dim here and not the class because on R 3.2
  # the `[.table` method did not exist and `shape_broadcast()`
  # gives back a matrix, not a table.

  x <- new_table(dim = c(0L, 1L))
  y <- new_table(dim = c(0L, 2L))

  expect_identical(dim(vec_cast(x, y)), c(0L, 2L))

  x <- new_table(dim = c(0L, 1L, 1L))
  y <- new_table(dim = c(0L, 2L, 3L))

  expect_identical(dim(vec_cast(x, y)), c(0L, 2L, 3L))
})

test_that("cannot decrease axis length", {
  x <- new_table(dim = c(0L, 3L))
  y <- new_table(dim = c(0L, 1L))

  expect_error(vec_cast(x, y), "Non-recyclable", class = "vctrs_error_incompatible_type")
})

test_that("cannot decrease dimensionality", {
  x <- new_table(dim = c(0L, 1L, 1L))
  y <- new_table(dim = c(0L, 1L))

  expect_error(vec_cast(x, y), "decrease dimensions", class = "vctrs_error_incompatible_type")
})

test_that("errors on non-tables", {
  expect_error(vec_cast(new_table(), 1), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(new_table(), 1L), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(new_table(), "1"), class = "vctrs_error_incompatible_type")

  expect_error(vec_cast(1, new_table()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(1L, new_table()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast("1", new_table()), class = "vctrs_error_incompatible_type")
})

test_that("inheritance is not allowed", {
  x <- new_table()

  y <- x
  class(y) <- c("footable", class(x))

  expect_error(vec_cast(x, y), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(y, x), class = "vctrs_error_incompatible_type")
})

test_that("can cast from, but not to, unspecified", {
  x <- new_table()

  expect_error(vec_cast(x, NA), class = "vctrs_error_incompatible_type")
  expect_identical(vec_cast(NA, x), new_table(NA_integer_, dim = 1L))

  x <- new_table(dim = c(0L, 2L))

  expect_error(vec_cast(x, NA), class = "vctrs_error_incompatible_type")
  expect_identical(vec_cast(NA, x), new_table(c(NA_integer_, NA_integer_), dim = c(1L, 2L)))
})

# Misc --------------------------------------------------------------------

test_that("`new_table()` validates input", {
  expect_error(new_table(1), "`x` must be an integer vector")
  expect_error(new_table(1L, 1), "`dim` must be an integer vector")
  expect_error(new_table(1:2, 1L), "must match the length of `x`")
})

test_that("ptype is correct", {
  tab1 <- new_table(1L, dim = 1L)
  tab2 <- new_table(1:2, dim = c(1L, 2L, 1L))

  expect_identical(vec_ptype(tab1), new_table())
  expect_identical(vec_ptype(tab2), new_table(dim = c(0L, 2L, 1L)))
})

test_that("can use a table in `vec_c()`", {
  expect_identical(vec_c(new_table()), new_table())
  expect_identical(vec_c(new_table(), new_table()), new_table())

  x <- new_table(1:5, 5L)
  y <- new_table(1:4, dim = c(2L, 2L))

  expect_identical(vec_c(x, x), new_table(c(1:5, 1:5), dim = 10L))
  expect_identical(vec_c(y, y), new_table(c(1:2, 1:2, 3:4, 3:4), dim = c(4L, 2L)))
  expect_identical(vec_c(x, y), new_table(c(1:5, 1:2, 1:5, 3:4), dim = c(7L, 2L)))
})

test_that("names of the first dimension are kept in `vec_c()`", {
  x <- new_table(1:4, c(2L, 2L))
  dimnames(x) <- list(c("r1", "r2"), c("c1", "c2"))

  xx <- vec_c(x, x)

  expect_identical(dimnames(xx), list(c("r1", "r2", "r1", "r2"), NULL))
})

test_that("can use a table in `vec_unchop()`", {
  x <- new_table(1:4, dim = c(2L, 2L))

  expect_identical(vec_unchop(list(x)), x)
  expect_identical(vec_unchop(list(x, x), list(1:2, 4:3)), vec_slice(x, c(1:2, 2:1)))
})
