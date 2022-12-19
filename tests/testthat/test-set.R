# vec_set_intersect -------------------------------------------------------

test_that("retains names of `x` elements", {
  x <- c(a = 1, b = 4, c = 1, d = 4, e = 2)
  y <- c(w = 3, x = 2, y = 1, z = 2)

  expect_identical(
    vec_set_intersect(x, y),
    c(a = 1, e = 2)
  )
})

test_that("returns elements in order they first appear in `x`", {
  expect_identical(vec_set_intersect(c(3, 1, 2, 3), c(2, 3)), c(3, 2))
})

test_that("returns unique elements", {
  expect_identical(vec_set_intersect(c(1, 2, 1), c(2, 2, 1)), c(1, 2))
})

test_that("has ordering / comparison consistency with `NA` values", {
  expect_identical(vec_set_intersect(c(NA_real_, 1), NA_real_), NA_real_)
  expect_identical(vec_set_intersect(c(1, NA_real_), NA_real_), NA_real_)

  expect_identical(vec_set_intersect(c(NA_real_, NaN), NaN), NaN)
  expect_identical(vec_set_intersect(c(NaN, NA_real_), NaN), NaN)
})

test_that("works correctly with unspecified logical vectors", {
  expect_identical(vec_set_intersect(NA, NA), NA)
  expect_identical(vec_set_intersect(NA, NA, ptype = NA), NA)
})

test_that("returns a vector of the common type", {
  expect_identical(vec_set_intersect(1L, c(2, 1)), 1)
})

test_that("errors nicely if common type can't be taken", {
  expect_snapshot(error = TRUE, {
    vec_set_intersect(1, "x")
  })
})

test_that("dots must be empty", {
  expect_snapshot(error = TRUE, {
    vec_set_intersect(1, 2, 3)
  })
})

test_that("`ptype` is respected", {
  expect_identical(vec_set_intersect(1, 1, ptype = integer()), 1L)

  expect_snapshot(error = TRUE, {
    vec_set_intersect(1, 1.5, ptype = integer())
  })
})

test_that("`x_arg` and `y_arg` can be adjusted", {
  expect_snapshot(error = TRUE, {
    vec_set_intersect(1, "2", x_arg = "foo", y_arg = "bar")
  })
  expect_snapshot(error = TRUE, {
    vec_set_intersect(1, "2", x_arg = "", y_arg = "")
  })
})

test_that("`error_call` can be adjusted", {
  my_set_intersect <- function() {
    vec_set_intersect(1, "x", error_call = current_env())
  }

  expect_snapshot(error = TRUE, {
    my_set_intersect()
  })
})
