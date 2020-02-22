context("test-altrep-rep")
skip_if_no_altrep()

test_that("No_NA method keeps compact vectors from being expanded", {
  test <- function(xs, nas, fns) {
    for (i in seq_along(xs)) {
      x <- xs[[i]]
      na <- nas[[i]]
      fn <- fns[[i]]

      x <- fn(x, 2)
      na <- fn(na, 2)

      expect_identical(is.na(x), c(FALSE, FALSE))
      expect_identical(is.na(na), c(TRUE, TRUE))

      expect_true(is_altrep_vctrs_compact_rep_compact(x))
      expect_true(is_altrep_vctrs_compact_rep_compact(na))
    }
  }

  xs <- list(1L, 1, "1")
  nas <- list(NA_integer_, NA_real_, NA_character_)

  fns <- list(
    new_altrep_vctrs_compact_rep_int,
    new_altrep_vctrs_compact_rep_dbl,
    new_altrep_vctrs_compact_rep_chr
  )

  test(xs, nas, fns)

  skip_if_no_altrep_3_6()

  xs <- list(TRUE)
  nas <- list(NA)

  fns <- list(
    new_altrep_vctrs_compact_rep_lgl
  )

  test(xs, nas, fns)
})

test_that("vec_ptype2 doesn't expand compact reps", {
  test <- function(xs, fns) {
    for (i in seq_along(xs)) {
      x <- xs[[i]]
      fn <- fns[[i]]

      x <- fn(x, 1)

      vec_ptype2(x, x)

      expect_true(is_altrep_vctrs_compact_rep_compact(x))
    }
  }

  xs <- list(1L, 1, "1")

  fns <- list(
    new_altrep_vctrs_compact_rep_int,
    new_altrep_vctrs_compact_rep_dbl,
    new_altrep_vctrs_compact_rep_chr
  )

  test(xs, fns)

  skip_if_no_altrep_3_6()

  xs <- list(TRUE)
  nas <- list(NA)

  fns <- list(
    new_altrep_vctrs_compact_rep_lgl
  )

  test(xs, fns)
})

# ------------------------------------------------------------------------------
context("test-altrep-rep-chr")

test_that("recycling named vectors generates ALTREP names which can still be repaired", {
  expect_named(vec_recycle(c(x = 1), 2), c("x", "x"))
})

# ------------------------------------------------------------------------------
context("test-altrep-rep-lgl")

test_that("`is_unspecified()` does not expand ALTREP compact rep lgls", {
  skip_if_no_altrep_3_6()

  x <- new_altrep_vctrs_compact_rep_lgl(TRUE, 2)
  y <- new_altrep_vctrs_compact_rep_lgl(NA_integer_, 2)

  expect_identical(is_unspecified(x), FALSE)
  expect_identical(is_unspecified(y), TRUE)

  expect_true(is_altrep_vctrs_compact_rep_compact(x))
  expect_true(is_altrep_vctrs_compact_rep_compact(y))
})

# ------------------------------------------------------------------------------
context("test-altrep-rep-dbl")

# ------------------------------------------------------------------------------
context("test-altrep-rep-int")
