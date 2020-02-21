context("test-altrep-rep-dbl")

skip_if(missing_altrep(), "Testing `vctrs_compact_rep_dbl` requires R 3.5+")

test_that("No_NA method keeps compact vectors from being expanded", {
  x <- new_altrep_vctrs_compact_rep_dbl(1, 2)
  y <- new_altrep_vctrs_compact_rep_dbl(NA_real_, 2)

  expect_identical(is.na(x), c(FALSE, FALSE))
  expect_identical(is.na(y), c(TRUE, TRUE))

  expect_true(is_altrep_vctrs_compact_rep_compact(x))
  expect_true(is_altrep_vctrs_compact_rep_compact(y))
})
