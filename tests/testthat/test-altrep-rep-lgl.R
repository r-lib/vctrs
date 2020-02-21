context("test-altrep-rep-lgl")

skip_if(missing_altrep_3_6(), "Testing `vctrs_compact_rep_lgl` requires R 3.6+")

test_that("No_NA method keeps compact vectors from being expanded", {
  x <- new_altrep_vctrs_compact_rep_lgl(TRUE, 2)
  y <- new_altrep_vctrs_compact_rep_lgl(NA, 2)

  expect_identical(is.na(x), c(FALSE, FALSE))
  expect_identical(is.na(y), c(TRUE, TRUE))

  expect_true(is_altrep_vctrs_compact_rep_compact(x))
  expect_true(is_altrep_vctrs_compact_rep_compact(y))
})
