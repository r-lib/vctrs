context("test-altrep-rep-chr")

skip_if(missing_altrep(), "Testing `vctrs_compact_rep_chr` requires R 3.5+")

test_that("No_NA method keeps compact vectors from being expanded", {
  x <- new_altrep_vctrs_compact_rep_chr("x", 2)
  y <- new_altrep_vctrs_compact_rep_chr(NA_character_, 2)

  expect_identical(is.na(x), c(FALSE, FALSE))
  expect_identical(is.na(y), c(TRUE, TRUE))

  expect_true(is_altrep_vctrs_compact_rep_compact(x))
  expect_true(is_altrep_vctrs_compact_rep_compact(y))
})

test_that("recycling named vectors generates ALTREP names which can still be repaired", {
  expect_named(vec_recycle(c(x = 1), 2), c("x", "x"))
})
