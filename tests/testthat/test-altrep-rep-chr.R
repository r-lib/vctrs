context("test-altrep-rep-chr")

skip_if(missing_altrep(), "Testing `vctrs_compact_rep_chr` requires R 3.5+")

test_that("recycling named vectors generates ALTREP names which can still be repaired", {
  expect_named(vec_recycle(c(x = 1), 2), c("x", "x"))
})
