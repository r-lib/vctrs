context("test-type-date-time")

test_that("raw have informative types", {
  expect_equal(vec_ptype_abbr(raw()), "raw")
  expect_equal(vec_ptype_full(raw()), "raw")
})
