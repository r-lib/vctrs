context("coerce")

test_that("combine logical NA", {
  expect_equal(combine(NA, 1L), c(NA, 1L))
  expect_equal(combine(NA, 1), c(NA, 1))
  expect_equal(combine(1L, NA), c(1L, NA))
  expect_equal(combine(1, NA), c(1, NA))
})

test_that("combine integer and double", {
  expect_equal(combine(1, 2L), c(1, 2))
  expect_equal(combine(1L, 2), c(1, 2))
})
