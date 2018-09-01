context("test-data")

test_that("strips vector attributes apart from names", {
  x <- new_vctr(1:10, a = 1, b = 2)
  expect_equal(vec_data(x), 1:10)

  x <- new_vctr(c(x = 1, y = 2), a = 1, b = 2)
  expect_equal(vec_data(x), c(x = 1, y = 2))
})
