context("test-shape-coerce")

test_that("NULLs are idempotent", {
  expect_equal(vecshape_coerce(NULL, vec_shape(1, 1)), NULL)
  expect_equal(vecshape_coerce(ones(1, 2), NULL), ones(1, 2))
})

test_that("vector recycling special cases are correct", {
  expect_equal(vecshape_coerce(1, vec_shape(5)), rep(1, 5))
  expect_equal(vecshape_coerce(1, vec_shape(5, 5)), array(1, c(5, 5)))
})
