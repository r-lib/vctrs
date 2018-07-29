context("test-coerce")


# vec_c -------------------------------------------------------------------

test_that("all inputs must be 1d", {
  expect_error(vec_c(mtcars))
})
