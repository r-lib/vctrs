context("test-cast-list")

test_that("silently extracts elements of length 1", {
  expect_equal(vec_list_cast(list(1, 2), double()), c(1, 2))
})

test_that("elements of length 0 become NA with warning", {
  x <- list(1, double())
  out <- expect_warning(vec_list_cast(x, double()), class = "warn_lossy_cast")
  expect_equal(out, c(1, NA))
})

test_that("elements of length >1 are truncated with warning", {
  x <- list(1, c(2, 1), c(3, 2, 1))
  out <- expect_warning(vec_list_cast(x, double()), class = "warn_lossy_cast")
  expect_equal(out, c(1, 2, 3))
})
