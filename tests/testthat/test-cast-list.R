context("test-cast-list")

test_that("silently extracts elements of length 1", {
  expect_equal(vec_list_cast(list(1, 2), double()), c(1, 2))
})

test_that("elements of length 0 become NA without error", {
  x <- list(1, double())
  out <- vec_list_cast(x, double())
  expect_equal(out, c(1, NA))
})

test_that("elements of length >1 are truncated with error", {
  x <- list(1, c(2, 1), c(3, 2, 1))
  expect_lossy(vec_list_cast(x, dbl()), dbl(1, 2, 3), x = list(), to = dbl())

  x <- list(c(2, 1), c(3, 2, 1))
  expect_lossy(vec_list_cast(x, dbl()), dbl(2, 3), x = list(), to = dbl())
})
