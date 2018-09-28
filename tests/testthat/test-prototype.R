context("test-prototype")


test_that(".ptype argument overrides others", {
  expect_equal(vec_ptype(.ptype = 1:10)[[1]], numeric())
})

test_that(".ptype required in strict mode", {
  old <- options(vctrs.no_guessing = TRUE)
  on.exit(options(old))

  expect_error(vec_ptype(), "strict mode")
})

test_that("can feed ptype into itself", {
  expect_equal(vec_ptype(vec_ptype(1:10))[[1]], numeric())
})

test_that("unspecified prototypes created from under specified inputs", {
  expect_equal(vec_ptype()[[1]], NULL)
  expect_equal(vec_ptype(NULL)[[1]], NULL)

  expect_equal(vec_ptype(NA)[[1]], logical())
  expect_equal(vec_ptype(NA, NULL)[[1]], logical())
  expect_equal(vec_ptype(NULL, NA)[[1]], logical())
})

test_that("unspecified prototypes created from data frame cols", {
  df <- data.frame(x = NA)
  expect_equal(vec_ptype(df)[[1]]$x, unspecified())
})

test_that("non-missing logical get correct type", {
  expect_equal(vec_ptype(TRUE)[[1]], logical())
})

