context("test-prototype")


test_that(".ptype argument overrides others", {
  expect_equal(vec_type_common(.ptype = 1:10), numeric())
})

test_that(".ptype required in strict mode", {
  old <- options(vctrs.no_guessing = TRUE)
  on.exit(options(old))

  expect_error(vec_type_common(), "strict mode")
})

test_that("can feed ptype into itself", {
  expect_equal(vec_type_common(vec_type_common(1:10)), numeric())
})

test_that("unspecified prototypes created from under specified inputs", {
  expect_equal(vec_type_common(), NULL)
  expect_equal(vec_type_common(NULL), NULL)

  expect_equal(vec_type_common(NA), logical())
  expect_equal(vec_type_common(NA, NULL), logical())
  expect_equal(vec_type_common(NULL, NA), logical())
})

test_that("unspecified prototypes created from data frame cols", {
  df <- data.frame(x = NA)
  expect_equal(vec_type_common(df)$x, unspecified())
})

test_that("non-missing logical get correct type", {
  expect_equal(vec_type_common(TRUE), logical())
})

