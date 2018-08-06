context("test-coerce")


test_that("type is required when vctrs.no_guessing is TRUE", {
  old <- options(vctrs.no_guessing = TRUE)
  on.exit(options(old))

  expect_error(vec_c(1), "strict mode is activated")
})

# vec_coerce --------------------------------------------------------------

test_that("empty input returns list()", {
  expect_equal(vec_coerce(), list())
})

test_that("output has consistent type", {
  expect_equal(vec_coerce(TRUE, 1), list_of(1, 1))
})
