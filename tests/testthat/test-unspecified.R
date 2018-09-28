context("test-unspecified")

test_that("unknown type is idempotent", {
  types <- list(
    unspecified(),
    logical(),
    integer(),
    double(),
    character(),
    new_factor(),
    new_ordered(),
    new_date(),
    new_datetime()
  )

  lhs <- map(types, vec_type2, x = unspecified())
  expect_equal(types, lhs)

  rhs <- map(types, vec_type2, y = unspecified())
  expect_equal(types, rhs)
})

test_that("has useful print method", {
  expect_known_output(unspecified(), print = TRUE, file = test_path("test-unspecified.txt"))
})
