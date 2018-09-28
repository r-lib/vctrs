context("test-unknown")

test_that("unknown type is idempotent", {
  types <- list(
    unknown(),
    logical(),
    integer(),
    double(),
    character(),
    new_factor(),
    new_ordered(),
    new_date(),
    new_datetime()
  )

  lhs <- map(types, vec_type2, x = unknown())
  expect_equal(types, lhs)

  rhs <- map(types, vec_type2, y = unknown())
  expect_equal(types, rhs)
})

test_that("has useful print method", {
  expect_known_output(unknown(), print = TRUE, file = test_path("test-unknown.txt"))
})
