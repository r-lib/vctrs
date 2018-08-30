context("test-record")


# constructor and accessors -----------------------------------------------

test_that("can construct and access components", {
  r <- new_record(list(x = 1, y = 2))

  expect_equal(length(r), 1)
  expect_equal(n_fields(r), 2)

  expect_equal(names(r), NULL)
  expect_equal(fields(r), c("x", "y"))

  expect_error(r$x, class = "error_unsupported")
  expect_equal(field(r, "x"), 1)
})

# invalid inputs ------------------------------------------------------------

test_that("must be list of equal length vectors", {
  expect_error(new_record(list()), "list of length 1")
  expect_error(new_record(list(x = environment())), "vector")
  expect_error(new_record(list(x = 1, y = 1:2)), "same length")
})

test_that("names must be unique", {
  expect_error(new_record(list(1, 2)), "unique names")
  expect_error(new_record(list(x = 1, 2)), "unique names")
  expect_error(new_record(list(x = 1, x = 2)), "unique names")
  expect_error(new_record(setNames(list(1, 2), "x")), "unique names")
})

test_that("no attributes", {
  x <- structure(list(x = 1:3), y = 1)
  expect_error(new_record(x), "no attributes")
})
