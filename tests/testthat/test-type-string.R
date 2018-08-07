context("test-type-string")

test_that("non objects default to type + shape", {
  expect_equal(vec_type_string(ones(10)), "double")
  expect_equal(vec_type_string(ones(10, 10)), "double[,10]")
})

test_that("objects default to first class", {
  x <- structure(1, class = "foofy")
  expect_equal(vec_type_string(x), "foofy")
})
