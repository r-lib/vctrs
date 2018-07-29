context("test-shape")

test_that("can't coerce non-vectors", {
  expect_error(as_vec_shape(environment()), "vector")
})

test_that("has useful print method", {
  expect_known_output(
    file = test_path("test-shape-print.txt"),
    {
      print(vec_shape(10))
      print(vec_shape(1, 2, 3, 4, 5))
    }
  )
})
