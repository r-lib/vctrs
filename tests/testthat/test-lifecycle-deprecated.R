
test_that("vec_as_index() still works", {
  local_lifecycle_silence()
  expect_identical(vec_as_index(-2, 10), vec_as_location(-2, 10))
  expect_identical(
    vec_as_index("cyl", length(mtcars), names(mtcars)),
    vec_as_location("cyl", length(mtcars), names(mtcars))
  )
})
