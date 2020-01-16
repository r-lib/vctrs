
test_that("vec_as_index() still works", {
  local_lifecycle_silence()
  expect_identical(vec_as_index(-2, 10), vec_as_location(-2, 10))
  expect_identical(
    vec_as_index("cyl", length(mtcars), names(mtcars)),
    vec_as_location("cyl", length(mtcars), names(mtcars))
  )
})

test_that("vec_duplicate_id() still works", {
  local_lifecycle_silence()
  expect_identical(vec_duplicate_id(c(1, 1, 3)), vec_first_loc(c(1, 1, 3)))
})
