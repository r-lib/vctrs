
test_that("vec_as_index() still works", {
  local_lifecycle_silence()
  expect_identical(vec_as_index(-2, 10), vec_as_location(-2, 10))
  expect_identical(
    vec_as_index("cyl", length(mtcars), names(mtcars)),
    vec_as_location("cyl", length(mtcars), names(mtcars))
  )
})

test_that("vec_repeat() still works", {
  local_lifecycle_silence()
  expect_identical(vec_repeat(1:2, times = 2), vec_rep(1:2, 2))
  expect_identical(vec_repeat(1:2, each = 2), vec_rep_each(1:2, 2))
})
