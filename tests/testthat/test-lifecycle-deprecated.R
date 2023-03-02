
test_that("vec_as_index() still works", {
  local_options(lifecycle_verbosity = "quiet")
  expect_identical(vec_as_index(-2, 10), vec_as_location(-2, 10))
  expect_identical(
    vec_as_index("cyl", length(mtcars), names(mtcars)),
    vec_as_location("cyl", length(mtcars), names(mtcars))
  )
})

test_that("vec_repeat() still works", {
  local_options(lifecycle_verbosity = "quiet")
  expect_identical(vec_repeat(1:2, times = 2), vec_rep(1:2, 2))
  expect_identical(vec_repeat(1:2, each = 2), vec_rep_each(1:2, 2))
})

test_that("vec_unchop() is soft-deprecated", {
  local_options(lifecycle_verbosity = "warning")
  expect_snapshot(vec_unchop(list(1), indices = list(1)))
})

test_that("vec_unchop() still works", {
  local_options(lifecycle_verbosity = "quiet")
  expect_identical(
    vec_unchop(list(1L, 2:3), indices = list(2, c(3, 1))),
    c(3L, 1L, 2L)
  )
})

test_that("vec_equal_na() is soft-deprecated", {
  local_options(lifecycle_verbosity = "warning")
  expect_snapshot(vec_equal_na(c(1, NA)))
})

test_that("vec_equal_na() still works", {
  local_options(lifecycle_verbosity = "quiet")
  expect_identical(
    vec_equal_na(c(1, NA, 2, NA)),
    c(FALSE, TRUE, FALSE, TRUE)
  )
})

test_that("vec_is_list() still works", {
  expect_false(vec_is_list(1))
  expect_true(vec_is_list(list()))
})
