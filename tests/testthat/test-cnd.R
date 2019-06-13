test_that("names_cannot_be_empty", {
  expect_cnd_equal(
    names_cannot_be_empty(1:3),
    .subclass = c("vctrs_error_names_cannot_be_empty", "vctrs_error_names", "vctrs_error"),
    locations = 1:3,
    message = "Names must not be empty."
  )
})

test_that("names_cannot_be_dot_dot", {
  expect_cnd_equal(
    names_cannot_be_dot_dot(1:3),
    .subclass = c("vctrs_error_names_cannot_be_dot_dot", "vctrs_error_names", "vctrs_error"),
    locations = 1:3,
    message = "Names must not be of the form `...` or `..j`."
  )
})

test_that("names_must_be_unique", {
  expect_cnd_equal(
    names_must_be_unique(1:3),
    .subclass = c("vctrs_error_names_must_be_unique", "vctrs_error_names", "vctrs_error"),
    locations = 1:3,
    message = "Names must be unique."
  )
})
