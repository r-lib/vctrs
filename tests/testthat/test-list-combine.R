test_that("basic `list_combine()` works", {
  values <- list(1:2, 3:4)
  indices <- list(c(4, 1), c(3, 2))

  expect_identical(
    list_combine(values, indices),
    c(2L, 4L, 3L, 1L)
  )
})

test_that("`indices` is required!", {
  expect_error(list_combine(list(1, 2)))
})

test_that("`x_arg` works", {
  expect_snapshot(error = TRUE, {
    list_combine(list(1, "2"), list(1, 2), x_arg = "xs")
  })
  expect_snapshot(error = TRUE, {
    list_combine(list(1, 2), list(1, 2, 3), x_arg = "xs")
  })
})

test_that("`indices_arg` works", {
  expect_snapshot(error = TRUE, {
    list_combine(list(1, 2), 1, indices_arg = "i")
  })
  expect_snapshot(error = TRUE, {
    list_combine(list(1, 2), list(1, 2, 3), indices_arg = "i")
  })
})

test_that("`...` must be empty", {
  expect_snapshot(error = TRUE, {
    list_combine(list(1, 2), list(1, 2), "foo")
  })
})
