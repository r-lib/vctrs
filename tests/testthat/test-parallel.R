test_that("9 possible variations of each combination are right", {
  N <- NA

  expect_identical(list_pall(list(T, T), missing = NULL), T)
  expect_identical(list_pall(list(T, F), missing = NULL), F)
  expect_identical(list_pall(list(T, N), missing = NULL), N)
  expect_identical(list_pall(list(F, T), missing = NULL), F)
  expect_identical(list_pall(list(F, F), missing = NULL), F)
  expect_identical(list_pall(list(F, N), missing = NULL), F)
  expect_identical(list_pall(list(N, T), missing = NULL), N)
  expect_identical(list_pall(list(N, F), missing = NULL), F)
  expect_identical(list_pall(list(N, N), missing = NULL), N)

  expect_identical(list_pall(list(T, T), missing = TRUE), T)
  expect_identical(list_pall(list(T, F), missing = TRUE), F)
  expect_identical(list_pall(list(T, N), missing = TRUE), T)
  expect_identical(list_pall(list(F, T), missing = TRUE), F)
  expect_identical(list_pall(list(F, F), missing = TRUE), F)
  expect_identical(list_pall(list(F, N), missing = TRUE), F)
  expect_identical(list_pall(list(N, T), missing = TRUE), T)
  expect_identical(list_pall(list(N, F), missing = TRUE), F)
  expect_identical(list_pall(list(N, N), missing = TRUE), T)

  expect_identical(list_pall(list(T, T), missing = FALSE), T)
  expect_identical(list_pall(list(T, F), missing = FALSE), F)
  expect_identical(list_pall(list(T, N), missing = FALSE), F)
  expect_identical(list_pall(list(F, T), missing = FALSE), F)
  expect_identical(list_pall(list(F, F), missing = FALSE), F)
  expect_identical(list_pall(list(F, N), missing = FALSE), F)
  expect_identical(list_pall(list(N, T), missing = FALSE), F)
  expect_identical(list_pall(list(N, F), missing = FALSE), F)
  expect_identical(list_pall(list(N, N), missing = FALSE), F)

  expect_identical(list_pany(list(T, T), missing = NULL), T)
  expect_identical(list_pany(list(T, F), missing = NULL), T)
  expect_identical(list_pany(list(T, N), missing = NULL), T)
  expect_identical(list_pany(list(F, T), missing = NULL), T)
  expect_identical(list_pany(list(F, F), missing = NULL), F)
  expect_identical(list_pany(list(F, N), missing = NULL), N)
  expect_identical(list_pany(list(N, T), missing = NULL), T)
  expect_identical(list_pany(list(N, F), missing = NULL), N)
  expect_identical(list_pany(list(N, N), missing = NULL), N)

  expect_identical(list_pany(list(T, T), missing = TRUE), T)
  expect_identical(list_pany(list(T, F), missing = TRUE), T)
  expect_identical(list_pany(list(T, N), missing = TRUE), T)
  expect_identical(list_pany(list(F, T), missing = TRUE), T)
  expect_identical(list_pany(list(F, F), missing = TRUE), F)
  expect_identical(list_pany(list(F, N), missing = TRUE), T)
  expect_identical(list_pany(list(N, T), missing = TRUE), T)
  expect_identical(list_pany(list(N, F), missing = TRUE), T)
  expect_identical(list_pany(list(N, N), missing = TRUE), T)

  expect_identical(list_pany(list(T, T), missing = FALSE), T)
  expect_identical(list_pany(list(T, F), missing = FALSE), T)
  expect_identical(list_pany(list(T, N), missing = FALSE), T)
  expect_identical(list_pany(list(F, T), missing = FALSE), T)
  expect_identical(list_pany(list(F, F), missing = FALSE), F)
  expect_identical(list_pany(list(F, N), missing = FALSE), F)
  expect_identical(list_pany(list(N, T), missing = FALSE), T)
  expect_identical(list_pany(list(N, F), missing = FALSE), F)
  expect_identical(list_pany(list(N, N), missing = FALSE), F)
})

test_that("works with empty inputs", {
  expect_identical(list_pall(list(logical(), logical())), logical())
  expect_identical(list_pany(list(logical(), logical())), logical())
})

test_that("works with no inputs", {
  expect_identical(list_pall(list()), logical())
  expect_identical(list_pany(list()), logical())
})

test_that("works with no inputs and specified `.size`", {
  expect_identical(list_pall(list(), size = 3), c(TRUE, TRUE, TRUE))
  expect_identical(list_pany(list(), size = 3), c(FALSE, FALSE, FALSE))
})

test_that("no casting is done", {
  expect_snapshot(error = TRUE, {
    list_pall(list(1))
  })
  expect_snapshot(error = TRUE, {
    list_pany(list(1))
  })

  # Arrays
  expect_snapshot(error = TRUE, {
    list_pall(list(array(TRUE)))
  })
  expect_snapshot(error = TRUE, {
    list_pany(list(array(TRUE)))
  })

  # Class
  expect_snapshot(error = TRUE, {
    list_pall(list(structure(TRUE, class = "foo")))
  })
  expect_snapshot(error = TRUE, {
    list_pany(list(structure(TRUE, class = "foo")))
  })
})

test_that("no recycling is done", {
  expect_snapshot(error = TRUE, {
    list_pall(list(TRUE, c(TRUE, TRUE, TRUE)))
  })
  expect_snapshot(error = TRUE, {
    list_pany(list(TRUE, c(TRUE, TRUE, TRUE)))
  })

  # With `size`
  expect_snapshot(error = TRUE, {
    list_pall(list(TRUE, c(TRUE, TRUE, TRUE)), size = 3L)
  })
  expect_snapshot(error = TRUE, {
    list_pany(list(TRUE, c(TRUE, TRUE, TRUE)), size = 3L)
  })
})

test_that("validates `x`", {
  expect_snapshot(error = TRUE, list_pall(1))
  expect_snapshot(error = TRUE, list_pany(1))

  expect_snapshot(error = TRUE, list_pall(1, x_arg = ""))
  expect_snapshot(error = TRUE, list_pany(1, x_arg = ""))

  expect_snapshot(error = TRUE, list_pall(1, x_arg = "foo"))
  expect_snapshot(error = TRUE, list_pany(1, x_arg = "foo"))

  expect_snapshot(error = TRUE, list_pall(data_frame(x = 1)))
  expect_snapshot(error = TRUE, list_pany(data_frame(x = 1)))

  expect_identical(list_pall(list_of(TRUE)), TRUE)
  expect_identical(list_pany(list_of(TRUE)), TRUE)
})

test_that("validates `missing`", {
  expect_snapshot(error = TRUE, list_pall(list(), missing = c(TRUE, FALSE)))
  expect_snapshot(error = TRUE, list_pany(list(), missing = c(TRUE, FALSE)))

  expect_snapshot(error = TRUE, list_pall(list(), missing = 1))
  expect_snapshot(error = TRUE, list_pany(list(), missing = 1))

  expect_snapshot(error = TRUE, list_pall(list(), missing = NA))
  expect_snapshot(error = TRUE, list_pany(list(), missing = NA))
})

test_that("validates `size`", {
  expect_snapshot(error = TRUE, list_pall(list(), size = c(1, 2)))
  expect_snapshot(error = TRUE, list_pany(list(), size = c(1, 2)))

  expect_snapshot(error = TRUE, list_pall(list(), size = 1.5))
  expect_snapshot(error = TRUE, list_pany(list(), size = 1.5))

  expect_snapshot(error = TRUE, list_pall(list(), size = NA_integer_))
  expect_snapshot(error = TRUE, list_pany(list(), size = NA_integer_))
})

test_that("names are used in errors", {
  expect_snapshot(error = TRUE, {
    foo <- list(x = 1.5)
    list_pall(foo)
  })
  expect_snapshot(error = TRUE, {
    foo <- list(x = c(TRUE, FALSE), y = logical())
    list_pany(foo)
  })
})
