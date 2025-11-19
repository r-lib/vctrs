test_that("9 possible variations of each combination are right", {
  N <- NA

  expect_identical(vec_pall(T, T, .missing = NA), T)
  expect_identical(vec_pall(T, F, .missing = NA), F)
  expect_identical(vec_pall(T, N, .missing = NA), N)
  expect_identical(vec_pall(F, T, .missing = NA), F)
  expect_identical(vec_pall(F, F, .missing = NA), F)
  expect_identical(vec_pall(F, N, .missing = NA), F)
  expect_identical(vec_pall(N, T, .missing = NA), N)
  expect_identical(vec_pall(N, F, .missing = NA), F)
  expect_identical(vec_pall(N, N, .missing = NA), N)

  expect_identical(vec_pall(T, T, .missing = TRUE), T)
  expect_identical(vec_pall(T, F, .missing = TRUE), F)
  expect_identical(vec_pall(T, N, .missing = TRUE), T)
  expect_identical(vec_pall(F, T, .missing = TRUE), F)
  expect_identical(vec_pall(F, F, .missing = TRUE), F)
  expect_identical(vec_pall(F, N, .missing = TRUE), F)
  expect_identical(vec_pall(N, T, .missing = TRUE), T)
  expect_identical(vec_pall(N, F, .missing = TRUE), F)
  expect_identical(vec_pall(N, N, .missing = TRUE), T)

  expect_identical(vec_pall(T, T, .missing = FALSE), T)
  expect_identical(vec_pall(T, F, .missing = FALSE), F)
  expect_identical(vec_pall(T, N, .missing = FALSE), F)
  expect_identical(vec_pall(F, T, .missing = FALSE), F)
  expect_identical(vec_pall(F, F, .missing = FALSE), F)
  expect_identical(vec_pall(F, N, .missing = FALSE), F)
  expect_identical(vec_pall(N, T, .missing = FALSE), F)
  expect_identical(vec_pall(N, F, .missing = FALSE), F)
  expect_identical(vec_pall(N, N, .missing = FALSE), F)

  expect_identical(vec_pany(T, T, .missing = NA), T)
  expect_identical(vec_pany(T, F, .missing = NA), T)
  expect_identical(vec_pany(T, N, .missing = NA), T)
  expect_identical(vec_pany(F, T, .missing = NA), T)
  expect_identical(vec_pany(F, F, .missing = NA), F)
  expect_identical(vec_pany(F, N, .missing = NA), N)
  expect_identical(vec_pany(N, T, .missing = NA), T)
  expect_identical(vec_pany(N, F, .missing = NA), N)
  expect_identical(vec_pany(N, N, .missing = NA), N)

  expect_identical(vec_pany(T, T, .missing = TRUE), T)
  expect_identical(vec_pany(T, F, .missing = TRUE), T)
  expect_identical(vec_pany(T, N, .missing = TRUE), T)
  expect_identical(vec_pany(F, T, .missing = TRUE), T)
  expect_identical(vec_pany(F, F, .missing = TRUE), F)
  expect_identical(vec_pany(F, N, .missing = TRUE), T)
  expect_identical(vec_pany(N, T, .missing = TRUE), T)
  expect_identical(vec_pany(N, F, .missing = TRUE), T)
  expect_identical(vec_pany(N, N, .missing = TRUE), T)

  expect_identical(vec_pany(T, T, .missing = FALSE), T)
  expect_identical(vec_pany(T, F, .missing = FALSE), T)
  expect_identical(vec_pany(T, N, .missing = FALSE), T)
  expect_identical(vec_pany(F, T, .missing = FALSE), T)
  expect_identical(vec_pany(F, F, .missing = FALSE), F)
  expect_identical(vec_pany(F, N, .missing = FALSE), F)
  expect_identical(vec_pany(N, T, .missing = FALSE), T)
  expect_identical(vec_pany(N, F, .missing = FALSE), F)
  expect_identical(vec_pany(N, N, .missing = FALSE), F)
})

test_that("works with empty inputs", {
  expect_identical(vec_pall(logical(), logical()), logical())
  expect_identical(vec_pany(logical(), logical()), logical())
})

test_that("works with no inputs", {
  expect_identical(vec_pall(), logical())
  expect_identical(vec_pany(), logical())
})

test_that("works with no inputs and specified `.size`", {
  expect_identical(vec_pall(.size = 3), c(TRUE, TRUE, TRUE))
  expect_identical(vec_pany(.size = 3), c(FALSE, FALSE, FALSE))
})

test_that("no casting is done", {
  expect_snapshot(error = TRUE, {
    vec_pall(1)
  })
  expect_snapshot(error = TRUE, {
    vec_pany(1)
  })

  # Arrays
  expect_snapshot(error = TRUE, {
    vec_pall(array(TRUE))
  })
  expect_snapshot(error = TRUE, {
    vec_pany(array(TRUE))
  })

  # Class
  expect_snapshot(error = TRUE, {
    vec_pall(structure(TRUE, class = "foo"))
  })
  expect_snapshot(error = TRUE, {
    vec_pany(structure(TRUE, class = "foo"))
  })
})

test_that("no recycling is done", {
  expect_snapshot(error = TRUE, {
    vec_pall(TRUE, c(TRUE, TRUE, TRUE))
  })
  expect_snapshot(error = TRUE, {
    vec_pany(TRUE, c(TRUE, TRUE, TRUE))
  })

  # With `.size`
  expect_snapshot(error = TRUE, {
    vec_pall(TRUE, c(TRUE, TRUE, TRUE), .size = 3L)
  })
  expect_snapshot(error = TRUE, {
    vec_pany(TRUE, c(TRUE, TRUE, TRUE), .size = 3L)
  })
})

test_that("validates `.missing`", {
  expect_snapshot(error = TRUE, vec_pall(.missing = c(TRUE, FALSE)))
  expect_snapshot(error = TRUE, vec_pany(.missing = c(TRUE, FALSE)))

  expect_snapshot(error = TRUE, vec_pall(.missing = 1))
  expect_snapshot(error = TRUE, vec_pany(.missing = 1))

  expect_snapshot(error = TRUE, vec_pall(.missing = NULL))
  expect_snapshot(error = TRUE, vec_pany(.missing = NULL))
})

test_that("validates `.size`", {
  expect_snapshot(error = TRUE, vec_pall(.size = c(1, 2)))
  expect_snapshot(error = TRUE, vec_pany(.size = c(1, 2)))

  expect_snapshot(error = TRUE, vec_pall(.size = 1.5))
  expect_snapshot(error = TRUE, vec_pany(.size = 1.5))

  expect_snapshot(error = TRUE, vec_pall(.size = NA_integer_))
  expect_snapshot(error = TRUE, vec_pany(.size = NA_integer_))
})

test_that("names are used in errors", {
  expect_snapshot(error = TRUE, {
    vec_pall(1.5, .arg = "x")
  })
  expect_snapshot(error = TRUE, {
    vec_pall(a = 1.5, .arg = "x")
  })
  expect_snapshot(error = TRUE, {
    x <- c(TRUE, FALSE)
    y <- logical()
    vec_pany(x, y)
  })
  expect_snapshot(error = TRUE, {
    x <- c(TRUE, FALSE)
    y <- logical()
    vec_pany(a = x, b = y, .arg = "x", .error_call = quote(foo()))
  })
})
