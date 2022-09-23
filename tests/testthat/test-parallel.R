test_that("9 possible variations of each combination are right", {
  N <- NA

  expect_identical(vec_pall(T, T, .na_rm = F), T)
  expect_identical(vec_pall(T, F, .na_rm = F), F)
  expect_identical(vec_pall(T, N, .na_rm = F), N)
  expect_identical(vec_pall(F, T, .na_rm = F), F)
  expect_identical(vec_pall(F, F, .na_rm = F), F)
  expect_identical(vec_pall(F, N, .na_rm = F), F)
  expect_identical(vec_pall(N, T, .na_rm = F), N)
  expect_identical(vec_pall(N, F, .na_rm = F), F)
  expect_identical(vec_pall(N, N, .na_rm = F), N)

  expect_identical(vec_pall(T, T, .na_rm = T), T)
  expect_identical(vec_pall(T, F, .na_rm = T), F)
  expect_identical(vec_pall(T, N, .na_rm = T), T)
  expect_identical(vec_pall(F, T, .na_rm = T), F)
  expect_identical(vec_pall(F, F, .na_rm = T), F)
  expect_identical(vec_pall(F, N, .na_rm = T), F)
  expect_identical(vec_pall(N, T, .na_rm = T), T)
  expect_identical(vec_pall(N, F, .na_rm = T), F)
  expect_identical(vec_pall(N, N, .na_rm = T), T)

  expect_identical(vec_pany(T, T, .na_rm = F), T)
  expect_identical(vec_pany(T, F, .na_rm = F), T)
  expect_identical(vec_pany(T, N, .na_rm = F), T)
  expect_identical(vec_pany(F, T, .na_rm = F), T)
  expect_identical(vec_pany(F, F, .na_rm = F), F)
  expect_identical(vec_pany(F, N, .na_rm = F), N)
  expect_identical(vec_pany(N, T, .na_rm = F), T)
  expect_identical(vec_pany(N, F, .na_rm = F), N)
  expect_identical(vec_pany(N, N, .na_rm = F), N)

  expect_identical(vec_pany(T, T, .na_rm = T), T)
  expect_identical(vec_pany(T, F, .na_rm = T), T)
  expect_identical(vec_pany(T, N, .na_rm = T), T)
  expect_identical(vec_pany(F, T, .na_rm = T), T)
  expect_identical(vec_pany(F, F, .na_rm = T), F)
  expect_identical(vec_pany(F, N, .na_rm = T), F)
  expect_identical(vec_pany(N, T, .na_rm = T), T)
  expect_identical(vec_pany(N, F, .na_rm = T), F)
  expect_identical(vec_pany(N, N, .na_rm = T), F)
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

test_that("casts inputs to logical", {
  expect_identical(vec_pall(1, 1, c(1, 0)), c(TRUE, FALSE))
  expect_identical(vec_pany(0, 0, c(1, 0)), c(TRUE, FALSE))

  expect_snapshot(error = TRUE, {
    vec_pall(1.5)
  })
  expect_snapshot(error = TRUE, {
    vec_pany(1.5)
  })
})

test_that("recycles inputs to common size", {
  expect_identical(vec_pall(TRUE, c(FALSE, TRUE)), c(FALSE, TRUE))
  expect_identical(vec_pany(TRUE, c(FALSE, TRUE)), c(TRUE, TRUE))

  expect_snapshot(error = TRUE, {
    vec_pall(c(TRUE, FALSE), c(TRUE, TRUE, TRUE))
  })
  expect_snapshot(error = TRUE, {
    vec_pany(c(TRUE, FALSE), c(TRUE, TRUE, TRUE))
  })
})

test_that("respects `.size`", {
  expect_identical(vec_pall(TRUE, .size = 3), c(TRUE, TRUE, TRUE))

  expect_snapshot(error = TRUE, {
    vec_pall(c(TRUE, FALSE), .size = 3L)
  })
})

test_that("validates `.na_rm`", {
  expect_snapshot(error = TRUE, vec_pall(.na_rm = c(TRUE, FALSE)))
  expect_snapshot(error = TRUE, vec_pany(.na_rm = c(TRUE, FALSE)))

  expect_snapshot(error = TRUE, vec_pall(.na_rm = 1))
  expect_snapshot(error = TRUE, vec_pany(.na_rm = 1))

  expect_snapshot(error = TRUE, vec_pall(.na_rm = NA))
  expect_snapshot(error = TRUE, vec_pany(.na_rm = NA))
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
    vec_pall(x = 1.5)
  })
  expect_snapshot(error = TRUE, {
    vec_pany(x = c(TRUE, FALSE), y = logical())
  })
})
