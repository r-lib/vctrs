context("test-c")

test_that("zero length input returns NULL", {
  expect_equal(vec_c(), NULL)
  expect_equal(vec_c(NULL), NULL)
  expect_equal(vec_c(NULL,), NULL)
  expect_equal(vec_c(NULL, NULL), NULL)
})

test_that("NULL is idempotent", {
  expect_equal(vec_c(NULL, 1L), 1L)
  expect_equal(vec_c(1L, NULL), 1L)
})

test_that("NA is idempotent", {
  expect_equal(vec_c(NA, 1L), c(NA, 1L))
  expect_equal(vec_c(NA, "x"), c(NA, "x"))
  expect_equal(vec_c(NA, factor("x")), factor(c(NA, "x")))
  expect_equal(vec_c(NA, new_date(0)), new_date(c(NA, 0)))
  expect_equal(vec_c(NA, new_datetime(0)), new_datetime(c(NA, 0)))
  expect_equal(vec_c(NA, new_duration(0)), new_duration(c(NA, 0)))
})

test_that("NA is logical if no other types intervene", {
  expect_equal(vec_c(logical()), logical())
  expect_equal(vec_c(NA), NA)
  expect_equal(vec_c(NA, NA), c(NA, NA))
})


test_that("different types are coerced to common", {
  expect_equal(vec_c(TRUE, 1L, 1), c(1, 1, 1))
  expect_equal(vec_c(TRUE, 2:4), 1:4)
})

test_that("specified .ptypes allows more casts", {
  expect_equal(vec_c(TRUE, .ptype = character()), "TRUE")
})

test_that("combines outer an inner names", {
  expect_equal(vec_c(x = 1), c(x = 1))
  expect_equal(vec_c(c(x = 1)), c(x = 1))

  expect_equal(vec_c(c(x = 1:2)), c(x1 = 1, x2 = 2))
  expect_equal(vec_c(y = c(x = 1)), c(y..x = 1))
})
