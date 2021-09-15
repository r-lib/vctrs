test_that("interleaving is working as expected", {
  expect_identical(
    vec_interleave(1:3, 4:6),
    c(1L, 4L, 2L, 5L, 3L, 6L)
  )
  expect_identical(
    vec_interleave(1:3, 4:6, 7:9),
    c(1L, 4L, 7L, 2L, 5L, 8L, 3L, 6L, 9L)
  )
})

test_that("data frames can be interleaved", {
  x <- data_frame(x = 1:2, y = c("a", "b"))
  y <- data_frame(x = 3:4, y = c("c", "d"))

  expect_identical(
    vec_interleave(x, y),
    vec_slice(vec_c(x, y), c(1, 3, 2, 4))
  )
})

test_that("works with `NULL` inputs", {
  expect_identical(
    vec_interleave(1:3, NULL, 4:6),
    vec_interleave(1:3, 4:6)
  )
})

test_that("allows for name repair", {
  x <- c(x = 1)

  expect_identical(
    vec_interleave(x, x),
    c(x = 1, x = 1)
  )

  expect_snapshot(vec_interleave(x, x, .name_repair = "unique"))
})

test_that("works with name specs", {
  x <- c(x = 1)
  y <- 1

  expect_named(
    vec_interleave(x = x, y = y, .name_spec = "{outer}_{inner}"),
    c("x_x", "y")
  )
})

test_that("recycles inputs", {
  expect_identical(
    vec_interleave(1:3, NA),
    c(1L, NA, 2L, NA, 3L, NA)
  )
  expect_identical(
    vec_interleave(integer(), NA),
    integer()
  )
})

test_that("works with no inputs", {
  expect_identical(vec_interleave(), NULL)
})

test_that("works with length zero input", {
  expect_identical(vec_interleave(integer(), integer()), integer())
})

test_that("respects ptype", {
  expect_identical(vec_interleave(.ptype = character()), character())
  expect_identical(vec_interleave(1L, 2L, .ptype = numeric()), c(1, 2))
})

test_that("uses recycling errors", {
  expect_snapshot(error = TRUE, vec_interleave(1:2, 1:3))
})

test_that("errors if the result would be a long vector", {
  expect_snapshot(
    error = TRUE,
    vec_interleave(seq_len(1e9), seq_len(1e9), seq_len(1e9))
  )
})
