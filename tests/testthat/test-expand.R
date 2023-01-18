test_that("expands the first column slowest by default", {
  x <- 1:4
  y <- 1:3
  z <- 1:2

  expect_identical(
    vec_expand_grid(x = x, y = y, z = z),
    data_frame(
      x = vec_rep(vec_rep_each(x, times = 6), times = 1),
      y = vec_rep(vec_rep_each(y, times = 2), times = 4),
      z = vec_rep(vec_rep_each(z, times = 1), times = 12)
    )
  )
})

test_that("can expand the first column fastest with `.vary`", {
  x <- 1:4
  y <- 1:3
  z <- 1:2

  expect_identical(
    vec_expand_grid(x = x, y = y, z = z, .vary = "fastest"),
    data_frame(
      x = vec_rep(vec_rep_each(x, times = 1), times = 6),
      y = vec_rep(vec_rep_each(y, times = 4), times = 2),
      z = vec_rep(vec_rep_each(z, times = 12), times = 1)
    )
  )
})

test_that("size 0 elements force a size 0 result", {
  expect_identical(
    vec_expand_grid(x = 1:3, y = integer(), z = 1:2),
    data_frame(x = integer(), y = integer(), z = integer())
  )

  expect_identical(
    vec_expand_grid(x = integer()),
    data_frame(x = integer())
  )
})

test_that("returns 1 row and 0 cols with no input", {
  # Because `prod(integer()) == 1L`
  expect_identical(vec_expand_grid(), data_frame(.size = 1L))
})

test_that("drops `NULL` values", {
  expect_identical(
    vec_expand_grid(NULL, NULL),
    vec_expand_grid()
  )

  # And that happens before all names checks
  expect_identical(
    vec_expand_grid(x = 1:2, x = NULL, y = 1:3, NULL),
    vec_expand_grid(x = 1:2, y = 1:3)
  )
})

test_that("works with data frame inputs", {
  x <- data_frame(a = 1:2, b = 2:3)
  y <- 1:3

  expect_identical(
    vec_expand_grid(x = x, y = y),
    data_frame(
      x = vec_rep(vec_rep_each(x, times = 3), times = 1),
      y = vec_rep(vec_rep_each(y, times = 1), times = 2),
    )
  )
})

test_that("`.name_repair` isn't affected by `.vary`", {
  expect <- vec_as_names(c("a", "b", "a", "z"), repair = "unique_quiet")

  expect_named(
    vec_expand_grid(a = 1, b = 2, a = 3, z = 4, .vary = "slowest", .name_repair = "unique_quiet"),
    expect
  )
  expect_named(
    vec_expand_grid(a = 1, b = 2, a = 3, z = 4, .vary = "fastest", .name_repair = "unique_quiet"),
    expect
  )
})

test_that("can use `.name_repair`", {
  expect_identical(
    vec_expand_grid(a = 1:2, a = 2:3, .name_repair = "minimal"),
    data_frame(a = c(1L, 1L, 2L, 2L), a = c(2L, 3L, 2L, 3L), .name_repair = "minimal")
  )
})

test_that("catches duplicate names by default", {
  expect_snapshot(error = TRUE, {
    vec_expand_grid(a = 1, a = 2)
  })
})

test_that("errors on non vectors and mentions the element name", {
  expect_snapshot(error = TRUE, {
    vec_expand_grid(y = environment())
  })
})

test_that("can adjust the `.error_call`", {
  my_expand_grid <- function() {
    vec_expand_grid(x = environment(), .error_call = current_env())
  }

  expect_snapshot(error = TRUE, {
    my_expand_grid()
  })
})

test_that("errors nicely when expansion results in a size larger than `R_len_t`", {
  # Windows 32-bit doesn't support long vectors of this size, and the
  # intermediate `r_ssize` will be too large
  skip_if(.Machine$sizeof.pointer < 8L, message = "No long vector support")

  x <- seq_len((2^31 - 1) / 2)
  y <- 1:3

  expect_snapshot(error = TRUE, {
    vec_expand_grid(x = x, y = y)
  })
})

test_that("errors nicely when expansion results in a size larger than `R_xlen_t`", {
  # Windows 32-bit doesn't support long vectors of this size, and the
  # intermediate `r_ssize` will be too large
  skip_if(.Machine$sizeof.pointer < 8L, message = "No long vector support")

  x <- seq_len(2^31 - 1)

  expect_snapshot(error = TRUE, transform = scrub_internal_error_line_number, {
    vec_expand_grid(x = x, y = x)
  })
})

test_that("validates `.vary`", {
  expect_snapshot(error = TRUE, {
    vec_expand_grid(.vary = 1)
  })
  expect_snapshot(error = TRUE, {
    vec_expand_grid(.vary = "x")
  })
})
