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

test_that("can repair names quietly", {
  local_name_repair_verbose()

  expect_snapshot({
    res_unique <- vec_interleave(
      c(x = 1),
      c(x = 2),
      .name_repair = "unique_quiet"
    )
    res_universal <- vec_interleave(
      c("if" = 1),
      c("in" = 2),
      .name_repair = "universal_quiet"
    )
  })
  expect_named(res_unique, c("x...1", "x...2"))
  expect_named(res_universal, c(".if", ".in"))
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
  # Purposefully returns `unspecified()`, which is the more useful result
  # for generic programming against this, like in `list_transpose()`
  expect_identical(vec_interleave(), unspecified())
  expect_identical(vec_interleave(NULL), unspecified())

  # `.size` affects the size of each element, thus it doesn't affect the output
  # when there are 0 elements
  expect_identical(vec_interleave(.size = 2), unspecified())
  expect_identical(vec_interleave(NULL, .size = 2), unspecified())
})

test_that("works with length zero input", {
  expect_identical(vec_interleave(integer(), integer()), integer())
})

test_that("respects `.ptype`", {
  expect_identical(vec_interleave(.ptype = character()), character())
  expect_identical(vec_interleave(NULL, .ptype = character()), character())

  expect_identical(vec_interleave(1L, 2L, .ptype = numeric()), c(1, 2))
})

test_that("reports type errors", {
  expect_snapshot(error = TRUE, {
    vec_interleave(1, "x")
  })
  expect_snapshot(error = TRUE, {
    vec_interleave(1, "x", .error_call = quote(foo()))
  })

  expect_snapshot(error = TRUE, {
    vec_interleave(1, "x", .ptype = double())
  })
  expect_snapshot(error = TRUE, {
    vec_interleave(1, "x", .ptype = double(), .error_call = quote(foo()))
  })

  # Index is right even with `NULL`!
  expect_snapshot(error = TRUE, {
    vec_interleave(1, NULL, "x")
  })
  expect_snapshot(error = TRUE, {
    vec_interleave(1, NULL, "x", .ptype = double())
  })
})

test_that("respects `.size`", {
  # Correctly does not report an error here
  expect_identical(
    vec_interleave(1:2, 3:4, .size = 2),
    c(1L, 3L, 2L, 4L)
  )

  # Useful for recycling to a known element size
  # in the case of all size 1 elements
  expect_identical(
    vec_interleave(1, 2, .size = 2),
    c(1, 2, 1, 2)
  )
})

test_that("reports recycling errors", {
  expect_snapshot(error = TRUE, {
    vec_interleave(1:2, 1:3)
  })
  expect_snapshot(error = TRUE, {
    vec_interleave(1:2, 1:3, .error_call = quote(foo()))
  })

  expect_snapshot(error = TRUE, {
    vec_interleave(1:2, 3:4, .size = 3)
  })
  expect_snapshot(error = TRUE, {
    vec_interleave(1:2, 3:4, .size = 3, .error_call = quote(foo()))
  })

  # Index is right even with `NULL`!
  expect_snapshot(error = TRUE, {
    vec_interleave(1:2, NULL, 1:3)
  })
  expect_snapshot(error = TRUE, {
    vec_interleave(1:2, NULL, 1:3, .size = 2)
  })
})

test_that("reports scalar errors", {
  expect_snapshot(error = TRUE, {
    vec_interleave(lm(1 ~ 1))
  })
  expect_snapshot(error = TRUE, {
    vec_interleave(lm(1 ~ 1), .error_call = quote(foo()))
  })

  # Index is right even with `NULL`!
  expect_snapshot(error = TRUE, {
    vec_interleave(1, NULL, lm(1 ~ 1))
  })
  expect_snapshot(error = TRUE, {
    vec_interleave(1, NULL, lm(1 ~ 1), .error_call = quote(foo()))
  })
})

test_that("`list_interleave()` checks for a list", {
  expect_snapshot(error = TRUE, {
    list_interleave(1)
  })
})
