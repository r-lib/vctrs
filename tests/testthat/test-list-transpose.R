test_that("transposes vectors", {
  expect_identical(
    list_transpose(list(1:2, 3:4, 5:6)),
    list(c(1L, 3L, 5L), c(2L, 4L, 6L))
  )
})

test_that("transposes data frames", {
  expect_identical(
    list_transpose(list(
      data_frame(a = 1:3, b = letters[1:3]),
      data_frame(a = 4:6, b = letters[4:6])
    )),
    list(
      data_frame(a = c(1L, 4L), b = letters[c(1L, 4L)]),
      data_frame(a = c(2L, 5L), b = letters[c(2L, 5L)]),
      data_frame(a = c(3L, 6L), b = letters[c(3L, 6L)])
    )
  )
})

test_that("works with empty `x`", {
  # Input:
  # - List size 0
  # - Element size 0 (inferred)
  expect_identical(list_transpose(list()), list())

  # Input
  # - List size 0
  # - Element size 2 (provided)
  # - Element type unspecified (inferred)
  # Output
  # - List size 2
  # - Element size 0
  # - Element type unspecified
  expect_identical(
    list_transpose(list(), size = 2),
    list(unspecified(), unspecified())
  )

  # Input
  # - List size 0
  # - Element size 2 (provided)
  # - Element type integer (provided)
  # Output
  # - List size 2
  # - Element size 0
  # - Element type integer
  expect_identical(
    list_transpose(list(), size = 2, ptype = integer()),
    list(integer(), integer())
  )
})

test_that("can recover original type and size with manual `ptype` and `size`", {
  # - List size 2
  # - Element size 0
  # - Element type integer
  x <- list(integer(), integer())

  # - List size 0
  # - Element size 2 (but no elements)
  # - Element type integer (but no elements)
  out <- list_transpose(x)
  expect_identical(out, list())

  # Simply transposing again doesn't recover the original `x`, but supplying
  # a known `ptype` and `size` does
  expect_identical(
    list_transpose(out, size = vec_size(x), ptype = vec_ptype_common(!!!x)),
    x
  )
})

test_that("retains only inner names", {
  # I don't think we should expose `name_spec`, we've hard coded it to `"inner"`
  # for now. What would this even do with outer names? Exposing `name_spec` for
  # the interleave step would allow making names of `a_w` and `b_y` via a glue
  # spec, which feels weird and not useful.
  x <- list(a = c(w = 1, x = 2), b = c(y = 3, z = 4))

  expect_identical(
    list_transpose(x),
    list(
      c(w = 1, y = 3),
      c(x = 2, z = 4)
    )
  )

  # Silent repair of duplicate data frame row names
  x <- list(
    data.frame(a = 1, row.names = "x"),
    data.frame(a = 2, row.names = "x")
  )

  expect_silent({
    expect_identical(
      list_transpose(x),
      list(data.frame(a = c(1, 2), row.names = c("x...1", "x...2")))
    )
  })
})

test_that("`x` must be a list", {
  expect_snapshot(error = TRUE, {
    list_transpose(1)
  })
  expect_snapshot(error = TRUE, {
    list_transpose(1, x_arg = "x", error_call = quote(foo()))
  })
})

test_that("`...` must be empty", {
  expect_snapshot(error = TRUE, {
    list_transpose(1, 2)
  })
})

test_that("respects `size`", {
  # Useful for the case where you somehow know the element size from somewhere
  # else, but you also happen to only have all size 1 elements right now
  expect_identical(
    list_transpose(list(1L, 2L), size = 3),
    list(1:2, 1:2, 1:2)
  )

  expect_snapshot(error = TRUE, {
    list_transpose(list(1:2), size = 3)
  })
})

test_that("respects `ptype`", {
  expect_identical(
    list_transpose(list(1, 2), ptype = integer()),
    list(1:2)
  )

  expect_snapshot(error = TRUE, {
    list_transpose(
      list(1, 2),
      ptype = character()
    )
  })
  expect_snapshot(error = TRUE, {
    list_transpose(
      list(1, 2),
      ptype = character(),
      x_arg = "x",
      error_call = quote(foo())
    )
  })
})

test_that("doesn't allow `NULL` elements", {
  # These would break the invariants around the size of the output relative
  # to the size of the input
  expect_snapshot(error = TRUE, {
    list_transpose(list(1:4, NULL, 5:8))
  })
})

test_that("doesn't allow scalar elements", {
  expect_snapshot(error = TRUE, {
    list_transpose(list(1:4, lm(1 ~ 1)))
  })
  expect_snapshot(error = TRUE, {
    list_transpose(list(1:4, lm(1 ~ 1)), x_arg = "x", error_call = quote(foo()))
  })
})
