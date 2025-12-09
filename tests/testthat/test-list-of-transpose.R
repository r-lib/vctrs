test_that("transposes vectors", {
  expect_identical(
    list_of_transpose(list_of2(1:2, 3:4, 5:6)),
    list_of2(c(1L, 3L, 5L), c(2L, 4L, 6L))
  )
})

test_that("transposes data frames", {
  expect_identical(
    list_of_transpose(list_of2(
      data_frame(a = 1:3, b = letters[1:3]),
      data_frame(a = 4:6, b = letters[4:6])
    )),
    list_of2(
      data_frame(a = c(1L, 4L), b = letters[c(1L, 4L)]),
      data_frame(a = c(2L, 5L), b = letters[c(2L, 5L)]),
      data_frame(a = c(3L, 6L), b = letters[c(3L, 6L)])
    )
  )
})

test_that("empty `x` is fully reversible", {
  x <- list_of(.ptype = integer(), .size = 0)
  out <- list_of_transpose(x)
  expect_identical(
    out,
    list_of(.ptype = integer(), .size = 0)
  )
  expect_identical(list_of_transpose(out), x)

  x <- list_of(.ptype = integer(), .size = 2)
  out <- list_of_transpose(x)
  expect_identical(
    out,
    list_of(integer(), integer(), .ptype = integer(), .size = 0)
  )
  expect_identical(list_of_transpose(out), x)
})

test_that("retains only inner names", {
  # I don't think we should expose `name_spec`, we've hard coded it to `"inner"`
  # for now. What would this even do with outer names? Exposing `name_spec` for
  # the interleave step would allow making names of `a_w` and `b_y` via a glue
  # spec, which feels weird and not useful.
  x <- list_of2(a = c(w = 1, x = 2), b = c(y = 3, z = 4))

  expect_identical(
    list_of_transpose(x),
    list_of2(
      c(w = 1, y = 3),
      c(x = 2, z = 4)
    )
  )

  # Silent repair of duplicate data frame row names
  x <- list_of2(
    data.frame(a = 1, row.names = "x"),
    data.frame(a = 2, row.names = "x")
  )

  expect_silent({
    expect_identical(
      list_of_transpose(x),
      list_of2(data.frame(a = c(1, 2), row.names = c("x...1", "x...2")))
    )
  })
})

test_that("`x` is validated", {
  expect_snapshot(error = TRUE, {
    list_of_transpose(1)
  })
  expect_snapshot(error = TRUE, {
    list_of_transpose(1, x_arg = "x", error_call = quote(foo()))
  })
})

test_that("`x` must be a fully specified list of", {
  expect_snapshot(error = TRUE, {
    x <- list_of(.ptype = integer(), .size = zap())
    list_of_transpose(x)
  })
  expect_snapshot(error = TRUE, {
    x <- list_of(.ptype = zap(), .size = 1)
    list_of_transpose(x)
  })
})

test_that("`...` must be empty", {
  expect_snapshot(error = TRUE, {
    list_of_transpose(list_of2(1), 2)
  })
})

test_that("doesn't allow `NULL` elements", {
  # These would break the invariants around the size of the output relative
  # to the size of the input if we just dropped them. We require that the user
  # drop them entirely or replace them up front. The `[<-` method for list-of
  # ensures that the type and size is maintained for whatever they replace with.
  expect_snapshot(error = TRUE, {
    list_of_transpose(list_of2(1:4, NULL, 5:8))
  })
})

test_that("`x` being a list subclass can't affect the transposition", {
  x <- new_list_of(
    list(1, 2),
    ptype = double(),
    size = 1L,
    class = "my_list"
  )

  expect_identical(
    list_of_transpose(x),
    list_of2(c(1, 2))
  )
})
