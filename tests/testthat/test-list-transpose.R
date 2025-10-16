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

test_that("recycles inputs to common size before transposing", {
  expect_identical(
    list_transpose(list(1, 2:3, 4)),
    list(c(1, 2, 4), c(1, 3, 4))
  )
  expect_snapshot(error = TRUE, {
    x <- list(1:2, 3:5)
    list_transpose(x)
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
  # to the size of the input if we just dropped them
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

test_that("`x` being a list subclass can't affect the transposition", {
  x <- structure(list(1, NULL, 2), class = c("my_list", "list"))

  null <- 0

  # Note how this is an error. We perform a cast like this internally.
  expect_snapshot(error = TRUE, {
    vec_cast(list(null), to = x)
  })

  # But we unclass `x` first, so it won't matter.
  # Our output type is always `<list>` and as long as `obj_is_list()`
  # passes, we don't care about the input type.
  expect_identical(
    list_transpose(x, null = null),
    list(c(1, 0, 2))
  )
})

test_that("`x` being a <list_of> doesn't affect the transposition", {
  # As a primitive function, `list_transpose()` doesn't know anything
  # about `<list_of>`, and shouldn't treat it specially

  # No preservation of type
  x <- list_of(.ptype = integer())
  expect_identical(list_transpose(x), list())
  expect_identical(list_transpose(x, ptype = character()), list())

  x <- list_of(NULL, .ptype = integer())
  expect_snapshot(error = TRUE, {
    list_transpose(x)
  })
  expect_identical(
    list_transpose(x, null = "x"),
    list()
  )
  expect_identical(
    list_transpose(x, null = "x", size = 2),
    list("x", "x")
  )

  # `ptype` overrules list-of type
  x <- list_of(1L, 2L)
  expect_identical(
    list_transpose(x, ptype = double()),
    list(c(1, 2))
  )

  # Common type determination with `null` overrules list-of type
  x <- list_of(1L, NULL, 2L)
  expect_identical(
    list_transpose(x, null = 0),
    list(c(1, 0, 2))
  )
})

test_that("`null` replaces `NULL` elements", {
  x <- list(1:2, NULL, 3:4, NULL)

  expect_identical(
    list_transpose(x, null = 0L),
    list(
      int(1, 0, 3, 0),
      int(2, 0, 4, 0)
    )
  )
})

test_that("`null` must be a vector", {
  x <- list(1, NULL)
  expect_snapshot(error = TRUE, {
    list_transpose(x, null = lm(1 ~ 1))
  })

  # Even when not used
  x <- list(1, 2)
  expect_snapshot(error = TRUE, {
    list_transpose(x, null = lm(1 ~ 1))
  })
})

test_that("`null` participates in common type determination", {
  x <- list(1L, NULL)
  expect_identical(
    list_transpose(x, null = 0),
    list(c(1, 0))
  )
  expect_snapshot(error = TRUE, {
    list_transpose(x, null = "x")
  })
  expect_snapshot(error = TRUE, {
    list_transpose(x, null = "x", ptype = double())
  })

  # Even when not used
  x <- list(1L, 2L)
  expect_identical(
    list_transpose(x, null = 0),
    list(c(1, 2))
  )
  expect_snapshot(error = TRUE, {
    list_transpose(x, null = "x")
  })
  expect_snapshot(error = TRUE, {
    list_transpose(x, null = "x", ptype = double())
  })
})

test_that("`null` is recycled to common size of inputs or `size`", {
  x <- list(1:2, NULL, 5:6)
  expect_identical(
    list_transpose(x, null = NA),
    list(c(1L, NA, 5L), c(2L, NA, 6L))
  )

  x <- list(1:2, NULL, 5:6)
  expect_identical(
    list_transpose(x, null = 3:4),
    list(c(1L, 3L, 5L), c(2L, 4L, 6L))
  )
})

test_that("`null` size 0 behavior", {
  # Element common size is inferred to be 0 from `x`
  #
  # I: List size 0, Element size 0
  # O: List size 0, Element size 0
  expect_identical(
    list_transpose(list(), null = double()),
    list()
  )
  # I: List size 1, Element size 0
  # O: List size 0, Element size 1
  expect_identical(
    list_transpose(list(NULL), null = double()),
    list()
  )

  # Element common size is inferred to be 1 from `x`
  #
  # I: List size 2, Element size 1 (can't recycle `null` to this)
  # O: List size 1, Element size 2
  expect_snapshot(error = TRUE, {
    list_transpose(list(1, 2), null = double())
  })
  # I: List size 3, Element size 1 (can't recycle `null` to this)
  # O: List size 1, Element size 3
  expect_snapshot(error = TRUE, {
    list_transpose(list(1, 2, NULL), null = double())
  })

  # Like with the `null` size >1 case, if you are programming with
  # `list_transpose()` and built `null` to be size 0, you obviously expect each
  # element to also be size 0. So to guard against the all size 1 element case
  # (when they should be recycled to a known size 0), supply the known element
  # size.
  size <- 0L
  null <- double()

  # I: List size 2, Element size 0
  # O: List size 0, Element size 2
  expect_identical(
    list_transpose(list(1, 2), null = null, size = size),
    list()
  )
})

test_that("`null` size 1 behavior", {
  # This is the easy to explain case because everything recycles as you'd
  # imagine it to work anyways

  # Element common size is inferred to be 0 from `x`
  #
  # I: List size 0, Element size 0 (can recycle `null` to this)
  # O: List size 0, Element size 0
  expect_identical(
    list_transpose(list(), null = 3),
    list()
  )
  # I: List size 1, Element size 0 (can recycle `null` to this)
  # O: List size 0, Element size 1
  expect_identical(
    list_transpose(list(NULL), null = 3),
    list()
  )

  # Element common size is inferred to be 1 from `x`
  #
  # I: List size 2, Element size 1
  # O: List size 1, Element size 2
  expect_identical(
    list_transpose(list(1, 2), null = 3),
    list(c(1, 2))
  )
  # I: List size 3, Element size 1
  # O: List size 1, Element size 3
  expect_identical(
    list_transpose(list(1, 2, NULL), null = 3),
    list(c(1, 2, 3))
  )
})

test_that("`null` size >1 behavior", {
  # Element common size is inferred to be 0 from `x`
  #
  # I: List size 0, Element size 0 (can't recycle `null` to this)
  # O: List size 0, Element size 0
  expect_snapshot(error = TRUE, {
    list_transpose(list(), null = 3:4)
  })
  # I: List size 1, Element size 0 (can't recycle `null` to this)
  # O: List size 0, Element size 1
  expect_snapshot(error = TRUE, {
    list_transpose(list(NULL), null = 3:4)
  })

  # Element common size is inferred to be 1 from `x`
  #
  # I: List size 2, Element size 1 (can't recycle `null` to this)
  # O: List size 1, Element size 2
  expect_snapshot(error = TRUE, {
    list_transpose(list(1, 2), null = 3:4)
  })
  # I: List size 3, Element size 1 (can't recycle `null` to this)
  # O: List size 1, Element size 3
  expect_snapshot(error = TRUE, {
    list_transpose(list(1, 2, NULL), null = 3:4)
  })

  # The idea is that if you are programming with `list_transpose()` and you are
  # supplying a length >1 `null`, then you obviously know the expected element
  # size, otherwise you wouldn't have been able to make `null`. So the correct
  # way to generically program with `list_transpose()` and `null` and guard
  # against both the empty list case and the all size 1 element case is to go
  # ahead and supply that known element size.
  size <- 2L
  null <- c(3, 4)

  # I: List size 0, Element size 2
  # O: List size 2, Element size 0
  expect_identical(
    list_transpose(list(), null = null, size = 2),
    list(double(), double())
  )
  # I: List size 1, Element size 2
  # O: List size 2, Element size 1
  expect_identical(
    list_transpose(list(NULL), null = null, size = 2),
    list(3, 4)
  )

  # I: List size 2, Element size 2
  # O: List size 2, Element size 2
  expect_identical(
    list_transpose(list(1, 2), null = null, size = 2),
    list(c(1, 2), c(1, 2))
  )
  # I: List size 3, Element size 2
  # O: List size 2, Element size 3
  expect_identical(
    list_transpose(list(1, 2, NULL), null = null, size = 2),
    list(c(1, 2, 3), c(1, 2, 4))
  )
})

test_that("`null` influences type in the empty `list()` case", {
  # Input
  # - List size 0
  # - Element size 0 (inferred from list)
  # - Element type integer (inferred from `null`)
  # Output
  # - List size 0
  # - Element size 0
  # - Element type integer
  expect_identical(
    list_transpose(list(), null = 1L),
    list()
  )

  # Input
  # - List size 0
  # - Element size 0 (supplied by `size`)
  # - Element type integer (inferred from `null`)
  # Output
  # - List size 0
  # - Element size 0
  # - Element type integer
  expect_identical(
    list_transpose(list(), null = 1L, size = 0),
    list()
  )

  # Input
  # - List size 0
  # - Element size 1 (supplied by `size`)
  # - Element type integer (inferred from `null`)
  # Output
  # - List size 1
  # - Element size 0
  # - Element type integer
  expect_identical(
    list_transpose(list(), null = 1L, size = 1),
    list(integer())
  )

  # Input
  # - List size 0
  # - Element size 2 (supplied by `size`)
  # - Element type integer (inferred from `null`)
  # Output
  # - List size 2
  # - Element size 0
  # - Element type integer
  expect_identical(
    list_transpose(list(), null = 1L, size = 2),
    list(integer(), integer())
  )
})

test_that("`null` influences type in the only `NULL` case", {
  # Input
  # - List size 2
  # - Element size 1 (inferred from `NULL` being treated as size 1)
  # - Element type integer (inferred from `null`)
  # Output
  # - List size 1
  # - Element size 2
  # - Element type integer
  expect_identical(
    list_transpose(list(NULL, NULL), null = 1L, size = 1L),
    list(c(1L, 1L))
  )
  expect_identical(
    list_transpose(list(NULL, NULL), null = 1L, size = 1L, ptype = double()),
    list(c(1, 1))
  )
})

test_that("`ptype` is finalized", {
  # `vec_ptype(NA)` alone returns `unspecified()`, must also call
  # `vec_ptype_finalize()`
  expect_identical(
    list_transpose(list(TRUE, FALSE), ptype = NA),
    list(c(TRUE, FALSE))
  )
})
