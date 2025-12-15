test_that("from_as_list_of_vectors = TRUE / to_as_list_of_vectors = TRUE", {
  expect_identical(
    vec_recode_values(
      x = c(1, 2, 3),
      from = list(c(2, 3), c(4, 1)),
      to = list(
        c("a", "b", "c"),
        c("d", "e", "f")
      ),
      from_as_list_of_vectors = TRUE,
      to_as_list_of_vectors = TRUE
    ),
    c("d", "b", "c")
  )
})

test_that("from_as_list_of_vectors = TRUE / to_as_list_of_vectors = TRUE (optimized to `from_as_list_of_vectors = FALSE`, `to_as_list_of_vectors = FALSE`)", {
  # First optimized to `to_as_list_of_vectors = FALSE`
  # Then further optimized to `from_as_list_of_vectors = FALSE`
  expect_identical(
    vec_recode_values(
      x = c(1, 2, 3),
      from = list(c(2, 3), c(4, 1)),
      to = list(
        "a",
        "b"
      ),
      from_as_list_of_vectors = TRUE,
      to_as_list_of_vectors = TRUE
    ),
    c("b", "a", "a")
  )
})

test_that("from_as_list_of_vectors = TRUE / to_as_list_of_vectors = FALSE (always optimized to `from_as_list_of_vectors = FALSE`)", {
  expect_identical(
    vec_recode_values(
      x = c(1, 2, 3),
      from = list(c(2, 3), c(4, 1)),
      to = c("a", "b"),
      from_as_list_of_vectors = TRUE,
      to_as_list_of_vectors = FALSE
    ),
    c("b", "a", "a")
  )
})

test_that("from_as_list_of_vectors = FALSE / to_as_list_of_vectors = TRUE", {
  expect_identical(
    vec_recode_values(
      x = c(1, 2, 3),
      from = c(2, 1),
      to = list(
        c("a", "b", "c"),
        c("d", "e", "f")
      ),
      from_as_list_of_vectors = FALSE,
      to_as_list_of_vectors = TRUE
    ),
    c("d", "b", NA)
  )
})

test_that("from_as_list_of_vectors = FALSE / to_as_list_of_vectors = TRUE (optimized to `to_as_list_of_vectors = FALSE`)", {
  expect_identical(
    vec_recode_values(
      x = c(1, 2, 3),
      from = c(2, 1),
      to = list(
        "a",
        "b"
      ),
      from_as_list_of_vectors = FALSE,
      to_as_list_of_vectors = TRUE
    ),
    c("b", "a", NA)
  )
})

test_that("from_as_list_of_vectors = FALSE / to_as_list_of_vectors = FALSE", {
  expect_identical(
    vec_recode_values(
      x = c(1, 2, 3),
      from = c(2, 1),
      to = c("a", "b"),
      from_as_list_of_vectors = FALSE,
      to_as_list_of_vectors = FALSE
    ),
    c("b", "a", NA)
  )
})

test_that("can treat list input as a vector (i.e. not as a container of vectors)", {
  # This is why we have `from_as_list_of_vectors` and `to_as_list_of_vectors` as arguments
  expect_identical(
    vec_recode_values(
      x = list(1, 2, 3),
      from = list(2, 5, 1),
      to = list("x", 1:2, 1:3),
    ),
    list(1:3, "x", NULL)
  )
})

test_that("`to` names are kept during `from_as_list_of_vectors, !to_as_list_of_vectors` optimization recycling", {
  expect_identical(
    vec_recode_values(
      x = c(2, 1, 4, 5),
      from = list(1:2, 3:4, 5:6),
      to = c(a = "x", b = "y", c = "z"),
      from_as_list_of_vectors = TRUE
    ),
    c(a = "x", a = "x", b = "y", c = "z")
  )
})

test_that("`to` list names are dropped with `to_as_list_of_vectors`", {
  # With `to_as_list_of_vectors` optimization
  expect_identical(
    vec_recode_values(
      x = c(1, 1),
      from = 1,
      to = list(a = "x"),
      to_as_list_of_vectors = TRUE
    ),
    c("x", "x")
  )
  # Without `to_as_list_of_vectors` optimization
  expect_identical(
    vec_recode_values(
      x = c(1, 1),
      from = 1,
      to = list(a = c("x", "y")),
      to_as_list_of_vectors = TRUE
    ),
    c("x", "y")
  )
})

test_that("`to` is recycled to `from_size` of `to_as_list_of_vectors`", {
  # With `to_as_list_of_vectors`
  expect_identical(
    vec_recode_values(
      x = c(1, 2, 3, 1, 4),
      from = c(1, 2),
      to = list(2),
      to_as_list_of_vectors = TRUE
    ),
    c(2, 2, NA, 2, NA)
  )
  expect_identical(
    vec_recode_values(
      x = c(1, 2, 3, 1, 4),
      from = c(1, 2),
      to = list(c(2, 3, 4, 5, 6)),
      to_as_list_of_vectors = TRUE
    ),
    c(2, 3, NA, 5, NA)
  )
  # This doesn't make too much sense for a user to provide, but is consistent
  expect_identical(
    vec_recode_values(
      x = c(1, 2, 3, 1, 4),
      from = list(c(1, 2), 4),
      to = list(2),
      from_as_list_of_vectors = TRUE,
      to_as_list_of_vectors = TRUE
    ),
    c(2, 2, NA, 2, 2)
  )
  # This doesn't make too much sense for a user to provide, but is consistent
  expect_identical(
    vec_recode_values(
      x = c(1, 2, 3, 1, 4),
      from = list(c(1, 2), 4),
      to = list(c(2, 3, 4, 5, 6)),
      from_as_list_of_vectors = TRUE,
      to_as_list_of_vectors = TRUE
    ),
    c(2, 3, NA, 5, 6)
  )

  # Without `to_as_list_of_vectors`
  expect_identical(
    vec_recode_values(
      x = c(1, 2, 3, 1),
      from = c(1, 2),
      to = 2
    ),
    c(2, 2, NA, 2)
  )
  expect_identical(
    vec_recode_values(
      x = c(1, 2, 3, 1, 4),
      from = list(c(1, 2), 4),
      to = 2,
      from_as_list_of_vectors = TRUE
    ),
    c(2, 2, NA, 2, 2)
  )
})

test_that("`from` list names are dropped with `from_as_list_of_vectors`", {
  # With `from_as_list_of_vectors, !to_as_list_of_vectors` optimization
  expect_identical(
    vec_recode_values(
      x = c(1, 2),
      from = list(a = 1, b = c(2, 3, 4)),
      to = c("x", "y"),
      from_as_list_of_vectors = TRUE
    ),
    c("x", "y")
  )
  # Without `from_as_list_of_vectors, !to_as_list_of_vectors` optimization
  expect_identical(
    vec_recode_values(
      x = c(1, 2),
      from = list(a = 1, b = c(2, 3, 4)),
      to = list(x = c("x1", "x2"), y = c("y1", "y2")),
      from_as_list_of_vectors = TRUE,
      to_as_list_of_vectors = TRUE
    ),
    c("x1", "y2")
  )
})

test_that("`vec_replace_values()` retains names of `x`", {
  # Mimicking `[<-` and `base::replace()`.
  # Note how `vec_recode_values()` "creates a new vector",
  # so it pulls the names from `to` and `default`.
  expect_identical(
    vec_replace_values(
      x = c(a = 1, b = 2, c = 0),
      from = c(2, 1),
      to = c(x = 3, y = 4)
    ),
    c(a = 4, b = 3, c = 0)
  )
  expect_identical(
    vec_recode_values(
      x = c(a = 1, b = 2, c = 0),
      from = c(2, 1),
      to = c(x = 3, y = 4)
    ),
    c(y = 4, x = 3, NA)
  )
})

test_that("`unmatched` errors are correct", {
  expect_snapshot(error = TRUE, {
    vec_recode_values(c(1, 2), from = 1, to = 0, unmatched = "error")
  })

  expect_snapshot(error = TRUE, {
    # `NA` must be matched!
    vec_recode_values(c(1, NA), from = 1, to = 0, unmatched = "error")
  })

  expect_snapshot(error = TRUE, {
    # Many locations
    vec_recode_values(1:100, from = 1, to = 0, unmatched = "error")
  })
})

test_that("`x` and `from` common type errors are correct", {
  expect_snapshot(error = TRUE, {
    vec_recode_values(1, from = "a", to = 1)
  })
  expect_snapshot(error = TRUE, {
    vec_recode_values(
      1,
      from = list("a"),
      to = 1,
      from_as_list_of_vectors = TRUE
    )
  })
})

test_that("`to` and `default` `ptype` errors are correct when it is inferred", {
  expect_snapshot(error = TRUE, {
    vec_recode_values(
      1,
      from = 1:2,
      to = list(1, "x"),
      to_as_list_of_vectors = TRUE
    )
  })
  expect_snapshot(error = TRUE, {
    vec_recode_values(
      1,
      from = 1:2,
      to = list(1, 2),
      default = "x",
      to_as_list_of_vectors = TRUE
    )
  })
  expect_snapshot(error = TRUE, {
    vec_recode_values(1, from = 1:2, to = 1, default = "x")
  })
})

test_that("`to` and `default` `ptype` errors are correct when it is user supplied", {
  expect_snapshot(error = TRUE, {
    vec_recode_values(1, from = 1, to = 1, ptype = foobar())
  })
  expect_snapshot(error = TRUE, {
    vec_recode_values(1, from = 1, to = 1, ptype = character())
  })
  expect_snapshot(error = TRUE, {
    vec_recode_values(
      1,
      from = 1,
      to = list(a = 1),
      ptype = character(),
      to_as_list_of_vectors = TRUE
    )
  })
  expect_snapshot(error = TRUE, {
    vec_recode_values(1, from = 1, to = "x", default = 1, ptype = character())
  })
})

test_that("`to` size is validated", {
  expect_snapshot(error = TRUE, {
    vec_recode_values(1:5, from = 1, to = 2:3)
  })
  expect_snapshot(error = TRUE, {
    vec_recode_values(
      1:5,
      from = list(1),
      to = 2:3,
      from_as_list_of_vectors = TRUE
    )
  })
  expect_snapshot(error = TRUE, {
    vec_recode_values(
      1:5,
      from = 1,
      to = list(2, 3),
      to_as_list_of_vectors = TRUE
    )
  })
  expect_snapshot(error = TRUE, {
    vec_recode_values(
      1:5,
      from = list(1),
      to = list(2, 3),
      from_as_list_of_vectors = TRUE,
      to_as_list_of_vectors = TRUE
    )
  })
  expect_snapshot(error = TRUE, {
    vec_recode_values(
      1:5,
      from = 1,
      to = list(a = 2:3),
      to_as_list_of_vectors = TRUE
    )
  })
})

test_that("`default` size is validated", {
  expect_snapshot(error = TRUE, {
    vec_recode_values(1:5, from = 1, to = 2, default = 1:2)
  })
})

test_that("`x` must be a vector", {
  expect_snapshot(error = TRUE, {
    vec_recode_values(foobar(), from = 1, to = 2, x_arg = ".x")
  })
})

test_that("`from` must be a vector or list of vectors", {
  expect_snapshot(error = TRUE, {
    vec_recode_values(1, from = foobar(), to = 2, from_arg = ".from")
  })
  expect_snapshot(error = TRUE, {
    vec_recode_values(
      1,
      from = 1,
      to = 2,
      from_as_list_of_vectors = TRUE,
      from_arg = ".from"
    )
  })
  expect_snapshot(error = TRUE, {
    vec_recode_values(
      1,
      from = list(a = foobar()),
      to = 2,
      from_as_list_of_vectors = TRUE,
      from_arg = ".from"
    )
  })
})

test_that("`to` must be a vector or list of vectors", {
  expect_snapshot(error = TRUE, {
    vec_recode_values(1, from = 1, to = foobar(), to_arg = ".to")
  })
  expect_snapshot(error = TRUE, {
    vec_recode_values(
      1,
      from = 1,
      to = 2,
      to_as_list_of_vectors = TRUE,
      to_arg = ".to"
    )
  })
  expect_snapshot(error = TRUE, {
    vec_recode_values(
      1,
      from = 1,
      to = list(a = foobar()),
      to_as_list_of_vectors = TRUE,
      to_arg = ".to"
    )
  })
})

test_that("`default` must be a vector", {
  expect_snapshot(error = TRUE, {
    vec_recode_values(
      1,
      from = 1,
      to = 2,
      default = foobar(),
      default_arg = ".default"
    )
  })
})

test_that("`from_as_list_of_vectors` and `to_as_list_of_vectors` are validated", {
  expect_snapshot(error = TRUE, {
    vec_recode_values(1, from = 1, to = 1, from_as_list_of_vectors = "x")
  })
  expect_snapshot(error = TRUE, {
    vec_recode_values(1, from = 1, to = 1, to_as_list_of_vectors = "x")
  })
  expect_snapshot(error = TRUE, {
    vec_replace_values(1, from = 1, to = 1, from_as_list_of_vectors = "x")
  })
  expect_snapshot(error = TRUE, {
    vec_replace_values(1, from = 1, to = 1, to_as_list_of_vectors = "x")
  })
})

test_that("`unmatched` is validated", {
  expect_snapshot(error = TRUE, {
    vec_recode_values(1, from = 1, to = 1, unmatched = "e")
  })
})

test_that("proof that `ptype` finalization is important", {
  # Imagine you have an input logical vector you are remapping
  # and it happens to only have `NA`s
  x <- c(NA, NA)
  from <- NA
  to <- FALSE

  # If no `ptype` finalization happened, then `ptype = x` would result in
  # `unspecified` being the output type and these would error. `list_combine()`
  # now does `ptype` finalization when an explicit `ptype` is provided, so this
  # works.
  expect_identical(
    vec_recode_values(x, from = from, to = to, default = x, ptype = x),
    c(FALSE, FALSE)
  )
  expect_identical(
    vec_replace_values(x, from = from, to = to),
    c(FALSE, FALSE)
  )
})

test_that("extraneous `to` attributes don't end up on the final output", {
  x <- c(1, 2, 3)

  from <- c(2, 3)
  to <- structure(c(0, -1), foo = "bar")
  expect_identical(
    vec_recode_values(x, from = from, to = to),
    c(NA, 0, -1)
  )

  from <- 2
  to <- list(
    structure(c(0, -1, -2), foo = "bar")
  )
  expect_identical(
    vec_recode_values(x, from = from, to = to, to_as_list_of_vectors = TRUE),
    c(NA, -1, NA)
  )

  # With `ptype2` computation forced by `default`
  from <- c(2, 3)
  to <- structure(c(0, -1), foo = "bar")
  expect_identical(
    vec_recode_values(x, from = from, to = to, default = NA_real_),
    c(NA, 0, -1)
  )

  # With `ptype2` computation forced by multiple `to` values
  from <- c(2, 3)
  to <- list(
    structure(c(0, -1, -2), foo = "bar"),
    c(-3, -4, -5)
  )
  expect_identical(
    vec_recode_values(x, from = from, to = to, to_as_list_of_vectors = TRUE),
    c(NA, -1, -5)
  )
})

test_that("extraneous `x` attributes don't end up on the final output", {
  # Because it is built on `vec_recode_values()`
  x <- structure(1, foo = "bar")

  expect_identical(
    vec_replace_values(x, from = 1, to = 2),
    2
  )
})

test_that("first `from` wins when there are overlaps", {
  # Same as `vec_case_when()`
  expect_identical(
    vec_recode_values(
      x = 1,
      from = c(1, 1),
      to = c("a", "b")
    ),
    "a"
  )
  expect_identical(
    vec_recode_values(
      x = 1,
      from = list(c(1, 3), c(1, 2)),
      to = c("a", "b"),
      from_as_list_of_vectors = TRUE
    ),
    "a"
  )
  expect_identical(
    vec_recode_values(
      x = c(1, 2),
      from = c(1, 1),
      to = list(c("a", "b"), c("c", "d")),
      to_as_list_of_vectors = TRUE
    ),
    c("a", NA)
  )
  expect_identical(
    vec_recode_values(
      x = c(1, 2),
      from = list(c(1, 3), c(1, 2)),
      to = list(c("a", "b"), c("c", "d")),
      from_as_list_of_vectors = TRUE,
      to_as_list_of_vectors = TRUE
    ),
    c("a", "d")
  )
})

test_that("works when `from` is a list of size 1 elements and `to` doesn't simplify", {
  # This is a case where we don't actually build the `from_map` because the
  # `from` size doesn't change as it flattens
  expect_identical(
    vec_recode_values(
      x = c(1, 2),
      from = list(1, 2),
      to = list(c("a", "b"), c("c", "d")),
      from_as_list_of_vectors = TRUE,
      to_as_list_of_vectors = TRUE
    ),
    c("a", "d")
  )
})

test_that("works when `to` is a length >1 vector and every element of `x` is matched by `from`", {
  # This is an optimized case where we directly use the index provided by
  # `vec_match()` to slice `to` with
  expect_identical(
    vec_recode_values(
      x = c(1, 2),
      from = c(2, 1),
      to = c("a", "b")
    ),
    c("b", "a")
  )
})

test_that("data frames - vector `from`, vector `to`", {
  x <- data_frame(a = 1:3, b = 3:5)
  from <- data_frame(a = int(3, 1), b = c(5, 3))

  # Recycling
  to <- data_frame(c = "a", d = "y")
  expect_identical(
    vec_recode_values(x, from = from, to = to),
    data_frame(c = c("a", NA, "a"), d = c("y", NA, "y"))
  )

  to <- data_frame(c = c("a", "b"), d = c("x", "y"))
  expect_identical(
    vec_recode_values(x, from = from, to = to),
    data_frame(c = c("b", NA, "a"), d = c("y", NA, "x"))
  )
  # List `from`, vector `to`

  # List `from`, list `to`
})

test_that("data frames - vector `from`, list `to`", {
  x <- data_frame(a = 1:3, b = 3:5)
  from <- data_frame(a = int(3, 1), b = c(5, 3))

  # Recycling to `from` size and `x` size
  to <- list(
    data_frame(c = "a", d = "y")
  )
  expect_identical(
    vec_recode_values(x, from = from, to = to, to_as_list_of_vectors = TRUE),
    data_frame(c = c("a", NA, "a"), d = c("y", NA, "y"))
  )

  # Recycling to `from` size
  to <- list(
    data_frame(c = c("a", "b", "c"), d = c("x", "y", "z"))
  )
  expect_identical(
    vec_recode_values(x, from = from, to = to, to_as_list_of_vectors = TRUE),
    data_frame(c = c("a", NA, "c"), d = c("x", NA, "z"))
  )

  # Recycling to `x` size
  to <- list(
    data_frame(c = "a", d = "x"),
    data_frame(c = "b", d = "y")
  )
  expect_identical(
    vec_recode_values(x, from = from, to = to, to_as_list_of_vectors = TRUE),
    data_frame(c = c("b", NA, "a"), d = c("y", NA, "x"))
  )
})

test_that("data frames - list `from`, vector `to`", {
  x <- data_frame(a = 1:3, b = 3:5)

  # Recycling `to` to `from` size (this would be strange)
  from <- list(
    data_frame(a = 1L, b = 3L),
    data_frame(a = 3:4, b = 5:6)
  )
  to <- data_frame(c = "a", d = "x")
  expect_identical(
    vec_recode_values(x, from = from, to = to, from_as_list_of_vectors = TRUE),
    data_frame(c = c("a", NA, "a"), d = c("x", NA, "x"))
  )

  from <- list(
    data_frame(a = 1L, b = 3L),
    data_frame(a = 3:4, b = 5:6)
  )
  to <- data_frame(c = c("a", "b"), d = c("x", "y"))
  expect_identical(
    vec_recode_values(x, from = from, to = to, from_as_list_of_vectors = TRUE),
    data_frame(c = c("a", NA, "b"), d = c("x", NA, "y"))
  )
})

test_that("data frames - list `from`, list `to`", {
  x <- data_frame(a = 1:3, b = 3:5)

  # Recycling `to` to `from` size (this would be strange)
  # Recycling `to` elements to `x` size
  from <- list(
    data_frame(a = 1L, b = 3L),
    data_frame(a = 3:4, b = 5:6)
  )
  to <- list(
    data_frame(c = "a", d = "x")
  )
  expect_identical(
    vec_recode_values(
      x,
      from = from,
      to = to,
      from_as_list_of_vectors = TRUE,
      to_as_list_of_vectors = TRUE
    ),
    data_frame(c = c("a", NA, "a"), d = c("x", NA, "x"))
  )

  # Recycling `to` elements to `x` size
  from <- list(
    data_frame(a = 1L, b = 3L),
    data_frame(a = 3:4, b = 5:6)
  )
  to <- list(
    data_frame(c = "a", d = "x"),
    data_frame(c = "b", d = "y")
  )
  expect_identical(
    vec_recode_values(
      x,
      from = from,
      to = to,
      from_as_list_of_vectors = TRUE,
      to_as_list_of_vectors = TRUE
    ),
    data_frame(c = c("a", NA, "b"), d = c("x", NA, "y"))
  )

  from <- list(
    data_frame(a = 1L, b = 3L),
    data_frame(a = 3:4, b = 5:6)
  )
  to <- list(
    data_frame(c = c("a", "b", "c"), d = c("x", "y", "z")),
    data_frame(c = c("aa", "bb", "cc"), d = c("xx", "yy", "zz"))
  )
  expect_identical(
    vec_recode_values(
      x,
      from = from,
      to = to,
      from_as_list_of_vectors = TRUE,
      to_as_list_of_vectors = TRUE
    ),
    data_frame(c = c("a", NA, "cc"), d = c("x", NA, "zz"))
  )
})
