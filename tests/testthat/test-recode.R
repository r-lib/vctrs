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

  # Naive implementation of `vec_replace_values()` errors here.
  # This is possibly our fault, because this seems wrong:
  # `vec_ptype_common(NA, NA) == logical()`
  # `vec_ptype_common(.ptype = NA) == unspecified()`
  expect_snapshot(error = TRUE, {
    vec_recode_values(x, from = from, to = to, default = x, ptype = x)
  })

  expect_identical(
    vec_replace_values(x, from = from, to = to),
    c(FALSE, FALSE)
  )
})
