test_that("works with data frames", {
  cases <- list(
    c(FALSE, TRUE, FALSE, FALSE),
    c(TRUE, TRUE, FALSE, FALSE),
    c(FALSE, TRUE, FALSE, TRUE)
  )
  values <- list(
    data_frame(x = 1, y = 2),
    data_frame(x = 3, y = 4),
    data_frame(x = 3:6, y = 4:7)
  )

  out <- vec_case_when(cases, values)

  expect_identical(
    out,
    data_frame(
      x = c(3, 1, NA, 6),
      y = c(4, 2, NA, 7)
    )
  )
})

test_that("first `TRUE` case wins", {
  cases <- list(
    c(TRUE, FALSE),
    c(TRUE, TRUE),
    c(TRUE, TRUE)
  )
  values <- list(
    1,
    2,
    3
  )

  expect_identical(
    vec_case_when(cases, values),
    c(1, 2)
  )
})

test_that("can replace missing values by catching with `is.na()`", {
  x <- c(1:3, NA)

  cases <- list(
    x <= 1,
    x <= 2,
    is.na(x)
  )
  values <- list(
    1,
    2,
    0
  )

  expect_identical(
    vec_case_when(cases, values),
    c(1, 2, NA, 0)
  )
})

test_that("Unused logical `NA` can still be cast to `values` ptype", {
  # Requires that casting happen before recycling, because it recycles
  # to size zero, resulting in a logical rather than an unspecified.
  expect_identical(
    vec_case_when(list(TRUE, FALSE), list("x", NA)),
    "x"
  )
  expect_identical(
    vec_case_when(list(FALSE, TRUE), list("x", NA)),
    NA_character_
  )
})

test_that("`cases` inputs can be size zero", {
  expect_identical(
    vec_case_when(
      list(logical(), logical()),
      list(1, 2)
    ),
    numeric()
  )

  expect_snapshot(error = TRUE, {
    vec_case_when(list(logical()), list(1:2))
  })
})

test_that("retains inner names of `values` inputs", {
  value1 <- c(x = 1, y = 2)
  value2 <- c(z = 3, w = 4)

  out <- vec_case_when(
    list(c(TRUE, FALSE), c(TRUE, TRUE)),
    list(a = value1, b = value2)
  )

  expect_named(out, c("x", "w"))
})

test_that("outer names have no affect over the output names", {
  value1 <- c(1, 2)
  value2 <- c(3, 4)

  out <- vec_case_when(
    list(c(TRUE, FALSE), c(FALSE, TRUE)),
    list(x = value1, y = value2)
  )
  expect_named(out, NULL)
})

test_that("`values` are cast to their common type", {
  expect_identical(vec_case_when(list(FALSE, TRUE), list(1, 2L)), 2)
  expect_identical(vec_case_when(list(FALSE, TRUE), list(1, NA)), NA_real_)

  expect_snapshot(error = TRUE, {
    vec_case_when(list(FALSE, TRUE), list(1, "x"))
  })
})

test_that("`values` must be size 1 or same size as the `cases`", {
  expect_identical(
    vec_case_when(
      list(c(TRUE, TRUE)),
      list(1)
    ),
    c(1, 1)
  )
  expect_identical(
    vec_case_when(
      list(c(TRUE, FALSE), c(TRUE, TRUE)),
      list(c(1, 2), c(3, 4))
    ),
    c(1, 4)
  )

  expect_snapshot(error = TRUE, {
    vec_case_when(
      list(c(TRUE, FALSE, TRUE, TRUE)),
      list(1:3)
    )
  })
})

test_that("Unhandled `NA` are given a value of `default`", {
  expect_identical(
    vec_case_when(list(NA), list(1)),
    NA_real_
  )

  expect_identical(
    vec_case_when(list(NA), list(1), default = 2),
    2
  )

  expect_identical(
    vec_case_when(
      list(
        c(FALSE, NA, TRUE, FALSE),
        c(NA, FALSE, TRUE, FALSE)
      ),
      list(
        2,
        3
      ),
      default = 4
    ),
    c(4, 4, 2, 4)
  )
})

test_that("`NA` is overridden by any `TRUE` values", {
  x <- c(1, 2, NA, 3)
  expect <- c("one", "not_one", "missing", "not_one")

  # `TRUE` overriding before the `NA`
  cases <- list(
    is.na(x),
    x == 1
  )
  values <- list(
    "missing",
    "one"
  )
  expect_identical(
    vec_case_when(
      cases,
      values,
      default = "not_one"
    ),
    expect
  )

  # `TRUE` overriding after the `NA`
  cases <- list(
    x == 1,
    is.na(x)
  )
  values <- list(
    "one",
    "missing"
  )
  expect_identical(
    vec_case_when(
      cases,
      values,
      default = "not_one"
    ),
    expect
  )
})

test_that("works when there is a used `default` and no missing values", {
  expect_identical(
    vec_case_when(list(c(TRUE, FALSE)), list(1), default = 3:4),
    c(1, 4)
  )
})

test_that("works when there are missing values but no `default`", {
  expect_identical(vec_case_when(list(c(TRUE, NA)), list(1)), c(1, NA))
})

test_that("A `NULL` `default` fills in with missing values", {
  expect_identical(
    vec_case_when(list(c(TRUE, FALSE, FALSE)), list(1)),
    c(1, NA, NA)
  )
})

test_that("`default` fills in all unused slots", {
  expect_identical(
    vec_case_when(list(c(TRUE, FALSE, FALSE)), list(1), default = 2),
    c(1, 2, 2)
  )
})

test_that("`default` is initialized correctly in the logical / unspecified case", {
  # i.e. `vec_ptype(NA)` is unspecified but the result should be finalized to logical
  expect_identical(vec_case_when(list(FALSE), list(NA)), NA)
})

test_that("`default` can be vectorized, and is sliced to fit as needed", {
  out <- vec_case_when(
    list(
      c(FALSE, TRUE, FALSE, TRUE, FALSE),
      c(FALSE, TRUE, FALSE, FALSE, TRUE)
    ),
    list(
      1:5,
      6:10
    ),
    default = 11:15
  )

  expect_identical(out, c(11L, 2L, 13L, 4L, 10L))
})

test_that("`default` must be size 1 or same size as `cases` (exact same as any other `values` input)", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(FALSE), list(1L), default = 2:3)
  })
})

test_that("`default` participates in common type determination (exact same as any other `values` input)", {
  expect_identical(vec_case_when(list(FALSE), list(1L), default = 2), 2)
})

test_that("`default` that is an unused logical `NA` can still be cast to `values` ptype", {
  # Requires that casting happen before recycling, because it recycles
  # to size zero, resulting in a logical rather than an unspecified.
  expect_identical(vec_case_when(list(TRUE), list("x"), default = NA), "x")
})

test_that("`default_arg` can be customized", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(FALSE), list(1L), default = 2:3, default_arg = "foo")
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(FALSE), list(1L), default = "x", default_arg = "foo")
  })
})

test_that("`cases_arg` is validated", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list("x"), list(1), cases_arg = 1)
  })
})

test_that("`values_arg` is validated", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(lm(1 ~ 1)), values_arg = 1)
  })
})

test_that("`default_arg` is validated", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(1), default = "x", default_arg = 1)
  })
})

test_that("`cases` must all be the same size", {
  expect_snapshot(error = TRUE, {
    vec_case_when(
      list(c(TRUE, FALSE), TRUE),
      list(1, 2)
    )
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(
      list(c(TRUE, FALSE), c(TRUE, FALSE, TRUE)),
      list(1, 2)
    )
  })
})

test_that("`cases` must be logical (and aren't cast to logical!)", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(1), list(2))
  })

  # Make sure input numbering is right in the error message!
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE, 3.5), list(2, 4))
  })

  # `vec_as_location()` would not allow this either
  x <- structure(c(FALSE, TRUE), class = "my_logical")
  expect_snapshot(error = TRUE, {
    vec_case_when(list(x), list(1), default = 2)
  })
})

test_that("`cases` are allowed to have attributes", {
  x <- structure(c(FALSE, TRUE), label = "foo")
  expect_identical(vec_case_when(list(x), list(1), default = 2), c(2, 1))
})

test_that("`cases` can't be arrays (#6862)", {
  x <- array(TRUE, dim = c(3, 3))
  y <- c("a", "b", "c")

  expect_snapshot(error = TRUE, {
    vec_case_when(list(x), list(y))
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(x), list(y), size = 3)
  })

  # Not even 1D arrays
  x <- array(TRUE, dim = 3)

  expect_snapshot(error = TRUE, {
    vec_case_when(list(x), list(y))
  })
})

test_that("`size` overrides the `cases` sizes", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(1), size = 5)
  })

  expect_snapshot(error = TRUE, {
    vec_case_when(
      list(c(TRUE, FALSE), c(TRUE, FALSE, TRUE)),
      list(1, 2),
      size = 2
    )
  })
})

test_that("0 `cases` result depends on `size` and `default` and `ptype`", {
  expect_identical(
    vec_case_when(
      cases = list(),
      values = list()
    ),
    unspecified()
  )

  expect_identical(
    vec_case_when(
      cases = list(),
      values = list(),
      size = 0
    ),
    unspecified()
  )
  expect_identical(
    vec_case_when(
      cases = list(),
      values = list(),
      size = 2
    ),
    unspecified(2)
  )

  expect_identical(
    vec_case_when(
      cases = list(),
      values = list(),
      default = integer()
    ),
    integer()
  )
  expect_identical(
    vec_case_when(
      cases = list(),
      values = list(),
      default = 1L
    ),
    integer()
  )
  expect_identical(
    vec_case_when(
      cases = list(),
      values = list(),
      ptype = integer()
    ),
    integer()
  )

  expect_identical(
    vec_case_when(
      cases = list(),
      values = list(),
      size = 2L,
      default = 1L
    ),
    c(1L, 1L)
  )
  expect_identical(
    vec_case_when(
      cases = list(),
      values = list(),
      size = 2L,
      default = 1:2
    ),
    c(1L, 2L)
  )
  expect_identical(
    vec_case_when(
      cases = list(),
      values = list(),
      size = 2L,
      ptype = integer()
    ),
    c(NA_integer_, NA_integer_)
  )
})

test_that("`vec_replace_when()` with empty `cases` is a no-op", {
  x <- 1:5

  expect_identical(
    vec_replace_when(x, cases = list(), values = list()),
    x
  )
})

test_that("`ptype` overrides the `values` types", {
  expect_identical(
    vec_case_when(list(FALSE, TRUE), list(1, 2), ptype = integer()),
    2L
  )

  expect_snapshot(error = TRUE, {
    vec_case_when(list(FALSE, TRUE), list(1, 2), ptype = character())
  })
})

test_that("number of `cases` and `values` must be the same", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list())
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE, TRUE), list(1))
  })
})

test_that("dots must be empty", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(1), 2)
  })
})

test_that("`cases` must be a list", {
  expect_snapshot(error = TRUE, {
    vec_case_when(1, list(2))
  })
})

test_that("`values` must be a list", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), 1)
  })
})

test_that("named inputs show up in the error message", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(x = 1.5), list(1))
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(x = 1.5), list(1), cases_arg = "foo")
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(x = 1.5), list(1), cases_arg = "")
  })

  expect_snapshot(error = TRUE, {
    vec_case_when(list(x = TRUE, y = c(TRUE, FALSE)), list(1, 2))
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(
      list(x = TRUE, y = c(TRUE, FALSE)),
      list(1, 2),
      cases_arg = "foo"
    )
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(
      list(x = TRUE, y = c(TRUE, FALSE)),
      list(1, 2),
      cases_arg = ""
    )
  })

  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE, FALSE), list(1, x = "y"))
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE, FALSE), list(1, x = "y"), values_arg = "foo")
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE, FALSE), list(1, x = "y"), values_arg = "")
  })

  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(NULL))
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(x = NULL))
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(NULL), values_arg = "foo")
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(x = NULL), values_arg = "foo")
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(NULL), values_arg = "")
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(x = NULL), values_arg = "")
  })
})

test_that("proof that `ptype` finalization is important", {
  # Imagine you have an input logical vector you are remapping
  # and it happens to only have `NA`s
  x <- c(NA, NA)
  cases <- list(x %in% NA)
  values <- list(FALSE)

  # If no `ptype` finalization happened, then `ptype = x` would result in
  # `unspecified` being the output type and these would error. `list_combine()`
  # now does `ptype` finalization when an explicit `ptype` is provided, so this
  # works.
  expect_identical(
    vec_case_when(cases, values, default = x, ptype = x),
    c(FALSE, FALSE)
  )
  expect_identical(
    vec_replace_when(x, cases, values),
    c(FALSE, FALSE)
  )
})

test_that("`unmatched` errors are correct", {
  cases <- list(
    c(TRUE, FALSE, TRUE, FALSE, FALSE, NA, NA, NA, TRUE),
    c(FALSE, TRUE, TRUE, FALSE, NA, FALSE, NA, TRUE, NA)
  )
  values <- list(
    1,
    2
  )
  expect_snapshot(error = TRUE, {
    vec_case_when(cases, values, unmatched = "error")
  })
})
