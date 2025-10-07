test_that("`x` must be a list", {
  expect_snapshot(error = TRUE, {
    list_unchop(1, indices = list(1))
  })
  expect_snapshot(error = TRUE, {
    list_unchop(
      1,
      indices = list(1),
      error_call = call("foo"),
      error_arg = "arg"
    )
  })
  expect_snapshot(error = TRUE, {
    list_unchop(data.frame(x = 1), indices = list(1))
  })
})

test_that("`indices` must be a list", {
  expect_snapshot(error = TRUE, {
    list_unchop(list(1), indices = 1)
  })
  expect_snapshot(error = TRUE, {
    list_unchop(list(1), indices = 1, error_call = call("foo"))
  })
  expect_snapshot(error = TRUE, {
    list_unchop(list(1), indices = data.frame(x = 1))
  })
})

test_that("`indices` must be a list of integers", {
  expect_error(
    list_unchop(list(1), indices = list("x")),
    class = "vctrs_error_subscript_type"
  )
  expect_error(
    list_unchop(list(1), indices = list(TRUE)),
    class = "vctrs_error_subscript_type"
  )
  expect_error(
    list_unchop(list(1), indices = list(quote(name))),
    class = "vctrs_error_subscript_type"
  )
})

test_that("`x` and `indices` must be lists of the same size", {
  expect_snapshot(error = TRUE, {
    list_unchop(list(1, 2), indices = list(1))
  })
})

test_that("can unchop with an AsIs list (#1463)", {
  x <- I(list(1, 2))
  expect_identical(list_unchop(x), c(1, 2))
})

test_that("can unchop empty vectors", {
  expect_null(list_unchop(list()))
  expect_null(list_unchop(list(), indices = list()))
  expect_identical(
    list_unchop(list(), indices = list(), ptype = numeric()),
    numeric()
  )
})

test_that("can unchop a list of NULL", {
  expect_null(list_unchop(list(NULL), indices = list(integer())))
  expect_identical(
    list_unchop(list(NULL), indices = list(integer()), ptype = numeric()),
    numeric()
  )
  expect_identical(
    list_unchop(
      list(NULL, NULL),
      indices = list(integer(), integer()),
      ptype = numeric()
    ),
    numeric()
  )
})

test_that("NULLs are ignored when unchopped with other vectors", {
  expect_identical(
    list_unchop(list("a", NULL, "b")),
    c("a", "b")
  )
  expect_identical(
    list_unchop(list("a", NULL, "b"), indices = list(2, integer(), 1)),
    c("b", "a")
  )

  # Homogeneous fallback
  expect_identical(
    list_unchop(list(foobar("a"), NULL, foobar("b"))),
    foobar(c("a", "b"))
  )
  expect_identical(
    list_unchop(
      list(foobar("a"), NULL, foobar("b")),
      indices = list(2, integer(), 1)
    ),
    foobar(c("b", "a"))
  )
  expect_identical(
    list_unchop(
      list(foobar("a"), NULL, foobar("b")),
      indices = list(2, 3, 1)
    ),
    foobar(c("b", "a", NA))
  )

  # Homoegeneous fallback (`NULL` at front)
  expect_identical(
    list_unchop(list(NULL, foobar("a"), foobar("b"))),
    foobar(c("a", "b"))
  )
  expect_identical(
    list_unchop(
      list(NULL, foobar("a"), foobar("b")),
      indices = list(integer(), 2, 1)
    ),
    foobar(c("b", "a"))
  )
  expect_identical(
    list_unchop(
      list(NULL, foobar("a"), foobar("b")),
      indices = list(3, 2, 1)
    ),
    foobar(c("b", "a", NA))
  )

  # `c()` fallback
  with_c_foobar({
    expect_identical(
      list_unchop(list(foobar("a"), NULL, foobar("b"))),
      foobar_c(c("a", "b"))
    )
    expect_identical(
      list_unchop(
        list(foobar("a"), NULL, foobar("b")),
        indices = list(2, integer(), 1)
      ),
      foobar_c(c("b", "a"))
    )
    expect_identical(
      list_unchop(
        list(foobar("a"), NULL, foobar("b")),
        indices = list(2, 3, 1)
      ),
      foobar_c(c("b", "a", NA))
    )
  })

  # `c()` fallback (`NULL` at front)
  with_c_foobar({
    expect_identical(
      list_unchop(list(NULL, foobar("a"), foobar("b"))),
      foobar_c(c("a", "b"))
    )
    expect_identical(
      list_unchop(
        list(NULL, foobar("a"), foobar("b")),
        indices = list(integer(), 2, 1)
      ),
      foobar_c(c("b", "a"))
    )
    expect_identical(
      list_unchop(
        list(NULL, foobar("a"), foobar("b")),
        indices = list(3, 2, 1)
      ),
      foobar_c(c("b", "a", NA))
    )
  })
})

test_that("can use a `NULL` element with a corresponding index", {
  # We've determined this is the behavior we are locked to in `list_unchop()`,
  # but in `list_combine()` we return `unspecified(2)` here because the user
  # also specifies a `size`, making this less ambiguous.
  expect_null(list_unchop(list(NULL), indices = list(1:2)))

  expect_identical(
    list_unchop(list(NULL), indices = list(1:2), ptype = integer()),
    c(NA_integer_, NA_integer_)
  )

  x <- list("a", NULL, c("b", "c"))
  indices <- list(3L, c(1L, 4L), c(2L, 5L))
  expect_identical(list_unchop(x, indices = indices), c(NA, "b", "a", NA, "c"))
})

test_that("can unchop atomic vectors", {
  expect_identical(list_unchop(list(1, 2), indices = list(2, 1)), c(2, 1))
  expect_identical(
    list_unchop(list("a", "b"), indices = list(2, 1)),
    c("b", "a")
  )
})

test_that("can unchop lists", {
  x <- list(list("a", "b"), list("c"))
  indices <- list(c(2, 3), 1)

  expect_identical(list_unchop(x, indices = indices), list("c", "a", "b"))
})

test_that("NA is logical if no other types intervene", {
  expect_identical(
    list_unchop(list(logical()), indices = list(integer())),
    logical()
  )
  expect_identical(
    list_unchop(list(NA), indices = list(1)),
    NA
  )
  expect_identical(
    list_unchop(list(NA, NA), indices = list(1, 2)),
    c(NA, NA)
  )
})

test_that("can unchop data frames of 1 column", {
  indices <- list(c(3, 1), c(2, 4))

  values <- list(
    data_frame(x = 1:2),
    data_frame(x = 3:4)
  )
  expect_identical(
    list_unchop(values, indices = indices),
    data_frame(x = int(2, 3, 1, 4))
  )

  # Homogeneous fallback (#1975)
  values <- list(
    data_frame(x = foobar(1:2)),
    data_frame(x = foobar(3:4))
  )
  expect_identical(
    list_unchop(values, indices = indices),
    data_frame(x = foobar(int(2, 3, 1, 4)))
  )

  # `c()` fallback
  with_c_foobar({
    values <- list(
      data_frame(x = foobar(1:2)),
      data_frame(x = foobar(3:4))
    )
    expect_identical(
      list_unchop(values, indices = indices),
      data_frame(x = foobar_c(int(2, 3, 1, 4)))
    )
  })
})

test_that("can unchop data frames of >1 column", {
  indices <- list(c(3, 1), c(2, 4))

  values <- list(
    data_frame(x = 1:2, y = letters[1:2], z = c(1, 2)),
    data_frame(x = 3:4, y = letters[3:4], z = c(3, 4))
  )
  expect_identical(
    list_unchop(values, indices = indices),
    data_frame(
      x = int(2, 3, 1, 4),
      y = letters[c(2, 3, 1, 4)],
      z = dbl(2, 3, 1, 4),
    )
  )

  # Homogeneous fallback (#1975)
  # Mix of fallback and non-fallback columns
  values <- list(
    data_frame(x = foobar(1:2), y = foobar(letters[1:2]), z = c(1, 2)),
    data_frame(x = foobar(3:4), y = foobar(letters[3:4]), z = c(3, 4))
  )
  expect_identical(
    list_unchop(values, indices = indices),
    data_frame(
      x = foobar(int(2, 3, 1, 4)),
      y = foobar(letters[c(2, 3, 1, 4)]),
      z = dbl(2, 3, 1, 4)
    )
  )

  # `c()` fallback
  # Mix of fallback and non-fallback columns
  with_c_foobar({
    values <- list(
      data_frame(x = foobar(1:2), y = foobar(letters[1:2]), z = c(1, 2)),
      data_frame(x = foobar(3:4), y = foobar(letters[3:4]), z = c(3, 4))
    )
    expect_identical(
      list_unchop(values, indices = indices),
      data_frame(
        x = foobar_c(int(2, 3, 1, 4)),
        y = foobar_c(letters[c(2, 3, 1, 4)]),
        z = dbl(2, 3, 1, 4)
      )
    )
  })
})

test_that("can unchop factors", {
  fctr1 <- factor("z")
  fctr2 <- factor(c("x", "y"))

  x <- list(fctr1, fctr2)
  indices <- list(2, c(3, 1))

  # levels are in the order they are seen!
  expect <- factor(c("y", "z", "x"), levels = c("z", "x", "y"))

  expect_identical(list_unchop(x, indices = indices), expect)
})

test_that("can fallback when unchopping matrices", {
  mat1 <- matrix(1:4, nrow = 2, ncol = 2)
  mat2 <- matrix(5:10, nrow = 3, ncol = 2)

  x <- list(mat1, mat2)
  indices <- list(c(4, 1), c(2, 3, 5))

  expect <- vec_slice(vec_c(mat1, mat2), vec_order(vec_c(!!!indices)))

  expect_identical(list_unchop(x, indices = indices), expect)
  expect_identical(list_unchop(x), vec_c(mat1, mat2))
})

test_that("can fallback when unchopping arrays of >2D", {
  arr1 <- array(1:8, c(2, 2, 2))
  arr2 <- matrix(9:10, c(1, 2))

  x <- list(arr1, arr2)
  indices <- list(c(3, 1), 2)

  expect <- vec_slice(vec_c(arr1, arr2), vec_order(vec_c(!!!indices)))

  expect_identical(list_unchop(x, indices = indices), expect)
  expect_identical(list_unchop(x), vec_c(arr1, arr2))
})

test_that("can unchop with all size 0 elements and get the right ptype", {
  indices <- list(integer(), integer())

  x <- list(integer(), integer())
  expect_identical(list_unchop(x, indices = indices), integer())

  # Homogeneous fallback
  x <- list(foobar(integer()), foobar(integer()))
  expect_identical(list_unchop(x, indices = indices), foobar(integer()))

  # `c()` fallback
  with_c_foobar({
    x <- list(foobar(integer()), foobar(integer()))
    expect_identical(list_unchop(x, indices = indices), foobar_c(integer()))
  })
})

test_that("can unchop with some size 0 elements", {
  x <- list(integer(), 1:2, integer())
  indices <- list(integer(), 2:1, integer())
  expect_identical(list_unchop(x, indices = indices), 2:1)
})

test_that("`NULL` is a valid index", {
  expect_identical(
    list_unchop(list(1, 2), indices = list(NULL, 1)),
    2
  )
  expect_snapshot(error = TRUE, {
    list_unchop(list(1, 2), indices = list(NULL, 2))
  })

  # Homogeneous fallback
  expect_identical(
    list_unchop(list(foobar(1), foobar(2)), indices = list(NULL, 1)),
    foobar(2)
  )
  expect_snapshot(error = TRUE, {
    list_unchop(list(foobar(1), foobar(2)), indices = list(NULL, 2))
  })

  # `c()` fallback
  with_c_foobar({
    expect_identical(
      list_unchop(list(foobar(1), foobar(2)), indices = list(NULL, 1)),
      foobar_c(2)
    )
    expect_snapshot(error = TRUE, {
      list_unchop(list(foobar(1), foobar(2)), indices = list(NULL, 2))
    })
  })
})

test_that("unchopping recycles elements of x to the size of the index", {
  indices <- list(c(3, 4, 5), c(2, 1))

  expect_identical(
    list_unchop(list(1, 2), indices = indices),
    c(2, 2, 1, 1, 1)
  )
  # Homogeneous fallback
  expect_identical(
    list_unchop(list(foobar(1), foobar(2)), indices = indices),
    foobar(c(2, 2, 1, 1, 1))
  )
  # `c()` fallback
  with_c_foobar({
    expect_identical(
      list_unchop(list(foobar(1), foobar(2)), indices = indices),
      foobar_c(c(2, 2, 1, 1, 1))
    )
  })

  indices <- list(1:3)

  expect_snapshot(error = TRUE, {
    list_unchop(list(1:2), indices = indices)
  })
  expect_snapshot(error = TRUE, {
    list_unchop(
      list(1:2),
      indices = indices,
      error_call = call("foo"),
      error_arg = "arg"
    )
  })

  # Homogeneous fallback
  expect_snapshot(error = TRUE, {
    list_unchop(list(foobar(1:2)), indices = indices)
  })
  expect_snapshot(error = TRUE, {
    list_unchop(
      list(foobar(1:2)),
      indices = indices,
      error_call = call("foo"),
      error_arg = "arg"
    )
  })

  # `c()` fallback
  with_c_foobar({
    expect_snapshot(error = TRUE, {
      list_unchop(list(foobar(1:2)), indices = indices)
    })
    expect_snapshot(error = TRUE, {
      list_unchop(
        list(foobar(1:2)),
        indices = indices,
        error_call = call("foo"),
        error_arg = "arg"
      )
    })
  })
})

test_that("unchopping takes the common type", {
  x <- list(1, "a")
  indices <- list(1, 2)

  expect_snapshot({
    (expect_error(
      list_unchop(x, indices = indices),
      class = "vctrs_error_incompatible_type"
    ))
    (expect_error(
      list_unchop(
        x,
        indices = indices,
        error_call = call("foo"),
        error_arg = "arg"
      ),
      class = "vctrs_error_incompatible_type"
    ))
  })

  x <- list(1, 2L)

  expect_type(list_unchop(x, indices = indices), "double")
})

test_that("common type failure uses positional errors", {
  expect_snapshot({
    x <- list(1, a = "x", 2)

    # Looking for `x[[1]]` and `x$a`
    (expect_error(list_unchop(x)))
    (expect_error(list_unchop(x, indices = list(2, 1, 3))))

    # Directed cast should also produce directional errors (#1690)
    (expect_error(list_unchop(x, ptype = double())))
    (expect_error(list_unchop(x, indices = list(2, 1, 3), ptype = double())))

    # Lossy cast
    y <- list(1, a = 2.5)
    (expect_error(list_unchop(y, ptype = integer())))
    (expect_error(list_unchop(y, indices = list(2, 1), ptype = integer())))
  })
})

test_that("can specify a ptype to override common type", {
  indices <- list(1, 2)

  x <- list(1, 2L)
  expect_identical(
    list_unchop(x, indices = indices, ptype = integer()),
    c(1L, 2L)
  )

  x <- list(1.5, 2)
  expect_snapshot({
    (expect_error(list_unchop(x, indices = indices, ptype = integer())))
    (expect_error(list_unchop(
      x,
      indices = indices,
      ptype = integer(),
      error_call = call("foo"),
      error_arg = "arg"
    )))
  })
})

test_that("leaving `indices = NULL` unchops sequentially", {
  x <- list(1:2, 3:5, 6L)
  expect_identical(list_unchop(x), 1:6)

  # Homogeneous fallback
  x <- list(foobar(1:2), foobar(3:5), foobar(6L))
  expect_identical(list_unchop(x), foobar(1:6))

  # `c()` fallback
  with_c_foobar({
    x <- list(foobar(1:2), foobar(3:5), foobar(6L))
    expect_identical(list_unchop(x), foobar_c(1:6))
  })
})

test_that("outer names are kept", {
  x <- list(x = 1, y = 2)
  expect_named(list_unchop(x), c("x", "y"))
  expect_named(list_unchop(x, indices = list(2, 1)), c("y", "x"))

  # Homogeneous fallback
  x <- list(x = foobar(1), y = foobar(2))
  expect_named(list_unchop(x), c("x", "y"))
  expect_named(list_unchop(x, indices = list(2, 1)), c("y", "x"))

  # `c()` fallback (dependent on `c()` implementation)
  with_c_foobar({
    x <- list(x = foobar(1), y = foobar(2))
    expect_identical(list_unchop(x), foobar_c(c(x = 1, y = 2)))
    expect_named(list_unchop(x), c("x", "y"))
    expect_named(list_unchop(x, indices = list(2, 1)), c("y", "x"))
  })
})

test_that("outer names are recycled in the right order", {
  x <- list(x = 1, y = 2)
  expect_error(list_unchop(x, indices = list(c(1, 2), 3)), "Can't merge")
  expect_named(
    list_unchop(x, indices = list(c(1, 3), 2), name_spec = "{outer}_{inner}"),
    c("x_1", "y", "x_2")
  )
  expect_named(
    list_unchop(x, indices = list(c(3, 1), 2), name_spec = "{outer}_{inner}"),
    c("x_2", "y", "x_1")
  )
})

test_that("outer names can be merged with inner names", {
  x <- list(x = c(a = 1), y = c(b = 2))
  expect_error(list_unchop(x), "Can't merge")
  expect_named(list_unchop(x, name_spec = "{outer}_{inner}"), c("x_a", "y_b"))
  expect_named(
    list_unchop(x, indices = list(2, 1), name_spec = "{outer}_{inner}"),
    c("y_b", "x_a")
  )
})

test_that("preserves names when inputs are cast to a common type (#1689)", {
  expect_named(list_unchop(list(c(a = 1)), ptype = integer()), "a")
  expect_named(
    list_unchop(list(c(a = 1)), ptype = integer(), indices = list(1)),
    "a"
  )

  # With name spec
  name_spec <- "{outer}_{inner}"
  expect_named(
    list_unchop(list(foo = c(a = 1)), ptype = integer(), name_spec = name_spec),
    "foo_a"
  )
  expect_named(
    list_unchop(
      list(foo = c(a = 1)),
      ptype = integer(),
      name_spec = name_spec,
      indices = list(1)
    ),
    "foo_a"
  )

  # When `x` elements are recycled, names are also recycled
  x <- list(c(a = 1), c(b = 2))
  indices <- list(1:2, 3:4)
  expect_named(
    list_unchop(x, indices = indices, ptype = integer()),
    c("a", "a", "b", "b")
  )

  expect_named(
    list_unchop(
      list(foo = c(a = 1)),
      ptype = integer(),
      name_spec = "inner"
    ),
    "a"
  )
  expect_named(
    list_unchop(
      list(foo = c(a = 1)),
      ptype = integer(),
      name_spec = "inner",
      indices = list(1)
    ),
    "a"
  )
})

test_that("not all inputs have to be named", {
  x <- list(c(a = 1), 2, c(c = 3))
  indices <- list(2, 1, 3)
  expect_named(list_unchop(x, indices = indices), c("", "a", "c"))

  # Homoegenous fallback
  x <- list(foobar(c(a = 1)), foobar(2), foobar(c(c = 3)))
  indices <- list(2, 1, 3)
  expect_named(list_unchop(x, indices = indices), c("", "a", "c"))

  # `c()` fallback
  with_c_foobar({
    x <- list(foobar(c(a = 1)), foobar(2), foobar(c(c = 3)))
    indices <- list(2, 1, 3)
    out <- list_unchop(x, indices = indices)
    expect_foobar_c(out)
    expect_named(out, c("", "a", "c"))
  })
})

test_that("list_unchop() keeps data frame row names", {
  df1 <- data.frame(x = 1:2, row.names = c("r1", "r2"))
  df2 <- data.frame(x = 3:4, row.names = c("r3", "r4"))

  x <- list(df1, df2)
  indices <- list(c(3, 1), c(2, 4))

  result <- list_unchop(x, indices = indices)
  expect <- c("r2", "r3", "r1", "r4")

  expect_identical(vec_names(result), expect)
})

test_that("individual data frame columns retain vector names", {
  df1 <- data_frame(x = c(a = 1, b = 2))
  df2 <- data_frame(x = c(c = 3))

  x <- list(df1, df2)
  indices <- list(c(1, 2), 3)

  result <- list_unchop(x, indices = indices)

  expect_named(result$x, c("a", "b", "c"))

  # Names should be identical to equivalent `vec_c()` call
  expect_identical(list_unchop(x, indices = indices), vec_c(!!!x))
})

test_that("df-col row names are repaired silently", {
  df1 <- data_frame(x = new_data_frame(list(a = 1), row.names = "inner"))
  df2 <- data_frame(x = new_data_frame(list(a = 2), row.names = "inner"))

  x <- list(df1, df2)
  indices <- list(1, 2)

  expect_silent({
    result <- list_unchop(x, indices = indices)
  })

  expect_identical(vec_names(result$x), c("inner...1", "inner...2"))
})

test_that("monitoring - can technically assign to the same location twice", {
  indices <- list(1:2, 1L)

  x <- list(1:2, 3L)
  expect_identical(
    list_unchop(x, indices = indices),
    c(3L, 2L, NA)
  )

  # Homogeneous fallback
  x <- list(foobar(1:2), foobar(3L))
  expect_identical(
    list_unchop(x, indices = indices),
    foobar(c(3L, 2L, NA))
  )

  # `c()` fallback
  with_c_foobar({
    x <- list(foobar(1:2), foobar(3L))
    expect_identical(
      list_unchop(x, indices = indices),
      foobar_c(c(3L, 2L, NA))
    )
  })
})

test_that("index values are validated", {
  x <- list(1, 2)
  indices1 <- list(4, 1)
  indices2 <- list(c(1, 4), 2)
  indices3 <- list(c(1, 3, 4), 2)

  expect_error(
    list_unchop(x, indices = indices1),
    class = "vctrs_error_subscript_oob"
  )
  expect_error(
    list_unchop(x, indices = indices2),
    class = "vctrs_error_subscript_oob"
  )

  expect_identical(list_unchop(x, indices = indices3), c(1, 2, 1, 1))
})

test_that("name repair is respected and happens after ordering according to `indices`", {
  local_name_repair_quiet()

  x <- list(c(a = 1), c(a = 2))
  indices <- list(2, 1)

  expect_named(list_unchop(x, indices = indices), c("a", "a"))
  expect_named(
    list_unchop(x, indices = indices, name_repair = "unique"),
    c("a...1", "a...2")
  )
})

test_that("list_unchop() can repair names quietly", {
  local_name_repair_verbose()

  x <- c(x = "a", x = "b", x = "c")
  indices <- list(2, c(3, 1))
  expect_snapshot({
    res <- list_unchop(
      vec_chop(x, indices = indices),
      indices = indices,
      name_repair = "unique_quiet"
    )
  })
  expect_named(res, c("x...1", "x...2", "x...3"))

  x <- c("if" = "a", "in" = "b", "for" = "c")
  indices <- list(2, c(3, 1))
  expect_snapshot({
    res <- list_unchop(
      vec_chop(x, indices = indices),
      indices = indices,
      name_repair = "universal_quiet"
    )
  })
  expect_named(res, c(".if", ".in", ".for"))
})

test_that("list_unchop() errors on unsupported location values", {
  expect_snapshot(error = TRUE, cnd_class = TRUE, {
    list_unchop(list(1, 2), indices = list(c(1, 2), 0))
  })
  expect_snapshot(error = TRUE, cnd_class = TRUE, {
    list_unchop(list(1), indices = list(-1))
  })

  # Homogeneous fallback
  expect_snapshot(error = TRUE, cnd_class = TRUE, {
    list_unchop(list(foobar(1), foobar(2)), indices = list(c(1, 2), 0))
  })
  expect_snapshot(error = TRUE, cnd_class = TRUE, {
    list_unchop(list(foobar(1)), indices = list(-1))
  })

  # `c()` fallback
  with_c_foobar({
    expect_snapshot(error = TRUE, cnd_class = TRUE, {
      list_unchop(list(foobar(1), foobar(2)), indices = list(c(1, 2), 0))
    })
    expect_snapshot(error = TRUE, cnd_class = TRUE, {
      list_unchop(list(foobar(1)), indices = list(-1))
    })
  })
})

test_that("missing values propagate", {
  indices <- list(c(NA_integer_, NA_integer_), c(NA_integer_, 3))

  expect_identical(
    list_unchop(
      list(1, 2),
      indices = indices
    ),
    c(NA, NA, 2, NA)
  )

  # Homogenous fallback
  expect_identical(
    list_unchop(
      list(foobar(1), foobar(2)),
      indices = indices
    ),
    foobar(c(NA, NA, 2, NA))
  )

  # `c()` fallback
  with_c_foobar({
    expect_identical(
      list_unchop(
        list(foobar(1), foobar(2)),
        indices = indices
      ),
      foobar_c(c(NA, NA, 2, NA))
    )
  })
})

test_that("list_unchop() works with simple homogeneous foreign S3 classes", {
  values <- list(
    foobar(1:2),
    foobar(3:4)
  )
  indices <- list(c(1, 3), c(2, 4))
  expect_identical(
    list_unchop(values, indices = indices),
    foobar(int(1, 3, 2, 4))
  )

  # And in data frame columns (#1975)
  values <- list(
    data_frame(x = foobar(1:2)),
    data_frame(x = foobar(3:4))
  )
  indices <- list(c(1, 3), c(2, 4))
  expect_identical(
    list_unchop(values, indices = indices),
    data_frame(x = foobar(int(1, 3, 2, 4)))
  )
})

test_that("list_unchop() fails with complex foreign S3 classes", {
  expect_snapshot({
    x <- structure(foobar(1), attr_foo = "foo")
    y <- structure(foobar(2), attr_bar = "bar")
    (expect_error(
      list_unchop(list(x, y)),
      class = "vctrs_error_incompatible_type"
    ))
    (expect_error(
      list_unchop(list(x, y), error_call = call("foo"), error_arg = "arg"),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("list_unchop() fails with complex foreign S4 classes", {
  expect_snapshot({
    joe <- .Counts(c(1L, 2L), name = "Joe")
    jane <- .Counts(3L, name = "Jane")
    (expect_error(
      list_unchop(list(joe, jane)),
      class = "vctrs_error_incompatible_type"
    ))
    (expect_error(
      list_unchop(list(joe, jane), error_call = call("foo"), error_arg = "arg"),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("list_unchop() falls back to c() if S3 method is available", {
  # Check off-by-one error
  expect_error(
    list_unchop(list(foobar(1), "", foobar(2)), indices = list(1, 2, 3)),
    class = "vctrs_error_incompatible_type"
  )

  # Fallback when the class implements `c()`
  local_c_foobar()

  expect_identical(
    list_unchop(list(foobar(1), foobar(2))),
    foobar_c(c(1, 2))
  )
  expect_identical(
    list_unchop(list(foobar(1), foobar(2)), indices = list(1, 2)),
    foobar_c(c(1, 2))
  )
  expect_identical(
    list_unchop(list(foobar(1), foobar(2)), indices = list(2, 1)),
    foobar_c(c(2, 1))
  )
  expect_identical(
    list_unchop(list(NULL, foobar(1), NULL, foobar(2))),
    foobar_c(c(1, 2))
  )

  # OOB error is respected
  expect_error(
    list_unchop(list(foobar(1), foobar(2)), indices = list(1, 3)),
    class = "vctrs_error_subscript_oob"
  )

  # Unassigned locations results in missing values.
  # Repeated assignment uses the last assigned value.
  expect_identical(
    list_unchop(list(foobar(c(1, 2)), foobar(3)), indices = list(c(1, 3), 1)),
    foobar_c(c(3, NA, 2))
  )
  expect_identical(
    list_unchop(list(foobar(c(1, 2)), foobar(3)), indices = list(c(2, NA), NA)),
    foobar_c(c(NA, 1, NA))
  )

  # Names are kept
  expect_identical(
    list_unchop(
      list(foobar(c(x = 1, y = 2)), foobar(c(x = 1))),
      indices = list(c(2, 1), 3)
    ),
    foobar_c(c(y = 2, x = 1, x = 1))
  )

  # Recycles to the size of index
  expect_identical(
    list_unchop(list(foobar(1), foobar(2)), indices = list(c(1, 3), 2)),
    foobar_c(c(1, 2, 1))
  )
  expect_identical(
    list_unchop(list(foobar(1), foobar(2)), indices = list(c(1, 2), integer())),
    foobar_c(c(1, 1))
  )
  expect_snapshot({
    (expect_error(
      list_unchop(
        list(foobar(1), foobar(2)),
        indices = list(c(1, 3), integer())
      ),
      class = "vctrs_error_subscript_oob"
    ))
  })
  expect_snapshot({
    x <- list(foobar(1:2))
    indices <- list(1:3)
    (expect_error(list_unchop(x, indices = indices)))
    (expect_error(list_unchop(
      x,
      indices = indices,
      error_arg = "arg",
      error_call = call("foo")
    )))
  })

  method_vctrs_c_fallback <- function(...) {
    xs <- list(...)
    xs <- map(xs, unclass)
    res <- exec("c", !!!xs)
    structure(res, class = "vctrs_c_fallback")
  }

  # Registered fallback
  s3_register("base::c", "vctrs_c_fallback", method_vctrs_c_fallback)
  expect_identical(
    list_unchop(
      list(
        structure(1, class = "vctrs_c_fallback"),
        structure(2, class = "vctrs_c_fallback")
      ),
      indices = list(2, 1)
    ),
    structure(c(2, 1), class = "vctrs_c_fallback")
  )

  # Don't fallback for S3 lists which are treated as scalars by default
  expect_error(
    list_unchop(list(foobar(list(1)), foobar(list(2)))),
    class = "vctrs_error_scalar_type"
  )
})

test_that("list_unchop() falls back even when ptype is supplied", {
  expect_foobar(
    list_unchop(list(foobar(1), foobar(2)), ptype = foobar(dbl()))
  )

  with_c_quux <- function(expr) {
    with_methods(expr, c.vctrs_foobar = function(...) quux(NextMethod()))
  }

  with_c_quux({
    expect_quux(
      list_unchop(
        list(foobar(1), foobar(2)),
        indices = list(1, 2),
        ptype = foobar(dbl())
      )
    )
  })

  with_c_quux({
    expect_quux(
      list_unchop(
        list(foobar(1, foo = TRUE), foobar(2, bar = TRUE)),
        indices = list(1, 2),
        ptype = foobar(dbl())
      )
    )
  })
})

test_that("list_unchop() falls back for S4 classes with a registered c() method", {
  joe <- .Counts(c(1L, 2L), name = "Joe")
  jane <- .Counts(3L, name = "Jane")

  expect_snapshot({
    (expect_error(
      list_unchop(list(joe, 1, jane), indices = list(c(1, 2), 3, 4)),
      class = "vctrs_error_incompatible_type"
    ))
  })

  local_c_counts()

  expect_identical(
    list_unchop(list(joe, jane), indices = list(c(1, 3), 2)),
    .Counts(c(1L, 3L, 2L), name = "Dispatched")
  )

  expect_identical(
    list_unchop(list(NULL, joe, jane), indices = list(integer(), c(1, 3), 2)),
    .Counts(c(1L, 3L, 2L), name = "Dispatched")
  )

  # Unassigned locations results in missing values.
  # Repeated assignment uses the last assigned value.
  expect_identical(
    list_unchop(list(joe, jane), indices = list(c(1, 3), 1)),
    .Counts(c(3L, NA, 2L), name = "Dispatched")
  )
  expect_identical(
    list_unchop(list(joe, jane), indices = list(c(2, NA), NA)),
    .Counts(c(NA, 1L, NA), name = "Dispatched")
  )
})

test_that("list_unchop() fallback doesn't support (most) `name_spec` or `ptype`", {
  local_c_foobar()

  foo <- structure(foobar(1), foo = "foo")
  bar <- structure(foobar(2), bar = "bar")

  expect_snapshot(error = TRUE, {
    list_unchop(
      list(foo, bar),
      indices = list(1, 2),
      name_spec = "{outer}_{inner}"
    )
  })
  expect_snapshot(error = TRUE, {
    list_unchop(
      list(foo, bar),
      indices = list(1, 2),
      name_spec = "{outer}_{inner}",
      error_call = call("foo")
    )
  })

  # Used to be an error about `ptype`
  x <- list(foobar(1))
  expect_snapshot(error = TRUE, {
    list_unchop(x, indices = list(1), ptype = "")
  })
})

test_that("list_unchop() fallback does support `name_spec = 'inner'`", {
  # Because of how useful it is, and how easy it is to implement!
  expect_identical(
    with_c_foobar(list_unchop(
      list(foobar(1), foobar(2)),
      indices = list(1, 2),
      name_spec = "inner"
    )),
    foobar_c(c(1, 2))
  )
  expect_identical(
    with_c_foobar(list_unchop(
      list(x = foobar(1), y = foobar(2)),
      indices = list(1, 2),
      name_spec = "inner"
    )),
    foobar_c(c(1, 2))
  )
  expect_identical(
    with_c_foobar(list_unchop(
      list(
        x = foobar(c(a = 1)),
        y = foobar(c(b = 2)),
        z = foobar(3)
      ),
      indices = list(1, 2, 3),
      name_spec = "inner"
    )),
    foobar_c(c(a = 1, b = 2, 3))
  )
})

test_that("list_unchop() supports numeric S3 indices", {
  local_methods(
    vec_ptype2.vctrs_foobar = function(x, y, ...) {
      UseMethod("vec_ptype2.vctrs_foobar")
    },
    vec_ptype2.vctrs_foobar.integer = function(x, y, ...) foobar(integer()),
    vec_cast.integer.vctrs_foobar = function(x, to, ...) vec_data(x)
  )

  expect_identical(list_unchop(list(1), indices = list(foobar(1L))), 1)
})

test_that("list_unchop() does not support non-numeric S3 indices", {
  expect_snapshot({
    (expect_error(
      list_unchop(list(1), indices = list(factor("x"))),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      list_unchop(list(1), indices = list(foobar(1L))),
      class = "vctrs_error_subscript_type"
    ))
  })
})

test_that("can ignore names in `list_unchop()` by providing a `zap()` name-spec (#232)", {
  expect_snapshot({
    (expect_error(list_unchop(list(a = c(b = 1:2)))))
    (expect_error(list_unchop(list(a = c(b = 1:2)), error_call = call("foo"))))
  })

  expect_identical(
    list_unchop(list(a = c(b = 1:2), b = 3L), name_spec = zap()),
    1:3
  )
  expect_identical(
    list_unchop(
      list(a = c(foo = 1:2), b = c(bar = 3L)),
      indices = list(2:1, 3),
      name_spec = zap()
    ),
    c(2L, 1L, 3L)
  )

  expect_snapshot({
    x <- list(a = c(b = letters), b = 3L)
    (expect_error(
      list_unchop(x, name_spec = zap()),
      class = "vctrs_error_incompatible_type"
    ))

    x <- list(a = c(foo = 1:2), b = c(bar = ""))
    (expect_error(
      list_unchop(
        x,
        indices = list(2:1, 3),
        name_spec = zap()
      ),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("can ignore outer names in `list_unchop()` by providing a 'inner' name-spec (#1988)", {
  expect_identical(
    list_unchop(
      list(x = c(a = 1, 2), y = c(3, b = 4)),
      indices = list(c(3, 1), c(2, 4)),
      name_spec = "inner"
    ),
    c(2, 3, a = 1, b = 4)
  )

  # Importantly, outer names are still used in error messages!
  expect_snapshot(error = TRUE, {
    list_unchop(
      list(x = c(a = 1), y = c(b = "2")),
      indices = list(1, 2),
      name_spec = "inner"
    )
  })
})

test_that("list_unchop() falls back to c() methods (#1120)", {
  expect_error(
    list_unchop(list(foobar(1), foobar(2, class = "foo"))),
    class = "vctrs_error_incompatible_type"
  )

  local_methods(
    c.vctrs_foobar = function(...) {
      out <- NextMethod()
      paste0(rep_along(out, "dispatched"), seq_along(out))
    }
  )

  # Homogeneous subclasses
  xs <- list(foobar(1), foobar(2, class = "foo"))

  expect_identical(
    list_unchop(xs),
    c("dispatched1", "dispatched2")
  )
  expect_identical(
    list_unchop(xs, indices = list(2, 1)),
    c("dispatched2", "dispatched1")
  )

  # Different subclasses
  xs <- list(
    foobar(c(x = 1, y = 2), class = "foo"),
    foobar(c(x = 1), foo = 1)
  )

  expect_identical(
    list_unchop(xs),
    c("dispatched1", "dispatched2", "dispatched3")
  )
  expect_identical(
    list_unchop(xs, indices = list(c(2, 1), 3)),
    c("dispatched2", "dispatched1", "dispatched3")
  )
})

test_that("list_unchop() fails if foreign classes are not homogeneous and there is no c() method", {
  xs <- list(
    foobar(c(x = 1, y = 2), class = "foo"),
    foobar(c(x = 1), foo = 1)
  )
  expect_error(
    list_unchop(xs),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    list_unchop(xs, indices = list(c(2, 1), 3)),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("calls cast method even with empty objects", {
  # https://github.com/paleolimbot/wk/issues/230

  # There is a common type, but the cast method is intended
  # to fail here for this test
  local_methods(
    vec_ptype2.vctrs_foobar.vctrs_foobar = function(x, y, ...) {
      x
    },
    vec_cast.vctrs_foobar.default = function(x, to, ...) {
      vec_default_cast(x, to)
    }
  )

  expect_snapshot(error = TRUE, {
    list_unchop(
      list(
        foobar(integer()),
        foobar(integer(), foo = "bar")
      ),
      indices = list(
        integer(),
        integer()
      )
    )
  })
})

test_that("Size 1 unspecified `NA` that isn't used doesn't error (#1989)", {
  # Works because we don't actually recycle `NA` to size 0 in the loop, we
  # just check that it can recycle. If we recycled to `logical()`, it would
  # no longer look unspecified and an error would be thrown instead.
  expect_identical(
    list_unchop(
      list("x", NA),
      indices = list(1L, integer())
    ),
    "x"
  )
  expect_identical(
    list_unchop(
      list("x", NA),
      indices = list(integer(), 1L)
    ),
    NA_character_
  )
})

test_that("list_unchop() and vec_c() are consistent-ish regarding `size` and empty inputs (#1980)", {
  x <- list()
  indices <- list()

  # These should be consistent and return `NULL` when no inputs are provided.
  # We treat this roughly equivalent to `unspecified(0)`.
  expect_identical(vec_c(), NULL)
  expect_identical(vec_c(), list_unchop(x, indices = indices))

  # These should be consistent and return `NULL` when no inputs are provided.
  # We treat this roughly equivalent to `unspecified(0)`.
  expect_identical(vec_c(NULL), NULL)
  expect_identical(
    vec_c(NULL),
    list_unchop(list(NULL), indices = list(integer()))
  )

  # This is ambiguous but we let this return `NULL` as well.
  # `list_combine()` doesn't have this ambiguity
  expect_null(list_unchop(list(NULL), indices = list(1:2)))
  expect_identical(
    list_combine(list(NULL), indices = list(1:2), size = 2),
    unspecified(2)
  )

  # These should be consistent and return size 0 `ptype`
  expect_identical(vec_c(.ptype = integer()), integer())
  expect_identical(
    vec_c(.ptype = integer()),
    list_unchop(x, indices = indices, ptype = integer())
  )
})
