test_that("basic `list_combine()` works", {
  values <- list(1:2, 3:4)
  indices <- list(c(4, 1), c(3, 2))
  size <- 4

  expect_identical_list_combine(
    x = values,
    indices = indices,
    size = size,
    expect = c(2L, 4L, 3L, 1L)
  )
})

test_that("`list_combine()` works with homogeneous fallback in `default`", {
  # This errors
  expect_snapshot(error = TRUE, {
    list_combine(
      list(foobar(1), 1),
      indices = list(1, 2),
      size = 2
    )
  })

  # So this should too
  expect_snapshot(error = TRUE, {
    list_combine(
      list(foobar(1)),
      indices = list(1),
      size = 2,
      default = 1
    )
  })
})

test_that("`list_combine()` returns `unspecified` to retain `size` invariant", {
  expect_identical(
    list_combine(list(), indices = list(), size = 0),
    unspecified()
  )
  expect_identical(
    list_combine(list(), indices = list(), size = 1),
    unspecified(1)
  )
  expect_identical(
    list_combine(list(), indices = list(), size = 2),
    unspecified(2)
  )

  # With only `NULL` elements, still no known `ptype`
  expect_identical(
    list_combine(list(NULL), indices = list(integer()), size = 0),
    unspecified(0)
  )
  expect_identical(
    list_combine(list(NULL), indices = list(integer()), size = 2),
    unspecified(2)
  )
  expect_identical(
    list_combine(list(NULL), indices = list(1:2), size = 2),
    unspecified(2)
  )

  # Now ptype is known
  expect_identical(
    list_combine(
      list(NULL),
      indices = list(integer()),
      ptype = numeric(),
      size = 1
    ),
    vec_init(numeric(), n = 1)
  )
})

test_that("`NULL`s are removed after computing `default`'s index", {
  expect_identical_list_combine(
    x = list(1:2, NULL, 7:8),
    indices = list(1:2, 5:6, 7:8),
    size = 8,
    default = 0L,
    expect = int(1, 2, 0, 0, NA, NA, 7, 8)
  )
})

test_that("can combine factors", {
  fctr1 <- factor("z")
  fctr2 <- factor(c("x", "y"))

  x <- list(fctr1, fctr2)
  indices <- list(2, c(3, 1))

  # levels are in the order they are seen!
  expect <- factor(c("y", "z", "x"), levels = c("z", "x", "y"))

  expect_identical(list_combine(x, indices = indices, size = 3), expect)

  # With `default`, the `default` is added in as the last item
  default <- factor("w")
  expect <- factor(c("y", "z", "x"), levels = c("z", "x", "y", "w"))
  expect_identical(
    list_combine(x, indices = indices, size = 3, default = default),
    expect
  )
})

test_that("preserves names when inputs are cast to a common type (#1689)", {
  expect_named(
    list_combine(
      list(c(a = 1)),
      indices = list(1),
      ptype = integer(),
      size = 1
    ),
    "a"
  )
  expect_named(
    list_combine(
      list(c(a = 1)),
      indices = list(TRUE),
      ptype = integer(),
      size = 1
    ),
    "a"
  )

  # With `default`
  expect_named(
    list_combine(
      list(c(a = 1)),
      indices = list(1),
      default = c(b = 0),
      ptype = integer(),
      size = 2
    ),
    c("a", "b")
  )
  expect_named(
    list_combine(
      list(c(a = 1)),
      indices = list(c(TRUE, FALSE)),
      default = c(b = 0),
      ptype = integer(),
      size = 2
    ),
    c("a", "b")
  )

  # With name spec
  name_spec <- "{outer}_{inner}"
  expect_named(
    list_combine(
      list(foo = c(a = 1)),
      indices = list(1),
      size = 1,
      ptype = integer(),
      name_spec = name_spec
    ),
    "foo_a"
  )
  expect_named(
    list_combine(
      list(foo = c(a = 1)),
      indices = list(1),
      default = c(b = 0),
      ptype = integer(),
      name_spec = name_spec,
      size = 2
    ),
    c("foo_a", "b")
  )
  expect_named(
    list_combine(
      list(foo = c(a = 1)),
      indices = list(c(TRUE, FALSE)),
      default = c(b = 0),
      ptype = integer(),
      name_spec = name_spec,
      size = 2
    ),
    c("foo_a", "b")
  )

  # When `x` elements are recycled, names are also recycled
  x <- list(c(a = 1), c(b = 2))
  expect_named(
    list_combine(
      x,
      indices = list(1:2, 3:4),
      size = 4,
      ptype = integer()
    ),
    c("a", "a", "b", "b")
  )
  expect_named(
    list_combine(
      x,
      indices = list(
        c(TRUE, TRUE, FALSE, FALSE),
        c(FALSE, FALSE, TRUE, TRUE)
      ),
      size = 4,
      ptype = integer()
    ),
    c("a", "a", "b", "b")
  )

  # When `default` elements are recycled, names are also recycled
  expect_named(
    list_combine(
      list(c(a = 1), c(b = 2)),
      indices = list(1, 3),
      default = c(c = 0),
      ptype = integer(),
      size = 4
    ),
    c("a", "c", "b", "c")
  )
  expect_named(
    list_combine(
      list(c(a = 1), c(b = 2)),
      indices = list(
        c(TRUE, FALSE, FALSE, FALSE),
        c(FALSE, FALSE, TRUE, FALSE)
      ),
      default = c(c = 0),
      ptype = integer(),
      size = 4
    ),
    c("a", "c", "b", "c")
  )
})

test_that("not all inputs have to be named", {
  x <- list(c(a = 1), 2, c(c = 3))
  indices <- list(2, 1, 3)

  expect_identical_list_combine(
    x = x,
    indices = indices,
    size = 3,
    expect = set_names(c(2, 1, 3), c("", "a", "c"))
  )
  expect_identical_list_combine(
    x = x,
    indices = indices,
    size = 4,
    default = 0,
    expect = set_names(c(2, 1, 3, 0), c("", "a", "c", ""))
  )
})

test_that("list_combine() keeps data frame row names", {
  df1 <- data.frame(x = 1:2, row.names = c("r1", "r2"))
  df2 <- data.frame(x = 3:4, row.names = c("r3", "r4"))
  x <- list(df1, df2)
  indices <- list(c(3, 1), c(2, 4))

  result <- list_combine(x, indices = indices, size = sum(lengths(indices)))
  expect <- c("r2", "r3", "r1", "r4")
  expect_identical(vec_names(result), expect)

  default <- data.frame(x = 0L, row.names = "d")
  result <- list_combine(x, indices = indices, size = 5, default = default)
  expect <- c("r2", "r3", "r1", "r4", "d")
  expect_identical(vec_names(result), expect)

  # With casting
  ptype <- data.frame(x = double())
  default <- data.frame(x = 0L, row.names = "d")
  result <- list_combine(
    x = x,
    indices = indices,
    size = 5,
    default = default,
    ptype = ptype
  )
  expect <- c("r2", "r3", "r1", "r4", "d")
  expect_identical(vec_names(result), expect)
  expect_type(result$x, "double")
})

test_that("df-col row names are repaired silently", {
  df1 <- data_frame(x = new_data_frame(list(a = 1), row.names = "inner"))
  df2 <- data_frame(x = new_data_frame(list(a = 2), row.names = "inner"))
  x <- list(df1, df2)
  indices <- list(1, 2)
  expect_silent({
    result <- list_combine(x, indices = indices, size = 2)
  })
  expect_identical(vec_names(result$x), c("inner...1", "inner...2"))

  default <- data_frame(x = new_data_frame(list(a = 0), row.names = "inner"))
  expect_silent({
    result <- list_combine(x, indices = indices, size = 3, default = default)
  })
  expect_identical(
    vec_names(result$x),
    c("inner...1", "inner...2", "inner...3")
  )
})

test_that("name repair is respected and happens after ordering according to `indices`", {
  local_name_repair_quiet()

  x <- list(c(a = 1), c(a = 2))

  indices <- list(2, 1)
  expect_named(list_combine(x, indices = indices, size = 2), c("a", "a"))
  expect_named(
    list_combine(x, indices = indices, size = 2, name_repair = "unique"),
    c("a...1", "a...2")
  )

  # With `default`
  indices <- list(3, 1)
  default <- c(a = 0)
  expect_named(
    list_combine(x, indices = indices, size = 3, default = default),
    c("a", "a", "a")
  )
  expect_named(
    list_combine(
      x,
      indices = indices,
      size = 3,
      default = default,
      name_repair = "unique"
    ),
    c("a...1", "a...2", "a...3")
  )
})

test_that("list_combine() works with simple homogeneous foreign S3 classes", {
  expect_identical(
    list_combine(
      list(foobar(1), foobar(2)),
      indices = list(1, 2),
      size = 2
    ),
    vec_c(foobar(c(1, 2)))
  )

  # With `default`
  expect_identical(
    list_combine(
      list(foobar(1), foobar(2)),
      indices = list(3, 1),
      size = 4,
      default = foobar(0)
    ),
    vec_c(foobar(c(2, 0, 1, 0)))
  )
})

test_that("list_combine() fails with complex foreign S3 classes", {
  expect_snapshot(error = TRUE, {
    x <- structure(foobar(1), attr_foo = "foo")
    y <- structure(foobar(2), attr_bar = "bar")
    list_combine(list(x, y), indices = list(1, 2), size = 2)
  })
  expect_snapshot(error = TRUE, {
    x <- structure(foobar(1), attr_foo = "foo")
    y <- structure(foobar(2), attr_bar = "bar")
    list_combine(
      list(x, y),
      indices = list(1, 2),
      size = 2,
      error_call = call("foo"),
      x_arg = "arg"
    )
  })

  # Want this to mention `default` by name
  expect_snapshot(error = TRUE, {
    x <- structure(foobar(1), attr_foo = "foo")
    default <- structure(foobar(2), attr_foo = "bar")
    list_combine(
      list(x),
      indices = list(1),
      size = 2,
      default = default
    )
  })
  expect_snapshot(error = TRUE, {
    x <- structure(foobar(1), attr_foo = "foo")
    default <- structure(foobar(2), attr_foo = "bar")
    list_combine(
      list(x),
      indices = list(1),
      size = 2,
      default = default,
      default_arg = "d"
    )
  })
})

test_that("list_combine() fails with complex foreign S4 classes", {
  expect_snapshot({
    joe <- .Counts(c(1L, 2L), name = "Joe")
    jane <- .Counts(3L, name = "Jane")
    (expect_error(
      list_combine(list(joe, jane), indices = list(1:2, 3), size = 3),
      class = "vctrs_error_incompatible_type"
    ))
    (expect_error(
      list_combine(
        list(joe, jane),
        indices = list(1:2, 3),
        size = 3,
        error_call = call("foo"),
        x_arg = "arg"
      ),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("list_combine() falls back to c() if S3 method is available", {
  # Check off-by-one error
  expect_error(
    list_combine(
      list(foobar(1), "", foobar(2)),
      indices = list(1, 2, 3),
      size = 3
    ),
    class = "vctrs_error_incompatible_type"
  )

  # Fallback when the class implements `c()`
  method_foobar <- function(...) {
    xs <- list(...)
    xs <- map(xs, unclass)
    res <- exec("c", !!!xs)
    foobar(res)
  }

  local_methods(
    c.vctrs_foobar = method_foobar
  )
  expect_identical(
    list_combine(list(foobar(1), foobar(2)), indices = list(1, 2), size = 2),
    foobar(c(1, 2))
  )
  expect_identical(
    list_combine(list(foobar(1), foobar(2)), indices = list(2, 1), size = 2),
    foobar(c(2, 1))
  )
  expect_identical(
    list_combine(
      list(NULL, foobar(1), NULL, foobar(2)),
      indices = list(integer(), 1, integer(), 2),
      size = 2
    ),
    foobar(c(1, 2))
  )

  # OOB error is respected
  expect_error(
    list_combine(
      list(foobar(1), foobar(2)),
      indices = list(1, 3),
      size = 2
    ),
    class = "vctrs_error_subscript_oob"
  )

  # Unassigned locations results in missing values.
  # Repeated assignment uses the last assigned value.
  expect_identical(
    list_combine(
      list(foobar(c(1, 2)), foobar(3)),
      indices = list(c(1, 3), 1),
      size = 3
    ),
    foobar(c(3, NA, 2))
  )
  expect_identical(
    list_combine(
      list(foobar(c(1, 2)), foobar(3)),
      indices = list(c(2, NA), NA),
      size = 3
    ),
    foobar(c(NA, 1, NA))
  )

  # Respects `default`
  expect_identical(
    list_combine(
      list(foobar(c(1, 2)), foobar(3)),
      indices = list(c(1, 3), 1),
      default = foobar(0),
      size = 4
    ),
    foobar(c(3, 0, 2, 0))
  )
  expect_identical(
    list_combine(
      list(foobar(c(1, 2)), foobar(3)),
      indices = list(c(1, 3), 1),
      default = foobar(c(4, 5, 6, 7)),
      size = 4
    ),
    foobar(c(3, 5, 2, 7))
  )

  # Names are kept
  expect_identical(
    list_combine(
      list(foobar(c(x = 1, y = 2)), foobar(c(x = 1))),
      indices = list(c(2, 1), 3),
      size = 3
    ),
    foobar(c(y = 2, x = 1, x = 1))
  )
  expect_identical(
    list_combine(
      list(foobar(c(x = 1, y = 2)), foobar(c(x = 1))),
      indices = list(c(2, 1), 3),
      size = 5,
      default = foobar(c(default = 0))
    ),
    foobar(c(y = 2, x = 1, x = 1, default = 0, default = 0))
  )

  # Recycles to the size of index
  expect_identical(
    list_combine(
      list(foobar(1), foobar(2)),
      indices = list(c(1, 3), 2),
      size = 3
    ),
    foobar(c(1, 2, 1))
  )
  expect_identical(
    list_combine(
      list(foobar(1), foobar(2)),
      indices = list(c(1, 2), integer()),
      size = 2
    ),
    foobar(c(1, 1))
  )
  expect_snapshot({
    (expect_error(
      list_combine(
        list(foobar(1), foobar(2)),
        indices = list(c(1, 3), integer()),
        size = 2
      ),
      class = "vctrs_error_subscript_oob"
    ))
  })
  expect_snapshot({
    x <- list(foobar(1:2))
    indices <- list(1:3)
    (expect_error(list_combine(x, indices = indices, size = 3)))
    (expect_error(list_combine(
      x,
      indices = indices,
      size = 3,
      x_arg = "arg",
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
    list_combine(
      list(
        structure(1, class = "vctrs_c_fallback"),
        structure(2, class = "vctrs_c_fallback")
      ),
      indices = list(2, 1),
      size = 2
    ),
    structure(c(2, 1), class = "vctrs_c_fallback")
  )

  # Don't fallback for S3 lists which are treated as scalars by default
  expect_error(
    list_combine(
      list(foobar(list(1)), foobar(list(2))),
      indices = list(1, 2),
      size = 2
    ),
    class = "vctrs_error_scalar_type"
  )
})

test_that("list_combine() falls back to c() if S3 method is available and respects `default`", {
  # Fallback when the class implements `c()`
  method_foobar <- function(...) {
    xs <- list(...)
    xs <- map(xs, unclass)
    res <- exec("c", !!!xs)
    foobar(res)
  }

  local_methods(
    c.vctrs_foobar = method_foobar
  )

  # Respects `default`
  expect_identical(
    list_combine(
      list(foobar(c(1, 2)), foobar(3)),
      indices = list(c(1, 3), 1),
      default = foobar(0),
      size = 4
    ),
    foobar(c(3, 0, 2, 0))
  )
  expect_identical(
    list_combine(
      list(foobar(c(1, 2)), foobar(3)),
      indices = list(c(1, 3), 1),
      default = foobar(c(4, 5, 6, 7)),
      size = 4
    ),
    foobar(c(3, 5, 2, 7))
  )

  # Names are kept
  expect_identical(
    list_combine(
      list(foobar(c(x = 1, y = 2)), foobar(c(x = 1))),
      indices = list(c(2, 1), 3),
      size = 3
    ),
    foobar(c(y = 2, x = 1, x = 1))
  )
  expect_identical(
    list_combine(
      list(foobar(c(x = 1, y = 2)), foobar(c(x = 1))),
      indices = list(c(2, 1), 3),
      size = 5,
      default = foobar(c(default = 0))
    ),
    foobar(c(y = 2, x = 1, x = 1, default = 0, default = 0))
  )
})

test_that("list_combine() falls back for S4 classes with a registered c() method", {
  joe <- .Counts(c(1L, 2L), name = "Joe")
  jane <- .Counts(3L, name = "Jane")
  expect_snapshot({
    (expect_error(
      list_combine(list(joe, 1, jane), indices = list(c(1, 2), 3, 4), size = 4),
      class = "vctrs_error_incompatible_type"
    ))
  })
  local_c_counts()
  expect_identical(
    list_combine(list(joe, jane), indices = list(c(1, 3), 2), size = 3),
    .Counts(c(1L, 3L, 2L), name = "Dispatched")
  )
  expect_identical(
    list_combine(
      list(NULL, joe, jane),
      indices = list(integer(), c(1, 3), 2),
      size = 3
    ),
    .Counts(c(1L, 3L, 2L), name = "Dispatched")
  )
  # Unassigned locations results in missing values.
  # Repeated assignment uses the last assigned value.
  expect_identical(
    list_combine(list(joe, jane), indices = list(c(1, 3), 1), size = 3),
    .Counts(c(3L, NA, 2L), name = "Dispatched")
  )
  expect_identical(
    list_combine(list(joe, jane), indices = list(c(2, NA), NA), size = 3),
    .Counts(c(NA, 1L, NA), name = "Dispatched")
  )

  # `default` is respected
  default <- .Counts(0L, name = "Unknown")

  expect_identical(
    list_combine(
      list(joe, jane),
      indices = list(c(1, 3), 1),
      default = default,
      size = 5
    ),
    .Counts(c(3L, 0L, 2L, 0L, 0L), name = "Dispatched")
  )
})

test_that("can ignore names in `list_combine()` by providing a `zap()` name-spec (#232)", {
  expect_snapshot({
    (expect_error(
      list_combine(
        list(a = c(b = 1:2)),
        indices = list(1:2),
        size = 2
      )
    ))
    (expect_error(
      list_combine(
        list(a = c(b = 1:2)),
        indices = list(1:2),
        size = 2,
        error_call = call("foo")
      )
    ))
  })

  expect_identical(
    list_combine(
      list(a = c(b = 1:2), b = 3L),
      indices = list(1:2, 3),
      size = 3,
      name_spec = zap()
    ),
    1:3
  )
  expect_identical(
    list_combine(
      list(a = c(foo = 1:2), b = c(bar = 3L)),
      indices = list(2:1, 3),
      size = 3,
      name_spec = zap()
    ),
    c(2L, 1L, 3L)
  )
  expect_identical(
    list_combine(
      list(a = c(foo = 1:2), b = c(bar = 3L)),
      indices = list(2:1, 3),
      default = c(baz = 0L),
      size = 4,
      name_spec = zap()
    ),
    c(2L, 1L, 3L, 0L)
  )

  expect_snapshot({
    x <- list(a = c(b = c("a", "b")), b = 3L)
    (expect_error(
      list_combine(
        x,
        indices = list(1:2, 3),
        size = 3,
        name_spec = zap()
      ),
      class = "vctrs_error_incompatible_type"
    ))

    x <- list(a = c(foo = 1:2), b = c(bar = ""))
    (expect_error(
      list_combine(
        x,
        indices = list(2:1, 3),
        size = 3,
        name_spec = zap()
      ),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("list_combine() falls back to c() methods (#1120)", {
  expect_error(
    list_combine(
      list(foobar(1), foobar(2, class = "foo")),
      indices = list(1, 2),
      size = 2
    ),
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
    list_combine(xs, indices = list(2, 1), size = 2),
    c("dispatched2", "dispatched1")
  )
  expect_identical(
    list_combine(xs, indices = list(3, 1), size = 4, default = foobar(0)),
    c("dispatched2", "dispatched3", "dispatched1", "dispatched4")
  )

  # Different subclasses
  xs <- list(
    foobar(c(x = 1, y = 2), class = "foo"),
    foobar(c(x = 1), foo = 1)
  )
  expect_identical(
    list_combine(xs, indices = list(c(2, 1), 3), size = 3),
    c("dispatched2", "dispatched1", "dispatched3")
  )
  expect_identical(
    list_combine(xs, indices = list(c(4, 1), 3), size = 5, default = foobar(0)),
    c("dispatched2", "dispatched4", "dispatched3", "dispatched1", "dispatched5")
  )
})

test_that("list_combine() fails if foreign classes are not homogeneous and there is no c() method", {
  xs <- list(
    foobar(c(x = 1, y = 2), class = "foo"),
    foobar(c(x = 1), foo = 1)
  )
  expect_error(
    list_combine(xs, indices = list(c(2, 1), 3), size = 3),
    class = "vctrs_error_incompatible_type"
  )

  x <- foobar(c(x = 1, y = 2), class = "foo")
  default <- foobar(c(x = 1), foo = 1)
  expect_snapshot(error = TRUE, {
    list_combine(
      list(x),
      indices = list(c(1, 2)),
      size = 3,
      default = default
    )
  })
})

test_that("recycling error indices are correct even with `NULL` removal", {
  # Should be element 3!
  expect_snapshot_list_combine(
    error = TRUE,
    x = list(1:2, NULL, 7:8),
    indices = list(1:2, 5:6, 7:9),
    size = 9,
    default = 0L
  )
})

test_that("size 1 `default` is recycled correctly", {
  expect_identical_list_combine(
    x = list(1:2, 5L, 7:8),
    indices = list(1:2, 5, 7:8),
    size = 9,
    default = 0L,
    expect = int(1, 2, 0, 0, 5, 0, 7, 8, 0)
  )
})

test_that("`indices` is required!", {
  expect_error(list_combine(list(1, 2), size = 2))
})

test_that("`size` is required!", {
  expect_error(list_combine(list(1, 2), indices = list(1, 2)))
})

test_that("`x_arg` works", {
  expect_snapshot(error = TRUE, {
    list_combine(list(1, "2"), indices = list(1, 2), size = 2, x_arg = "xs")
  })
  expect_snapshot(error = TRUE, {
    list_combine(list(1, 2), indices = list(1, 2, 3), size = 2, x_arg = "xs")
  })
})

test_that("`indices_arg` works", {
  expect_snapshot(error = TRUE, {
    list_combine(list(1, 2), indices = 1, size = 2, indices_arg = "i")
  })
  expect_snapshot(error = TRUE, {
    list_combine(
      list(1, 2),
      indices = list(1, 2, 3),
      size = 2,
      indices_arg = "i"
    )
  })
})

test_that("`...` must be empty", {
  expect_snapshot(error = TRUE, {
    list_combine(list(1, 2), indices = list(1, 2), size = 2, "foo")
  })
})

test_that("list_combine() `default` is inserted correctly", {
  xs <- list("a", "b")
  indices <- list(1, 3)

  expect_identical_list_combine(
    x = xs,
    indices = indices,
    size = 4,
    default = "c",
    expect = c("a", "c", "b", "c")
  )
})

test_that("list_combine() `default` is inserted correctly with data frames", {
  xs <- list(
    data.frame(a = 1:2, b = 1:2),
    data.frame(a = 3L, b = 3L)
  )
  indices <- list(1:2, 5)

  expect_identical(
    list_combine(
      x = xs,
      indices = indices,
      size = 6,
      default = data.frame(a = 0L, b = NA_integer_)
    ),
    data.frame(
      a = int(1, 2, 0, 0, 3, 0),
      b = int(1, 2, NA, NA, 3, NA)
    )
  )
})

test_that("list_combine() `default` is inserted correctly when there are duplicate indices", {
  xs <- list("a", "b", "c")
  indices <- list(1, 1, 3)

  expect_identical_list_combine(
    x = xs,
    indices = indices,
    size = 4,
    default = "d",
    expect = c("b", "d", "c", "d")
  )
})

test_that("list_combine() `default` is inserted correctly when it is the size of `size`", {
  xs <- list("2", "4")
  indices <- list(2, 4)

  default <- letters[1:5]

  expect_identical_list_combine(
    x = xs,
    indices = indices,
    size = 5,
    default = default,
    expect = c("a", "2", "c", "4", "e")
  )
})

test_that("list_combine() `default` is correctly not used when all spots are filled", {
  xs <- list("a", "b", "c")
  indices <- list(1, 2, 3)

  expect_identical_list_combine(
    x = xs,
    indices = indices,
    size = 3,
    default = "d",
    expect = c("a", "b", "c")
  )
})

test_that("list_combine() `default` names work correctly with `name_spec`", {
  xs <- list(x = c(a = "a"), y = c(b = "b"), z = c(c = "c"))
  indices <- list(1, 3, NA)

  expect_identical(
    list_combine(
      xs,
      indices = indices,
      size = 5,
      default = c(d = "d"),
      name_spec = "{outer}_{inner}"
    ),
    c(x_a = "a", d = "d", y_b = "b", d = "d", d = "d")
  )
})

test_that("list_combine() `size` type is validated", {
  expect_snapshot(error = TRUE, {
    list_combine(list(1), indices = list(1), size = "x")
  })
})

test_that("list_combine() `indices` are validated against `size`", {
  # TODO: Not the best error message here
  expect_snapshot_list_combine(
    error = TRUE,
    x = list(1),
    indices = list(3),
    size = 2
  )
})

test_that("list_combine() `default` vector check is done", {
  expect_snapshot_list_combine(
    error = TRUE,
    x = list(1),
    indices = list(1),
    size = 1,
    default = lm(1 ~ 1),
    default_arg = "d"
  )
})

test_that("list_combine() `default` size check is done", {
  # Must be size 1, or same size as `size`
  expect_identical_list_combine(
    x = list(1),
    indices = list(2),
    size = 3,
    default = 0,
    default_arg = "d",
    expect = c(0, 1, 0)
  )
  expect_snapshot_list_combine(
    error = TRUE,
    x = list(1L),
    indices = list(1),
    size = 1,
    default = 1:2,
    default_arg = "d"
  )
})

test_that("list_combine() `default` is taken into account when computing `ptype`", {
  expect_identical(
    list_combine(
      list(1L),
      indices = list(1),
      size = 2,
      default = 1.5,
      default_arg = "d"
    ),
    c(1, 1.5)
  )

  # `default` is not in output, but helps determine output type!
  expect_identical(
    list_combine(
      list(1L),
      indices = list(1),
      size = 1,
      default = 1.5,
      default_arg = "d"
    ),
    1
  )

  # Empty `xs` and `indices`, but `default` is provided and determines type,
  # which would otherwise be <unspecified>
  expect_identical_list_combine(
    x = list(),
    indices = list(),
    size = 2,
    default = 0,
    default_arg = "d",
    expect = c(0, 0)
  )

  # Computed `ptype` among `xs` isn't compatible with `default`
  expect_snapshot(error = TRUE, {
    list_combine(
      list(x = 1),
      indices = list(1),
      size = 2,
      default = "a",
      default_arg = "d"
    )
  })

  # Provided `ptype` isn't compatible with `default`
  expect_snapshot(error = TRUE, {
    list_combine(
      list(x = 1L),
      indices = list(1),
      size = 2,
      default = 1.5,
      default_arg = "d",
      ptype = integer()
    )
  })
})

test_that("list_combine() works with data frame `default` with fallback columns", {
  x <- list(
    data_frame(a = foobar(1:2), b = 3:4, c = foobar(5:6)),
    data_frame(a = foobar(3L), b = 5L, c = foobar(7L))
  )
  indices <- list(
    c(3, NA),
    2
  )
  default <- data_frame(a = foobar(0L), b = 0L, c = foobar(-1L))

  expect_identical(
    list_combine(
      x = x,
      indices = indices,
      size = 5,
      default = default
    ),
    data_frame(
      a = foobar(int(0, 3, 1, 0, 0)),
      b = int(0, 5, 3, 0, 0),
      c = foobar(int(-1, 7, 5, -1, -1))
    )
  )

  with_c_foobar({
    expect_identical(
      list_combine(
        x = x,
        indices = indices,
        size = 5,
        default = default
      ),
      data_frame(
        a = foobar_c(int(0, 3, 1, 0, 0)),
        b = int(0, 5, 3, 0, 0),
        c = foobar_c(int(-1, 7, 5, -1, -1))
      )
    )
  })
})

test_that("list_combine() `unmatched = 'error'` doesn't error when all locations are covered", {
  expect_identical_list_combine(
    x = list(1:3),
    indices = list(1:3),
    size = 3,
    unmatched = "error",
    expect = 1:3
  )
  expect_identical_list_combine(
    x = list(1:3),
    indices = list(c(TRUE, TRUE, TRUE)),
    size = 3,
    unmatched = "error",
    expect = 1:3
  )
})

test_that("list_combine() `unmatched = 'error'` doesn't error in the empty case", {
  expect_identical(
    list_combine(list(), indices = list(), size = 0, unmatched = "error"),
    unspecified()
  )
})

test_that("list_combine() `unmatched = 'error'` errors with unmatched `indices` when `size` is used", {
  expect_snapshot(error = TRUE, {
    # Duplicates result in unmatched locations
    list_combine(
      list(1, 1),
      indices = list(1, 1),
      size = 2,
      unmatched = "error"
    )
  })
  expect_snapshot(error = TRUE, {
    # `NA` results in unmatched locations
    list_combine(
      list(1, 1),
      indices = list(1, NA),
      size = 2,
      unmatched = "error"
    )
  })
  expect_snapshot(error = TRUE, {
    # `NA` results in unmatched locations
    list_combine(
      list(1:9, 1:9),
      indices = list(
        c(TRUE, FALSE, NA, TRUE, FALSE, NA, TRUE, FALSE, NA),
        c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA, NA)
      ),
      size = 9,
      unmatched = "error"
    )
  })
  expect_snapshot(error = TRUE, {
    # Unused location
    list_combine(
      list(1, 3),
      indices = list(1, 3),
      size = 3,
      unmatched = "error"
    )
  })
  expect_snapshot(error = TRUE, {
    # Unused location
    list_combine(
      list(1, 1),
      indices = list(c(TRUE, FALSE), c(TRUE, FALSE)),
      size = 2,
      unmatched = "error"
    )
  })
  expect_snapshot(error = TRUE, {
    list_combine(
      list(),
      indices = list(),
      size = 2,
      unmatched = "error"
    )
  })
})

test_that("list_combine() `unmatched = 'error'` errors pluralize correctly", {
  expect_snapshot(error = TRUE, {
    # One location
    list_combine(
      list(1, 3),
      indices = list(1, 3),
      size = 3,
      unmatched = "error"
    )
  })
  expect_snapshot(error = TRUE, {
    # Two locations
    list_combine(
      list(1, 3),
      indices = list(1, 3),
      size = 4,
      unmatched = "error"
    )
  })
  expect_snapshot(error = TRUE, {
    # Many locations
    list_combine(
      list(1, 3),
      indices = list(1, 3),
      size = 100,
      unmatched = "error"
    )
  })
})

test_that("list_combine() `unmatched = 'error'` error classes are as expected", {
  cnd <- catch_cnd(list_combine(
    list(1, 3),
    indices = list(1, 3),
    size = 3,
    unmatched = "error"
  ))
  expect_true(inherits_all(
    cnd,
    c("vctrs_error_combine_unmatched", "vctrs_error_combine")
  ))
})

test_that("list_combine() `unmatched = 'error'` can't be set when `default` is also set", {
  expect_snapshot(error = TRUE, {
    list_combine(
      list(1),
      indices = list(1),
      default = 1,
      size = 1,
      unmatched = "error"
    )
  })
  expect_snapshot(error = TRUE, {
    list_combine(
      list(1),
      indices = list(1),
      default = 1,
      size = 1,
      unmatched = "error",
      default_arg = ".default",
      error_call = quote(foo())
    )
  })
})

test_that("list_combine() `unmatched` is validated", {
  expect_snapshot(error = TRUE, {
    list_combine(
      list(1),
      indices = list(1),
      size = 1,
      unmatched = "e"
    )
  })
  expect_snapshot(error = TRUE, {
    list_combine(
      list(1),
      indices = list(1),
      size = 1,
      unmatched = c("a", "b")
    )
  })
  expect_snapshot(error = TRUE, {
    list_combine(
      list(1),
      indices = list(1),
      size = 1,
      unmatched = NA_character_
    )
  })

  # With error call
  expect_snapshot(error = TRUE, {
    list_combine(
      list(1),
      indices = list(1),
      size = 1,
      unmatched = "e",
      error_call = quote(foo())
    )
  })
  expect_snapshot(error = TRUE, {
    list_combine(
      list(1),
      indices = list(1),
      size = 1,
      unmatched = c("a", "b"),
      error_call = quote(foo())
    )
  })
})

test_that("list_combine() `multiple` is validated", {
  expect_snapshot(error = TRUE, {
    list_combine(
      list(1),
      indices = list(1),
      size = 1,
      multiple = "a"
    )
  })
  expect_snapshot(error = TRUE, {
    list_combine(
      list(1),
      indices = list(1),
      size = 1,
      multiple = c("a", "b")
    )
  })
  expect_snapshot(error = TRUE, {
    list_combine(
      list(1),
      indices = list(1),
      size = 1,
      multiple = NA_character_
    )
  })

  # With error call
  expect_snapshot(error = TRUE, {
    list_combine(
      list(1),
      indices = list(1),
      size = 1,
      multiple = "a",
      error_call = quote(foo())
    )
  })
  expect_snapshot(error = TRUE, {
    list_combine(
      list(1),
      indices = list(1),
      size = 1,
      multiple = c("a", "b"),
      error_call = quote(foo())
    )
  })
})

test_that("`NA` indices are considered unmatched locations", {
  # The `NA` index is considered "unmatched". It's the same as providing a
  # logical index with an `NA` in it, like indices in this case of: list(c(TRUE,
  # FALSE, FALSE), c(FALSE, NA, TRUE)) We consider that `NA` to be unmatched,
  # and running vec_as_location on that gives the same index as this.
  expect_identical_list_combine(
    x = list(1L, 2:3),
    indices = list(1, c(NA, 3)),
    size = 3,
    default = 0L,
    expect = c(1L, 0L, 3L)
  )
  expect_identical_list_combine(
    x = list(1L, 2:3),
    indices = list(c(TRUE, FALSE, FALSE), c(FALSE, NA, TRUE)),
    size = 3,
    default = 0L,
    expect = c(1L, 0L, 3L)
  )

  # Which means this errors
  expect_snapshot(error = TRUE, {
    list_combine(
      x = list(1, 2:3),
      indices = list(1, c(NA, 3)),
      size = 3,
      unmatched = "error"
    )
  })
  expect_snapshot(error = TRUE, {
    list_combine(
      x = list(1, 2:3),
      indices = list(c(TRUE, FALSE, FALSE), c(FALSE, NA, TRUE)),
      size = 3,
      unmatched = "error"
    )
  })
})

test_that("`indices` corresponding to `NULL` values are considered matched", {
  # These are considered "matched" but there is nothing to put there,
  # so `NA` ends up in the slot
  expect_identical_list_combine(
    x = list(1, NULL),
    indices = list(1, c(2, 3)),
    size = 3,
    default = 0,
    expect = c(1, NA, NA)
  )
  expect_identical_list_combine(
    x = list(1, NULL),
    indices = list(c(TRUE, FALSE, FALSE), c(FALSE, TRUE, TRUE)),
    size = 3,
    default = 0,
    expect = c(1, NA, NA)
  )

  # Which means this is not an error
  expect_identical_list_combine(
    x = list(1, NULL),
    indices = list(1, c(2, 3)),
    size = 3,
    unmatched = "error",
    expect = c(1, NA, NA)
  )
  expect_identical_list_combine(
    x = list(1, NULL),
    indices = list(c(TRUE, FALSE, FALSE), c(FALSE, TRUE, TRUE)),
    size = 3,
    unmatched = "error",
    expect = c(1, NA, NA)
  )
})

test_that("`x` must be a list", {
  expect_snapshot(error = TRUE, {
    list_combine(1, indices = list(1), size = 1)
  })
  expect_snapshot(error = TRUE, {
    list_combine(
      1,
      indices = list(1),
      size = 1,
      error_call = call("foo"),
      x_arg = "arg"
    )
  })
  expect_snapshot(error = TRUE, {
    list_combine(data.frame(x = 1), indices = list(1), size = 1)
  })
})

test_that("`indices` must be a list", {
  expect_snapshot(error = TRUE, {
    list_combine(list(1), indices = 1, size = 1)
  })
  expect_snapshot(error = TRUE, {
    list_combine(list(1), indices = 1, size = 1, error_call = call("foo"))
  })
  expect_snapshot(error = TRUE, {
    list_combine(list(1), indices = data.frame(x = 1), size = 1)
  })
})

test_that("`indices` has a restricted type", {
  expect_error(
    list_combine(list(1), indices = list("x"), size = 1),
    class = "vctrs_error_subscript_type"
  )
  expect_error(
    list_combine(list(1), indices = list(quote(name)), size = 1),
    class = "vctrs_error_subscript_type"
  )
})

test_that("`x` and `indices` must be lists of the same size", {
  expect_snapshot(error = TRUE, {
    list_combine(list(1, 2), indices = list(1), size = 1)
  })
})

test_that("can combine with an AsIs list (#1463)", {
  x <- I(list(1, 2))
  expect_identical(list_combine(x, indices = list(1, 2), size = 2), c(1, 2))
})

test_that("can combine empty vectors", {
  expect_identical(
    list_combine(
      list(),
      indices = list(),
      size = 0
    ),
    unspecified()
  )
  expect_identical(
    list_combine(
      list(),
      indices = list(),
      size = 0,
      multiple = "first"
    ),
    unspecified()
  )
  expect_identical(
    list_combine(
      list(),
      indices = list(),
      size = 0,
      ptype = numeric()
    ),
    numeric()
  )
  expect_identical(
    list_combine(
      list(),
      indices = list(),
      size = 0,
      ptype = numeric(),
      multiple = "first"
    ),
    numeric()
  )
})

test_that("can combine a list of NULL", {
  expect_identical(
    list_combine(list(NULL), indices = list(integer()), size = 0),
    unspecified(0)
  )
  expect_identical(
    list_combine(list(NULL), indices = list(integer()), size = 1),
    unspecified(1)
  )
  expect_identical(
    list_combine(
      list(NULL),
      indices = list(integer()),
      size = 0,
      ptype = numeric()
    ),
    numeric()
  )
  expect_identical(
    list_combine(
      list(NULL),
      indices = list(integer()),
      ptype = numeric(),
      size = 1
    ),
    vec_init(numeric(), n = 1)
  )
  expect_identical(
    list_combine(
      list(NULL, NULL),
      indices = list(integer(), integer()),
      size = 0,
      ptype = numeric()
    ),
    numeric()
  )
})

test_that("NULLs are ignored when combined with other vectors", {
  expect_identical_list_combine(
    x = list("a", NULL, "b"),
    indices = list(2, integer(), 1),
    size = 2,
    expect = c("b", "a")
  )
})

test_that("can use a `NULL` element with a corresponding index", {
  # Because of `size = 2`, we must return something with `size = 2`.
  # Our sized identity element is `unspecified(2)`.
  expect_identical(
    list_combine(list(NULL), indices = list(1:2), size = 2L),
    unspecified(2)
  )

  expect_identical(
    list_combine(list(NULL), indices = list(1:2), size = 2, ptype = integer()),
    c(NA_integer_, NA_integer_)
  )

  x <- list("a", NULL, c("b", "c"))
  indices <- list(3L, c(1L, 4L), c(2L, 5L))
  expect_identical_list_combine(
    x = x,
    indices = indices,
    size = 5,
    expect = c(NA, "b", "a", NA, "c")
  )
})

test_that("can combine atomic vectors", {
  expect_identical_list_combine(
    x = list(1, 2),
    indices = list(2, 1),
    size = 2,
    expect = c(2, 1)
  )
  expect_identical_list_combine(
    x = list("a", "b"),
    indices = list(2, 1),
    size = 2,
    expect = c("b", "a")
  )
})

test_that("can combine lists", {
  x <- list(list("a", "b"), list("c"))
  indices <- list(c(2, 3), 1)

  expect_identical(
    list_combine(x, indices = indices, size = 3),
    list("c", "a", "b")
  )
})

test_that("can combine data frames", {
  df1 <- data_frame(x = 1:2)
  df2 <- data_frame(x = 3:4)

  x <- list(df1, df2)
  indices <- list(c(3, 1), c(2, 4))

  expect <- vec_slice(vec_c(df1, df2), vec_order(vec_c(!!!indices)))

  expect_identical(list_combine(x, indices = indices, size = 4), expect)
})

test_that("can fallback when combining matrices", {
  mat1 <- matrix(1:4, nrow = 2, ncol = 2)
  mat2 <- matrix(5:10, nrow = 3, ncol = 2)

  x <- list(mat1, mat2)
  indices <- list(c(4, 1), c(2, 3, 5))

  expect <- vec_slice(vec_c(mat1, mat2), vec_order(vec_c(!!!indices)))

  expect_identical(list_combine(x, indices = indices, size = 5), expect)
})

test_that("can fallback when combining matrices and using `default`", {
  mat1 <- matrix(1:4, nrow = 2, ncol = 2)
  mat2 <- matrix(5:10, nrow = 3, ncol = 2)

  x <- list(mat1, mat2)
  indices <- list(c(5, 1), c(2, 3, 7))

  default <- matrix(11:24, nrow = 7, ncol = 2)
  size <- 7

  expect <- vec_c(
    vec_slice(mat1, 2),
    vec_slice(mat2, 1),
    vec_slice(mat2, 2),
    vec_slice(default, 4),
    vec_slice(mat1, 1),
    vec_slice(default, 6),
    vec_slice(mat2, 3)
  )

  expect_identical(
    list_combine(x, indices = indices, size = size, default = default),
    expect
  )
})

test_that("can fallback when combining arrays of >2D", {
  arr1 <- array(1:8, c(2, 2, 2))
  arr2 <- matrix(9:10, c(1, 2))

  x <- list(arr1, arr2)
  indices <- list(c(3, 1), 2)

  expect <- vec_slice(vec_c(arr1, arr2), vec_order(vec_c(!!!indices)))

  expect_identical(list_combine(x, indices = indices, size = 3), expect)
})

test_that("can combine with all size 0 elements and get the right ptype", {
  x <- list(integer(), integer())
  indices <- list(integer(), integer())
  expect_identical_list_combine(
    x = x,
    indices = indices,
    size = 0,
    expect = integer()
  )
})

test_that("can combine with some size 0 elements", {
  x <- list(integer(), 1:2, integer())
  indices <- list(integer(), 2:1, integer())
  expect_identical_list_combine(
    x = x,
    indices = indices,
    size = 2,
    expect = 2:1
  )
})

test_that("NULL is a valid index", {
  expect_identical_list_combine(
    x = list(1, 2),
    indices = list(NULL, 1),
    size = 1,
    expect = 2
  )
  expect_snapshot_list_combine(
    error = TRUE,
    x = list(1, 2),
    indices = list(NULL, 2),
    size = 1
  )
})

test_that("combining recycles elements of x to the size of the index", {
  x <- list(1, 2)

  expect_identical_list_combine(
    x = x,
    indices = list(c(3, 4, 5), c(2, 1)),
    size = 5,
    expect = c(2, 2, 1, 1, 1)
  )
  expect_identical_list_combine(
    x = x,
    indices = list(
      c(FALSE, FALSE, TRUE, TRUE, TRUE),
      c(TRUE, TRUE, FALSE, FALSE, FALSE)
    ),
    size = 5,
    expect = c(2, 2, 1, 1, 1)
  )
  expect_identical_list_combine(
    x = x,
    indices = list(c(3, 4, 5), c(2, 1)),
    size = 5,
    slice_x = TRUE,
    expect = c(2, 2, 1, 1, 1)
  )
  expect_identical_list_combine(
    x = x,
    indices = list(
      c(FALSE, FALSE, TRUE, TRUE, TRUE),
      c(TRUE, TRUE, FALSE, FALSE, FALSE)
    ),
    size = 5,
    slice_x = TRUE,
    expect = c(2, 2, 1, 1, 1)
  )

  x <- list(1:2)

  expect_snapshot_list_combine(
    error = TRUE,
    x = x,
    indices = list(1:3),
    size = 4
  )
  expect_snapshot_list_combine(
    error = TRUE,
    x = x,
    indices = list(1:3),
    size = 4,
    x_arg = "arg"
  )

  expect_snapshot_list_combine(
    error = TRUE,
    x = x,
    indices = list(1:3),
    size = 4,
    slice_x = TRUE
  )
  expect_snapshot_list_combine(
    error = TRUE,
    x = x,
    indices = list(1:3),
    size = 4,
    slice_x = TRUE,
    x_arg = "arg"
  )

  expect_snapshot_list_combine(
    error = TRUE,
    x = x,
    indices = list(c(TRUE, TRUE, TRUE, TRUE)),
    size = 4
  )
  expect_snapshot_list_combine(
    error = TRUE,
    x = x,
    indices = list(c(TRUE, TRUE, TRUE, TRUE)),
    size = 4,
    x_arg = "arg"
  )

  expect_snapshot_list_combine(
    error = TRUE,
    x = x,
    indices = list(c(TRUE, TRUE, TRUE, TRUE)),
    size = 4,
    slice_x = TRUE
  )
})

test_that("combining takes the common type", {
  x <- list(1, "a")
  indices <- list(1, 2)

  expect_snapshot_list_combine(
    error = TRUE,
    x = x,
    indices = indices,
    size = 2
  )

  x <- list(1, 2L)
  expect_identical(list_combine(x, indices = indices, size = 2), c(1, 2))
})

test_that("common type failure uses positional errors", {
  expect_snapshot({
    x <- list(1, a = "x", 2)

    # Looking for `x[[1]]` and `x$a`
    (expect_error(list_combine(x, indices = list(2, 1, 3), size = 3)))

    # Directed cast should also produce directional errors (#1690)
    (expect_error(list_combine(
      x,
      indices = list(2, 1, 3),
      size = 3,
      ptype = double()
    )))

    # Lossy cast
    y <- list(1, a = 2.5)
    (expect_error(list_combine(
      y,
      indices = list(2, 1),
      size = 2,
      ptype = integer()
    )))
  })
})

test_that("can specify a ptype to override common type", {
  indices <- list(1, 2)

  x <- list(1, 2L)
  expect_identical(
    list_combine(x, indices = indices, size = 2, ptype = integer()),
    c(1L, 2L)
  )

  x <- list(1.5, 2)
  expect_snapshot(error = TRUE, cnd_class = TRUE, {
    list_combine(x, indices = indices, size = 2, ptype = integer())
  })
  expect_snapshot(error = TRUE, cnd_class = TRUE, {
    list_combine(
      x,
      indices = indices,
      size = 2,
      ptype = integer(),
      error_call = call("foo"),
      x_arg = "arg"
    )
  })
})

test_that("common type is correctly computed with unspecified values and a `default` (#2094)", {
  expect_identical(
    list_combine(
      x = list(NA),
      indices = list(1),
      size = 1,
      default = "a"
    ),
    NA_character_
  )
  expect_identical(
    list_combine(
      x = list(NA),
      indices = list(1),
      size = 2,
      default = "a"
    ),
    c(NA, "a")
  )
})

test_that("outer names are kept", {
  x <- list(x = 1, y = 2)
  expect_named_list_combine(
    x = x,
    indices = list(2, 1),
    size = 2,
    expect = c("y", "x")
  )
})

test_that("outer names are recycled in the right order", {
  x <- list(x = 1, y = 2)

  expect_snapshot(error = TRUE, {
    list_combine(x, indices = list(c(1, 2), 3), size = 3)
  })
  expect_named(
    list_combine(
      x,
      indices = list(c(1, 3), 2),
      size = 3,
      name_spec = "{outer}_{inner}"
    ),
    c("x_1", "y", "x_2")
  )
  expect_named(
    list_combine(
      x,
      indices = list(c(3, 1), 2),
      size = 3,
      name_spec = "{outer}_{inner}"
    ),
    c("x_2", "y", "x_1")
  )
})

test_that("outer names can be merged with inner names", {
  x <- list(x = c(a = 1), y = c(b = 2))
  expect_named(
    list_combine(
      x,
      indices = list(2, 1),
      size = 2,
      name_spec = "{outer}_{inner}"
    ),
    c("y_b", "x_a")
  )
})

test_that("preserves names when inputs are cast to a common type (#1689)", {
  expect_named(
    list_combine(
      list(c(a = 1)),
      indices = list(1),
      size = 1,
      ptype = integer()
    ),
    "a"
  )

  # With `default`
  expect_named(
    list_combine(
      list(c(a = 1)),
      indices = list(1),
      default = c(b = 0),
      ptype = integer(),
      size = 2
    ),
    c("a", "b")
  )

  # With name spec
  name_spec <- "{outer}_{inner}"
  expect_named(
    list_combine(
      list(foo = c(a = 1)),
      ptype = integer(),
      name_spec = name_spec,
      indices = list(1),
      size = 1
    ),
    "foo_a"
  )
  expect_named(
    list_combine(
      list(foo = c(a = 1)),
      indices = list(1),
      default = c(b = 0),
      ptype = integer(),
      name_spec = name_spec,
      size = 2
    ),
    c("foo_a", "b")
  )

  # When `x` elements are recycled, names are also recycled
  x <- list(c(a = 1), c(b = 2))
  indices <- list(1:2, 3:4)
  expect_named(
    list_combine(x, indices = indices, size = 4, ptype = integer()),
    c("a", "a", "b", "b")
  )

  # When `default` elements are recycled, names are also recycled
  expect_named(
    list_combine(
      list(c(a = 1), c(b = 2)),
      indices = list(1, 3),
      default = c(c = 0),
      ptype = integer(),
      size = 4
    ),
    c("a", "c", "b", "c")
  )
})

test_that("individual data frame columns retain vector names", {
  df1 <- data_frame(x = c(a = 1, b = 2))
  df2 <- data_frame(x = c(c = 3))

  x <- list(df1, df2)
  indices <- list(c(1, 2), 3)

  result <- list_combine(x, indices = indices, size = 3)

  expect_named(result$x, c("a", "b", "c"))

  # Names should be identical to equivalent `vec_c()` call
  expect_identical(
    list_combine(x, indices = indices, size = 3),
    vec_c(!!!x)
  )
})

test_that("assigning to the same location twice means last wins", {
  x <- list(1:2, 3L)
  indices <- list(1:2, 1L)

  expect_identical_list_combine(
    x = x,
    indices = indices,
    size = 3,
    expect = c(3L, 2L, NA)
  )
  expect_identical_list_combine(
    x = x,
    indices = indices,
    size = 2,
    expect = c(3L, 2L)
  )
})

test_that("when assigning to the same location, names are continually overwritten (#2019)", {
  expect_identical_list_combine(
    x = list(
      c(a = 1, b = 2),
      c(c = 3, d = 4)
    ),
    indices = list(
      c(1, 2),
      c(1, 3)
    ),
    size = 3,
    multiple = "last",
    expect = c(c = 3, b = 2, d = 4)
  )
  expect_identical_list_combine(
    x = list(
      c(a = 1, b = 2),
      c(c = 3, d = 4)
    ),
    indices = list(
      c(1, 2),
      c(1, 3)
    ),
    size = 3,
    multiple = "first",
    expect = c(a = 1, b = 2, d = 4)
  )
})

test_that("df-cols: when assigning to the same location, names are continually overwritten (#2019)", {
  expect_identical(
    list_combine(
      x = list(
        data_frame(x = c(a = 1, b = 2), y = c("foo", "bar")),
        data_frame(x = c(c = 0), y = "baz")
      ),
      indices = list(
        c(1, 2),
        1
      ),
      size = 2,
      multiple = "last"
    ),
    data_frame(
      x = c(c = 0, b = 2),
      y = c("baz", "bar")
    )
  )
  expect_identical(
    list_combine(
      x = list(
        data_frame(x = c(a = 1, b = 2), y = c("foo", "bar")),
        data_frame(x = c(c = 0), y = "baz")
      ),
      indices = list(
        c(1, 2),
        1
      ),
      size = 2,
      multiple = "first"
    ),
    data_frame(
      x = c(a = 1, b = 2),
      y = c("foo", "bar")
    )
  )

  # Homogenous fallback
  expect_identical(
    list_combine(
      x = list(
        data_frame(x = foobar(c(a = 1, b = 2)), y = c("foo", "bar")),
        data_frame(x = foobar(c(c = 0)), y = "baz")
      ),
      indices = list(
        c(1, 2),
        1
      ),
      size = 2,
      multiple = "last"
    ),
    data_frame(
      x = foobar(c(c = 0, b = 2)),
      y = c("baz", "bar")
    )
  )
  expect_identical(
    list_combine(
      x = list(
        data_frame(x = foobar(c(a = 1, b = 2)), y = c("foo", "bar")),
        data_frame(x = foobar(c(c = 0)), y = "baz")
      ),
      indices = list(
        c(1, 2),
        1
      ),
      size = 2,
      multiple = "first"
    ),
    data_frame(
      x = foobar(c(a = 1, b = 2)),
      y = c("foo", "bar")
    )
  )

  # `c()` fallback
  with_c_foobar({
    expect_identical(
      list_combine(
        x = list(
          data_frame(x = foobar(c(a = 1, b = 2)), y = c("foo", "bar")),
          data_frame(x = foobar(c(c = 0)), y = "baz")
        ),
        indices = list(
          c(1, 2),
          1
        ),
        size = 2,
        multiple = "last"
      ),
      data_frame(
        x = foobar_c(c(c = 0, b = 2)),
        y = c("baz", "bar")
      )
    )
  })
  with_c_foobar({
    expect_identical(
      list_combine(
        x = list(
          data_frame(x = foobar(c(a = 1, b = 2)), y = c("foo", "bar")),
          data_frame(x = foobar(c(c = 0)), y = "baz")
        ),
        indices = list(
          c(1, 2),
          1
        ),
        size = 2,
        multiple = "first"
      ),
      data_frame(
        x = foobar_c(c(a = 1, b = 2)),
        y = c("foo", "bar")
      )
    )
  })
})

test_that("when assigning to the same location, names are cleared as needed (#2019)", {
  expect_identical_list_combine(
    x = list(
      c(a = 1, b = 2),
      c(3, 4)
    ),
    indices = list(
      c(1, 2),
      c(1, 3)
    ),
    size = 3,
    multiple = "last",
    expect = c(3, b = 2, 4)
  )
  expect_identical_list_combine(
    x = list(
      c(1, 2),
      c(c = 3, d = 4)
    ),
    indices = list(
      c(1, 2),
      c(1, 3)
    ),
    size = 3,
    multiple = "first",
    expect = c(1, 2, d = 4)
  )
})

test_that("df-cols: when assigning to the same location, names are cleared as needed (#2019)", {
  # - 1st element has names
  # - 2nd element doesn't have names, so `""` is used as the name to overwrite
  #   the names written when inserting the 1st element
  # - Reversed for `multiple = "first"`
  expect_identical(
    list_combine(
      x = list(
        data_frame(x = c(a = 1, b = 2), y = c("foo", "bar")),
        data_frame(x = 0, y = "baz")
      ),
      indices = list(
        c(1, 2),
        1
      ),
      size = 2,
      multiple = "last"
    ),
    data_frame(
      x = set_names(c(0, 2), c("", "b")),
      y = c("baz", "bar")
    )
  )
  expect_identical(
    list_combine(
      x = list(
        data_frame(x = c(1, 2), y = c("foo", "bar")),
        data_frame(x = c(c = 0), y = "baz")
      ),
      indices = list(
        c(1, 2),
        1
      ),
      size = 2,
      multiple = "first"
    ),
    data_frame(
      x = set_names(c(1, 2), c("", "")),
      y = c("foo", "bar")
    )
  )

  # Homogenous fallback
  expect_identical(
    list_combine(
      x = list(
        data_frame(x = foobar(c(a = 1, b = 2)), y = c("foo", "bar")),
        data_frame(x = foobar(0), y = "baz")
      ),
      indices = list(
        c(1, 2),
        1
      ),
      size = 2,
      multiple = "last"
    ),
    data_frame(
      x = foobar(set_names(c(0, 2), c("", "b"))),
      y = c("baz", "bar")
    )
  )
  expect_identical(
    list_combine(
      x = list(
        data_frame(x = foobar(c(1, 2)), y = c("foo", "bar")),
        data_frame(x = foobar(c(c = 0)), y = "baz")
      ),
      indices = list(
        c(1, 2),
        1
      ),
      size = 2,
      multiple = "first"
    ),
    data_frame(
      x = foobar(set_names(c(1, 2), c("", ""))),
      y = c("foo", "bar")
    )
  )

  # `c()` fallback
  with_c_foobar({
    expect_identical(
      list_combine(
        x = list(
          data_frame(x = foobar(c(a = 1, b = 2)), y = c("foo", "bar")),
          data_frame(x = foobar(0), y = "baz")
        ),
        indices = list(
          c(1, 2),
          1
        ),
        size = 2,
        multiple = "last"
      ),
      data_frame(
        x = foobar_c(set_names(c(0, 2), c("", "b"))),
        y = c("baz", "bar")
      )
    )
  })
  with_c_foobar({
    expect_identical(
      list_combine(
        x = list(
          data_frame(x = foobar(c(1, 2)), y = c("foo", "bar")),
          data_frame(x = foobar(c(c = 0)), y = "baz")
        ),
        indices = list(
          c(1, 2),
          1
        ),
        size = 2,
        multiple = "first"
      ),
      data_frame(
        x = foobar_c(set_names(c(1, 2), c("", ""))),
        y = c("foo", "bar")
      )
    )
  })
})

test_that("index values are validated", {
  x <- list(1, 2)
  indices1 <- list(4, 1)
  indices2 <- list(c(1, 4), 2)
  indices3 <- list(c(1, 3, 4), 2)

  expect_error(
    list_combine(x, indices = indices1, size = 2),
    class = "vctrs_error_subscript_oob"
  )
  expect_error(
    list_combine(x, indices = indices2, size = 3),
    class = "vctrs_error_subscript_oob"
  )

  expect_identical(
    list_combine(x, indices = indices3, size = 4),
    c(1, 2, 1, 1)
  )
})

test_that("name repair is respected and happens after ordering according to `indices`", {
  local_name_repair_quiet()

  x <- list(c(a = 1), c(a = 2))

  indices <- list(2, 1)
  expect_named(list_combine(x, indices = indices, size = 2), c("a", "a"))
  expect_named(
    list_combine(x, indices = indices, size = 2, name_repair = "unique"),
    c("a...1", "a...2")
  )

  # With `default`
  indices <- list(3, 1)
  default <- c(a = 0)
  expect_named(
    list_combine(x, indices = indices, size = 3, default = default),
    c("a", "a", "a")
  )
  expect_named(
    list_combine(
      x,
      indices = indices,
      size = 3,
      default = default,
      name_repair = "unique"
    ),
    c("a...1", "a...2", "a...3")
  )
})

test_that("list_combine() can repair names quietly", {
  local_name_repair_verbose()

  x <- c(x = "a", x = "b", x = "c")
  indices <- list(2, c(3, 1))
  expect_snapshot({
    res <- list_combine(
      vec_chop(x, indices = indices),
      indices = indices,
      size = 3,
      name_repair = "unique_quiet"
    )
  })
  expect_named(res, c("x...1", "x...2", "x...3"))

  x <- c("if" = "a", "in" = "b", "for" = "c")
  indices <- list(2, c(3, 1))
  expect_snapshot({
    res <- list_combine(
      vec_chop(x, indices = indices),
      indices = indices,
      size = 3,
      name_repair = "universal_quiet"
    )
  })
  expect_named(res, c(".if", ".in", ".for"))
})

test_that("list_combine() errors on unsupported location values", {
  expect_snapshot_list_combine(
    error = TRUE,
    x = list(1, 2),
    indices = list(c(1, 2), 0),
    size = 3
  )
  expect_snapshot_list_combine(
    error = TRUE,
    x = list(1),
    indices = list(-1),
    size = 1
  )
})

test_that("missing values propagate", {
  expect_identical_list_combine(
    x = list(1, 2),
    indices = list(c(NA_integer_, NA_integer_), c(NA_integer_, 3)),
    size = 4,
    expect = c(NA, NA, 2, NA)
  )
})

test_that("list_combine() fallback doesn't support `name_spec` or `ptype`", {
  expect_snapshot({
    foo <- structure(foobar(1), foo = "foo")
    bar <- structure(foobar(2), bar = "bar")
    (expect_error(
      with_c_foobar(list_combine(
        list(foo, bar),
        indices = list(1, 2),
        size = 2,
        name_spec = "{outer}_{inner}"
      )),
      "name specification"
    ))
    # With error call
    (expect_error(
      with_c_foobar(list_combine(
        list(foo, bar),
        indices = list(1, 2),
        size = 2,
        name_spec = "{outer}_{inner}",
        error_call = call("foo")
      )),
      "name specification"
    ))
    # Used to be an error about `ptype`
    x <- list(foobar(1))
    (expect_error(
      with_c_foobar(list_combine(x, indices = list(1), size = 1, ptype = "")),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("list_combine() supports numeric S3 indices", {
  local_methods(
    vec_ptype2.vctrs_foobar = function(x, y, ...) {
      UseMethod("vec_ptype2.vctrs_foobar")
    },
    vec_ptype2.vctrs_foobar.integer = function(x, y, ...) foobar(integer()),
    vec_cast.integer.vctrs_foobar = function(x, to, ...) vec_data(x)
  )

  expect_identical(
    list_combine(list(1), indices = list(foobar(1L)), size = 1),
    1
  )
})

test_that("list_combine() does not support non-numeric S3 indices", {
  expect_snapshot({
    (expect_error(
      list_combine(list(1), indices = list(factor("x")), size = 1),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      list_combine(list(1), indices = list(foobar(1L)), size = 1),
      class = "vctrs_error_subscript_type"
    ))
  })
})

test_that("list_combine() supports named `indices` (#2095)", {
  # Particularly in the fallback case where we have to `vec_c()` the `indices`

  # With outer names on `indices`:
  expect_identical_list_combine(
    x = list(c("a", "b")),
    indices = list(a = c(1, 2)),
    size = 2,
    expect = c("a", "b")
  )
  expect_identical_list_combine(
    x = list(c("a", "b", "c", "d")),
    indices = list(a = c(FALSE, TRUE, FALSE, TRUE)),
    size = 4,
    slice_x = TRUE,
    expect = c(NA, "b", NA, "d")
  )

  # With outer and inner names on `indices`:
  expect_identical_list_combine(
    x = list(c("a", "b"), c("c", "d")),
    indices = list(a = c(x = 1, y = 2), b = c(x = 3, y = 4)),
    size = 4,
    expect = c("a", "b", "c", "d")
  )
  expect_identical_list_combine(
    x = list(c("a", "b", "c", "d"), c("e", "f", "g", "h")),
    indices = list(
      a = c(w = FALSE, x = TRUE, y = FALSE, z = TRUE),
      b = c(w = FALSE, x = FALSE, y = TRUE, z = FALSE)
    ),
    size = 4,
    slice_x = TRUE,
    expect = c(NA, "b", "g", "d")
  )
})

test_that("`list_combine()` with `slice_x = FALSE`", {
  values <- list(1:2, 3:4)
  size <- 4

  indices <- list(
    c(TRUE, FALSE, FALSE, TRUE),
    c(FALSE, TRUE, TRUE, FALSE)
  )
  expect_identical_list_combine(
    x = values,
    indices = indices,
    size = size,
    expect = int(1, 3, 4, 2)
  )

  indices <- list(
    c(1, 4),
    c(2, 3)
  )
  expect_identical_list_combine(
    x = values,
    indices = indices,
    size = size,
    expect = int(1, 3, 4, 2)
  )
})

test_that("`list_combine()` with `slice_x = FALSE` / recycling", {
  values <- list(1L, 2L)
  size <- 4

  indices <- list(
    c(TRUE, FALSE, FALSE, TRUE),
    c(FALSE, TRUE, TRUE, FALSE)
  )
  expect_identical_list_combine(
    x = values,
    indices = indices,
    size = size,
    expect = int(1, 2, 2, 1)
  )

  indices <- list(
    c(1, 4),
    c(2, 3)
  )
  expect_identical_list_combine(
    x = values,
    indices = indices,
    size = size,
    expect = int(1, 2, 2, 1)
  )
})

test_that("`list_combine()` with `slice_x = TRUE`", {
  values <- list(1:4, 5:8)
  size <- 4

  indices <- list(
    c(TRUE, FALSE, FALSE, TRUE),
    c(FALSE, TRUE, TRUE, FALSE)
  )
  expect_identical_list_combine(
    x = values,
    indices = indices,
    size = size,
    slice_x = TRUE,
    expect = int(1, 6, 7, 4)
  )

  indices <- list(
    c(1, 4),
    c(2, 3)
  )
  expect_identical_list_combine(
    x = values,
    indices = indices,
    size = size,
    slice_x = TRUE,
    expect = int(1, 6, 7, 4)
  )
})

test_that("`list_combine()` with `slice_x = TRUE` / recycling", {
  values <- list(1L, 2L)
  size <- 4

  indices <- list(
    c(TRUE, FALSE, FALSE, TRUE),
    c(FALSE, TRUE, TRUE, FALSE)
  )
  expect_identical_list_combine(
    x = values,
    indices = indices,
    size = size,
    slice_x = TRUE,
    expect = int(1, 2, 2, 1)
  )

  indices <- list(
    c(1, 4),
    c(2, 3)
  )
  expect_identical_list_combine(
    x = values,
    indices = indices,
    size = size,
    slice_x = TRUE,
    expect = int(1, 2, 2, 1)
  )
})

test_that("`list_combine()` with logical `indices` checks `indices` size", {
  values <- list(1L, 2L)
  indices <- list(
    c(TRUE, FALSE, FALSE, TRUE),
    c(FALSE, TRUE, TRUE, FALSE)
  )
  size <- 5

  # This isn't the most obvious error but it is hard to know how to do better.
  # Ideally it would report a size error for `indices`, right now it falls
  # through to `list_as_locations()` which doesn't allow logical indices.
  expect_snapshot_list_combine(
    error = TRUE,
    x = values,
    indices = indices,
    size = size
  )
})

test_that("`multiple` can let first index win", {
  x <- list(
    1:3,
    4:6
  )
  indices <- list(
    c(1, 2, 3),
    c(2, 3, 4)
  )
  size <- 4

  expect_identical_list_combine(
    x = x,
    indices = indices,
    size = size,
    multiple = "first",
    expect = int(1, 2, 3, 6)
  )
  expect_identical_list_combine(
    x = x,
    indices = indices,
    size = size,
    multiple = "last",
    expect = int(1, 4, 5, 6)
  )
})

test_that("`multiple` works with data frames", {
  x <- list(
    data_frame(a = 1:3, b = foobar(4:6), c = foobar(c(7, 8, 9))),
    data_frame(a = 4:6, b = foobar(7:9), c = foobar(c(10, 11, 12)))
  )
  indices <- list(
    c(1, 2, 3),
    c(2, 3, 4)
  )
  size <- 4

  # Normal and homogeneous fallback mixed
  expect_identical(
    list_combine(
      x = x,
      indices = indices,
      size = size,
      multiple = "first"
    ),
    data_frame(
      a = int(1, 2, 3, 6),
      b = foobar(int(4, 5, 6, 9)),
      c = foobar(dbl(7, 8, 9, 12))
    )
  )

  # Normal and `c()` fallback mixed
  with_c_foobar({
    expect_identical(
      list_combine(
        x = x,
        indices = indices,
        size = size,
        multiple = "first"
      ),
      data_frame(
        a = int(1, 2, 3, 6),
        b = foobar_c(int(4, 5, 6, 9)),
        c = foobar_c(dbl(7, 8, 9, 12))
      )
    )
  })
})

test_that("`multiple` works with data frame columns", {
  x <- list(
    data_frame(
      a = 1:3,
      b = data_frame(x = c("a", "b", "c"), y = foobar(4:6)),
      c = foobar(c(7, 8, 9))
    ),
    data_frame(
      a = 4:6,
      b = data_frame(x = c("d", "e", "f"), y = foobar(7:9)),
      c = foobar(c(10, 11, 12))
    )
  )
  indices <- list(
    c(1, 2, 3),
    c(2, 3, 4)
  )
  size <- 4

  # Normal and homogeneous fallback mixed
  expect_identical(
    list_combine(
      x = x,
      indices = indices,
      size = size,
      multiple = "first"
    ),
    data_frame(
      a = int(1, 2, 3, 6),
      b = data_frame(
        x = c("a", "b", "c", "f"),
        y = foobar(int(4, 5, 6, 9))
      ),
      c = foobar(dbl(7, 8, 9, 12))
    )
  )

  # Normal and `c()` fallback mixed
  with_c_foobar({
    expect_identical(
      list_combine(
        x = x,
        indices = indices,
        size = size,
        multiple = "first"
      ),
      data_frame(
        a = int(1, 2, 3, 6),
        b = data_frame(
          x = c("a", "b", "c", "f"),
          y = foobar_c(int(4, 5, 6, 9))
        ),
        c = foobar_c(dbl(7, 8, 9, 12))
      )
    )
  })
})

test_that("`multiple` shows correctly indexed errors", {
  # In fallback, reversal happens after recycling and slicing
  expect_snapshot_list_combine(
    error = TRUE,
    x = list(1:2, 3L),
    indices = list(1:3, 4),
    size = 4,
    multiple = "first"
  )
  expect_snapshot_list_combine(
    error = TRUE,
    x = list(1:2, 3L),
    indices = list(1:3, 4),
    size = 4,
    multiple = "last"
  )

  # If there is only 1 issue in `x` sizes, they report consistently
  expect_snapshot_list_combine(
    error = TRUE,
    x = list(1:4, 3:5),
    indices = list(1:3, 4),
    size = 4,
    slice_x = TRUE,
    multiple = "first"
  )
  expect_snapshot_list_combine(
    error = TRUE,
    x = list(1:4, 3:5),
    indices = list(1:3, 4),
    size = 4,
    slice_x = TRUE,
    multiple = "last"
  )

  # If there are multiple `x` issues, because we reverse the iteration
  # order in the main path in the `multiple = "first"` case, we end up
  # reporting the last problem first, while the fallback case still
  # reports the fist problem first. The indices in the error are correct
  # in both cases, so this inconsistency is allowed.
  expect_snapshot_list_combine(
    error = TRUE,
    x = list(1:2, 3:5),
    indices = list(1:3, 4),
    size = 4,
    slice_x = TRUE,
    multiple = "first"
  )
  expect_snapshot_list_combine(
    error = TRUE,
    x = list(1:2, 3:5),
    indices = list(1:3, 4),
    size = 4,
    slice_x = TRUE,
    multiple = "last"
  )
})

test_that("`multiple` also applies to names", {
  expect_identical_list_combine(
    x = list(c(a = 1, b = 2), c(c = 3)),
    indices = list(1:2, 2),
    size = 2,
    multiple = "first",
    expect = c(a = 1, b = 2)
  )
  expect_identical_list_combine(
    x = list(c(a = 1, b = 2), c(c = 3)),
    indices = list(1:2, 2),
    size = 2,
    multiple = "last",
    expect = c(a = 1, c = 3)
  )
})

test_that("`multiple` doesn't affect `default`", {
  expect_identical_list_combine(
    x = list(1:2, 4L),
    indices = list(c(1, 4), 4),
    size = 5,
    multiple = "first",
    default = 0L,
    expect = int(1, 0, 0, 2, 0)
  )
  expect_identical_list_combine(
    x = list(1:2, 4L),
    indices = list(c(1, 4), 4),
    size = 5,
    multiple = "last",
    default = 0L,
    expect = int(1, 0, 0, 4, 0)
  )
})

test_that("`multiple` doesn't apply WITHIN a single index", {
  # You always get the last value within a single index vector.
  # That possibly makes `multiple = "first"` a little confusing,
  # but `multiple` is mostly useful with logical vectors anyways
  # (case_when() style approach), so it doesn't matter much.
  expect_identical_list_combine(
    x = list(1:2, 3:4),
    indices = list(c(1, 1), c(1, 1)),
    size = 1,
    multiple = "first",
    expect = 2L
  )
  expect_identical_list_combine(
    x = list(1:2, 3:4),
    indices = list(c(1, 1), c(1, 1)),
    size = 1,
    multiple = "last",
    expect = 4L
  )
})

test_that("`compact_seq()` work as `indices`", {
  expect_identical_list_combine(
    x = list(1:3, 4:5),
    indices = list(compact_seq(2, 3), compact_seq(0, 2)),
    size = 5,
    expect = int(4:5, 1:3)
  )
  expect_identical_list_combine(
    x = list(1:3, 4L),
    indices = list(compact_seq(2, 3), 1),
    size = 5,
    expect = int(4, NA, 1:3)
  )
})

test_that("`compact_seq()` `indices` work with `unmatched`", {
  expect_snapshot_list_combine(
    error = TRUE,
    x = list(1:2, 4:5),
    indices = list(compact_seq(3, 2), compact_seq(0, 2)),
    size = 5,
    unmatched = "error"
  )
  expect_snapshot_list_combine(
    error = TRUE,
    x = list(1:2, 4L),
    indices = list(compact_seq(3, 2), 1),
    size = 5,
    unmatched = "error"
  )
})

test_that("`compact_seq()` `indices` work with `default`", {
  expect_identical_list_combine(
    x = list(1:2, 4:5),
    indices = list(compact_seq(3, 2), compact_seq(0, 2)),
    size = 5,
    default = 0L,
    expect = int(4:5, 0L, 1:2)
  )
  expect_identical_list_combine(
    x = list(1:2, 4L),
    indices = list(compact_seq(3, 2), 1),
    size = 5,
    default = 0L,
    expect = int(4L, 0L, 0L, 1:2)
  )
})
