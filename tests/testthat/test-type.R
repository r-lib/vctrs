test_that("vec_ptype() is a no-op for NULL", {
  expect_null(vec_ptype(NULL))
})

test_that("vec_ptype() errors on scalars", {
  expect_error(vec_ptype(quote(name)), class = "vctrs_error_scalar_type")
  expect_error(vec_ptype(quote(fn())), class = "vctrs_error_scalar_type")
})

test_that(".ptype argument overrides others", {
  expect_equal(vec_ptype_common(.ptype = 1:10), numeric())
})

test_that(".ptype required in strict mode", {
  old <- options(vctrs.no_guessing = TRUE)
  on.exit(options(old))

  expect_error(vec_ptype_common(), "strict mode")
})

test_that("can feed ptype into itself", {
  expect_equal(vec_ptype_common(vec_ptype_common(1:10)), numeric())
})

test_that("finalised prototypes created from under specified inputs", {
  expect_equal(vec_ptype_common(), NULL)
  expect_equal(vec_ptype_common(NULL), NULL)

  expect_equal(vec_ptype_common(NA), logical())
  expect_equal(vec_ptype_common(NA, NULL), logical())
  expect_equal(vec_ptype_common(NULL, NA), logical())
})

test_that("finalised prototypes created from under specified data frame cols", {
  df <- data.frame(x = NA)
  expect_equal(vec_ptype_common(df)$x, logical())
})

test_that("non-missing logical get correct type", {
  expect_equal(vec_ptype_common(TRUE), logical())
})

test_that("output tests", {
  expect_snapshot(vec_ptype_show())
  expect_snapshot(vec_ptype_show(integer()))
  expect_snapshot(vec_ptype_show(integer(), double()))
  expect_snapshot(vec_ptype_show(logical(), integer(), double()))
})

test_that("vec_ptype_common() handles matrices", {
  m <- matrix(1:4, nrow = 2)
  expect_identical(vec_ptype_common(m, m), matrix(int(), ncol = 2))
})

test_that("vec_ptype_common() doesn't mutate input", {
  x <- list(a = 1L, b = 2)
  expect_identical(vec_ptype_common(!!!x), numeric())
  expect_identical(x, list(a = 1L, b = 2))
})

test_that("vec_ptype_common() includes index in argument tag", {
  df1 <- tibble(x = tibble(y = tibble(z = 1)))
  df2 <- tibble(x = tibble(y = tibble(z = "a")))

  # Create a column name too large for default buffer
  nm <- str_dup("foobarfoobar", 10)
  large_df1 <- set_names(df1, nm)
  large_df2 <- set_names(df2, nm)

  expect_snapshot(error = TRUE, vec_ptype_common(df1, df2))
  expect_snapshot(error = TRUE, vec_ptype_common(df1, df1, df2))
  expect_snapshot(error = TRUE, vec_ptype_common(large_df1, large_df2))

  # Names
  expect_snapshot(error = TRUE, vec_ptype_common(foo = TRUE, bar = "foo"))
  expect_snapshot(
    error = TRUE,
    vec_ptype_common(foo = TRUE, baz = FALSE, bar = "foo")
  )
  expect_snapshot(error = TRUE, vec_ptype_common(foo = df1, bar = df2))
  expect_snapshot(error = TRUE, vec_ptype_common(df1, df1, bar = df2))

  # One splice box
  expect_snapshot(error = TRUE, vec_ptype_common(TRUE, !!!list(1, "foo")))
  expect_snapshot(error = TRUE, vec_ptype_common(TRUE, !!!list(1, 2), "foo"))
  expect_snapshot(
    error = TRUE,
    vec_ptype_common(1, !!!list(TRUE, FALSE), "foo")
  )

  # One named splice box
  expect_snapshot(
    error = TRUE,
    vec_ptype_common(foo = TRUE, !!!list(FALSE, FALSE), bar = "foo")
  )
  expect_snapshot(
    error = TRUE,
    vec_ptype_common(foo = TRUE, !!!list(bar = 1, "foo"))
  )
  expect_snapshot(
    error = TRUE,
    vec_ptype_common(foo = TRUE, !!!list(bar = "foo"))
  )
  expect_snapshot(
    error = TRUE,
    vec_ptype_common(foo = TRUE, !!!list(bar = FALSE), baz = "chr")
  )

  # Two splice boxes in next and current
  expect_snapshot(
    error = TRUE,
    vec_ptype_common(foo = TRUE, !!!list(bar = FALSE), !!!list(baz = "chr"))
  )
})

test_that("vec_ptype_common() of many data frames is their pairwise common type", {
  xs <- list(
    data_frame(a = 1L, b = "x"),
    data_frame(c = factor("u"), a = 2),
    NULL,
    data_frame(b = factor("y"), d = new_date(0)),
    tibble(e = data_frame(p = 1), c = factor("v")),
    data_frame(f = NA, e = data_frame(q = "z"), a = TRUE),
    NULL,
    data_frame(d = new_datetime(0, tzone = "UTC"), f = 1L)
  )
  expect_identical(
    vec_ptype_common(!!!xs),
    vec_ptype_finalise(Reduce(vec_ptype2, xs, NULL))
  )

  # Enough columns to grow the hash table, in an order that differs from the
  # order they first appeared in
  wide <- lapply(1:40, function(i) {
    new_data_frame(set_names(list(i, i), sprintf("w%02d", c(i, 41 - i))))
  })
  expect_identical(
    vec_ptype_common(!!!wide),
    vec_ptype_finalise(Reduce(vec_ptype2, wide, NULL))
  )
})

test_that("vec_ptype_common() of many data frames handles the common class fallback", {
  local_methods(
    c.vctrs_foobar = function(...) foobar(NextMethod())
  )

  # `vec_ptype2()` turns `x` into a fallback type even though no other data
  # frame has it
  xs <- list(
    data_frame(x = foobar(1)),
    data_frame(y = foobar(2)),
    data_frame(z = 3),
    data_frame(y = foobar(4))
  )
  expect_identical(
    vec_ptype_common_fallback(!!!xs),
    vec_ptype_finalise(Reduce(
      function(acc, x) vec_ptype2_opts(acc, x, opts = enabled_fallback_opts()),
      xs,
      NULL
    ))
  )
})

test_that("vec_ptype_common() of many tibbles retypes data frame columns at `NULL`", {
  local_methods(
    c.vctrs_foobar = function(...) quux(NextMethod())
  )

  # `vec_ptype2()` of a tibble and `NULL` turns `b$x` into a fallback type
  xs <- list(
    tibble(b = data_frame(x = foobar(1))),
    tibble(a = 1),
    NULL,
    tibble(c = 1),
    tibble(d = 2),
    NULL,
    tibble(e = 3, b = data_frame(y = foobar(4)))
  )
  expect_identical(
    vec_ptype_common_fallback(!!!xs),
    vec_ptype_finalise(Reduce(
      function(acc, x) vec_ptype2_opts(acc, x, opts = enabled_fallback_opts()),
      xs,
      NULL
    ))
  )

  # So `vec_rbind()` combines it with `c()`
  xs <- list(
    tibble(b = data_frame(x = foobar(1))),
    tibble(a = 1),
    NULL,
    tibble(c = 1),
    tibble(d = 2)
  )
  out <- vec_rbind(!!!xs)
  expect_identical(out$b$x, quux(c(1, NA, NA, NA)))
})

test_that("vec_ptype_common() of many data frames matches names like `vec_match()`", {
  utf8 <- "caf\u00e9"
  latin1 <- iconv(utf8, "UTF-8", "latin1")

  xs <- list(
    new_data_frame(set_names(list(1), utf8)),
    data_frame(a = 1),
    data_frame(b = 1),
    new_data_frame(set_names(list(2L), latin1)),
    data_frame(a = 1)
  )
  out <- vec_ptype_common(!!!xs)
  expect_identical(out, vec_ptype_finalise(Reduce(vec_ptype2, xs, NULL)))
  expect_named(out, c(utf8, "a", "b"))

  # Duplicate and missing names
  xs <- list(
    data_frame(a = 1),
    data_frame(b = 1),
    new_data_frame(list(c = 1, c = "x")),
    data_frame(a = 1),
    new_data_frame(set_names(list(1), NA_character_)),
    data_frame(b = 1)
  )
  expect_identical(
    vec_ptype_common(!!!xs),
    vec_ptype_finalise(Reduce(vec_ptype2, xs, NULL))
  )
})

test_that("vec_ptype_common() of many data frames reports the same incompatible column", {
  expect_snapshot(error = TRUE, {
    vec_ptype_common(
      data_frame(a = 1),
      data_frame(b = 1),
      data_frame(c = 1),
      data_frame(b = "x")
    )
  })

  # Columns are combined in the order they first appeared in
  expect_snapshot(error = TRUE, {
    vec_ptype_common(
      data_frame(a = 1, b = 1),
      data_frame(c = 1),
      data_frame(d = 1),
      data_frame(b = "x", a = "x")
    )
  })

  # A tibble takes over as the current input
  expect_snapshot(error = TRUE, {
    vec_ptype_common(
      x = data_frame(a = 1),
      y = data_frame(b = 1),
      z = tibble(c = 1),
      w = data_frame(d = 1),
      v = data_frame(a = "x")
    )
  })
})

test_that("proxied types are have s3 bare type", {
  for (x in proxied_empty_types) {
    expect_identical(vec_typeof_bare(x), "s3")
  }
})

test_that("vec_ptype() preserves attributes of unproxied structures", {
  expect_identical(vec_ptype(foobar(dbl(1))), foobar(dbl()))
})

test_that("vec_ptype() errors on scalar lists", {
  expect_error(vec_ptype(foobar(list())), class = "vctrs_error_scalar_type")
})

test_that("can retrieve type info", {
  exp <- list(type = "integer", had_proxy_method = FALSE)
  expect_identical(vec_type_info(1:3), exp)

  exp <- list(type = "s3", had_proxy_method = FALSE)
  expect_identical(vec_type_info(~foo), exp)

  x <- as.POSIXlt(new_datetime(0))
  exp <- list(type = "s3", had_proxy_method = TRUE)
  expect_identical(vec_type_info(x), exp)
})

test_that("can retrieve proxy info", {
  exp <- list(type = "integer", had_proxy_method = FALSE, proxy = 1:3)
  expect_identical(vec_proxy_info(1:3), exp)

  exp <- list(type = "scalar", had_proxy_method = FALSE, proxy = ~foo)
  expect_identical(vec_proxy_info(~foo), exp)

  x <- as.POSIXlt(new_datetime(0))
  proxy <- new_data_frame(unclass(x))
  exp <- list(
    type = "dataframe",
    had_proxy_method = TRUE,
    proxy = proxy
  )
  expect_identical(vec_proxy_info(x), exp)
})

test_that("class_type() detects classes", {
  expect_identical(class_type(list()), "none")
  expect_identical(class_type(foobar(list())), "unknown")
  expect_identical(class_type(structure(list(), class = "list")), "list")
  expect_identical(
    class_type(subclass(structure(list(), class = "list"))),
    "list"
  )
  expect_identical(
    class_type(I(subclass(structure(list(), class = "list")))),
    "list"
  )

  expect_identical(class_type(I(list())), "bare_asis")
  expect_identical(class_type(I(1)), "bare_asis")

  expect_identical(class_type(data.frame()), "bare_data_frame")
  expect_identical(class_type(tibble::tibble()), "bare_tibble")
  expect_identical(class_type(subclass(data.frame())), "data_frame")

  expect_identical(class_type(new_factor()), "bare_factor")
  expect_identical(class_type(new_ordered()), "bare_ordered")
  expect_identical(class_type(subclass(new_factor())), "unknown")
  expect_identical(class_type(subclass(new_ordered())), "unknown")

  expect_identical(class_type(new_date()), "bare_date")
  expect_identical(class_type(new_datetime()), "bare_posixct")
  expect_identical(class_type(as.POSIXlt(new_date())), "bare_posixlt")
  expect_identical(class_type(subclass(new_date())), "unknown")
  expect_identical(class_type(subclass(new_datetime())), "unknown")

  expect_identical(class_type(NA), "none")
  expect_identical(class_type(foobar()), "unknown")
})

test_that("vec_ptype() handles class-less yet OBJECT gremlins", {
  gremlin <- stats::model.frame(freeny)
  expect_error(vec_ptype(gremlin), NA)
  expect_error(vec_c(gremlin), NA)
  expect_error(vec_init(gremlin), NA)
  expect_error(vec_slice(gremlin, 1), NA)
})

test_that("explicit list subclasses are vectors", {
  list_subclass <- function(x) {
    structure(x, class = c("custom_subclass", "list"))
  }

  x <- list_subclass(list())
  expect_true(vec_is(x))

  df <- data.frame(x = 1:2)
  df$z <- list_subclass(list(1, 2))

  expect_identical(vec_slice(df, 1)$z, list_subclass(list(1)))
})

test_that("the type of a classed data frame with an unspecified column retains unspecifiedness", {
  df1 <- subclass(data_frame(x = 1, y = NA))
  df2 <- subclass(data_frame(x = 1, y = unspecified(1)))
  expect <- subclass(data_frame(x = numeric(), y = unspecified()))

  expect_identical(vec_ptype(df1), expect)
  expect_identical(vec_ptype(df2), expect)
})

test_that("vec_ptype() methods can be written", {
  local_methods(
    vec_ptype.vctrs_foobar = function(x, ...) "dispatch"
  )
  expect_identical(vec_ptype(foobar()), "dispatch")
})

test_that("vec_ptype_finalise() works with NULL", {
  expect_identical(vec_ptype_finalise(NULL), NULL)
})

test_that("vec_ptype_finalise() works recursively over bare data frames", {
  df <- new_data_frame(list(
    x = numeric(),
    y = unspecified()
  ))
  expect <- data_frame(x = numeric(), y = logical())

  expect_identical(vec_ptype_finalise(df), expect)
})

test_that("vec_ptype_finalise() works recursively over classed data frames", {
  df <- new_data_frame(list(
    x = numeric(),
    y = unspecified()
  ))
  df <- subclass(df)
  expect <- subclass(data_frame(x = numeric(), y = logical()))

  expect_identical(vec_ptype_finalise(df), expect)
})

test_that("vec_ptype_finalise() can handle data frame columns", {
  df <- data_frame(x = numeric(), y = data_frame(z = unspecified()))
  expect <- data_frame(x = numeric(), y = data_frame(z = logical()))

  expect_identical(vec_ptype_finalise(df), expect)
})

test_that("vec_ptype_finalise() requires vector types", {
  expect_error(
    vec_ptype_finalise(quote(name)),
    class = "vctrs_error_scalar_type"
  )
  expect_error(vec_ptype_finalise(foobar()), class = "vctrs_error_scalar_type")
})

# This might change in the future if we decide that prototypes don't
# have names
test_that("vec_ptype() preserves type of names and row names", {
  expect_identical(vec_ptype(c(foo = 1)), named(dbl()))
  expect_identical(vec_ptype(mtcars), mtcars[0, ])
  expect_identical(vec_ptype(foobar(mtcars)), foobar(mtcars[0, ]))
})

test_that("vec_ptype_common() handles spliced names consistently (#1570)", {
  args1 <- list(a = "foo", b = "bar")
  args2 <- list(y = NULL, z = 1)

  y_name <- "y"
  z_name <- "z"

  expect_snapshot(error = TRUE, {
    vec_ptype_common(
      a = "foo",
      b = "bar",
      y = NULL,
      z = 1
    )

    vec_ptype_common(
      !!!args1,
      !!!args2
    )

    vec_ptype_common(
      !!!args1,
      "{y_name}" := NULL,
      "{z_name}" := 1
    )
  })
})

test_that("vec_ptype() and vec_ptype2() don't finalize their output", {
  expect_identical(vec_ptype(NA), unspecified())
  expect_identical(vec_ptype2(NA, NA), unspecified())
  expect_identical(vec_ptype2(NA, NULL), unspecified())
  expect_identical(vec_ptype2(NULL, NA), unspecified())

  expect_identical(vec_ptype(unspecified()), unspecified())
  expect_identical(vec_ptype2(unspecified(), unspecified()), unspecified())
  expect_identical(vec_ptype2(unspecified(), NULL), unspecified())
  expect_identical(vec_ptype2(NULL, unspecified()), unspecified())
})

test_that("vec_ptype_common() always finalizes its output (#2099)", {
  expect_identical(vec_ptype_common(NA), logical())
  expect_identical(vec_ptype_common(NA, NA), logical())
  expect_identical(vec_ptype_common(unspecified(1)), logical())
  expect_identical(vec_ptype_common(unspecified(1), unspecified(1)), logical())

  # Even explicit `.ptype`s
  expect_identical(
    vec_ptype_common(.ptype = NA),
    logical()
  )
  expect_identical(
    vec_ptype_common(.ptype = unspecified(1)),
    logical()
  )
})

test_that("vec_ptype_common() lets you opt out of ptype finalization (#2094)", {
  expect_identical(
    vec_ptype_common(NA, .finalise = FALSE),
    unspecified()
  )
  expect_identical(
    vec_ptype_common(NA, NA, .finalise = FALSE),
    unspecified()
  )
  expect_identical(
    vec_ptype_common(unspecified(1), .finalise = FALSE),
    unspecified()
  )
  expect_identical(
    vec_ptype_common(unspecified(1), unspecified(1), .finalise = FALSE),
    unspecified()
  )

  # Works for explicit `.ptype` too
  expect_identical(
    vec_ptype_common(.ptype = NA, .finalise = FALSE),
    unspecified()
  )
  expect_identical(
    vec_ptype_common(.ptype = unspecified(1), .finalise = FALSE),
    unspecified()
  )
})

test_that("vec_ptype_common_params() lets you opt out of ptype finalization", {
  expect_identical(
    vec_ptype_common_params(NA, .finalise = FALSE),
    unspecified()
  )
  # Works for explicit `.ptype` too
  expect_identical(
    vec_ptype_common_params(.ptype = NA, .finalise = FALSE),
    unspecified()
  )
})

test_that("`.finalise` is validated", {
  expect_snapshot(error = TRUE, {
    vec_ptype_common(.finalise = 1)
  })
  expect_snapshot(error = TRUE, {
    vec_ptype_common_params(.finalise = 1)
  })
})
