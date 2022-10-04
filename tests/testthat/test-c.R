local_name_repair_quiet()

test_that("zero length input returns NULL", {
  expect_equal(vec_c(), NULL)
  expect_equal(vec_c(NULL), NULL)
  expect_equal(vec_c(NULL,), NULL)
  expect_equal(vec_c(NULL, NULL), NULL)
})

test_that("NULL is idempotent", {
  expect_equal(vec_c(NULL, 1L), 1L)
  expect_equal(vec_c(1L, NULL), 1L)
})

test_that("NA is idempotent", {
  expect_equal(vec_c(NA, 1L), c(NA, 1L))
  expect_equal(vec_c(NA, "x"), c(NA, "x"))
  expect_equal(vec_c(NA, factor("x")), factor(c(NA, "x")))
  expect_equal(vec_c(NA, new_date(0)), new_date(c(NA, 0)))
  expect_equal(vec_c(NA, new_datetime(0)), new_datetime(c(NA, 0)))
  expect_equal(vec_c(NA, new_duration(0)), new_duration(c(NA, 0)))
})

test_that("NA is logical if no other types intervene", {
  expect_equal(vec_c(logical()), logical())
  expect_equal(vec_c(NA), NA)
  expect_equal(vec_c(NA, NA), c(NA, NA))
})


test_that("different types are coerced to common", {
  expect_equal(vec_c(TRUE, 1L, 1), c(1, 1, 1))
  expect_equal(vec_c(TRUE, 2:4), 1:4)
})

test_that("specified .ptypes do not allow more casts", {
  expect_error(
    vec_c(TRUE, .ptype = character()),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("common type failure uses error call and error arg (#1641, #1692)", {
  expect_snapshot(error = TRUE, {
    vec_c("x", 1, .error_call = call("foo"), .error_arg = "arg")
  })
  expect_snapshot(error = TRUE, {
    vec_c("x", .ptype = integer(), .error_call = call("foo"), .error_arg = "arg")
  })
})

test_that("common type failure uses positional errors", {
  expect_snapshot({
    # Looking for `..1` and `a`
    (expect_error(vec_c(1, a = "x", 2)))

    # Directed cast should also produce positional errors (#1690)
    (expect_error(vec_c(1, a = "x", 2, .ptype = double(), .error_arg = "arg")))

    # Lossy cast
    (expect_error(vec_c(1, a = 2.5, .ptype = integer())))
  })
})

test_that("combines outer an inner names", {
  expect_equal(vec_c(x = 1), c(x = 1))
  expect_equal(vec_c(c(x = 1)), c(x = 1))

  expect_equal(vec_c(c(x = 1:2)), c(x1 = 1, x2 = 2))
  expect_error(vec_c(y = c(x = 1)), "Please supply")
})

test_that("can bind data.frame columns", {
  df <- data.frame(x = NA, y = 1:2)
  df$x <- data.frame(a = 1:2)

  expected <- data.frame(x = NA, y = c(1:2, 1:2))
  expected$x <- data.frame(a = c(1:2, 1:2))

  expect_equal(vec_c(df, df), expected)
})

test_that("vec_c() handles matrices", {
  m <- matrix(1:4, nrow = 2)
  dimnames(m) <- list(c("foo", "bar"), c("baz", "quux"))

  # FIXME: `vec_ptype_common(m, m)` doesn't return dimension names
  exp <- matrix(c(1:2, 1:2, 3:4, 3:4), nrow = 4)
  rownames(exp) <- c("foo", "bar", "foo", "bar")

  expect_identical(vec_c(m, m), exp)

  expect_error(vec_c(outer = m), "Please supply")
})

test_that("vec_c() includes index in argument tag", {
  df1 <- tibble(x = tibble(y = tibble(z = 1)))
  df2 <- tibble(x = tibble(y = tibble(z = "a")))

  expect_snapshot(error = TRUE, vec_c(df1, df2))
  expect_snapshot(error = TRUE, vec_c(df1, df1, df2))
  expect_snapshot(error = TRUE, vec_c(foo = df1, bar = df2))
})

test_that("vec_c() handles record classes", {
  local_rational_class()

  out <- vec_c(rational(1, 2), 1L, NA)

  expect_true(vec_is(out, rational(1, 2)))
  expect_size(out, 3)
  expect_identical(vec_proxy(out), data.frame(n = c(1L, 1L, NA), d = c(2L, 1L, NA)))
})

test_that("can mix named and unnamed vectors (#271)", {
  expect_identical(vec_c(c(a = 1), 2), c(a = 1, 2))
  expect_identical(vec_c(0, c(a = 1), 2, b = 3), c(0, a = 1, 2, b =3))
})

test_that("preserves names when inputs are cast to a common type (#1690)", {
  expect_named(vec_c(c(a = 1), .ptype = integer()), "a")
  expect_named(vec_c(foo = c(a = 1), .ptype = integer(), .name_spec = "{outer}_{inner}"), "foo_a")
})

test_that("vec_c() repairs names", {
  local_name_repair_quiet()

  # Default minimal repair
  expect_named(vec_c(a = 1, a = 2, `_` = 3), c("a", "a", "_"))
  out <- vec_c(!!!set_names(1, NA))
  expect_named(out, "")

  expect_named(vec_c(a = 1, a = 2, `_` = 3, .name_repair = "unique"), c("a...1", "a...2", "_"))
  expect_error(vec_c(a = 1, a = 2, `_` = 3, .name_repair = "check_unique"), class = "vctrs_error_names_must_be_unique")

  expect_named(vec_c(a = 1, a = 2, `_` = 3, .name_repair = "universal"), c("a...1", "a...2", "._"))

  expect_named(vec_c(a = 1, a = 2, .name_repair = ~ toupper(.)), c("A", "A"))
})

test_that("vec_c() can repair names quietly", {
  local_name_repair_verbose()

   expect_snapshot({
     res_unique <- vec_c(x = TRUE, x = 0, .name_repair = "unique_quiet")
     res_universal <- vec_c("if" = TRUE, "in" = 0, .name_repair = "universal_quiet")
   })
   expect_named(res_unique, c("x...1", "x...2"))
   expect_named(res_universal, c(".if", ".in"))
})

test_that("vec_c() doesn't use outer names for data frames (#524)", {
  x <- data.frame(inner = 1)
  expect_equal(vec_c(outer = x), x)

  a <- data.frame(x = 1L)
  b <- data.frame(x = 2L)
  expect_equal(vec_c(foo = a, bar = b), data.frame(x = 1:2))
})

test_that("vec_c() preserves row names and inner names", {
  x <- data.frame(a = 1, row.names = "r1")
  y <- data.frame(a = 2, row.names = "r2")
  expect_equal(rownames(vec_c(x, y)), c("r1", "r2"))
  expect_equal(rownames(vec_c(x, x)), c("r1...1", "r1...2"))

  vec_x <- set_names(1:3, letters[1:3])
  vec_y <- c(FOO = 4L)
  oo_x <- set_names(as.POSIXlt(c("2020-01-01", "2020-01-02", "2020-01-03")), letters[1:3])
  oo_y <- as.POSIXlt(c(FOO = "2020-01-04"))
  df_x <- new_data_frame(list(x = 1:3), row.names = letters[1:3])
  df_y <- new_data_frame(list(x = 4L), row.names = "d")
  mat_x <- matrix(1:3, 3, dimnames = list(letters[1:3]))
  mat_y <- matrix(4L, 1, dimnames = list("d"))
  nested_x <- new_data_frame(
    list(df = df_x, mat = mat_x, vec = vec_x, oo = oo_x),
    row.names = c("foo", "bar", "baz")
  )
  nested_y <- new_data_frame(
    list(df = df_y, mat = mat_y, vec = vec_y, oo = oo_y),
    row.names = c("quux")
  )

  nested_out <- vec_c(nested_x, nested_y)
  expect_identical(row.names(nested_out), c("foo", "bar", "baz", "quux"))
  expect_identical(row.names(nested_out$df), c("a", "b", "c", "d"))
  expect_identical(row.names(nested_out$mat), c("a", "b", "c", "d"))
  expect_identical(names(nested_out$vec), c("a", "b", "c", "FOO"))
  expect_identical(names(nested_out$oo), c("a", "b", "c", "FOO"))
})

test_that("vec_c() outer names work with proxied objects", {
  x <- as.POSIXlt(new_datetime(0))
  exp <- set_names(x, "outer")
  expect_equal(vec_c(outer = x), exp)

  named_x <- set_names(x, "inner")
  exp <- set_names(named_x, "outer_inner")
  expect_error(vec_c(outer = named_x), "Please supply")
  expect_equal(vec_c(outer = named_x, .name_spec = "{outer}_{inner}"), exp)

  xs <- as.POSIXlt(new_datetime(c(0, 1)))
  exp <- set_names(xs, c("outer_1", "outer_2"))
  expect_error(vec_c(outer = xs), "Please supply")
  expect_equal(vec_c(outer = xs, .name_spec = "{outer}_{inner}"), exp)
})

test_that("vec_c() works with simple homogeneous foreign S3 classes", {
  expect_identical(vec_c(foobar(1), foobar(2)), vec_c(foobar(c(1, 2))))
  expect_identical(vec_c(NULL, foobar(1), foobar(2)), vec_c(foobar(c(1, 2))))
})

test_that("vec_c() works with simple homogeneous foreign S4 classes", {
  joe1 <- .Counts(c(1L, 2L), name = "Joe")
  joe2 <- .Counts(3L, name = "Joe")
  expect_identical(vec_c(joe1, joe2), .Counts(1:3, name = "Joe"))
})

test_that("vec_c() fails with complex foreign S3 classes", {
  expect_snapshot({
    x <- structure(foobar(1), attr_foo = "foo")
    y <- structure(foobar(2), attr_bar = "bar")
    (expect_error(vec_c(x, y), class = "vctrs_error_incompatible_type"))
    (expect_error(vec_c(x, y, .error_call = call("foo"), .error_arg = "arg"), class = "vctrs_error_incompatible_type"))
  })
})

test_that("vec_c() fails with complex foreign S4 classes", {
  expect_snapshot({
    joe <- .Counts(c(1L, 2L), name = "Joe")
    jane <- .Counts(3L, name = "Jane")
    (expect_error(vec_c(joe, jane), class = "vctrs_error_incompatible_type"))
    (expect_error(vec_c(joe, jane, .error_call = call("foo"), .error_arg = "arg"), class = "vctrs_error_incompatible_type"))
  })
})

test_that("vec_c() falls back to c() if S3 method is available", {
  # Check off-by-one error
  expect_error(
    vec_c(foobar(1), "", foobar(2)),
    class = "vctrs_error_incompatible_type"
  )

  # Fallback when the class implements `c()`
  method <- function(...) rep_along(list(...), "dispatched")
  local_methods(
    c.vctrs_foobar = method
  )
  expect_identical(
    vec_c(foobar(1), foobar(2, class = "foo")),
    c("dispatched", "dispatched")
  )
  expect_identical(
    vec_c(NULL, foobar(1), NULL, foobar(2, class = "foo")),
    c("dispatched", "dispatched")
  )

  # Registered fallback
  s3_register("base::c", "vctrs_c_fallback", method)
  expect_identical(
    vec_c(
      structure(1, class = "vctrs_c_fallback"),
      structure(2, class = "vctrs_c_fallback")
    ),
    c("dispatched", "dispatched")
  )

  # Don't fallback for S3 lists which are treated as scalars by default
  expect_error(
    vec_c(foobar(list(1)), foobar(list(2))),
    class = "vctrs_error_scalar_type"
  )
})

test_that("c() fallback is consistent (FIXME)", {
  out <- with_methods(
    c.vctrs_foobar = function(...) structure(NextMethod(), class = "dispatched"),
    list(
      direct = vec_c(foobar(1L), foobar(2L)),
      df = vec_c(data_frame(x = foobar(1L)), data_frame(x = foobar(2L))),
      tib = vec_c(tibble(x = foobar(1L)), tibble(x = foobar(2L))),
      foreign_df = vec_c(foobaz(data_frame(x = foobar(1L))), foobaz(data_frame(x = foobar(2L))))
    )
  )

  # Proper `c()` dispatch:
  expect_identical(out$direct, structure(1:2, class = "dispatched"))

  # Inconsistent:
  expect_identical(out$df$x, foobar(1:2))
  expect_identical(out$tib$x, foobar(1:2))
  expect_identical(out$foreign_df$x, foobar(1:2))
})

test_that("vec_c() falls back to c() if S4 method is available", {
  joe1 <- .Counts(c(1L, 2L), name = "Joe")
  joe2 <- .Counts(3L, name = "Joe")

  c_counts <- function(x, ...) {
    xs <- list(x, ...)

    xs_data <- lapply(xs, function(x) x@.Data)
    new_data <- do.call(c, xs_data)

    .Counts(new_data, name = x@name)
  }

  local_s4_method("c", methods::signature(x = "vctrs_Counts"), c_counts)

  expect_identical(
    vec_c(joe1, joe2),
    .Counts(c(1L, 2L, 3L), name = "Joe")
  )

  expect_identical(
    vec_c(NULL, joe1, joe2),
    .Counts(c(1L, 2L, 3L), name = "Joe")
  )
})

test_that("vec_c() fallback doesn't support `name_spec` or `ptype`", {
  expect_snapshot({
    (expect_error(
      with_c_foobar(vec_c(foobar(1), foobar(2), .name_spec = "{outer}_{inner}")),
      "name specification"
    ))

    # Used to be an error about `ptype`
    (expect_error(
      with_c_foobar(vec_c(foobar(1), foobar(2), .ptype = "")),
      class = "vctrs_error_incompatible_type"
    ))

    # Uses error call (#1641)
    (expect_error(
      with_c_foobar(vec_c(
        foobar(1), foobar(2),
        .error_call = call("foo"),
        .name_spec = "{outer}_{inner}"
      ))
    ))
  })
})

test_that("vec_c() doesn't fall back when ptype2 is implemented", {
  new_quux <- function(x) structure(x, class = "vctrs_quux")

  with_methods(
    vec_ptype2.vctrs_foobar.vctrs_foobar = function(x, y, ...) new_quux(int()),
    vec_cast.vctrs_quux.vctrs_foobar = function(x, to, ...) new_quux(x),
    vec_restore.vctrs_quux = function(x, ...) new_quux(x),
    c.vctrs_foobar = function(...) foobar(NextMethod()),
    {
      expect_s3_class(c(foobar(1:3), foobar(4L)), "vctrs_foobar")
      expect_s3_class(vec_c(foobar(1:3), foobar(4L)), "vctrs_quux")
    }
  )
})

test_that("vec_c() falls back even when ptype is supplied", {
  expect_foobar(vec_c(foobar(1), foobar(2), .ptype = foobar(dbl())))

  with_methods(
    c.vctrs_foobar = function(...) quux(NextMethod()),
    {
      expect_quux(vec_c(foobar(1), foobar(2), .ptype = foobar(dbl())))
      expect_quux(vec_c(foobar(1, foo = TRUE), foobar(2, bar = TRUE), .ptype = foobar(dbl())))
    }
  )
})

test_that("vec_implements_ptype2() is FALSE for scalars", {
  expect_false(vec_implements_ptype2(quote(foo)))
})

test_that("vec_implements_ptype2() and vec_c() fallback are compatible with old registration", {
  foo <- structure(NA, class = "vctrs_implements_ptype2_false")
  expect_false(vec_implements_ptype2(foo))

  vec_ptype2.vctrs_implements_ptype2_true <- function(...) NULL
  s3_register(
    "vctrs::vec_ptype2",
    "vctrs_implements_ptype2_true",
    vec_ptype2.vctrs_implements_ptype2_true
  )

  bar <- structure(NA, class = "vctrs_implements_ptype2_true")
  expect_true(vec_implements_ptype2(bar))

  local_methods(
    `c.vctrs_implements_ptype2_true` = function(...) stop("never called")
  )

  expect_identical(vec_c(bar), bar)
})

test_that("can ignore names in `vec_c()` by providing a `zap()` name-spec (#232)", {
  expect_error(vec_c(a = c(b = 1:2)))
  expect_identical(vec_c(a = c(b = 1:2), b = 3L, .name_spec = zap()), 1:3)

  expect_snapshot({
    (expect_error(
      vec_c(a = c(b = letters), b = 1, .name_spec = zap()),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("can concatenate subclasses of `vctrs_vctr` which don't have ptype2 methods", {
  x <- new_vctr(1, class = "vctrs_foo")
  expect_identical(vec_c(x, x), new_vctr(c(1, 1), class = "vctrs_foo"))
})

test_that("base c() fallback handles unspecified chunks", {
  local_methods(
    c.vctrs_foobar = function(...) {
      x <- NextMethod()

      # Should not be passed any unspecified chunks
      if (anyNA(x)) {
        abort("tilt")
      }

      foobar(x)
    },
    `[.vctrs_foobar` = function(x, i, ...) {
      # Return a quux to detect dispatch
      quux(NextMethod())
    }
  )

  out <- vec_c(foobar(1:2), rep(NA, 2))
  expect_identical(out, quux(c(1:2, NA, NA)))

  out <- vec_c(rep(NA, 2), foobar(1:2), NA)
  expect_identical(out, quux(c(NA, NA, 1:2, NA)))
})

test_that("can zap outer names from a name-spec (#1215)", {
  zap_outer_spec <- function(outer, inner) if (is_character(inner)) inner

  expect_null(
    names(vec_c(a = 1:2, .name_spec = zap_outer_spec))
  )
  expect_identical(
    names(vec_c(a = 1:2, c(foo = 3L), .name_spec = zap_outer_spec)),
    c("", "", "foo")
  )

  expect_null(
    names(list_unchop(list(a = 1:2), indices = list(1:2), name_spec = zap_outer_spec))
  )
  expect_identical(
    names(list_unchop(list(a = 1:2, c(foo = 3L)), indices = list(1:2, 3), name_spec = zap_outer_spec)),
    c("", "", "foo")
  )
})

test_that("named empty vectors force named output (#1263)", {
  x <- set_names(int(), chr())

  expect_named(vec_c(x), chr())
  expect_named(vec_c(x, x), chr())
  expect_named(vec_c(x, 1L), "")
  expect_named(vec_c(x, 1), "")

  expect_named(list_unchop(list(x), list(int())), chr())
  expect_named(list_unchop(list(x, x), list(int(), int())), chr())
  expect_named(list_unchop(list(x, 1L), list(int(), 1)), "")

  expect_named(list_unchop(list(x, 1), list(int(), 1)), "")
})

# Golden tests -------------------------------------------------------

test_that("concatenation performs expected allocations", {
  vec_c_list <- function(x, ptype = NULL) {
    vec_c(!!!x, .ptype = ptype)
  }

  expect_snapshot({
    ints <- rep(list(1L), 1e2)
    dbls <- rep(list(1), 1e2)

    # Extra allocations from `list2()`, see r-lib/rlang#937
    "# `vec_c()` "
    "Integers"
    with_memory_prof(vec_c_list(ints))

    "Doubles"
    with_memory_prof(vec_c_list(dbls))

    "Integers to integer"
    with_memory_prof(vec_c_list(ints, ptype = int()))

    "Doubles to integer"
    with_memory_prof(vec_c_list(dbls, ptype = int()))


    "# `list_unchop()` "
    "Integers"
    with_memory_prof(list_unchop(ints))

    "Doubles"
    with_memory_prof(list_unchop(dbls))

    "Integers to integer"
    with_memory_prof(list_unchop(ints, ptype = int()))

    "Doubles to integer"
    with_memory_prof(list_unchop(dbls, ptype = int()))


    "# Concatenation with names"

    "Named integers"
    ints <- rep(list(set_names(1:3, letters[1:3])), 1e2)
    with_memory_prof(list_unchop(ints))

    "Named matrices"
    mat <- matrix(1:4, 2, dimnames = list(c("foo", "bar")))
    mats <- rep(list(mat), 1e2)
    with_memory_prof(list_unchop(mats))

    "Data frame with named columns"
    df <- data_frame(
      x = set_names(as.list(1:2), c("a", "b")),
      y = set_names(1:2, c("A", "B")),
      z = data_frame(Z = set_names(1:2, c("Za", "Zb")))
    )
    dfs <- rep(list(df), 1e2)
    with_memory_prof(list_unchop(dfs))

    "Data frame with rownames (non-repaired, non-recursive case)"
    df <- data_frame(x = 1:2)
    dfs <- rep(list(df), 1e2)
    dfs <- map2(dfs, seq_along(dfs), set_rownames_recursively)
    with_memory_prof(list_unchop(dfs))

    "Data frame with rownames (repaired, non-recursive case)"
    dfs <- map(dfs, set_rownames_recursively)
    with_memory_prof(list_unchop(dfs))

    "Data frame with rownames (non-repaired, recursive case) (#1217)"
    df <- data_frame(
      x = 1:2,
      y = data_frame(x = 1:2)
    )
    dfs <- rep(list(df), 1e2)
    dfs <- map2(dfs, seq_along(dfs), set_rownames_recursively)
    with_memory_prof(list_unchop(dfs))

    "Data frame with rownames (repaired, recursive case) (#1217)"
    dfs <- map(dfs, set_rownames_recursively)
    with_memory_prof(list_unchop(dfs))

    "list-ofs (#1496)"
    make_list_of <- function(n) {
      df <- tibble::tibble(
        x = new_list_of(vec_chop(1:n), ptype = integer())
      )
      vec_chop(df)
    }
    with_memory_prof(list_unchop(make_list_of(1e3)))
    with_memory_prof(list_unchop(make_list_of(2e3)))
    with_memory_prof(list_unchop(make_list_of(4e3)))
  })
})

test_that("can dispatch many times", {
  # This caused a crash when counters were not correctly protected
  foo <- structure(
    list(x.sorted = numeric(0), tp = numeric(0), fp = numeric(0)),
    row.names = integer(0),
    class = c("vctrs_foobar", "tbl_df", "tbl", "data.frame")
  )
  x <- lapply(1:200, function(...) foo)
  expect_error(NA, object = vctrs::list_unchop(x))
})

test_that("dots splicing clones as appropriate", {
  x <- list(a = 1)
  vctrs::vec_cbind(!!!x)
  expect_equal(x, list(a = 1))

  x <- list(a = 1)
  vctrs::vec_rbind(!!!x)
  expect_equal(x, list(a = 1))

  x <- list(a = 1)
  vctrs::vec_c(!!!x)
  expect_equal(x, list(a = 1))


  x <- list(a = 1)
  vctrs::vec_cbind(!!!x, 2)
  expect_equal(x, list(a = 1))

  x <- list(a = 1)
  vctrs::vec_rbind(!!!x, 2)
  expect_equal(x, list(a = 1))

  x <- list(a = 1)
  vctrs::vec_c(!!!x, 2)
  expect_equal(x, list(a = 1))
})
