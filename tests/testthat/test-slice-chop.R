
test_that("vec_chop() throws error with non-vector inputs", {
  expect_error(vec_chop(NULL), class = "vctrs_error_scalar_type")
  expect_error(vec_chop(environment()), class = "vctrs_error_scalar_type")
})

test_that("atomics are split into a list", {
  x <- 1:5
  expect_equal(vec_chop(x), as.list(x))

  x <- letters[1:5]
  expect_equal(vec_chop(x), as.list(x))
})

test_that("atomic names are kept", {
  x <- set_names(1:5)
  result <- lapply(vec_chop(x), names)
  expect_equal(result, as.list(names(x)))
})

test_that("base R classed objects are split into a list", {
  fctr <- factor(c("a", "b"))
  expect <- lapply(vec_seq_along(fctr), vec_slice, x = fctr)
  expect_equal(vec_chop(fctr), expect)

  date <- new_date(c(0, 1))
  expect <- lapply(vec_seq_along(date), vec_slice, x = date)
  expect_equal(vec_chop(date), expect)
})

test_that("base R classed object names are kept", {
  fctr <- set_names(factor(c("a", "b")))
  result <- lapply(vec_chop(fctr), names)
  expect_equal(result, as.list(names(fctr)))
})

test_that("list elements are split", {
  x <- list(1, 2)
  result <- list(vec_slice(x, 1), vec_slice(x, 2))
  expect_equal(vec_chop(x), result)
})

test_that("data frames are split rowwise", {
  x <- data_frame(x = 1:2, y = c("a", "b"))
  result <- list(vec_slice(x, 1), vec_slice(x, 2))
  expect_equal(vec_chop(x), result)
})

test_that("vec_chop() keeps data frame row names", {
  x <- data_frame(x = 1:2, y = c("a", "b"))
  rownames(x) <- c("r1", "r2")
  result <- lapply(vec_chop(x), rownames)
  expect_equal(result, list("r1", "r2"))
})

test_that("matrices / arrays are split rowwise", {
  x <- array(1:12, c(2, 2, 2))
  result <- list(vec_slice(x, 1), vec_slice(x, 2))
  expect_equal(vec_chop(x), result)
})

test_that("matrix / array row names are kept", {
  x <- array(1:12, c(2, 2, 2), dimnames = list(c("r1", "r2"), c("c1", "c2")))
  result <- lapply(vec_chop(x), rownames)
  expect_equal(result, list("r1", "r2"))
})

test_that("matrices / arrays without row names have other dimension names kept", {
  x <- array(1:12, c(2, 2, 2), dimnames = list(NULL, c("c1", "c2")))
  result <- lapply(vec_chop(x), colnames)
  expect_equal(result, list(c("c1", "c2"), c("c1", "c2")))
})

test_that("vec_chop() doesn't restore when attributes have already been restored", {
  local_methods(
    `[.vctrs_foobar` = function(x, i, ...) structure("dispatched", foo = "bar"),
    vec_restore.vctrs_foobar = function(...) structure("dispatched-and-restored", foo = "bar")
  )

  result <- vec_chop(foobar(NA))[[1]]
  expect_equal(result, structure("dispatched", foo = "bar"))
})

test_that("vec_chop() does not restore when attributes have not been restored by `[`", {
  local_methods(
    `[.vctrs_foobar` = function(x, i, ...) "dispatched",
    vec_restore.vctrs_foobar = function(...) "dispatched-and-restored"
  )

  result <- vec_chop(foobar(NA))[[1]]
  expect_equal(result, "dispatched")
})

test_that("vec_chop() falls back to `[` for shaped objects with no proxy", {
  x <- foobar(1)
  dim(x) <- c(1, 1)
  result <- vec_chop(x)[[1]]
  expect_equal(result, x)
})

test_that("`indices` are validated", {
  expect_error(vec_chop(1, 1), "`indices` must be a list of index values, or `NULL`")
  expect_error(vec_chop(1, list(1.5)), class = "vctrs_error_subscript_type")
  expect_error(vec_chop(1, list(2)), class = "vctrs_error_subscript_oob")
})

test_that("size 0 `indices` list is allowed", {
  expect_equal(vec_chop(1, list()), list())
})

test_that("individual index values of size 0 are allowed", {
  expect_equal(vec_chop(1, list(integer())), list(numeric()))

  df <- data.frame(a = 1, b = "1")
  expect_equal(vec_chop(df, list(integer())), list(vec_ptype(df)))
})

test_that("data frame row names are kept when `indices` are used", {
  x <- data_frame(x = 1:2, y = c("a", "b"))
  rownames(x) <- c("r1", "r2")
  result <- lapply(vec_chop(x, list(1, 1:2)), rownames)
  expect_equal(result, list("r1", c("r1", "r2")))
})

test_that("vec_chop(<atomic>, indices =) can be equivalent to the default", {
  x <- 1:5
  indices <- as.list(vec_seq_along(x))
  expect_equal(vec_chop(x, indices), vec_chop(x))
})

test_that("vec_chop(<data.frame>, indices =) can be equivalent to the default", {
  x <- data.frame(x = 1:5)
  indices <- as.list(vec_seq_along(x))
  expect_equal(vec_chop(x, indices), vec_chop(x))
})

test_that("vec_chop(<array>, indices =) can be equivalent to the default", {
  x <- array(1:8, c(2, 2, 2))
  indices <- as.list(vec_seq_along(x))
  expect_equal(vec_chop(x, indices), vec_chop(x))
})

test_that("`indices` cannot use names", {
  x <- set_names(1:3, c("a", "b", "c"))
  expect_error(vec_chop(x, list("a", c("b", "c"))), class = "vctrs_error_subscript_type")

  x <- array(1:4, c(2, 2), dimnames = list(c("r1", "r2")))
  expect_error(vec_chop(x, list("r1")), class = "vctrs_error_subscript_type")

  x <- data.frame(x = 1, row.names = "r1")
  expect_error(vec_chop(x, list("r1")), class = "vctrs_error_subscript_type")
})

test_that("fallback method with `indices` works", {
  fctr <- factor(c("a", "b"))
  indices <- list(1, c(1, 2))

  expect_equal(
    vec_chop(fctr, indices),
    map(indices, vec_slice, x = fctr)
  )
})

test_that("vec_chop() falls back to `[` for shaped objects with no proxy when indices are provided", {
  x <- foobar(1)
  dim(x) <- c(1, 1)
  result <- vec_chop(x, list(1))[[1]]
  expect_equal(result, x)
})

test_that("vec_chop() with data frame proxies always uses the proxy's length info", {
  local_methods(
    vec_proxy.vctrs_proxy = function(x, ...) {
      x <- proxy_deref(x)
      new_data_frame(list(x = x$x, y = x$y))
    },
    vec_restore.vctrs_proxy = function(x, to, ...) {
      new_proxy(list(x = x$x, y = x$y))
    }
  )

  x <- new_proxy(list(x = 1:2, y = 3:4))
  result <- vec_chop(x)

  result1 <- result[[1]]
  result2 <- result[[2]]

  expect1 <- new_proxy(list(x = 1L, y = 3L))
  expect2 <- new_proxy(list(x = 2L, y = 4L))

  expect_identical(proxy_deref(result1), proxy_deref(expect1))
  expect_identical(proxy_deref(result2), proxy_deref(expect2))
})

test_that("ALTREP objects always generate materialized chops (#1450)", {
  skip_if(getRversion() <= "3.5.0")

  x <- .Call(vctrs_altrep_rle_Make, c(foo = 10L, bar = 5L))

  # `x` starts in compact form
  expect_false(.Call(vctrs_altrep_rle_is_materialized, x))

  result <- vec_chop(x)

  # Chopping materializes `x`
  expect_true(.Call(vctrs_altrep_rle_is_materialized, x))

  # And chopped elements are not ALTREP vectors
  expect_false(any(map_lgl(result, is_altrep)))

  expect <- vec_chop(c(rep("foo", 10), rep("bar", 5)))
  expect_identical(result, expect)
})

# vec_chop + compact_seq --------------------------------------------------

# `start` is 0-based

test_that("can chop base vectors with compact seqs", {
  start <- 1L
  size <- 2L
  expect_identical(vec_chop_seq(lgl(1, 0, 1), start, size), list(lgl(0, 1)))
  expect_identical(vec_chop_seq(int(1, 2, 3), start, size), list(int(2, 3)))
  expect_identical(vec_chop_seq(dbl(1, 2, 3), start, size), list(dbl(2, 3)))
  expect_identical(vec_chop_seq(cpl(1, 2, 3), start, size), list(cpl(2, 3)))
  expect_identical(vec_chop_seq(chr("1", "2", "3"), start, size), list(chr("2", "3")))
  expect_identical(vec_chop_seq(raw2(1, 2, 3), start, size), list(raw2(2, 3)))
  expect_identical(vec_chop_seq(list(1, 2, 3), start, size), list(list(2, 3)))
})

test_that("can chop with a decreasing compact seq", {
  expect_equal(vec_chop_seq(int(1, 2, 3), 1L, 2L, FALSE), list(int(2, 1)))
})

test_that("can chop with multiple compact seqs", {
  start <- c(1L, 0L)
  size <- c(1L, 3L)

  expect_equal(
    vec_chop_seq(int(1, 2, 3), start, size),
    list(int(2), int(1, 2, 3))
  )
})

test_that("can chop S3 objects using the fallback method with compact seqs", {
  x <- factor(c("a", "b", "c", "d"))
  expect_equal(vec_chop_seq(x, 0L, 0L), list(vec_slice(x, integer())))
  expect_equal(vec_chop_seq(x, 0L, 1L), list(vec_slice(x, 1L)))
  expect_equal(vec_chop_seq(x, 2L, 2L), list(vec_slice(x, 3:4)))
})

# list_unchop --------------------------------------------------------------

test_that("`x` must be a list", {
  expect_snapshot(error = TRUE, {
    list_unchop(1, list(1))
  })
  expect_snapshot(error = TRUE, {
    list_unchop(1, list(1), error_call = call("foo"))
  })
  expect_snapshot(error = TRUE, {
    list_unchop(data.frame(x=1), list(1))
  })
})

test_that("`indices` must be a list", {
  expect_snapshot(error = TRUE, {
    list_unchop(list(1), 1)
  })
  expect_snapshot(error = TRUE, {
    list_unchop(list(1), 1, error_call = call("foo"))
  })
  expect_snapshot(error = TRUE, {
    list_unchop(list(1), data.frame(x=1))
  })
})

test_that("`indices` must be a list of integers", {
  expect_error(list_unchop(list(1), list("x")), class = "vctrs_error_subscript_type")
  expect_error(list_unchop(list(1), list(TRUE)), class = "vctrs_error_subscript_type")
  expect_error(list_unchop(list(1), list(quote(name))), class = "vctrs_error_subscript_type")
})

test_that("`x` and `indices` must be lists of the same size", {
  expect_error(list_unchop(list(1, 2), list(1)), "`x` and `indices` must be lists of the same size")
})

test_that("can unchop with an AsIs list (#1463)", {
  x <- I(list(1, 2))
  expect_identical(list_unchop(x), c(1, 2))
})

test_that("can unchop empty vectors", {
  expect_null(list_unchop(list()))
  expect_null(list_unchop(list(), list()))
  expect_identical(list_unchop(list(), list(), ptype = numeric()), numeric())
})

test_that("can unchop a list of NULL", {
  expect_null(list_unchop(list(NULL), list(integer())))
  expect_identical(list_unchop(list(NULL), list(integer()), ptype = numeric()), numeric())
  expect_identical(list_unchop(list(NULL, NULL), list(integer(), integer()), ptype = numeric()), numeric())
})

test_that("NULLs are ignored when unchopped with other vectors", {
  expect_identical(list_unchop(list("a", NULL, "b")), c("a", "b"))
  expect_identical(list_unchop(list("a", NULL, "b"), list(2, integer(), 1)), c("b", "a"))
})

test_that("can use a `NULL` element with a corresponding index", {
  # FIXME: Probably not quite right, but not entirely clear what it should be:
  # - Maybe `unspecified(2)`?
  # - Or should `NULL`s even be allowed in `list_unchop()`?
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
  expect_identical(list_unchop(list(1, 2), list(2, 1)), c(2, 1))
  expect_identical(list_unchop(list("a", "b"), list(2, 1)), c("b", "a"))
})

test_that("can unchop lists", {
  x <- list(list("a", "b"), list("c"))
  indices <- list(c(2, 3), 1)

  expect_identical(list_unchop(x, indices), list("c", "a", "b"))
})

test_that("can unchop data frames", {
  df1 <- data_frame(x = 1:2)
  df2 <- data_frame(x = 3:4)

  x <- list(df1, df2)
  indices <- list(c(3, 1), c(2, 4))

  expect <- vec_slice(vec_c(df1, df2), vec_order(vec_c(!!! indices)))

  expect_identical(list_unchop(x, indices), expect)
})

test_that("can unchop factors", {
  fctr1 <- factor("z")
  fctr2 <- factor(c("x", "y"))

  x <- list(fctr1, fctr2)
  indices <- list(2, c(3, 1))

  # levels are in the order they are seen!
  expect <- factor(c("y", "z", "x"), levels = c("z", "x", "y"))

  expect_identical(list_unchop(x, indices), expect)
})

test_that("can fallback when unchopping matrices", {
  mat1 <- matrix(1:4, nrow = 2, ncol = 2)
  mat2 <- matrix(5:10, nrow = 3, ncol = 2)

  x <- list(mat1, mat2)
  indices <- list(c(4, 1), c(2, 3, 5))

  expect <- vec_slice(vec_c(mat1, mat2), vec_order(vec_c(!!! indices)))

  expect_identical(list_unchop(x, indices), expect)
  expect_identical(list_unchop(x), vec_c(mat1, mat2))
})

test_that("can fallback when unchopping arrays of >2D", {
  arr1 <- array(1:8, c(2, 2, 2))
  arr2 <- matrix(9:10, c(1, 2))

  x <- list(arr1, arr2)
  indices <- list(c(3, 1), 2)

  expect <- vec_slice(vec_c(arr1, arr2), vec_order(vec_c(!!! indices)))

  expect_identical(list_unchop(x, indices), expect)
  expect_identical(list_unchop(x), vec_c(arr1, arr2))
})

test_that("can unchop with all size 0 elements and get the right ptype", {
  x <- list(integer(), integer())
  indices <- list(integer(), integer())
  expect_identical(list_unchop(x, indices), integer())
})

test_that("can unchop with some size 0 elements", {
  x <- list(integer(), 1:2, integer())
  indices <- list(integer(), 2:1, integer())
  expect_identical(list_unchop(x, indices), 2:1)
})

test_that("NULL is a valid index", {
  expect_equal(list_unchop(list(1, 2), list(NULL, 1)), 2)
  expect_error(list_unchop(list(1, 2), list(NULL, 2)), class = "vctrs_error_subscript_oob")
})

test_that("unchopping recycles elements of x to the size of the index", {
  x <- list(1, 2)
  indices <- list(c(3, 4, 5), c(2, 1))
  expect_identical(list_unchop(x, indices), c(2, 2, 1, 1, 1))

  x <- list(1:2)
  indices <- list(1:3)
  expect_snapshot({
    (expect_error(list_unchop(x, indices = indices)))
    (expect_error(list_unchop(x, indices = indices, error_call = call("foo"))))
  })
})

test_that("unchopping takes the common type", {
  x <- list(1, "a")
  indices <- list(1, 2)

  expect_snapshot({
    (expect_error(list_unchop(x, indices), class = "vctrs_error_incompatible_type"))
    (expect_error(list_unchop(x, indices, error_call = call("foo")), class = "vctrs_error_incompatible_type"))
  })

  x <- list(1, 2L)

  expect_type(list_unchop(x, indices), "double")
})

test_that("common type failure uses positional errors", {
  expect_snapshot({
    # Looking for `..1` and `a`
    (expect_error(list_unchop(list(1, a = "x", 2))))
    (expect_error(list_unchop(list(1, a = "x", 2), indices = list(2, 1, 3))))

    # Directed cast should also produce directional errors (#1690)
    (expect_error(list_unchop(list(1, a = "x", 2), ptype = double())))
    (expect_error(list_unchop(list(1, a = "x", 2), indices = list(2, 1, 3), ptype = double())))

    # Lossy cast
    (expect_error(list_unchop(list(1, a = 2.5), ptype = integer())))
    (expect_error(list_unchop(list(1, a = 2.5), indices = list(2, 1), ptype = integer())))
  })
})

test_that("can specify a ptype to override common type", {
  indices <- list(1, 2)

  x <- list(1, 2L)
  expect_identical(list_unchop(x, indices, ptype = integer()), c(1L, 2L))

  x <- list(1.5, 2)
  expect_snapshot({
    (expect_error(list_unchop(x, indices = indices, ptype = integer())))
    (expect_error(list_unchop(x, indices = indices, ptype = integer(), error_call = call("foo"))))
  })
})

test_that("leaving `indices = NULL` unchops sequentially", {
  x <- list(1:2, 3:5, 6L)
  expect_identical(list_unchop(x), 1:6)
})

test_that("outer names are kept", {
  x <- list(x = 1, y = 2)
  expect_named(list_unchop(x), c("x", "y"))
  expect_named(list_unchop(x, list(2, 1)), c("y", "x"))
})

test_that("outer names are recycled in the right order", {
  x <- list(x = 1, y = 2)
  expect_error(list_unchop(x, list(c(1, 2), 3)), "Can't merge")
  expect_named(list_unchop(x, list(c(1, 3), 2), name_spec = "{outer}_{inner}"), c("x_1", "y", "x_2"))
  expect_named(list_unchop(x, list(c(3, 1), 2), name_spec = "{outer}_{inner}"), c("x_2", "y", "x_1"))
})

test_that("outer names can be merged with inner names", {
  x <- list(x = c(a = 1), y = c(b = 2))
  expect_error(list_unchop(x), "Can't merge")
  expect_named(list_unchop(x, name_spec = "{outer}_{inner}"), c("x_a", "y_b"))
  expect_named(list_unchop(x, list(2, 1), name_spec = "{outer}_{inner}"), c("y_b", "x_a"))
})

test_that("preserves names when inputs are cast to a common type (#1689)", {
  expect_named(list_unchop(list(c(a = 1)), ptype = integer()), "a")
  expect_named(list_unchop(list(c(a = 1)), ptype = integer(), indices = list(1)), "a")

  # With name spec
  name_spec <- "{outer}_{inner}"
  expect_named(list_unchop(list(foo = c(a = 1)), ptype = integer(), name_spec = name_spec), "foo_a")
  expect_named(list_unchop(list(foo = c(a = 1)), ptype = integer(), name_spec = name_spec, indices = list(1)), "foo_a")

  # When `x` elements are recycled, names are also recycled
  x <- list(c(a = 1), c(b = 2))
  indices <- list(1:2, 3:4)
  expect_named(list_unchop(x, indices = indices, ptype = integer()), c("a", "a", "b", "b"))
})

test_that("not all inputs have to be named", {
  x <- list(c(a = 1), 2, c(c = 3))
  indices <- list(2, 1, 3)

  expect_named(list_unchop(x, indices), c("", "a", "c"))
})

test_that("list_unchop() keeps data frame row names", {
  df1 <- data.frame(x = 1:2, row.names = c("r1", "r2"))
  df2 <- data.frame(x = 3:4, row.names = c("r3", "r4"))

  x <- list(df1, df2)
  indices <- list(c(3, 1), c(2, 4))

  result <- list_unchop(x, indices)
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
  x <- list(1:2, 3L)
  indices <- list(1:2, 1L)

  expect_identical(list_unchop(x, indices), c(3L, 2L, NA))
})

test_that("index values are validated", {
  x <- list(1, 2)
  indices1 <- list(4, 1)
  indices2 <- list(c(1, 4), 2)
  indices3 <- list(c(1, 3, 4), 2)

  expect_error(list_unchop(x, indices1), class = "vctrs_error_subscript_oob")
  expect_error(list_unchop(x, indices2), class = "vctrs_error_subscript_oob")

  expect_identical(list_unchop(x, indices3), c(1, 2, 1, 1))
})

test_that("name repair is respected and happens after ordering according to `indices`", {
  local_name_repair_quiet()

  x <- list(c(a = 1), c(a = 2))
  indices <- list(2, 1)

  expect_named(list_unchop(x, indices), c("a", "a"))
  expect_named(list_unchop(x, indices, name_repair = "unique"), c("a...1", "a...2"))
})

test_that("list_unchop() errors on unsupported location values", {
  expect_snapshot({
    (expect_error(
      list_unchop(list(1, 2), list(c(1, 2), 0)),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      list_unchop(list(1), list(-1)),
      class = "vctrs_error_subscript_type"
    ))
  })
})

test_that("missing values propagate", {
  expect_identical(
    list_unchop(list(1, 2), list(c(NA_integer_, NA_integer_), c(NA_integer_, 3))),
    c(NA, NA, 2, NA)
  )
})

test_that("list_unchop() works with simple homogeneous foreign S3 classes", {
  expect_identical(list_unchop(list(foobar(1), foobar(2))), vec_c(foobar(c(1, 2))))
})

test_that("list_unchop() fails with complex foreign S3 classes", {
  expect_snapshot({
    x <- structure(foobar(1), attr_foo = "foo")
    y <- structure(foobar(2), attr_bar = "bar")
    (expect_error(list_unchop(list(x, y)), class = "vctrs_error_incompatible_type"))
    (expect_error(list_unchop(list(x, y), error_call = call("foo")), class = "vctrs_error_incompatible_type"))
  })
})

test_that("list_unchop() fails with complex foreign S4 classes", {
  expect_snapshot({
    joe <- .Counts(c(1L, 2L), name = "Joe")
    jane <- .Counts(3L, name = "Jane")
    (expect_error(list_unchop(list(joe, jane)), class = "vctrs_error_incompatible_type"))
    (expect_error(list_unchop(list(joe, jane), error_call = call("foo")), class = "vctrs_error_incompatible_type"))
  })
})

test_that("list_unchop() falls back to c() if S3 method is available", {
  # Check off-by-one error
  expect_error(
    list_unchop(list(foobar(1), "", foobar(2)), list(1, 2, 3)),
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
    list_unchop(list(foobar(1), foobar(2))),
    foobar(c(1, 2))
  )
  expect_identical(
    list_unchop(list(foobar(1), foobar(2)), list(1, 2)),
    foobar(c(1, 2))
  )
  expect_identical(
    list_unchop(list(foobar(1), foobar(2)), list(2, 1)),
    foobar(c(2, 1))
  )
  expect_identical(
    list_unchop(list(NULL, foobar(1), NULL, foobar(2))),
    foobar(c(1, 2))
  )

  # OOB error is respected
  expect_error(
    list_unchop(list(foobar(1), foobar(2)), list(1, 3)),
    class = "vctrs_error_subscript_oob"
  )

  # Unassigned locations results in missing values.
  # Repeated assignment uses the last assigned value.
  expect_identical(
    list_unchop(list(foobar(c(1, 2)), foobar(3)), list(c(1, 3), 1)),
    foobar(c(3, NA, 2))
  )
  expect_identical(
    list_unchop(list(foobar(c(1, 2)), foobar(3)), list(c(2, NA), NA)),
    foobar(c(NA, 1, NA))
  )

  # Names are kept
  expect_identical(
    list_unchop(list(foobar(c(x = 1, y = 2)), foobar(c(x = 1))), list(c(2, 1), 3)),
    foobar(c(y = 2, x = 1, x = 1))
  )

  # Recycles to the size of index
  expect_identical(
    list_unchop(list(foobar(1), foobar(2)), list(c(1, 3), 2)),
    foobar(c(1, 2, 1))
  )
  expect_identical(
    list_unchop(list(foobar(1), foobar(2)), list(c(1, 2), integer())),
    foobar(c(1, 1))
  )
  expect_error(
    list_unchop(list(foobar(1), foobar(2)), list(c(1, 3), integer())),
    class = "vctrs_error_subscript_oob"
  )

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
      list(2, 1)
    ),
    structure(c(2, 1), class = "vctrs_c_fallback")
  )

  # Don't fallback for S3 lists which are treated as scalars by default
  expect_error(
    list_unchop(list(foobar(list(1)), foobar(list(2)))),
    class = "vctrs_error_scalar_type"
  )
})

test_that("list_unchop() falls back for S4 classes with a registered c() method", {
  joe <- .Counts(c(1L, 2L), name = "Joe")
  jane <- .Counts(3L, name = "Jane")

  expect_snapshot({
    (expect_error(
      list_unchop(list(joe, 1, jane), list(c(1, 2), 3, 4)),
      class = "vctrs_error_incompatible_type"
    ))
  })

  local_c_counts()

  expect_identical(
    list_unchop(list(joe, jane), list(c(1, 3), 2)),
    .Counts(c(1L, 3L, 2L), name = "Dispatched")
  )

  expect_identical(
    list_unchop(list(NULL, joe, jane), list(integer(), c(1, 3), 2)),
    .Counts(c(1L, 3L, 2L), name = "Dispatched")
  )

  # Unassigned locations results in missing values.
  # Repeated assignment uses the last assigned value.
  expect_identical(
    list_unchop(list(joe, jane), list(c(1, 3), 1)),
    .Counts(c(3L, NA, 2L), name = "Dispatched")
  )
  expect_identical(
    list_unchop(list(joe, jane), list(c(2, NA), NA)),
    .Counts(c(NA, 1L, NA), name = "Dispatched")
  )
})

test_that("list_unchop() fallback doesn't support `name_spec` or `ptype`", {
  expect_snapshot({
    foo <- structure(foobar(1), foo = "foo")
    bar <- structure(foobar(2), bar = "bar")
    (expect_error(
      with_c_foobar(list_unchop(list(foo, bar), name_spec = "{outer}_{inner}")),
      "name specification"
    ))
    # With error call
    (expect_error(
      with_c_foobar(list_unchop(list(foo, bar), name_spec = "{outer}_{inner}", error_call = call("foo"))),
      "name specification"
    ))
    # Used to be an error about `ptype`
    (expect_error(
      with_c_foobar(list_unchop(list(foobar(1)), ptype = "")),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("list_unchop() supports numeric S3 indices", {
  local_methods(
    vec_ptype2.vctrs_foobar = function(x, y, ...) UseMethod("vec_ptype2.vctrs_foobar"),
    vec_ptype2.vctrs_foobar.integer = function(x, y, ...) foobar(integer()),
    vec_cast.integer.vctrs_foobar = function(x, to, ...) vec_data(x)
  )

  expect_identical(list_unchop(list(1), list(foobar(1L))), 1)
})

test_that("list_unchop() does not support non-numeric S3 indices", {
  expect_snapshot({
    (expect_error(
      list_unchop(list(1), list(factor("x"))),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      list_unchop(list(1), list(foobar(1L))),
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
    (expect_error(
      list_unchop(list(a = c(b = letters), b = 3L), name_spec = zap()),
      class = "vctrs_error_incompatible_type"
    ))
    (expect_error(
      list_unchop(
        list(a = c(foo = 1:2), b = c(bar = "")),
        indices = list(2:1, 3),
        name_spec = zap()
      ),
      class = "vctrs_error_incompatible_type"
    ))
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
    list_unchop(xs, list(c(2, 1), 3)),
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
    list_unchop(xs, list(c(2, 1), 3)),
    class = "vctrs_error_incompatible_type"
  )
})
