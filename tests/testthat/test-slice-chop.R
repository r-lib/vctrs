
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

test_that("vec_chop() keeps data frame row names for data frames with 0 columns (#1722)", {
  x <- data_frame(.size = 3)
  rownames(x) <- c("r1", "r2", "r3")

  out <- lapply(vec_chop(x), rownames)
  expect_identical(out, list("r1", "r2", "r3"))

  out <- vec_chop(x, indices = list(c(2, NA), 3))
  out <- lapply(out, rownames)
  expect_identical(out, list(c("r2", "...2"), "r3"))

  out <- vec_chop(x, sizes = c(1, 2, 0))
  out <- lapply(out, rownames)
  expect_identical(out, list("r1", c("r2", "r3"), character()))
})

test_that("data frames with 0 columns retain the right number of rows (#1722)", {
  x <- data_frame(.size = 4)

  one <- data_frame(.size = 1L)
  expect_identical(
    vec_chop(x),
    list(one, one, one, one)
  )

  expect_identical(
    vec_chop(x, indices = list(c(1, 3, 2), c(3, NA))),
    list(
      data_frame(.size = 3),
      data_frame(.size = 2)
    )
  )

  expect_identical(
    vec_chop(x, sizes = c(3, 1, 0)),
    list(
      data_frame(.size = 3),
      data_frame(.size = 1),
      data_frame(.size = 0)
    )
  )
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

  result <- vec_chop(foobar(NA), indices = list(1))[[1]]
  expect_equal(result, structure("dispatched", foo = "bar"))

  result <- vec_chop(foobar(NA), sizes = 1)[[1]]
  expect_equal(result, structure("dispatched", foo = "bar"))
})

test_that("vec_chop() does not restore when attributes have not been restored by `[`", {
  local_methods(
    `[.vctrs_foobar` = function(x, i, ...) "dispatched",
    vec_restore.vctrs_foobar = function(...) "dispatched-and-restored"
  )

  result <- vec_chop(foobar(NA))[[1]]
  expect_equal(result, "dispatched")

  result <- vec_chop(foobar(NA), indices = list(1))[[1]]
  expect_equal(result, "dispatched")

  result <- vec_chop(foobar(NA), sizes = 1)[[1]]
  expect_equal(result, "dispatched")
})

test_that("vec_chop() falls back to `[` for shaped objects with no proxy", {
  x <- foobar(1)
  dim(x) <- c(1, 1)

  result <- vec_chop(x)[[1]]
  expect_equal(result, x)

  result <- vec_chop(x, indices = list(1))[[1]]
  expect_equal(result, x)

  result <- vec_chop(x, sizes = 1)[[1]]
  expect_equal(result, x)
})

test_that("`indices` are validated", {
  expect_snapshot(error = TRUE, {
    vec_chop(1, indices = 1)
  })
  expect_snapshot({
    (expect_error(vec_chop(1, indices = list(1.5)), class = "vctrs_error_subscript_type"))
  })
  expect_snapshot({
    (expect_error(vec_chop(1, indices = list(2)), class = "vctrs_error_subscript_oob"))
  })
})

test_that("`sizes` are validated", {
  expect_snapshot(error = TRUE, {
    vec_chop("a", sizes = "a")
  })
  expect_snapshot(error = TRUE, {
    vec_chop("a", sizes = 2)
  })
  expect_snapshot(error = TRUE, {
    vec_chop("a", sizes = -1)
  })
  expect_snapshot(error = TRUE, {
    vec_chop("a", sizes = NA_integer_)
  })
  expect_snapshot(error = TRUE, {
    vec_chop("a", sizes = c(1, 1))
  })
})

test_that("can't use both `indices` and `sizes`", {
  expect_snapshot(error = TRUE, {
    vec_chop(1, indices = list(1), sizes = 1)
  })
})

test_that("`sizes` allows `0`", {
  expect_identical(
    vec_chop(c("a", "b"), sizes = c(1, 0, 0, 1, 0)),
    list("a", character(), character(), "b", character())
  )
})

test_that("size 0 `indices` list is allowed", {
  expect_equal(vec_chop(1, indices = list()), list())
})

test_that("individual index values of size 0 are allowed", {
  expect_equal(vec_chop(1, indices = list(integer())), list(numeric()))

  df <- data.frame(a = 1, b = "1")
  expect_equal(vec_chop(df, indices = list(integer())), list(vec_ptype(df)))
})

test_that("individual index values of `NULL` are allowed", {
  expect_equal(vec_chop(1, indices = list(NULL)), list(numeric()))

  df <- data.frame(a = 1, b = "1")
  expect_equal(vec_chop(df, indices = list(NULL)), list(vec_ptype(df)))
})

test_that("data frame row names are kept when `indices` or `sizes` are used", {
  x <- data_frame(x = 1:2, y = c("a", "b"))
  rownames(x) <- c("r1", "r2")

  result <- lapply(vec_chop(x, indices = list(1, 1:2)), rownames)
  expect_equal(result, list("r1", c("r1", "r2")))

  result <- lapply(vec_chop(x, sizes = c(1, 0, 1)), rownames)
  expect_equal(result, list("r1", character(), "r2"))
})

test_that("vec_chop(<atomic>, indices/sizes =) can be equivalent to the default", {
  x <- 1:5

  indices <- as.list(vec_seq_along(x))
  expect_equal(vec_chop(x, indices = indices), vec_chop(x))

  sizes <- vec_rep(1L, times = vec_size(x))
  expect_equal(vec_chop(x, sizes = sizes), vec_chop(x))
})

test_that("vec_chop(<data.frame>, indices/sizes =) can be equivalent to the default", {
  x <- data.frame(x = 1:5)

  indices <- as.list(vec_seq_along(x))
  expect_equal(vec_chop(x, indices = indices), vec_chop(x))

  sizes <- vec_rep(1L, times = vec_size(x))
  expect_equal(vec_chop(x, sizes = sizes), vec_chop(x))
})

test_that("vec_chop(<array>, indices/sizes =) can be equivalent to the default", {
  x <- array(1:8, c(2, 2, 2))

  indices <- as.list(vec_seq_along(x))
  expect_equal(vec_chop(x, indices = indices), vec_chop(x))

  sizes <- vec_rep(1L, times = vec_size(x))
  expect_equal(vec_chop(x, sizes = sizes), vec_chop(x))
})

test_that("`indices` cannot use names", {
  x <- set_names(1:3, c("a", "b", "c"))
  expect_error(vec_chop(x, indices = list("a", c("b", "c"))), class = "vctrs_error_subscript_type")

  x <- array(1:4, c(2, 2), dimnames = list(c("r1", "r2")))
  expect_error(vec_chop(x, indices = list("r1")), class = "vctrs_error_subscript_type")

  x <- data.frame(x = 1, row.names = "r1")
  expect_error(vec_chop(x, indices = list("r1")), class = "vctrs_error_subscript_type")
})

test_that("fallback method with `indices` and `sizes` works", {
  fctr <- factor(c("a", "b"))

  indices <- list(1, c(1, 2))
  sizes <- c(1, 0, 1)

  expect_equal(
    vec_chop(fctr, indices = indices),
    map(indices, vec_slice, x = fctr)
  )
  expect_equal(
    vec_chop(fctr, sizes = sizes),
    list(vec_slice(fctr, 1), vec_slice(fctr, 0), vec_slice(fctr, 2))
  )
})

test_that("vec_chop() falls back to `[` for shaped objects with no proxy when `indices` or `sizes` are provided", {
  x <- foobar(1)
  dim(x) <- c(1, 1)

  result <- vec_chop(x, indices = list(1))[[1]]
  expect_equal(result, x)

  result <- vec_chop(x, sizes = 1)[[1]]
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

test_that("`vec_chop(x, indices)` backwards compatible behavior works", {
  # No issues here
  expect_identical(
    vec_chop(1:2, list(1, 2)),
    vec_chop(1:2, indices = list(1, 2))
  )

  # Errors still talk about `indices`
  expect_snapshot(error = TRUE, {
    vec_chop(1:2, 1)
  })
  expect_snapshot(error = TRUE, {
    vec_chop(1, list(1), sizes = 1)
  })

  # These cases aren't allowed because they weren't possible previously either
  expect_snapshot(error = TRUE, {
    vec_chop(1, list(1), 2)
  })
  expect_snapshot(error = TRUE, {
    vec_chop(1, list(1), indices = list(1))
  })
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

test_that("data frames with 0 columns retain the right number of rows with compact seqs (#1722)", {
  x <- data_frame(.size = 4)

  out <- vec_chop_seq(x, starts = c(0L, 0L, 2L), sizes = c(0L, 2L, 1L))
  out <- map_int(out, vec_size)

  expect_identical(out, c(0L, 2L, 1L))
})

# list_unchop --------------------------------------------------------------

test_that("`x` must be a list", {
  expect_snapshot(error = TRUE, {
    list_unchop(1, indices = list(1))
  })
  expect_snapshot(error = TRUE, {
    list_unchop(1, indices = list(1), error_call = call("foo"), error_arg = "arg")
  })
  expect_snapshot(error = TRUE, {
    list_unchop(data.frame(x=1), indices = list(1))
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
    list_unchop(list(1), indices = data.frame(x=1))
  })
})

test_that("`indices` must be a list of integers", {
  expect_error(list_unchop(list(1), indices = list("x")), class = "vctrs_error_subscript_type")
  expect_error(list_unchop(list(1), indices = list(TRUE)), class = "vctrs_error_subscript_type")
  expect_error(list_unchop(list(1), indices = list(quote(name))), class = "vctrs_error_subscript_type")
})

test_that("`x` and `indices` must be lists of the same size", {
  expect_error(list_unchop(list(1, 2), indices = list(1)), "`x` and `indices` must be lists of the same size")
})

test_that("can unchop with an AsIs list (#1463)", {
  x <- I(list(1, 2))
  expect_identical(list_unchop(x), c(1, 2))
})

test_that("can unchop empty vectors", {
  expect_null(list_unchop(list()))
  expect_null(list_unchop(list(), indices = list()))
  expect_identical(list_unchop(list(), indices = list(), ptype = numeric()), numeric())
})

test_that("can unchop a list of NULL", {
  expect_null(list_unchop(list(NULL), indices = list(integer())))
  expect_identical(list_unchop(list(NULL), indices = list(integer()), size = 0), unspecified(0))
  expect_identical(list_unchop(list(NULL), indices = list(integer()), size = 1), unspecified(1))
  expect_identical(list_unchop(list(NULL), indices = list(integer()), ptype = numeric()), numeric())
  expect_identical(list_unchop(list(NULL), indices = list(integer()), ptype = numeric(), size = 1), vec_init(numeric(), n = 1))
  expect_identical(list_unchop(list(NULL, NULL), indices = list(integer(), integer()), ptype = numeric()), numeric())
})

test_that("NULLs are ignored when unchopped with other vectors", {
  expect_identical(list_unchop(list("a", NULL, "b")), c("a", "b"))
  expect_identical(list_unchop(list("a", NULL, "b"), indices = list(2, integer(), 1)), c("b", "a"))
})

test_that("can use a `NULL` element with a corresponding index", {
  # FIXME: Probably not quite right, but not entirely clear what it should be:
  # - Maybe `unspecified(2)` like when `size` is specified?
  # - Or should `NULL`s even be allowed in `list_unchop()`?
  expect_null(list_unchop(list(NULL), indices = list(1:2)))
  expect_identical(list_unchop(list(NULL), indices = list(1:2), size = 2L), unspecified(2))

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
  expect_identical(list_unchop(list("a", "b"), indices = list(2, 1)), c("b", "a"))
})

test_that("can unchop lists", {
  x <- list(list("a", "b"), list("c"))
  indices <- list(c(2, 3), 1)

  expect_identical(list_unchop(x, indices = indices), list("c", "a", "b"))
})

test_that("can unchop data frames", {
  df1 <- data_frame(x = 1:2)
  df2 <- data_frame(x = 3:4)

  x <- list(df1, df2)
  indices <- list(c(3, 1), c(2, 4))

  expect <- vec_slice(vec_c(df1, df2), vec_order(vec_c(!!! indices)))

  expect_identical(list_unchop(x, indices = indices), expect)
})

test_that("can unchop factors", {
  fctr1 <- factor("z")
  fctr2 <- factor(c("x", "y"))

  x <- list(fctr1, fctr2)
  indices <- list(2, c(3, 1))

  # levels are in the order they are seen!
  expect <- factor(c("y", "z", "x"), levels = c("z", "x", "y"))
  expect_identical(list_unchop(x, indices = indices), expect)

  # With `default`, the `default` is added in as the last item
  default <- factor("w")
  expect <- factor(c("y", "z", "x"), levels = c("z", "x", "y", "w"))
  expect_identical(
    list_unchop(x, indices = indices, size = 3, default = default),
    expect
  )
})

test_that("can fallback when unchopping matrices", {
  mat1 <- matrix(1:4, nrow = 2, ncol = 2)
  mat2 <- matrix(5:10, nrow = 3, ncol = 2)

  x <- list(mat1, mat2)
  indices <- list(c(4, 1), c(2, 3, 5))

  expect <- vec_slice(vec_c(mat1, mat2), vec_order(vec_c(!!! indices)))

  expect_identical(list_unchop(x, indices = indices), expect)
  expect_identical(list_unchop(x), vec_c(mat1, mat2))
})

test_that("can fallback when unchopping matrices and using `default`", {
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
    list_unchop(x, indices = indices, size = size, default = default),
    expect
  )
})

test_that("can fallback when unchopping arrays of >2D", {
  arr1 <- array(1:8, c(2, 2, 2))
  arr2 <- matrix(9:10, c(1, 2))

  x <- list(arr1, arr2)
  indices <- list(c(3, 1), 2)

  expect <- vec_slice(vec_c(arr1, arr2), vec_order(vec_c(!!! indices)))

  expect_identical(list_unchop(x, indices = indices), expect)
  expect_identical(list_unchop(x), vec_c(arr1, arr2))
})

test_that("can unchop with all size 0 elements and get the right ptype", {
  x <- list(integer(), integer())
  indices <- list(integer(), integer())
  expect_identical(list_unchop(x, indices = indices), integer())
})

test_that("can unchop with some size 0 elements", {
  x <- list(integer(), 1:2, integer())
  indices <- list(integer(), 2:1, integer())
  expect_identical(list_unchop(x, indices = indices), 2:1)
})

test_that("NULL is a valid index", {
  expect_equal(list_unchop(list(1, 2), indices = list(NULL, 1)), 2)
  expect_error(list_unchop(list(1, 2), indices = list(NULL, 2)), class = "vctrs_error_subscript_oob")
})

test_that("unchopping recycles elements of x to the size of the index", {
  x <- list(1, 2)
  indices <- list(c(3, 4, 5), c(2, 1))
  expect_identical(list_unchop(x, indices = indices), c(2, 2, 1, 1, 1))

  x <- list(1:2)
  indices <- list(1:3)
  expect_snapshot({
    (expect_error(list_unchop(x, indices = indices)))
    (expect_error(list_unchop(x, indices = indices, error_call = call("foo"), error_arg = "arg")))
  })
})

test_that("unchopping takes the common type", {
  x <- list(1, "a")
  indices <- list(1, 2)

  expect_snapshot({
    (expect_error(list_unchop(x, indices = indices), class = "vctrs_error_incompatible_type"))
    (expect_error(list_unchop(x, indices = indices, error_call = call("foo"), error_arg = "arg"), class = "vctrs_error_incompatible_type"))
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
  expect_identical(list_unchop(x, indices = indices, ptype = integer()), c(1L, 2L))

  x <- list(1.5, 2)
  expect_snapshot({
    (expect_error(list_unchop(x, indices = indices, ptype = integer())))
    (expect_error(list_unchop(x, indices = indices, ptype = integer(), error_call = call("foo"), error_arg = "arg")))
  })
})

test_that("leaving `indices = NULL` unchops sequentially", {
  x <- list(1:2, 3:5, 6L)
  expect_identical(list_unchop(x), 1:6)
})

test_that("outer names are kept", {
  x <- list(x = 1, y = 2)
  expect_named(list_unchop(x), c("x", "y"))
  expect_named(list_unchop(x, indices = list(2, 1)), c("y", "x"))
})

test_that("outer names are recycled in the right order", {
  x <- list(x = 1, y = 2)
  expect_error(list_unchop(x, indices = list(c(1, 2), 3)), "Can't merge")
  expect_named(list_unchop(x, indices = list(c(1, 3), 2), name_spec = "{outer}_{inner}"), c("x_1", "y", "x_2"))
  expect_named(list_unchop(x, indices = list(c(3, 1), 2), name_spec = "{outer}_{inner}"), c("x_2", "y", "x_1"))
})

test_that("outer names can be merged with inner names", {
  x <- list(x = c(a = 1), y = c(b = 2))
  expect_error(list_unchop(x), "Can't merge")
  expect_named(list_unchop(x, name_spec = "{outer}_{inner}"), c("x_a", "y_b"))
  expect_named(list_unchop(x, indices = list(2, 1), name_spec = "{outer}_{inner}"), c("y_b", "x_a"))
})

test_that("preserves names when inputs are cast to a common type (#1689)", {
  expect_named(
    list_unchop(list(c(a = 1)), ptype = integer()),
    "a"
  )
  expect_named(
    list_unchop(list(c(a = 1)), ptype = integer(), indices = list(1)),
    "a"
  )

  # With `default`
  expect_named(
    list_unchop(
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
    list_unchop(
      list(foo = c(a = 1)),
      ptype = integer(),
      name_spec = name_spec
    ),
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
  expect_named(
    list_unchop(
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
    list_unchop(x, indices = indices, ptype = integer()),
    c("a", "a", "b", "b")
  )

  # When `default` elements are recycled, names are also recycled
  expect_named(
    list_unchop(
      list(c(a = 1), c(b = 2)),
      indices = list(1, 3),
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

  expect_named(
    list_unchop(x, indices = indices),
    c("", "a", "c")
  )
  expect_named(
    list_unchop(x, indices = indices, size = 4, default = 0),
    c("", "a", "c", "")
  )
})

test_that("list_unchop() keeps data frame row names", {
  df1 <- data.frame(x = 1:2, row.names = c("r1", "r2"))
  df2 <- data.frame(x = 3:4, row.names = c("r3", "r4"))

  x <- list(df1, df2)
  indices <- list(c(3, 1), c(2, 4))

  result <- list_unchop(x, indices = indices)
  expect <- c("r2", "r3", "r1", "r4")
  expect_identical(vec_names(result), expect)

  default <- data.frame(x = 0L, row.names = "d")
  result <- list_unchop(x, indices = indices, size = 5, default = default)
  expect <- c("r2", "r3", "r1", "r4", "d")
  expect_identical(vec_names(result), expect)

  # With casting
  ptype <- data.frame(x = double())
  default <- data.frame(x = 0L, row.names = "d")
  result <- list_unchop(
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

  default <- data_frame(x = new_data_frame(list(a = 0), row.names = "inner"))
  expect_silent({
    result <- list_unchop(x, indices = indices, size = 3, default = default)
  })
  expect_identical(vec_names(result$x), c("inner...1", "inner...2", "inner...3"))
})

test_that("monitoring - can technically assign to the same location twice", {
  x <- list(1:2, 3L)
  indices <- list(1:2, 1L)

  expect_identical(list_unchop(x, indices = indices), c(3L, 2L, NA))
})

test_that("index values are validated", {
  x <- list(1, 2)
  indices1 <- list(4, 1)
  indices2 <- list(c(1, 4), 2)
  indices3 <- list(c(1, 3, 4), 2)

  expect_error(list_unchop(x, indices = indices1), class = "vctrs_error_subscript_oob")
  expect_error(list_unchop(x, indices = indices2), class = "vctrs_error_subscript_oob")

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

  # With `default`
  indices <- list(3, 1)
  default <- c(a = 0)
  expect_named(
    list_unchop(x, indices = indices, size = 3, default = default),
    c("a", "a", "a")
  )
  expect_named(
    list_unchop(
      x,
      indices = indices,
      size = 3,
      default = default,
      name_repair = "unique"
    ),
    c("a...1", "a...2", "a...3")
  )
})

test_that("list_unchop() can repair names quietly", {
  local_name_repair_verbose()

  x <- c(x = "a", x = "b", x = "c")
  indices <- list(2, c(3, 1))
  expect_snapshot({
    res <- list_unchop(vec_chop(x, indices = indices), indices = indices, name_repair = "unique_quiet")
  })
  expect_named(res, c("x...1", "x...2", "x...3"))

  x <- c("if" = "a", "in" = "b", "for" = "c")
  indices <- list(2, c(3, 1))
  expect_snapshot({
    res <- list_unchop(vec_chop(x, indices = indices), indices = indices, name_repair = "universal_quiet")
  })
  expect_named(res, c(".if", ".in", ".for"))
})

test_that("list_unchop() errors on unsupported location values", {
  expect_snapshot({
    (expect_error(
      list_unchop(list(1, 2), indices = list(c(1, 2), 0)),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      list_unchop(list(1), indices = list(-1)),
      class = "vctrs_error_subscript_type"
    ))
  })
})

test_that("missing values propagate", {
  expect_identical(
    list_unchop(list(1, 2), indices = list(c(NA_integer_, NA_integer_), c(NA_integer_, 3))),
    c(NA, NA, 2, NA)
  )
})

test_that("list_unchop() and vec_c() are consistent-ish regarding `size` and empty inputs (#1980)", {
  x <- list()
  indices <- list()

  # These should be consistent and return `NULL` when no inputs are provided.
  # We treat this roughly equivalent to `unspecified(0)`.
  expect_identical(vec_c(), NULL)
  expect_identical(vec_c(), list_unchop(x, indices = indices))

  # These should be consistent and return size 0 `ptype`
  expect_identical(vec_c(.ptype = integer()), integer())
  expect_identical(vec_c(.ptype = integer()), list_unchop(x, indices = indices, ptype = integer()))

  # `list_unchop()` allows you to require an output `size`. When `size` is
  # specified and the `ptype` isn't known, you get `unspecified(size)`. Again,
  # this is roughly equivalent to `NULL` but retains the `size` information.
  expect_identical(list_unchop(x, indices = indices, size = 0), unspecified())
  expect_identical(list_unchop(x, indices = indices, size = 5), unspecified(5))

  # If the `ptype` is known, you of course get a typed output
  expect_identical(list_unchop(x, indices = indices, size = 0, ptype = integer()), integer())
  expect_identical(list_unchop(x, indices = indices, size = 5, ptype = integer()), rep(NA_integer_, 5))
})

test_that("list_unchop() works with simple homogeneous foreign S3 classes", {
  expect_identical(
    list_unchop(list(foobar(1), foobar(2))),
    vec_c(foobar(c(1, 2)))
  )

  # With `default`
  expect_identical(
    list_unchop(
      list(foobar(1), foobar(2)),
      indices = list(3, 1),
      size = 4,
      default = foobar(0)
    ),
    vec_c(foobar(c(2, 0, 1, 0)))
  )
})

test_that("list_unchop() fails with complex foreign S3 classes", {
  expect_snapshot({
    x <- structure(foobar(1), attr_foo = "foo")
    y <- structure(foobar(2), attr_bar = "bar")
    (expect_error(list_unchop(list(x, y)), class = "vctrs_error_incompatible_type"))
    (expect_error(list_unchop(list(x, y), error_call = call("foo"), error_arg = "arg"), class = "vctrs_error_incompatible_type"))
  })

  expect_snapshot(error = TRUE, {
    x <- structure(foobar(1), attr_foo = "foo")
    default <- structure(foobar(2), attr_foo = "bar")
    list_unchop(list(x), indices = list(1), size = 2, default = default)
  })
})

test_that("list_unchop() fails with complex foreign S4 classes", {
  expect_snapshot({
    joe <- .Counts(c(1L, 2L), name = "Joe")
    jane <- .Counts(3L, name = "Jane")
    (expect_error(list_unchop(list(joe, jane)), class = "vctrs_error_incompatible_type"))
    (expect_error(list_unchop(list(joe, jane), error_call = call("foo"), error_arg = "arg"), class = "vctrs_error_incompatible_type"))
  })
})

test_that("list_unchop() falls back to c() if S3 method is available", {
  # Check off-by-one error
  expect_error(
    list_unchop(list(foobar(1), "", foobar(2)), indices = list(1, 2, 3)),
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
    list_unchop(list(foobar(1), foobar(2)), indices = list(1, 2)),
    foobar(c(1, 2))
  )
  expect_identical(
    list_unchop(list(foobar(1), foobar(2)), indices = list(2, 1)),
    foobar(c(2, 1))
  )
  expect_identical(
    list_unchop(list(NULL, foobar(1), NULL, foobar(2))),
    foobar(c(1, 2))
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
    foobar(c(3, NA, 2))
  )
  expect_identical(
    list_unchop(list(foobar(c(1, 2)), foobar(3)), indices = list(c(2, NA), NA)),
    foobar(c(NA, 1, NA))
  )

  # Respects `default`
  expect_identical(
    list_unchop(
      list(foobar(c(1, 2)), foobar(3)),
      indices = list(c(1, 3), 1),
      default = foobar(0),
      size = 4
    ),
    foobar(c(3, 0, 2, 0))
  )
  expect_identical(
    list_unchop(
      list(foobar(c(1, 2)), foobar(3)),
      indices = list(c(1, 3), 1),
      default = foobar(c(4, 5, 6, 7)),
      size = 4
    ),
    foobar(c(3, 5, 2, 7))
  )

  # Names are kept
  expect_identical(
    list_unchop(
      list(foobar(c(x = 1, y = 2)), foobar(c(x = 1))),
      indices = list(c(2, 1), 3)
    ),
    foobar(c(y = 2, x = 1, x = 1))
  )
  expect_identical(
    list_unchop(
      list(foobar(c(x = 1, y = 2)), foobar(c(x = 1))),
      indices = list(c(2, 1), 3),
      size = 5,
      default = foobar(c(default = 0))
    ),
    foobar(c(y = 2, x = 1, x = 1, default = 0, default = 0))
  )

  # Recycles to the size of index
  expect_identical(
    list_unchop(list(foobar(1), foobar(2)), indices = list(c(1, 3), 2)),
    foobar(c(1, 2, 1))
  )
  expect_identical(
    list_unchop(list(foobar(1), foobar(2)), indices = list(c(1, 2), integer())),
    foobar(c(1, 1))
  )
  expect_snapshot({
    (expect_error(
      list_unchop(list(foobar(1), foobar(2)), indices = list(c(1, 3), integer())),
      class = "vctrs_error_subscript_oob"
    ))
  })
  expect_snapshot({
    x <- list(foobar(1:2))
    indices <- list(1:3)
    (expect_error(list_unchop(x, indices = indices)))
    (expect_error(list_unchop(x, indices = indices, error_arg = "arg", error_call = call("foo"))))
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

  default <- .Counts(0L, name = "Unknown")

  expect_identical(
    list_unchop(
      list(joe, jane),
      indices = list(c(1, 3), 1),
      default = default,
      size = 5
    ),
    .Counts(c(3L, 0L, 2L, 0L, 0L), name = "Dispatched")
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
    x <- list(foobar(1))
    (expect_error(
      with_c_foobar(list_unchop(x, ptype = "")),
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
  expect_identical(
    list_unchop(
      list(a = c(foo = 1:2), b = c(bar = 3L)),
      indices = list(2:1, 3),
      default = c(baz = 0L),
      size = 4,
      name_spec = zap()
    ),
    c(2L, 1L, 3L, 0L)
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
  expect_identical(
    list_unchop(xs, indices = list(3, 1), size = 4, default = foobar(0)),
    c("dispatched2", "dispatched3", "dispatched1", "dispatched4")
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
  expect_identical(
    list_unchop(xs, indices = list(c(4, 1), 3), size = 5, default = foobar(0)),
    c("dispatched2", "dispatched4", "dispatched3", "dispatched1", "dispatched5")
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

  x <- foobar(c(x = 1, y = 2), class = "foo")
  default <- foobar(c(x = 1), foo = 1)
  expect_snapshot(error = TRUE, {
    list_unchop(list(x), indices = list(c(1, 2)), size = 3, default = default)
  })
})

test_that("Size 1 unspecified `NA` that isn't used doesn't error", {
  # Requires that casting happen before recycling, because the `NA` recycles
  # to size zero since it isn't used, resulting in a logical rather than an
  # unspecified (#1989).
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

test_that("list_unchop() `default` is inserted correctly", {
  xs <- list("a", "b")
  indices <- list(1, 3)

  expect_identical(
    list_unchop(xs, indices = indices, size = 4, default = "c"),
    c("a", "c", "b", "c")
  )
})

test_that("list_unchop() `default` is inserted correctly with data frames", {
  xs <- list(
    data.frame(a = 1:2, b = 1:2),
    data.frame(a = 3, b = 3)
  )
  indices <- list(1:2, 5)

  expect_identical(
    list_unchop(xs, indices = indices, size = 6, default = data.frame(a = 0, b = NA)),
    data.frame(
      a = c(1, 2, 0, 0, 3, 0),
      b = c(1, 2, NA, NA, 3, NA)
    )
  )
})

test_that("list_unchop() `default` is inserted correctly when there are duplicate indices", {
  xs <- list("a", "b", "c")
  indices <- list(1, 1, 3)

  expect_identical(
    list_unchop(xs, indices = indices, size = 4, default = "d"),
    c("b", "d", "c", "d")
  )
})

test_that("list_unchop() `default` is inserted correctly when it is the size of `size`", {
  xs <- list("2", "4")
  indices <- list(2, 4)

  default <- letters[1:5]

  expect_identical(
    list_unchop(xs, indices = indices, size = 5, default = default),
    c("a", "2", "c", "4", "e")
  )
})

test_that("list_unchop() `default` is correctly not used when all spots are filled", {
  xs <- list("a", "b", "c")
  indices <- list(1, 2, 3)

  expect_identical(
    list_unchop(xs, indices = indices, size = 3, default = "d"),
    c("a", "b", "c")
  )
})

test_that("list_unchop() `default` names work correctly with `name_spec`", {
  xs <- list(x = c(a = "a"), y = c(b = "b"), z = c(c = "c"))
  indices <- list(1, 3, NA)

  expect_identical(
    list_unchop(
      xs,
      indices = indices,
      size = 5,
      default = c(d = "d"),
      name_spec = "{outer}_{inner}"
    ),
    c(x_a = "a", d = "d", y_b = "b", d = "d", d = "d")
  )
})

test_that("list_unchop() `indices` is required when `default` is specified", {
  # Otherwise `default` would never be utilized
  expect_snapshot(error = TRUE, {
    list_unchop(list(1), default = 0)
  })
})

test_that("list_unchop() `indices` is required when `size` is specified", {
  # Otherwise we'd lay out `xs` in sequence and would have space left at
  # the end of the vector, which doesn't make much sense
  expect_snapshot(error = TRUE, {
    list_unchop(list(1), size = 1)
  })
})

test_that("list_unchop() `size` is required when `default` is specified", {
  # Otherwise we don't have a clear indication of what to recycle `default`
  # against. We could use `sum(lengths(indices))` and place `default` in unused
  # spots (in the case of overlapping `indices`), but I think this is more of
  # a weird side effect of this function rather than a feature.
  expect_snapshot(error = TRUE, {
    list_unchop(list(1), indices = list(1), default = 0)
  })
})

test_that("list_unchop() `size` type is validated", {
  expect_snapshot(error = TRUE, {
    list_unchop(list(1), indices = list(1), size = "x")
  })
})

test_that("list_unchop() `indices` are validated against `size`", {
  # TODO: Not the best error message here
  expect_snapshot(error = TRUE, {
    list_unchop(list(1), indices = list(3), size = 2)
  })
})

test_that("list_unchop() `default` vector check is done", {
  expect_snapshot(error = TRUE, {
    list_unchop(
      list(1),
      indices = list(1),
      size = 1,
      default = lm(1 ~ 1),
      default_arg = "d"
    )
  })
})

test_that("list_unchop() `default` size check is done", {
  # Must be size 1, or same size as `size`
  expect_identical(
    list_unchop(
      list(1),
      indices = list(2),
      size = 3,
      default = 0,
      default_arg = "d"
    ),
    c(0, 1, 0)
  )
  expect_snapshot(error = TRUE, {
    list_unchop(
      list(1),
      indices = list(1),
      size = 1,
      default = 1:2,
      default_arg = "d"
    )
  })
})

test_that("list_unchop() `default` is taken into account when computing `ptype`", {
  expect_identical(
    list_unchop(
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
    list_unchop(
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
  expect_identical(
    list_unchop(
      list(),
      indices = list(),
      size = 2,
      default = 0,
      default_arg = "d"
    ),
    c(0, 0)
  )

  # Computed `ptype` among `xs` isn't compatible with `default`
  expect_snapshot(error = TRUE, {
    list_unchop(
      list(x = 1),
      indices = list(1),
      size = 2,
      default = "a",
      default_arg = "d"
    )
  })

  # Provided `ptype` isn't compatible with `default`
  expect_snapshot(error = TRUE, {
    list_unchop(
      list(x = 1L),
      indices = list(1),
      size = 2,
      default = 1.5,
      default_arg = "d",
      ptype = integer()
    )
  })
})

test_that("list_unchop() `unmatched = 'error'` doesn't error when all locations are covered", {
  expect_identical(
    list_unchop(list(1:3), indices = list(1:3), size = 3, unmatched = "error"),
    1:3
  )
})

test_that("list_unchop() `unmatched = 'error'` doesn't error in the empty case", {
  expect_identical(
    list_unchop(list(), indices = list(), unmatched = "error"),
    NULL
  )
  expect_identical(
    list_unchop(list(), indices = list(), size = 0, unmatched = "error"),
    unspecified()
  )
})

test_that("list_unchop() `unmatched = 'error'` errors with implied output length", {
  expect_snapshot(error = TRUE, {
    # Implied output length of 2 from `indices`, but duplicates result in unmatched locations
    list_unchop(list(1, 1), indices = list(1, 1), unmatched = "error")
  })
  expect_snapshot(error = TRUE, {
    # Implied output length of 2 from `indices`, but `NA` results in unmatched locations
    list_unchop(list(1, 1), indices = list(1, NA), unmatched = "error")
  })
})

test_that("list_unchop() `unmatched = 'error'` errors with unmatched `indices` when `size` is used", {
  expect_snapshot(error = TRUE, {
    list_unchop(
      list(1, 3),
      indices = list(1, 3),
      size = 3,
      unmatched = "error"
    )
  })
})

test_that("list_unchop() `unmatched = 'error'` errors pluralize correctly", {
  expect_snapshot(error = TRUE, {
    # One location
    list_unchop(
      list(1, 3),
      indices = list(1, 3),
      size = 3,
      unmatched = "error"
    )
  })
  expect_snapshot(error = TRUE, {
    # Two locations
    list_unchop(
      list(1, 3),
      indices = list(1, 3),
      size = 4,
      unmatched = "error"
    )
  })
  expect_snapshot(error = TRUE, {
    # Many locations
    list_unchop(
      list(1, 3),
      indices = list(1, 3),
      size = 100,
      unmatched = "error"
    )
  })
})

test_that("list_unchop() `unmatched = 'error'` error classes are as expected", {
  cnd <- catch_cnd(list_unchop(
    list(1, 3),
    indices = list(1, 3),
    size = 3,
    unmatched = "error"
  ))
  expect_true(inherits_all(
    cnd,
    c("vctrs_error_unchop_unmatched", "vctrs_error_unchop")
  ))
})

test_that("list_unchop() `unmatched = 'error'` can't be set when `default` is also set", {
  expect_snapshot(error = TRUE, {
    list_unchop(
      list(1),
      indices = list(1),
      default = 1,
      size = 1,
      unmatched = "error"
    )
  })
})

test_that("list_unchop() `unmatched` is validated", {
  expect_snapshot(error = TRUE, {
    list_unchop(list(1), indices = list(1), unmatched = "e")
  })
})
