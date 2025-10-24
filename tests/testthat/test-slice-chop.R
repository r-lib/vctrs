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
    vec_restore.vctrs_foobar = function(...) {
      structure("dispatched-and-restored", foo = "bar")
    }
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
    (expect_error(
      vec_chop(1, indices = list(1.5)),
      class = "vctrs_error_subscript_type"
    ))
  })
  expect_snapshot({
    (expect_error(
      vec_chop(1, indices = list(2)),
      class = "vctrs_error_subscript_oob"
    ))
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
  expect_error(
    vec_chop(x, indices = list("a", c("b", "c"))),
    class = "vctrs_error_subscript_type"
  )

  x <- array(1:4, c(2, 2), dimnames = list(c("r1", "r2")))
  expect_error(
    vec_chop(x, indices = list("r1")),
    class = "vctrs_error_subscript_type"
  )

  x <- data.frame(x = 1, row.names = "r1")
  expect_error(
    vec_chop(x, indices = list("r1")),
    class = "vctrs_error_subscript_type"
  )
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
  x <- chr_rle(foo = 10L, bar = 5L)

  # `x` starts in compact form
  expect_false(chr_rle_is_materialized(x))

  result <- vec_chop(x)

  # Chopping materializes `x`
  expect_true(chr_rle_is_materialized(x))

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
  expect_identical(
    vec_chop_seq(chr("1", "2", "3"), start, size),
    list(chr("2", "3"))
  )
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

test_that("`vec_chop()` can't take `compact_seq()` indices directly", {
  expect_snapshot(error = TRUE, transform = scrub_internal_error_line_number, {
    vec_chop(1:2, indices = list(compact_seq(1, 2)))
  })
})
