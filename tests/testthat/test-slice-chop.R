# vec_chop ----------------------------------------------------------------

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

test_that("data frame row names are kept", {
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

test_that("vec_chop() restores when attributes have not been restored by `[`", {
  local_methods(
    `[.vctrs_foobar` = function(x, i, ...) "dispatched",
    vec_restore.vctrs_foobar = function(...) "dispatched-and-restored"
  )

  result <- vec_chop(foobar(NA))[[1]]
  expect_equal(result, "dispatched-and-restored")
})

test_that("vec_chop() falls back to `[` for shaped objects with no proxy", {
  x <- foobar(1)
  dim(x) <- c(1, 1)
  result <- vec_chop(x)[[1]]
  expect_equal(result, x)
})

test_that("`indices` are validated", {
  expect_error(vec_chop(1, 1), "`indices` must be a list of index values, or `NULL`")
  expect_error(vec_chop(1, list(1.5)), class = "vctrs_error_cast_lossy")
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

test_that("`indices` can use names", {
  x <- set_names(1:3, c("a", "b", "c"))

  expect_equal(
    vec_chop(x, list(1, 2:3)),
    vec_chop(x, list("a", c("b", "c")))
  )
})

test_that("`indices` can use array row names", {
  x <- array(1:4, c(2, 2), dimnames = list(c("r1", "r2")))

  expect_equal(
    vec_chop(x, list("r1")),
    vec_chop(x, list(1))
  )
})

test_that("`indices` cannot use data frame row names", {
  df <- data.frame(x = 1, row.names = "r1")
  expect_error(vec_chop(df, list("r1")), "Can't use character")
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
  expect_identical(vec_chop_seq(bytes(1, 2, 3), start, size), list(bytes(2, 3)))
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

