# splits ------------------------------------------------------------------

test_that("can split empty vector", {
  out <- vec_split(integer(), character())

  expect_s3_class(out, "data.frame")
  expect_equal(out$key, character())
  expect_equal(out$val, list_of(.ptype = integer()))
})

test_that("split data frame with data frame", {
  df <- data.frame(x = c(1, 1, 2), y = c(1, 1, 1))
  out <- vec_split(df, df)

  expect_s3_class(out, "data.frame")
  expect_equal(out$key, data.frame(x = c(1, 2), y = c(1, 1)))
  expect_equal(out$val, list_of(
    data.frame(x = c(1, 1), y = c(1, 1)),
    data.frame(x = 2, y = 1)
  ))
})

test_that("x and by must be same size", {
  expect_error(
    vec_split(1:3, 1:2),
    "same size"
  )
})

test_that("split takes the equality proxy (#375)", {
  scoped_comparable_tuple()
  x <- tuple(c(1, 2, 1), 1:3)
  expect_identical(nrow(vec_split(1:3, x)), 2L)
})

test_that("split works with different encodings", {
  encs <- encodings()
  expect_identical(nrow(vec_split(1:3, encs)), 1L)
})

# split id ---------------------------------------------------------------

test_that("can locate unique groups of an empty vector", {
  out <- vec_split_id(integer())

  expect_s3_class(out, "data.frame")
  expect_equal(out$key, integer())
  expect_equal(out$id, list_of(.ptype = integer()))
})

test_that("can locate unique groups of a data frame", {
  df <- data_frame(x = c(1, 1, 1, 2, 2), y = c("a", "a", "b", "a", "b"))
  out <- vec_split_id(df)

  expect_equal(nrow(out), 4L)
  expect_equal(out$key, vec_unique(df))
})

test_that("can locate unique groups of a data frame with a list column", {
  df <- data_frame(x = list(1:2, 1:2, "a", 5.5, "a"))
  out <- vec_split_id(df)

  expect_equal(nrow(out), 3L)
  expect_equal(out$key, vec_unique(df))
})

test_that("`x` must be a vector", {
  expect_error(vec_split_id(environment()), class = "vctrs_error_scalar_type")
})

test_that("`key` column retains full type information", {
  x <- factor(letters[c(1, 2, 1)], levels = letters[1:3])
  out <- vec_split_id(x)

  expect_equal(levels(out$key), levels(x))
})

test_that("`key` and `value` retain names", {
  x <- c(a = 1, b = 2, c = 1, a = 1)
  split <- vec_split(x, x)
  expect_identical(split$key, c(a = 1, b = 2))
  expect_identical(split$val[[1]], c(a = 1, c = 1, a = 1))
  expect_identical(split$val[[2]], c(b = 2))
})

test_that("vec_split_id takes the equality proxy", {
  scoped_comparable_tuple()
  x <- tuple(c(1, 2, 1), 1:3)
  expect_equal(vec_split_id(x)$key, x[1:2])
  expect_equal(vec_split_id(x)$id, list_of(c(1L, 3L), 2L))

  x <- as.POSIXlt(new_datetime(c(1, 2, 1)))
  expect_equal(vec_split_id(x)$key, x[1:2])
  expect_equal(vec_split_id(x)$id, list_of(c(1L, 3L), 2L))
})

test_that("vec_split_id works with different encodings", {
  encs <- encodings()
  expect_identical(nrow(vec_split_id(encs)), 1L)
})

# vec_split_along ---------------------------------------------------------

test_that("`NULL` is passed through", {
  expect_equal(vec_split_along(NULL), NULL)
})

test_that("atomics are split into a list", {
  expect_equal(vec_split_along(1:5), lapply(1:5, identity))
  expect_equal(vec_split_along(letters), lapply(letters, identity))
})

test_that("atomic names are kept", {
  x <- set_names(1:5)
  expect_equal(lapply(vec_split_along(x), names), as.list(names(x)))
})

test_that("base R classed objects are split into a list", {
  fctr <- factor(c("a", "b"))
  expect_equal(vec_split_along(fctr), lapply(vec_seq_along(fctr), vec_slice, x = fctr))

  date <- new_date(c(0, 1))
  expect_equal(vec_split_along(date), lapply(vec_seq_along(date), vec_slice, x = date))
})

test_that("base R classed object names are kept", {
  fctr <- set_names(factor(c("a", "b")))
  expect_equal(lapply(vec_split_along(fctr), names), as.list(names(fctr)))
})

test_that("list elements are split", {
  x <- list(1, 2)
  expect_equal(vec_split_along(list(1, 2)), lapply(vec_seq_along(x), vec_slice, x = x))
})

test_that("data frames are split rowwise", {
  x <- data_frame(x = 1:2, y = c("a", "b"))
  expect_equal(vec_split_along(x), lapply(vec_seq_along(x), vec_slice, x = x))
})

test_that("data frame row names are kept", {
  x <- data_frame(x = 1:2, y = c("a", "b"))
  rownames(x) <- c("r1", "r2")
  expect_equal(lapply(vec_split_along(x), rownames), list("r1", "r2"))
})

test_that("matrices / arrays are split rowwise", {
  x <- array(1:12, c(2, 2, 2))
  expect_equal(vec_split_along(x), lapply(vec_seq_along(x), vec_slice, x = x))
})

test_that("matrix / array row names are kept", {
  x <- array(1:12, c(2, 2, 2), dimnames = list(c("r1", "r2"), c("c1", "c2")))
  expect_equal(lapply(vec_split_along(x), rownames), list("r1", "r2"))
})

test_that("matrices / arrays without row names have other dimension names kept", {
  x <- array(1:12, c(2, 2, 2), dimnames = list(NULL, c("c1", "c2")))
  expect_equal(lapply(vec_split_along(x), colnames), list(c("c1", "c2"), c("c1", "c2")))
})

test_that("vec_split_along throws error with non-vector inputs", {
  expect_error(vec_split_along(environment()), class = "vctrs_error_scalar_type")
})

test_that("vec_split_along() doesn't restore when attributes have already been restored", {
  scoped_global_bindings(
    `[.vctrs_foobar` = function(x, i, ...) structure("dispatched", foo = "bar"),
    vec_restore.vctrs_foobar = function(...) structure("dispatched-and-restored", foo = "bar")
  )
  expect_equal(vec_split_along(foobar(NA)), list(structure("dispatched", foo = "bar")))
})

test_that("vec_split_along() restores when attributes have not been restored by `[`", {
  scoped_global_bindings(
    `[.vctrs_foobar` = function(x, i, ...) "dispatched",
    vec_restore.vctrs_foobar = function(...) "dispatched-and-restored"
  )
  expect_equal(vec_split_along(foobar(NA)), list("dispatched-and-restored"))
})

test_that("vec_split_along() falls back to `[` for shaped objects with no proxy", {
  x <- foobar(1)
  dim(x) <- c(1, 1)
  expect_equal(vec_split_along(x), list(x))
})
