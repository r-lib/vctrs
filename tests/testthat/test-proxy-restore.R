
test_that("default vec_restore() restores attributes except names", {
  to <- structure(NA, foo = "foo", bar = "bar")
  expect_identical(vec_restore.default(NA, to), structure(NA, foo = "foo", bar = "bar"))

  to <- structure(NA, names = "a", foo = "foo", bar = "bar")
  expect_identical(vec_restore.default(NA, to), structure(NA, foo = "foo", bar = "bar"))

  to <- structure(NA, foo = "foo", names = "a", bar = "bar")
  expect_identical(vec_restore.default(NA, to), structure(NA, foo = "foo", bar = "bar"))

  to <- structure(NA, foo = "foo", bar = "bar", names = "a")
  expect_identical(vec_restore.default(NA, to), structure(NA, foo = "foo", bar = "bar"))
})

test_that("default vec_restore() restores objectness", {
  to <- structure(NA, class = "foo")
  x <- vec_restore.default(NA, to)
  expect_true(is.object(x))
  expect_s3_class(x, "foo")
})

test_that("data frame vec_restore() checks type", {
  expect_error(vec_restore(NA, mtcars), "Attempt to restore data frame from a logical")
})

test_that("data frame restore forces character column names", {
  df <- new_data_frame(list(1))
  expect_named(vec_restore(df, df), "")
})

test_that("can use vctrs primitives from vec_restore() without inflooping", {
  local_methods(
    vec_restore.vctrs_foobar = function(x, to, ...) {
      vec_ptype(x)
      vec_init(x)
      obj_check_vector(x)
      vec_slice(x, 0)
      "woot"
    }
  )

  foobar <- new_vctr(1:3, class = "vctrs_foobar")
  expect_identical(vec_slice(foobar, 2), "woot")
})

test_that("dimensions are preserved by default restore method", {
  x <- foobar(1:4)
  dim(x) <- c(2, 2)
  dimnames(x) <- list(a = c("foo", "bar"), b = c("quux", "hunoz"))

  exp <- foobar(c(1L, 3L))
  dim(exp) <- c(1, 2)
  dimnames(exp) <- list(a = "foo", b = c("quux", "hunoz"))

  expect_identical(vec_slice(x, 1), exp)
})

test_that("names attribute isn't set when restoring 1D arrays using 2D+ objects", {
  x <- foobar(1:2)
  dim(x) <- c(2)
  nms <- c("foo", "bar")
  dimnames(x) <- list(nms)

  res <- vec_restore(x, matrix(1))

  expect_null(attributes(res)$names)
  expect_equal(attr(res, "names"), nms)
  expect_equal(names(res), nms)
})

test_that("arguments are not inlined in the dispatch call (#300)", {
  local_methods(
    vec_restore.vctrs_foobar = function(x, to, ...) sys.call(),
    vec_proxy.vctrs_foobar = unclass
  )
  call <- vec_restore(foobar(list(1)), foobar(list(1)))
  expect_equal(call, quote(vec_restore.vctrs_foobar(x = x, to = to)))
})

test_that("restoring to non-bare data frames calls `vec_bare_df_restore()` before dispatching", {
  x <- list(x = numeric())
  to <- new_data_frame(x, class = "tbl_foobar")

  local_methods(
    vec_restore.tbl_foobar = function(x, to, ..., n) {
      if (is.data.frame(x)) {
        abort(class = "error_df_restore_was_called")
      }
    }
  )

  expect_error(vec_restore(x, to), class = "error_df_restore_was_called")
})

test_that("row names are not restored if target is not a data frame", {
  proxy <- data.frame(x = 1)
  out <- vec_restore(proxy, to = foobar(""))
  exp <- list(names = "x", class = "vctrs_foobar")
  expect_identical(attributes(out), exp)
})

test_that("attributes are properly restored when they contain special attributes", {
  exp <- list(foo = TRUE, bar = TRUE)

  x <- structure(list(), foo = TRUE, names = chr(), bar = TRUE)
  out <- vec_restore_default(list(), x)
  expect_identical(attributes(out), exp)

  # Was broken by #943
  x <- structure(list(), foo = TRUE, names = chr(), row.names = int(), bar = TRUE)
  out <- vec_restore_default(list(), x)
  expect_identical(attributes(out), exp)
})

test_that("names<- is not called with partial data (#1108)", {
  x <- set_names(foobar(1:2), c("a", "b"))

  values <- list()
  local_methods(
    `names<-.vctrs_foobar` = function(x, value) {
      if (!is_null(value)) {
        values <<- c(values, list(value))
      }
      NextMethod()
    }
  )

  vec_c(x, x)
  expect_equal(values, list(c("a", "b", "a", "b")))
})

test_that("recursive proxy and restore work with recursive records", {
  new_recursive_rcrd <- function(x) {
    new_rcrd(
      list(field = x),
      class = "my_recursive_rcrd"
    )
  }

  internal <- new_rcrd(list(internal_field = 1:2))
  x <- new_recursive_rcrd(data_frame(col = internal))

  proxy <- vec_proxy_recurse(x)
  exp <- data_frame(field = data_frame(col = data_frame(internal_field = 1:2)))
  expect_equal(proxy, exp)
  expect_equal(vec_restore_recurse(proxy, x), x)

  # Non-recursive case doesn't proxy `internal`
  proxy <- vec_proxy(x)
  exp <- data_frame(field = data_frame(col = internal))
  expect_equal(proxy, exp)
  expect_equal(vec_restore(proxy, x), x)

  x_exp <- new_recursive_rcrd(data_frame(col = vec_rep(internal, 2)))
  expect_equal(
    list_unchop(list(x, x)),
    x_exp
  )

  df <- data_frame(x = x)
  df_exp <- data_frame(x = x_exp)
  expect_equal(vec_rbind(df, df), df_exp)
  expect_equal(vec_c(df, df), df_exp)
})
