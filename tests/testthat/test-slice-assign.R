context("test-slice-assign")

test_that("slice-assign throws error with non-vector inputs", {
  x <- environment()
  expect_error(vec_slice(x, 1L) <- 1L, class = "vctrs_error_scalar_type")
})

test_that("slice-assign throws error with non-vector `value`", {
  x <- 1L
  expect_error(vec_slice(x, 1L) <- NULL, class = "vctrs_error_scalar_type")
  expect_error(vec_slice(x, 1L) <- environment(), class = "vctrs_error_scalar_type")
})

test_that("can slice-assign NULL", {
  x <- NULL
  vec_slice(x, 1L) <- 1
  expect_identical(x, NULL)
})

test_that("can slice-assign base vectors", {
  x <- rep(FALSE, 3)
  vec_slice(x, 2) <- TRUE
  expect_identical(x, lgl(FALSE, TRUE, FALSE))

  x <- rep(0L, 3)
  vec_slice(x, 2) <- 1L
  expect_identical(x, int(0L, 1L, 0L))

  x <- rep(0., 3)
  vec_slice(x, 2) <- 1
  expect_identical(x, dbl(0, 1, 0))

  x <- rep(0i, 3)
  vec_slice(x, 2) <- 1i
  expect_identical(x, cpl(0i, 1i, 0i))

  x <- rep("", 3)
  vec_slice(x, 2) <- "foo"
  expect_identical(x, chr("", "foo", ""))

  x <- as.raw(rep(0, 3))
  vec_slice(x, 2) <- as.raw(1)
  expect_identical(x, as.raw(c(0, 1, 0)))
})

test_that("can assign base vectors", {
  x <- rep(FALSE, 3)
  expect_identical(vec_assign(x, 2, TRUE), lgl(FALSE, TRUE, FALSE))
  expect_identical(x, rep(FALSE, 3))

  x <- rep(0L, 3)
  expect_identical(vec_assign(x, 2, 1L), int(0L, 1L, 0L))
  expect_identical(x, rep(0L, 3))

  x <- rep(0., 3)
  expect_identical(vec_assign(x, 2, 1), dbl(0, 1, 0))
  expect_identical(x, rep(0., 3))

  x <- rep(0i, 3)
  expect_identical(vec_assign(x, 2, 1i), cpl(0i, 1i, 0i))
  expect_identical(x, rep(0i, 3))

  x <- rep("", 3)
  expect_identical(vec_assign(x, 2, "foo"), chr("", "foo", ""))
  expect_identical(x, rep("", 3))

  x <- as.raw(rep(0, 3))
  expect_identical(vec_assign(x, 2, as.raw(1)), as.raw(c(0, 1, 0)))
  expect_identical(x, as.raw(rep(0, 3)))
})

test_that("can assign shaped base vectors", {
  mat <- as.matrix

  x <- mat(rep(FALSE, 3))
  expect_identical(vec_assign(x, 2, TRUE), mat(lgl(FALSE, TRUE, FALSE)))
  expect_identical(x, mat(rep(FALSE, 3)))

  x <- mat(rep(0L, 3))
  expect_identical(vec_assign(x, 2, 1L), mat(int(0L, 1L, 0L)))
  expect_identical(x, mat(rep(0L, 3)))

  x <- mat(rep(0, 3))
  expect_identical(vec_assign(x, 2, 1), mat(dbl(0, 1, 0)))
  expect_identical(x, mat(rep(0, 3)))

  x <- mat(rep(0i, 3))
  expect_identical(vec_assign(x, 2, 1i), mat(cpl(0i, 1i, 0i)))
  expect_identical(x, mat(rep(0i, 3)))

  x <- mat(rep("", 3))
  expect_identical(vec_assign(x, 2, "foo"), mat(chr("", "foo", "")))
  expect_identical(x, mat(rep("", 3)))

  x <- mat(as.raw(rep(0, 3)))
  expect_identical(vec_assign(x, 2, as.raw(1)), mat(as.raw(c(0, 1, 0))))
  expect_identical(x, mat(as.raw(rep(0, 3))))
})

test_that("can slice-assign lists", {
  x <- rep(list(NULL), 3)
  vec_slice(x, 2) <- list(NA)
  expect_identical(x, list(NULL, NA, NULL))
})

test_that("can slice-assign shaped lists", {
  mat <- as.matrix
  x <- mat(rep(list(NULL), 3))
  vec_slice(x, 2) <- list(NA)
  expect_identical(x, mat(list(NULL, NA, NULL)))
})

test_that("can assign lists", {
  x <- rep(list(NULL), 3)
  expect_identical(vec_assign(x, 2, list(NA)), list(NULL, NA, NULL))
  expect_identical(x, rep(list(NULL), 3))
})

test_that("can assign shaped lists", {
  mat <- as.matrix
  x <- mat(rep(list(NULL), 3))
  expect_identical(vec_assign(x, 2, list(NA)), mat(list(NULL, NA, NULL)))
  expect_identical(x, mat(rep(list(NULL), 3)))
})

test_that("can assign object of any dimensionality", {
  x1 <- ones(2)
  x2 <- ones(2, 3)
  x3 <- ones(2, 3, 4)
  x4 <- ones(2, 3, 4, 5)

  expect_identical(vec_assign(x1, 1L, 2L), array(rep(c(2, 1), 1),  dim = 2))
  expect_identical(vec_assign(x2, 1L, 2L), array(rep(c(2, 1), 3),  dim = c(2, 3)))
  expect_identical(vec_assign(x3, 1L, 2L), array(rep(c(2, 1), 12), dim = c(2, 3, 4)))
  expect_identical(vec_assign(x4, 1L, 2L), array(rep(c(2, 1), 60), dim = c(2, 3, 4, 5)))
})

test_that("atomics can't be assigned in lists", {
  x <- list(NULL)
  expect_error(vec_slice(x, 1) <- 1, class = "vctrs_error_incompatible_type")
  expect_error(vec_assign(x, 1, 2), class = "vctrs_error_incompatible_type")

  expect_error(vec_slice(x, 1) <- "foo", class = "vctrs_error_incompatible_type")
  expect_error(vec_assign(x, 1, "foo"), class = "vctrs_error_incompatible_type")
})

test_that("Unspecified `NA` vector can be assigned into lists", {
  x <- list(1, 2)
  vec_slice(x, 1) <- NA
  expect_identical(x, list(NULL, 2))
})

test_that("monitoring test - unspecified() can be assigned in lists", {
  x <- list(1, 2)
  expect_error(vec_slice(x, 1) <- unspecified(1), NA)
  expect_equal(x, list(NULL, 2))
})

test_that("can assign and slice-assign data frames", {
  df <- data.frame(x = 1:2)
  df$y <- data.frame(a = 2:1)

  orig <- duplicate(df, shallow = FALSE)

  other <- data.frame(x = 3)
  other$y <- data.frame(a = 3)

  exp <- data.frame(x = int(3, 2))
  exp$y <- data.frame(a = int(3, 1))

  expect_identical(vec_assign(df, 1, other), exp)
  expect_identical(df, orig)

  vec_slice(df, 1) <- other
  expect_identical(df, exp)
})

test_that("can slice-assign using logical index", {
  x <- c(2, 1)
  vec_slice(x, TRUE) <- 3
  expect_equal(x, c(3, 3))

  vec_slice(x, c(TRUE, FALSE)) <- 4
  expect_equal(x, c(4, 3))

  expect_error(
    vec_assign(x, c(TRUE, FALSE, TRUE), 5),
    class = "vctrs_error_subscript_size"
  )
  expect_error(
    vec_assign(mtcars, c(TRUE, FALSE), mtcars[1, ]),
    class = "vctrs_error_subscript_size"
  )
})

test_that("slice-assign ignores NA in logical subsetting", {
  x <- c(NA, 1, 2)
  expect_equal(`vec_slice<-`(x, x > 0, 1), c(NA, 1, 1))
  expect_equal(`vec_slice<-`(x, x > 0, c(NA, 2:1)), c(NA, 2, 1))
})

test_that("slice-assign with arrays ignores NA in logical subsetting", {
  mat <- as.matrix
  x <- c(NA, 1, 2)
  expect_equal(`vec_slice<-`(mat(x), x > 0, 1), mat(c(NA, 1, 1)))
  expect_equal(`vec_slice<-`(mat(x), x > 0, c(NA, 2:1)), mat(c(NA, 2, 1)))
})

test_that("slice-assign ignores NA in integer subsetting", {
  x <- 0:2
  expect_equal(`vec_slice<-`(x, c(NA, 2:3), 1), c(0, 1, 1))
  expect_equal(`vec_slice<-`(x, c(NA, 2:3), c(NA, 2:1)), c(0, 2, 1))
})

test_that("slice-assign with arrays ignores NA in integer subsetting", {
  mat <- as.matrix
  x <- mat(0:2)
  expect_equal(`vec_slice<-`(x, c(NA, 2:3), 1), mat(c(0, 1, 1)))
  expect_equal(`vec_slice<-`(x, c(NA, 2:3), c(NA, 2:1)), mat(c(0, 2, 1)))
})

test_that("can't modify subset with missing argument", {
  x <- 1:3
  expect_error(vec_slice(x, ) <- 2L)
})

test_that("can modify subset with recycled NA argument", {
  x <- 1:3
  vec_slice(x, NA) <- 2L
  expect_identical(x, 1:3)
})

test_that("can modify subset with recycled TRUE argument", {
  x <- 1:3
  vec_slice(x, TRUE) <- 2L
  expect_identical(x, rep(2L, 3))
})

test_that("can modify subset with recycled FALSE argument", {
  x <- 1:3
  vec_slice(x, FALSE) <- 2L
  expect_identical(x, 1:3)
})

test_that("can modify subset with NULL argument", {
  x <- 1:3
  vec_slice(x, NULL) <- 2L

  expect_identical(x, 1:3)
})

test_that("can slice-assign with missing indices", {
  x <- 1:3
  y <- 4:6
  test <- c(NA, TRUE, FALSE)
  vec_slice(x, test) <- vec_slice(y, test)
  expect_identical(x, int(1, 5, 3))
})

test_that("slice-assign checks vectorness", {
  x <- foobar(list(1))
  expect_error(vec_slice(x, 1) <- 10, class = "vctrs_error_scalar_type")
})

test_that("a coercible RHS is cast to LHS before assignment (#140)", {
  x <- 1:2
  expect_error(vec_slice(x, 1) <- "1", class = "vctrs_error_incompatible_type")

  x <- c("foo", "bar")
  expect_error(vec_slice(x, 1) <- 1, class = "vctrs_error_incompatible_type")

  x <- 1:2
  expect_error(vec_slice(x, 1) <- 3.5, class = "vctrs_error_cast_lossy")

  allow_lossy_cast(vec_slice(x, 1) <- 3.5)
  expect_identical(x, int(3, 2))

  x <- matrix(1:4, 2)
  vec_slice(x, 1) <- matrix(c(FALSE, FALSE), 1)
  expect_identical(x, matrix(int(0, 2, 0, 4), 2))
  expect_error(vec_assign(x, 1, matrix(c("", ""), 1)), class = "vctrs_error_incompatible_type")
})

test_that("slice-assign takes the proxy", {
  local_proxy()

  x <- new_proxy(1:3)
  y <- new_proxy(20:21)

  vec_slice(x, 2:3) <- y

  expect_identical(proxy_deref(x), int(1, 20, 21))
})

test_that("can use names to vec_slice<-() a named object", {
  x0 <- c(a = 1, b = 2)
  x1 <- c(a = 1, a = 2)

  vec_slice(x0, "b") <- 3
  expect_identical(x0, c(a = 1, b = 3))

  vec_slice(x1, "a") <- 3
  expect_identical(x1, c(a = 3, a = 2))
})

test_that("can use names to vec_slice<-() a named object", {
  x0 <- 1:3

  expect_error(
    vec_slice(x0, letters[1]) <- 4L,
    "Can't use character names to index an unnamed vector.",
    fixed = TRUE
  )
  expect_error(
    vec_slice(x0, letters[25:27]) <- 5L,
    "Can't use character names to index an unnamed vector.",
    fixed = TRUE
  )
})

test_that("slice-assign falls back to `[<-` when proxy is not implemented", {
  obj <- foobar(c("foo", "bar", "baz"))
  expect_error(vec_slice(obj, 1:2) <- TRUE, class = "vctrs_error_incompatible_type")

  vec_slice(obj, 1:2) <- foobar("quux")

  vec_ptype2(foobar(""), foobar(""))
  vec_cast(foobar(""), foobar(""))
  #> Error: Can't cast <vctrs_foobar> to <vctrs_foobar>

  local_methods(
    `[<-.vctrs_foobar` = function(x, i, value) {
      x <- unclass(x)
      x[i] <- "dispatched"
      x
    },
    vec_ptype2.logical.vctrs_foobar = function(...) foobar(""),
    vec_ptype2.vctrs_foobar = function(...) foobar(""),
    vec_cast.vctrs_foobar = function(...) NULL,
    vec_cast.vctrs_foobar.logical = function(x, to, ...) foobar(rep("", length(x)))
  )

  obj <- foobar(c("foo", "bar", "baz"))
  vec_slice(obj, 1:2) <- TRUE
  expect_identical(obj, foobar(c("dispatched", "dispatched", "baz")))
})

test_that("vec_assign() can always assign unspecified values into foreign vector types", {
  obj <- foobar(c("foo", "bar", "baz"))
  expect <- foobar(c(NA, "bar", "baz"))

  expect_identical(vec_assign(obj, 1, NA), expect)
  expect_identical(vec_assign(obj, 1, unspecified(1)), expect)
})

test_that("slice-assign casts to `to` before falling back to `[<-` (#443)", {
  called <- FALSE

  local_methods(
    vec_proxy.vctrs_proxy = proxy_deref,
    vec_ptype2.vctrs_proxy = function(...) NULL,
    vec_ptype2.vctrs_proxy.vctrs_foobar = function(...) new_proxy(NA),
    vec_cast.vctrs_foobar = function(...) NULL,
    vec_cast.vctrs_foobar.vctrs_proxy = function(x, ...) foobar(proxy_deref(x)),
    `[<-.vctrs_foobar` = function(x, i, value) {
      called <<- TRUE
      expect_identical(value, foobar(10))
    }
  )

  x <- foobar(1)
  y <- new_proxy(10)
  vec_slice(x, 1) <- y
  expect_true(called)
})

test_that("index and value are sliced before falling back", {
  # Work around a bug in base R `[<-`
  lhs <- foobar(c(NA, 1:4))
  rhs <- foobar(int(0L, 10L))
  exp <- foobar(int(10L, 1:4))
  expect_identical(vec_assign(lhs, c(NA, 1), rhs), exp)
})

test_that("can assign to data frame", {
  x <- data_frame(x = 1:3)
  y <- data_frame(x = 20)
  expect_identical(vec_assign(x, 2, y), data_frame(x = int(1, 20, 3)))
})

test_that("can assign to a data frame with matrix columns (#625)", {
  df <- tibble(x = 1:2, y = matrix(1:4, nrow = 2))
  expect_identical(vec_assign(df, 2L, df[1,]), vec_slice(df, c(1, 1)))
})

test_that("assigning to a factor doesn't produce corrupt levels (#853)", {
  x <- factor(c("a", NA), levels = c("a", "b"))
  value <- factor("b", levels = "b")

  res <- vec_assign(x, 2, value)
  expect_identical(res, factor(c("a", "b")))

  res <- vec_assign(x, 1:2, value)
  expect_identical(res, factor(c("b", "b"), levels = c("a", "b")))
})

test_that("can slice-assign unspecified vectors with default type2 method", {
  local_rational_class()
  x <- rational(1:2, 2:3)
  x[[1]] <- NA
  expect_identical(x, rational(c(NA, 2L), c(NA, 3L)))
})

test_that("`vec_assign()` validates `x_arg`", {
  expect_error(vec_assign(1, 1, 1, x_arg = 1), "must be a string")
  expect_error(vec_assign(1, 1, 1, x_arg = c("x", "y")), "must be a string")
  expect_error(vec_assign(1, 1, 1, x_arg = NA_character_), "must be a string")
})

test_that("`vec_assign()` validates `value_arg`", {
  expect_error(vec_assign(1, 1, 1, value_arg = 1), "must be a string")
  expect_error(vec_assign(1, 1, 1, value_arg = c("x", "y")), "must be a string")
  expect_error(vec_assign(1, 1, 1, value_arg = NA_character_), "must be a string")
})

test_that("`vec_assign()` requires recyclable value", {
  verify_errors({
    expect_error(
      vec_assign(1:3, 1:3, 1:2),
      class = "vctrs_error_recycle_incompatible_size"
    )
  })
})

test_that("logical subscripts must match size of indexed vector", {
  verify_errors({
    expect_error(
      vec_assign(1:2, c(TRUE, FALSE, TRUE), 5),
      class = "vctrs_error_subscript_size"
    )
  })
})

test_that("must assign existing elements", {
  verify_errors({
    expect_error(
      vec_assign(1:3, 5, 10),
      class = "vctrs_error_subscript_oob"
    )
    expect_error(
      vec_assign(1:3, "foo", 10),
      "unnamed vector"
    )
    expect_error(
      vec_slice(letters, -100) <- "foo",
      class = "vctrs_error_subscript_oob"
    )
    expect_error(
      vec_assign(set_names(letters), "foo", "bar"),
      class = "vctrs_error_subscript_oob"
    )
  })
})

test_that("must assign with proper negative locations", {
  verify_errors({
    expect_error(
      vec_assign(1:3, c(-1, 1), 1:2),
      class = "vctrs_error_subscript_type"
    )
    expect_error(
      vec_assign(1:3, c(-1, NA), 1:2),
      class = "vctrs_error_subscript_type"
    )
  })
})

test_that("`vec_assign()` error args can be overridden", {
  verify_errors({
    expect_error(
      vec_assign(1:2, 1L, "x", x_arg = "foo", value_arg = "bar"),
      class = "vctrs_error_incompatible_type"
    )
    expect_error(
      vec_assign(1:2, 1L, 1:2, value_arg = "bar"),
      class = "vctrs_error_recycle_incompatible_size"
    )
  })
})

test_that("names are not assigned by default", {
  vec_x <- set_names(1:3, letters[1:3])
  vec_y <- c(FOO = 4L)
  vec_out <- c(a = 1L, b = 4L, c = 3L)
  expect_identical(
    vec_assign(vec_x, 2, vec_y),
    vec_out
  )

  df_x <- new_data_frame(list(x = 1:3), row.names = letters[1:3])
  df_y <- new_data_frame(list(x = 4L), row.names = "FOO")
  df_out <- new_data_frame(list(x = c(1L, 4L, 3L)), row.names = letters[1:3])
  expect_identical(
    vec_assign(df_x, 2, df_y),
    df_out
  )

  mat_x <- matrix(1:3, 3, dimnames = list(letters[1:3]))
  mat_y <- matrix(4L, 1, dimnames = list("FOO"))
  mat_out <- matrix(c(1L, 4L, 3L), dimnames = list(letters[1:3]))
  expect_identical(
    vec_assign(mat_x, 2, mat_y),
    mat_out
  )

  nested_x <- new_data_frame(list(df = df_x, mat = mat_x, vec = vec_x), row.names = c("foo", "bar", "baz"))
  nested_y <- new_data_frame(list(df = df_y, mat = mat_y, vec = vec_y), row.names = c("quux"))
  nested_out <- new_data_frame(list(df = df_out, mat = mat_out, vec = vec_out), row.names = c("foo", "bar", "baz"))
  expect_identical(
    vec_assign(nested_x, 2, nested_y),
    nested_out
  )
})

test_that("can optionally assign names", {
  vec_x <- set_names(1:3, letters[1:3])
  vec_y <- c(FOO = 4L)
  vec_out <- c(a = 1L, FOO = 4L, c = 3L)
  expect_identical(
    vec_assign_params(vec_x, 2, vec_y, assign_names = TRUE),
    vec_out
  )

  oo_x <- set_names(as_posixlt(c("2020-01-01", "2020-01-02", "2020-01-03")), letters[1:3])
  oo_y <- as_posixlt(c(FOO = "2020-01-04"))
  oo_out <- as_posixlt(c(a = "2020-01-01", FOO = "2020-01-04", c = "2020-01-03"))
  expect_identical(
    vec_assign_params(oo_x, 2, oo_y, assign_names = TRUE),
    oo_out
  )

  df_x <- new_data_frame(list(x = 1:3), row.names = letters[1:3])
  df_y <- new_data_frame(list(x = 4L), row.names = "FOO")
  df_out <- new_data_frame(list(x = c(1L, 4L, 3L)), row.names = c("a", "FOO", "c"))
  expect_identical(
    vec_assign_params(df_x, 2, df_y, assign_names = TRUE),
    df_out
  )

  mat_x <- matrix(1:3, 3, dimnames = list(letters[1:3]))
  mat_y <- matrix(4L, 1, dimnames = list("FOO"))
  mat_out <- matrix(c(1L, 4L, 3L), dimnames = list(c("a", "FOO", "c")))
  expect_identical(
    vec_assign_params(mat_x, 2, mat_y, assign_names = TRUE),
    mat_out
  )

  nested_x <- new_data_frame(list(df = df_x, mat = mat_x, vec = vec_x, oo = oo_x), row.names = c("foo", "bar", "baz"))
  nested_y <- new_data_frame(list(df = df_y, mat = mat_y, vec = vec_y, oo = oo_y), row.names = c("quux"))
  nested_out <- new_data_frame(list(df = df_out, mat = mat_out, vec = vec_out, oo = oo_out), row.names = c("foo", "quux", "baz"))

  expect_identical(
    vec_assign_params(nested_x, 2, nested_y, assign_names = TRUE),
    nested_out
  )
})

test_that("assignment requires that the value proxy is the same type as the output proxy", {
  x <- foobar(1)
  y <- foobar("a")

  local_foobar_proxy()
  local_methods(
    vec_cast.vctrs_foobar.vctrs_foobar = function(x, to, ...) x
  )

  expect_error(
    vec_assign(x, 1, y),
    "`double` incompatible with `value` proxy of type `character`"
  )
})

test_that("assignment allows a df `value`'s column to be a different type than its proxy (#1082)", {
  x <- new_data_frame(list(x = foobar(1)))
  y <- new_data_frame(list(x = foobar(2)))

  local_methods(
    # proxying foobar wraps it in a 1 col df
    vec_proxy.vctrs_foobar = function(x, ...) {
      attributes(x) <- NULL
      new_data_frame(list(vec = x))
    },
    # restoring extracts the column
    vec_restore.vctrs_foobar = function(x, to, ...) {
      foobar(x$vec)
    },
    vec_ptype2.vctrs_foobar.vctrs_foobar = function(x, y, ...) x
  )

  expect1 <- new_data_frame(list(x = foobar(c(1, 1))))
  expect2 <- new_data_frame(list(x = foobar(2)))

  expect_identical(vec_rbind(x, x), expect1)
  expect_identical(vec_assign(x, 1, y), expect2)
})

test_that("monitoring: assignment to a data frame with unshared columns doesn't overwrite (#986)", {
  x <- new_df_unshared_col()
  value <- new_data_frame(list(x = 2))
  expect <- new_data_frame(list(x = 1L))

  # - On R < 4.0.0, the NAMED value of the column is 0.
  # - On R >= 4.0.0, the refcnt of the column is 1 from the call to
  #   `SET_VECTOR_ELT()` in `new_df_unshared_col()`.
  expect_false(maybe_shared_col(x, 1L))

  new <- vec_assign(x, 1, value)

  # On R < 4.0.0, `vec_assign()` shallow duplicates `x`, which recursively
  # bumps the NAMED-ness of each column of `x` to the max value of 7 by
  # calling `ENSURE_NAMEDMAX()` on it. So the columns of `x` are all considered
  # shared from that.

  # On R >= 4.0.0, references are tracked more precisely.
  # - `new_df_unshared_col()` calls `SET_VECTOR_ELT()` when setting the
  #   column into `x`, bumping the column's namedness to 1.
  # - Then, at the start of `df_assign()`, `x` is shallow duplicated and
  #   assigned to `out`. This calls `ENSURE_NAMEDMAX()` on each column,
  #   however this does nothing on R 4.0.0. The refcnt of each column is instead
  #   incremented by 1 by calls to `SET_VECTOR_ELT()` in `duplicate1()`.
  #   So now it is at 2.
  # - But then in `df_assign()` we use `SET_VECTOR_ELT()` on `out`, overwriting
  #   each column. This actually decrements the refcnt on the value that was
  #   in `out` before the column was overwritten. The column of `out` that it
  #   decrements the refcnt for is the same SEXP as that column in `x`, so now
  #   it is back to 1, and it is not considered shared.

  if (getRversion() >= "4.0.0") {
    expect_false(maybe_shared_col(x, 1L))
  } else {
    expect_true(maybe_shared_col(x, 1L))
  }

  # Expect no changes to `x`!
  expect_identical(x, expect)
})

test_that("monitoring: assignment to atomic vectors doesn't modify by reference", {
  x <- c(1, 2, 3)
  expect <- c(1, 2, 3)

  vec_assign(x, 2, 3)

  expect_identical(x, expect)
})

# vec_assign + compact_seq -------------------------------------------------

# `start` is 0-based

test_that("can assign shaped base vectors with compact seqs", {
  start <- 1L
  size <- 2L
  increasing <- TRUE
  mat <- as.matrix
  expect_identical(vec_assign_seq(mat(lgl(1, 0, 1)), NA, start, size, increasing), mat(lgl(1, NA, NA)))
  expect_identical(vec_assign_seq(mat(int(1, 2, 3)), NA, start, size, increasing), mat(int(1, NA, NA)))
  expect_identical(vec_assign_seq(mat(dbl(1, 2, 3)), NA, start, size, increasing), mat(dbl(1, NA, NA)))
  expect_identical(vec_assign_seq(mat(cpl(1, 2, 3)), NA, start, size, increasing), mat(cpl(1, NA, NA)))
  expect_identical(vec_assign_seq(mat(chr("1", "2", "3")), NA, start, size, increasing), mat(chr("1", NA, NA)))
  expect_identical(vec_assign_seq(mat(bytes(1, 2, 3)), bytes(1), start, size, increasing), mat(bytes(1, 1, 1)))
  expect_identical(vec_assign_seq(mat(list(1, 2, 3)), NA, start, size, increasing), mat(list(1, NULL, NULL)))
})

test_that("can assign shaped base vectors with decreasing compact seqs", {
  start <- 2L
  size <- 2L
  increasing <- FALSE
  mat <- as.matrix
  expect_identical(vec_assign_seq(mat(lgl(1, 0, 1)), NA, start, size, increasing), mat(lgl(1, NA, NA)))
  expect_identical(vec_assign_seq(mat(int(1, 2, 3)), NA, start, size, increasing), mat(int(1, NA, NA)))
  expect_identical(vec_assign_seq(mat(dbl(1, 2, 3)), NA, start, size, increasing), mat(dbl(1, NA, NA)))
  expect_identical(vec_assign_seq(mat(cpl(1, 2, 3)), NA, start, size, increasing), mat(cpl(1, NA, NA)))
  expect_identical(vec_assign_seq(mat(chr("1", "2", "3")), NA, start, size, increasing), mat(chr("1", NA, NA)))
  expect_identical(vec_assign_seq(mat(bytes(1, 2, 3)), bytes(1), start, size, increasing), mat(bytes(1, 1, 1)))
  expect_identical(vec_assign_seq(mat(list(1, 2, 3)), NA, start, size, increasing), mat(list(1, NULL, NULL)))
})

test_that("can assign shaped base vectors with size 0 compact seqs", {
  start <- 1L
  size <- 0L
  increasing <- TRUE
  mat <- as.matrix

  expect_identical(vec_assign_seq(mat(lgl(1, 0, 1)), NA, start, size, increasing), mat(mat(lgl(1, 0, 1))))
  expect_identical(vec_assign_seq(mat(int(1, 2, 3)), NA, start, size, increasing), mat(int(1, 2, 3)))
  expect_identical(vec_assign_seq(mat(dbl(1, 2, 3)), NA, start, size, increasing), mat(dbl(1, 2, 3)))
  expect_identical(vec_assign_seq(mat(cpl(1, 2, 3)), NA, start, size, increasing), mat(cpl(1, 2, 3)))
  expect_identical(vec_assign_seq(mat(chr("1", "2", "3")), NA, start, size, increasing), mat(chr("1", "2", "3")))
  expect_identical(vec_assign_seq(mat(bytes(1, 2, 3)), bytes(1), start, size, increasing), mat(bytes(1, 2, 3)))
  expect_identical(vec_assign_seq(mat(list(1, 2, 3)), NA, start, size, increasing), mat(list(1, 2, 3)))
})

test_that("can assign object of any dimensionality with compact seqs", {
  x1 <- ones(3)
  x2 <- ones(3, 4)
  x3 <- ones(3, 4, 5)
  x4 <- ones(3, 4, 5, 6)

  start <- 0L
  size <- 2L
  increasing <- TRUE
  mat <- as.matrix

  expect_identical(vec_assign_seq(x1, 2, start, size, increasing), array(rep(c(2, 2, 1), 1), dim = 3))
  expect_identical(vec_assign_seq(x2, 2, start, size, increasing), array(rep(c(2, 2, 1), 4), dim = c(3, 4)))
  expect_identical(vec_assign_seq(x3, 2, start, size, increasing), array(rep(c(2, 2, 1), 20), dim = c(3, 4, 5)))
  expect_identical(vec_assign_seq(x4, 2, start, size, increasing), array(rep(c(2, 2, 1), 120), dim = c(3, 4, 5, 6)))
})

test_that("vec_assign2() handles atomic vectors", {
  x <- c(a = 1L, b = 2L, c = 3L)
  exp <- c(a = 1L, b = 0L, c = 3L)

  expect_identical(vec_assign2(x, 2, FALSE), exp)

  local_hidden()
  expect_identical(vec_assign2(new_hidden(x), 2, FALSE), new_hidden(exp))

  rcrd <- new_rcrd(list(x = 1:3))
  rcrd_exp <- new_rcrd(list(x = c(1L, 0L, 3L)))
  expect_identical(vec_assign2(rcrd, 2, new_rcrd(list(x = FALSE))), rcrd_exp)
})

test_that("vec_assign2() handles lists", {
  x <- list(a = 1L, b = 2L, c = 3:4)
  exp1 <- list(a = 1L, b = FALSE, c = 3:4)
  exp2 <- list(a = 1L, b = NULL, c = 3:4)
  exp3 <- list(a = 1L, b = list(NULL), c = 3:4)

  expect_identical(vec_assign2(x, 2, FALSE), exp1)
  expect_identical(vec_assign2(x, 2, NULL), exp2)
  expect_identical(vec_assign2(x, 2, list(NULL)), exp3)

  local_list_rcrd_methods()
  expect_identical(vec_assign2(new_list_rcrd(x), 2, FALSE), new_list_rcrd(exp1))
  expect_identical(vec_assign2(new_list_rcrd(x), 2, NULL), new_list_rcrd(exp2))
  expect_identical(vec_assign2(new_list_rcrd(x), 2, list(NULL)), new_list_rcrd(exp3))
})

test_that("zap() is currently disallowed", {
  expect_error(vec_assign2(list(1), 1, zap()), "Can't zap")
})


# Golden tests ------------------------------------------------------------

test_that("slice and assign have informative errors", {
  verify_output(test_path("error", "test-slice-assign.txt"), {
    "# `vec_assign()` requires recyclable value"
    vec_assign(1:3, 1:3, 1:2)

    "# logical subscripts must match size of indexed vector"
    vec_assign(1:2, c(TRUE, FALSE, TRUE), 5)
    vec_assign(mtcars, c(TRUE, FALSE), mtcars[1, ])

    "# must assign existing elements"
    vec_assign(1:3, 5, 10)
    vec_assign(1:3, "foo", 10)
    vec_slice(letters, -100) <- "foo"
    vec_assign(set_names(letters), "foo", "bar")

    "# must assign with proper negative locations"
    vec_assign(1:3, c(-1, 1), 1:2)
    vec_assign(1:3, c(-1, NA), 1:2)

    "# `vec_assign()` error args can be overridden"
    vec_assign(1:2, 1L, "x", x_arg = "foo", value_arg = "bar")
    vec_assign(1:2, 1L, 1:2, value_arg = "bar")
  })
})
