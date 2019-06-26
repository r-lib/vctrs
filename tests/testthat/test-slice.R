context("test-slice")

test_that("vec_slice throws error with non-vector inputs", {
  expect_error(vec_slice(environment(), 1L), class = "vctrs_error_scalar_type")
})

test_that("can subset base vectors", {
  i <- 2:3
  expect_identical(vec_slice(lgl(1, 0, 1), i), lgl(0, 1))
  expect_identical(vec_slice(int(1, 2, 3), i), int(2, 3))
  expect_identical(vec_slice(dbl(1, 2, 3), i), dbl(2, 3))
  expect_identical(vec_slice(cpl(1, 2, 3), i), cpl(2, 3))
  expect_identical(vec_slice(chr("1", "2", "3"), i), chr("2", "3"))
  expect_identical(vec_slice(bytes(1, 2, 3), i), bytes(2, 3))
  expect_identical(vec_slice(list(1, 2, 3), i), list(2, 3))
})

test_that("can subset shaped base vectors", {
  i <- 2:3
  mat <- as.matrix
  expect_identical(vec_slice(mat(lgl(1, 0, 1)), i), mat(lgl(0, 1)))
  expect_identical(vec_slice(mat(int(1, 2, 3)), i), mat(int(2, 3)))
  expect_identical(vec_slice(mat(dbl(1, 2, 3)), i), mat(dbl(2, 3)))
  expect_identical(vec_slice(mat(cpl(1, 2, 3)), i), mat(cpl(2, 3)))
  expect_identical(vec_slice(mat(chr("1", "2", "3")), i), mat(chr("2", "3")))
  expect_identical(vec_slice(mat(bytes(1, 2, 3)), i), mat(bytes(2, 3)))
  expect_identical(vec_slice(mat(list(1, 2, 3)), i), mat(list(2, 3)))
})

test_that("can subset with missing indices", {
  for (i in list(int(2L, NA), lgl(FALSE, TRUE, NA))) {
    expect_identical(vec_slice(lgl(1, 0, 1), i), lgl(0, NA))
    expect_identical(vec_slice(int(1, 2, 3), i), int(2, NA))
    expect_identical(vec_slice(dbl(1, 2, 3), i), dbl(2, NA))
    expect_identical(vec_slice(cpl(1, 2, 3), i), cpl(2, NA))
    expect_identical(vec_slice(chr("1", "2", "3"), i), c("2", NA))
    expect_identical(vec_slice(bytes(1, 2, 3), i), bytes(2, 0))
    expect_identical(vec_slice(list(1, 2, 3), i), list(2, NULL))
  }
})

test_that("can subset with a recycled NA", {
  expect_identical(vec_slice(1:3, NA), int(NA, NA, NA))
  expect_identical(vec_slice(new_vctr(1:3), NA), new_vctr(int(NA, NA, NA)))

  rownames <- rep_len("", nrow(mtcars))
  rownames <- vec_as_names(rownames, repair = "unique")
  expect_identical(vec_slice(mtcars, NA), structure(mtcars[NA, ], row.names = rownames))
})

test_that("can subset with a recycled TRUE", {
  expect_identical(vec_slice(1:3, TRUE), 1:3)
  expect_identical(vec_slice(mtcars, TRUE), mtcars)
  expect_identical(vec_slice(new_vctr(1:3), TRUE), new_vctr(1:3))
})

test_that("can subset with a recycled FALSE", {
  expect_identical(vec_slice(1:3, FALSE), int())
  expect_identical(vec_slice(mtcars, FALSE), mtcars[NULL, ])
  expect_identical(vec_slice(new_vctr(1:3), FALSE), new_vctr(integer()))
})

test_that("can't index beyond the end of a vector", {
  expect_error(vec_slice(1:2, 3L), glue::glue(
    "Can't index beyond the end of a vector.\n",
    "The vector has length 2 and you've tried to subset element 3."
  ))
  expect_error(vec_slice(1:2, -3L), "Can't index beyond the end of a vector.")
})

test_that("slicing non existing elements fails", {
  expect_error(vec_as_index("foo", 1L, "f"), "non-existing")
  expect_error(vec_slice(c(f = 1), "foo"), "non-existing")
})

test_that("can subset object of any dimensionality", {
  x0 <- c(1, 1)
  x1 <- ones(2)
  x2 <- ones(2, 3)
  x3 <- ones(2, 3, 4)
  x4 <- ones(2, 3, 4, 5)

  expect_equal(vec_slice(x0, 1L), 1)
  expect_identical(vec_slice(x1, 1L), ones(1))
  expect_identical(vec_slice(x2, 1L), ones(1, 3))
  expect_identical(vec_slice(x3, 1L), ones(1, 3, 4))
  expect_identical(vec_slice(x4, 1L), ones(1, 3, 4, 5))
})

test_that("can subset using logical index", {
  x0 <- c(1, 1)

  expect_identical(vec_slice(x0, TRUE), x0)
  expect_identical(vec_slice(x0, c(TRUE, FALSE)), 1)

  expect_error(
    vec_slice(x0, c(TRUE, FALSE, TRUE)),
    "has size 2 whereas the index has size 3",
    fixed = TRUE
  )

  expect_error(
    vec_slice(x0, lgl()),
    "has size 2 whereas the index has size 0",
    fixed = TRUE
  )

  expect_error(
    vec_slice(mtcars, c(TRUE, FALSE)),
    "has size 32 whereas the index has size 2"
  )
})

test_that("can subset data frame columns", {
  df <- data.frame(x = 1:2)
  df$y <- data.frame(a = 2:1)

  expect_equal(vec_slice(df, 1L)$y, vec_slice(df$y, 1L))
})

test_that("can subset empty data frames", {
  df <- new_data_frame(n = 3L)
  expect_equal(vec_size(vec_slice(df, integer())), 0)
  expect_equal(vec_size(vec_slice(df, 1L)), 1)
  expect_equal(vec_size(vec_slice(df, 1:3)), 3)

  df$df <- df
  expect_equal(vec_size(vec_slice(df, integer())), 0)
  expect_equal(vec_size(vec_slice(df, 1L)), 1)
  expect_equal(vec_size(vec_slice(df, 1:3)), 3)
})

test_that("ignores NA in logical subsetting", {
  x <- c(NA, 1, 2)
  expect_equal(vec_slice(x, x > 0), c(NA, 1, 2))
})

test_that("ignores NA in integer subsetting", {
  expect_equal(vec_slice(0:2, c(NA, 2:3)), c(NA, 1, 2))
})

test_that("can't slice with missing argument", {
  expect_error(vec_slice(1:3))
  expect_error(vec_slice(mtcars))
  expect_error(vec_slice(new_vctr(1:3)))
})

test_that("can slice with NULL argument", {
  expect_identical(vec_slice(1:3, NULL), integer())
  expect_identical(vec_slice(iris, NULL), iris[0, ])
  expect_identical(vec_slice(new_vctr(1:3), NULL), new_vctr(integer()))
})

test_that("slicing unclassed structures preserves attributes", {
  x <- structure(1:3, foo = "bar")
  expect_identical(vec_slice(x, 1L), structure(1L, foo = "bar"))
})

test_that("can slice with negative indices", {
  expect_identical(vec_slice(1:3, -c(1L, 3L)), 2L)
  expect_identical(vec_slice(mtcars, -(1:30)), vec_slice(mtcars, 31:32))

  expect_error(vec_slice(1:3, -c(1L, NA)), "mix of negative indices and missing values")
  expect_error(vec_slice(1:3, c(-1L, 1L)), "mix of negative and positive indices")
})

test_that("0 is ignored in negative indices", {
  expect_identical(vec_slice(1:3, c(-2L, 0L)), int(1L, 3L))
  expect_identical(vec_slice(1:3, c(0L, -2L)), int(1L, 3L))
})

test_that("0 is ignored in positive indices", {
  expect_identical(vec_slice(1:3, 0L), int())
  expect_identical(vec_slice(1:3, c(0L, 0L)), int())
  expect_identical(vec_slice(1:3, c(0L, 2L, 0L)), 2L)
})

test_that("can slice with double indices", {
  expect_identical(vec_slice(1:3, dbl(2, 3)), 2:3)
  expect_lossy(vec_as_index(2^31, 3L), na_int, x = dbl(), to = int())
})

test_that("vec_as_index() checks type", {
  expect_error(vec_as_index(quote(foo), 1L), "must be an integer, character, or logical vector, not a symbol")
  expect_error(vec_as_index("foo", "bar"), class = "vctrs_error_assert_ptype")
  expect_error(vec_as_index("foo", 1L, names = 1L), "must be a character vector")
})

test_that("can `vec_slice()` S3 objects without dispatch infloop", {
  expect_identical(new_vctr(1:3)[1], new_vctr(1L))
  expect_identical(new_vctr(as.list(1:3))[1], new_vctr(list(1L)))
})

test_that("can `vec_slice()` records", {
  out <- vec_slice(new_rcrd(list(a = 1L, b = 2L)), rep(1, 3))
  expect_size(out, 3)

  out <- vec_init(new_rcrd(list(a = 1L, b = 2L)), 2)
  expect_size(out, 2)
})

test_that("vec_restore() is called after proxied slicing", {
  scoped_global_bindings(
    vec_proxy.vctrs_foobar = identity,
    vec_restore.vctrs_foobar = function(x, to, ...) "dispatch"
  )
  expect_identical(vec_slice(foobar(1:3), 2), "dispatch")
})

test_that("vec_slice() is proxied", {
  scoped_proxy()
  x <- vec_slice(new_proxy(1:3), 2:3)
  expect_identical(proxy_deref(x), 2:3)
})

test_that("dimensions are preserved by vec_slice()", {
  attrib <- NULL

  scoped_global_bindings(
    vec_restore.vctrs_foobar = function(x, ...) attrib <<- attributes(x)
  )

  x <- foobar(1:4)
  dim(x) <- c(2, 2)
  dimnames(x) <- list(a = c("foo", "bar"), b = c("quux", "hunoz"))

  vec_slice(x, 1)

  exp <- list(dim = 1:2, dimnames = list(a = "foo", b = c("quux", "hunoz")))
  expect_identical(attrib, exp)
})

test_that("can slice shaped objects by name", {
  x <- matrix(1:2)

  expect_error(vec_slice(x, "foo"), "unnamed")

  dimnames(x) <- list(c("foo", "bar"))

  expect_equal(vec_slice(x, "foo"), vec_slice(x, 1L))
  expect_error(vec_slice(x, "baz"), "non-existing")
})

test_that("vec_slice() unclasses input before calling `vec_restore()`", {
  class <- NULL
  scoped_global_bindings(
    vec_proxy.vctrs_foobar = identity,
    vec_restore.vctrs_foobar = function(x, ...) class <<- class(x)
  )

  x <- foobar(1:4)
  dim(x) <- c(2, 2)

  vec_slice(x, 1)
  expect_identical(class, "matrix")
})

test_that("can call `vec_slice()` from `[` methods with shaped objects without infloop", {
  scoped_global_bindings(
    `[.vctrs_foobar` = function(x, i, ...) vec_slice(x, i)
  )

  x <- foobar(1:4)
  dim(x) <- c(2, 2)

  exp <- foobar(c(1L, 3L))
  dim(exp) <- c(1, 2)
  expect_identical(x[1], exp)
})

test_that("vec_slice() falls back to `[` with S3 objects", {
  scoped_global_bindings(
    `[.vctrs_foobar` = function(x, i, ...) "dispatched"
  )
  expect_identical(vec_slice(foobar(NA), 1), foobar("dispatched"))

  expect_error(vec_slice(foobar(list(NA)), 1), class = "vctrs_error_scalar_type")
  scoped_global_bindings(
    vec_proxy.vctrs_foobar = identity
  )
  expect_identical(vec_slice(foobar(list(NA)), 1), foobar(list(NA)))
})

test_that("vec_slice() doesn't restore when attributes have already been restored", {
  scoped_global_bindings(
    `[.vctrs_foobar` = function(x, i, ...) structure("dispatched", foo = "bar"),
    vec_restore.vctrs_foobar = function(...) stop("not called")
  )
  expect_error(vec_slice(foobar(NA), 1), NA)
})

test_that("can vec_slice() without inflooping when restore calls math generics", {
  scoped_global_bindings(
    new_foobar = function(x) {
      new_vctr(as.double(x), class = "vctrs_foobar")
    },
    vec_restore.vctrs_foobar = function(x, ...) {
      abs(x)
      sum(x)
      mean(x)
      is.finite(x)
      is.infinite(x)
      is.nan(x)
      new_foobar(x)
    }
  )
  expect_identical(new_foobar(1:10)[1:2], new_foobar(1:2))
})

test_that("vec_restore() is called after slicing data frames", {
  scoped_global_bindings(
    vec_restore.vctrs_tabble = function(...) "dispatched"
  )
  df <- structure(mtcars, class = c("vctrs_tabble", "data.frame"))
  expect_identical(vec_slice(df, 1), "dispatched")
})

test_that("additional subscripts are forwarded to `[`", {
  scoped_global_bindings(
    `[.vctrs_foobar` = function(x, i, ...) vec_index(x, i, ...)
  )

  x <- foobar(c("foo", "bar", "quux", "hunoz"))
  dim(x) <- c(2, 2)

  exp <- foobar("quux")
  dim(exp) <- c(1, 1)

  expect_identical(x[1, 2], exp)
})

test_that("can use names to vec_slice() a named object", {
  x0 <- c(a = 1, b = 2)
  x1 <- c(a = 1, a = 2)

  expect_identical(vec_slice(x0, letters[1]), c(a = 1))
  expect_identical(vec_slice(x0, letters[2:1]), c(b = 2, a = 1))
  expect_identical(vec_slice(x1, letters[1]), c(a = 1))

  expect_error(vec_slice(x0, letters[3:1]), "non-existing")
  expect_error(vec_slice(x1, letters[2]), "non-existing")
})

test_that("can't use names to vec_slice() an unnamed object", {
  x0 <- 1:3

  expect_error(
    vec_slice(x0, letters[1]),
    "Can't use character to index an unnamed vector.",
    fixed = TRUE
  )
  expect_error(
    vec_slice(x0, letters[25:27]),
    "Can't use character to index an unnamed vector.",
    fixed = TRUE
  )
})

test_that("can slice with missing character indices (#244)", {
  expect_identical(vec_as_index(na_chr, 2L, c("x", "")), na_int)
  expect_identical(vec_slice(c(x = 1), na_chr), set_names(na_dbl, ""))
  expect_identical(vec_slice(c(x = "foo"), na_chr), set_names(na_chr, ""))
})

test_that("missing indices don't create NA names", {
  x <- set_names(letters)
  expect_identical(vec_slice(x, na_int), set_names(na_chr, ""))
  expect_identical(vec_slice(x, int(1, NA, 3, NA)), chr(a = "a", NA, c = "c", NA))

  # Preserves existing NA names
  x <- set_names(1:2, c(NA, "foo"))
  expect_identical(vec_slice(x, 1:2), x)
})

test_that("vec_slice throws error with non-vector inputs", {
  expect_error(vec_slice(environment(), 1L), class = "vctrs_error_scalar_type")
})

test_that("vec_slice() asserts vectorness (#301)", {
  expect_error(vec_slice(NULL, 1), class = "vctrs_error_scalar_type")
})


# vec_init ----------------------------------------------------------------

test_that("na of atomic vectors is as expected", {
  expect_equal(vec_init(TRUE), NA)
  expect_equal(vec_init(1L), NA_integer_)
  expect_equal(vec_init(1), NA_real_)
  expect_equal(vec_init("x"), NA_character_)
  expect_equal(vec_init(1i), NA_complex_)
})

test_that("na of factor preserves levels", {
  f1 <- factor("a", levels = c("a", "b"))
  f2 <- vec_init(f1)

  expect_equal(levels(f1), levels(f2))
})

test_that("na of POSIXct preserves tz", {
  dt1 <- as.POSIXct("2010-01-01", tz = "America/New_York")
  dt2 <- vec_init(dt1)
  expect_equal(attr(dt2, "tzone"), "America/New_York")
})

test_that("na of list is list(NULL)", {
  expect_equal(vec_init(list()), list(NULL))
})

test_that("na of array is 1d slice", {
  x1 <- array(1:12, c(2, 3, 4))
  x2 <- vec_init(x1)

  expect_equal(x2, array(NA_integer_, c(1, 3, 4)))
})

test_that("na of list-array is 1d slice", {
  x1 <- array(as.list(1:12), c(2, 3, 4))
  x2 <- vec_init(x1)

  expect_equal(x2, array(list(), c(1, 3, 4)))
})

test_that("vec_init() asserts vectorness (#301)", {
  expect_error(vec_init(NULL), class = "vctrs_error_scalar_type")
})
