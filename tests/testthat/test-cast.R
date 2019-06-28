context("test-cast")

# vec_cast ---------------------------------------------------------------

test_that("new classes are uncoercible by default", {
  x <- structure(1:10, class = "vctrs_nonexistant")
  expect_error(vec_cast(1, x), class = "vctrs_error_incompatible_cast")
  expect_error(vec_cast(x, 1), class = "vctrs_error_incompatible_cast")
})

test_that("dimensionality matches output" ,{
  x1 <- matrix(TRUE, nrow = 1, ncol = 1)
  x2 <- matrix(1, nrow = 0, ncol = 2)
  expect_dim(vec_cast(x1, x2), c(1, 2))
  expect_dim(vec_cast(TRUE, x2), c(1, 2))

  x <- matrix(1, nrow = 2, ncol = 2)
  expect_error(vec_cast(x, logical()), class = "vctrs_error_incompatible_cast")
})

test_that("empty input to vec_cast_common() returns list()", {
  expect_equal(vec_cast_common(), list())
  expect_equal(vec_cast_common(NULL, NULL), list(NULL, NULL))
})

test_that("identical structures can be cast to each other", {
  expect_identical(vec_cast(foobar("foo"), foobar("bar")), foobar("foo"))
  expect_identical(vec_coercible_cast(foobar("foo"), foobar("bar")), foobar("foo"))
})

test_that("inputs to vec_coercible_cast() are checked", {
  expect_error(vec_coercible_cast("", "", x_arg = 1), "must be a string")
  expect_error(vec_coercible_cast("", "", to_arg = chr()), "must be a string")
})

test_that("cast common preserves names", {
  expect_identical(vec_cast_common(foo = 1, bar = 2L), list(foo = 1, bar = 2))
})

test_that("cast errors create helpful messages (#57, #225)", {
  expect_known_output(file = test_path("test-cast-error-nested.txt"), {
    # Lossy cast
    try2(vec_cast("foo", 10))

    # Incompatible cast
    try2(vec_cast(factor("foo"), 10))


    ## Nested data frames

    # Lossy cast
    x <- tibble(a = tibble(b = "foo"))
    y <- tibble(a = tibble(b = 10))
    try2(vec_cast(x, y))

    # Incompatible cast
    x <- tibble(a = tibble(b = factor("foo")))
    try2(vec_cast(x, y))

    # Common cast error
    try2(vec_cast_common(x, y))
  })
})


# vec_restore -------------------------------------------------------------

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
  expect_is(x, "foo")
})

test_that("data frame vec_restore() checks type", {
  expect_error(vec_restore(NA, mtcars), "Attempt to restore data frame from a logical")
})

test_that("can use vctrs primitives from vec_restore() without inflooping", {
  scoped_global_bindings(
    vec_restore.vctrs_foobar = function(x, to, ...) {
      vec_ptype(x)
      vec_init(x)
      vec_assert(x)
      vec_slice(x, 0)
      "woot"
    }
  )

  foobar <- new_vctr(1:3, class = "vctrs_foobar")
  expect_identical(vec_slice(foobar, 2), "woot")
})

test_that("vec_restore() passes `n` argument to methods", {
  scoped_global_bindings(
    vec_restore.vctrs_foobar = function(x, to, ..., n) n
  )
  expect_identical(vec_slice(foobar(1:3), 2), 1L)
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
  scoped_global_bindings(
    vec_restore.vctrs_foobar = function(x, to, ..., n) sys.call(),
    vec_proxy.vctrs_foobar = unclass
  )
  call <- vec_restore(foobar(list(1)), foobar(list(1)))
  expect_equal(call, quote(vec_restore.vctrs_foobar(x = x, to = to, n = n)))
})


# Conditions --------------------------------------------------------------

test_that("can suppress cast errors selectively", {
  f <- function() vec_cast(factor("a"), to = factor("b"))
  expect_error(regexp = NA, allow_lossy_cast(f()))
  expect_error(regexp = NA, allow_lossy_cast(f(), x_ptype = factor("a")))
  expect_error(regexp = NA, allow_lossy_cast(f(), to_ptype = factor("b")))
  expect_error(regexp = NA, allow_lossy_cast(f(), x_ptype = factor("a"), to_ptype = factor("b")))
  expect_error(allow_lossy_cast(f(), x_ptype = factor("c")), class = "vctrs_error_cast_lossy")
  expect_error(allow_lossy_cast(f(), x_ptype = factor("b"), to_ptype = factor("a")), class = "vctrs_error_cast_lossy")
  expect_error(allow_lossy_cast(f(), x_ptype = factor("a"), to_ptype = factor("c")), class = "vctrs_error_cast_lossy")
})

test_that("can signal deprecation warnings for lossy casts", {
  scoped_lifecycle_warnings()

  lossy_cast <- function() {
    maybe_lossy_cast(TRUE, factor("foo"), factor("bar"), lossy = TRUE, .deprecation = TRUE)
  }

  expect_true(expect_warning(lossy_cast(), "detected a lossy transformation"))
  expect_true(expect_warning(regexp = NA, allow_lossy_cast(lossy_cast())))
  expect_true(expect_warning(regexp = NA, allow_lossy_cast(lossy_cast(), factor("foo"), factor("bar"))))
  expect_true(expect_warning(allow_lossy_cast(lossy_cast(), factor("bar"), double())))
})
