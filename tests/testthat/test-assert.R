
test_that("basic assert is idempotent", {
  x <- new_vctr(1:4)

  expect_true(vec_is(x))
  expect_identical(vec_assert(x), x)

  expect_identical(vec_assert(x), new_vctr(1:4))
  expect_false(withVisible(vec_assert(x))$visible)

  expect_true(vec_is(1:4))
  expect_identical(vec_assert(1:4), 1:4)
})

test_that("asserting ptype", {
  x <- new_vctr(1:4)
  good <- new_vctr(integer())

  expect_true(vec_is(x, good))
  expect_error(vec_assert(x, good), NA)

  # Is this the correct error message?
  bad <- new_vctr(double())
  expect_false(vec_is(x, bad))
  expect_error(vec_assert(x, bad), class = "vctrs_error_assert_ptype")
})

test_that("asserting size", {
  x <- new_vctr(1:4)

  expect_true(vec_is(x, size = 4))
  expect_error(vec_assert(x, size = 4), NA)

  expect_false(vec_is(x, size = 5))
  expect_error(vec_assert(x, size = 5), class = "vctrs_error_assert_size")
})

test_that("vec_assert() labels input", {
  expect_error(
    vec_assert(new_vctr(1:4), size = 5),
    regexp = "`new_vctr\\(1:4\\)` must have",
    class = "vctrs_error_assert_size"
  )
  expect_error(
    vec_assert(new_vctr(1:4), size = 5, arg = "foobar"),
    regexp = "`foobar` must have",
    class = "vctrs_error_assert_size"
  )
})

test_that("bare atomic vectors are vectors but not recursive", {
  expect_true(vec_is_vector(TRUE))
  expect_true(vec_is_vector(1L))
  expect_true(vec_is_vector(1))
  expect_true(vec_is_vector(1i))
  expect_true(vec_is_vector("foo"))
  expect_true(vec_is_vector(as.raw(1)))
})

test_that("S3 atomic vectors are vectors", {
  expect_true(vec_is_vector(foobar(TRUE)))
  expect_true(vec_is_vector(foobar(1L)))
  expect_true(vec_is_vector(foobar(1)))
  expect_true(vec_is_vector(foobar(1i)))
  expect_true(vec_is_vector(foobar("foo")))
  expect_true(vec_is_vector(foobar(as.raw(1))))
})

test_that("bare lists are vectors", {
  expect_true(vec_is_vector(list()))
})

test_that("S3 lists are not vectors by default", {
  expect_false(vec_is_vector(foobar()))
  local_foobar_proxy()
  expect_true(vec_is_vector(foobar()))
})

test_that("data frames and records are vectors", {
  expect_true(vec_is_vector(mtcars))
  expect_true(vec_is_vector(new_rcrd(list(x = 1, y = 2))))
})

test_that("non-vector base types are scalars", {
  expect_identical(vec_typeof(quote(foo)), "scalar")
  expect_identical(vec_typeof(pairlist("")), "scalar")
  expect_identical(vec_typeof(function() NULL), "scalar")
  expect_identical(vec_typeof(env()), "scalar")
  expect_identical(vec_typeof(quote(foo)), "scalar")
  expect_identical(vec_typeof(~foo), "scalar")
  expect_identical(vec_typeof(base::`{`), "scalar")
  expect_identical(vec_typeof(base::c), "scalar")
  expect_identical(vec_typeof(expression()), "scalar")

  expect_false(vec_is_vector(quote(foo)))
  expect_false(vec_is_vector(pairlist("")))
  expect_false(vec_is_vector(function() NULL))
  expect_false(vec_is_vector(env()))
  expect_false(vec_is_vector(~foo))
  expect_false(vec_is_vector(base::`{`))
  expect_false(vec_is_vector(base::c))
  expect_false(vec_is_vector(expression()))

  expect_false(vec_is(quote(foo)))
  expect_false(vec_is(pairlist("")))
  expect_false(vec_is(function() NULL))
  expect_false(vec_is(env()))
  expect_false(vec_is(~foo))
  expect_false(vec_is(base::`{`))
  expect_false(vec_is(base::c))
  expect_false(vec_is(expression()))

  expect_error(vec_assert(quote(foo)), class = "vctrs_error_scalar_type")
  expect_error(vec_assert(pairlist("")), class = "vctrs_error_scalar_type")
  expect_error(vec_assert(function() NULL), class = "vctrs_error_scalar_type")
  expect_error(vec_assert(env()), class = "vctrs_error_scalar_type")
  expect_error(vec_assert(~foo), class = "vctrs_error_scalar_type")
  expect_error(vec_assert(base::`{`), class = "vctrs_error_scalar_type")
  expect_error(vec_assert(base::c), class = "vctrs_error_scalar_type")
  expect_error(vec_assert(expression()), class = "vctrs_error_scalar_type")
})

test_that("non-vector types can be proxied", {
  x <- new_proxy(1:3)

  expect_identical(vec_typeof(x), "scalar")
  expect_false(vec_is_vector(x))
  expect_false(vec_is(x))
  expect_error(vec_assert(x), class = "vctrs_error_scalar_type")

  local_env_proxy()

  expect_identical(vec_typeof(x), "integer")
  expect_true(vec_is_vector(x))
  expect_true(vec_is(x))
  expect_error(regexp = NA, vec_assert(x))
})

test_that("vec_assert() uses friendly type in error messages", {
   # Friendly type will be generated in rlang in the future. Upstream
   # changes should not cause CRAN failures.
  skip_on_cran()
  expect_error(vec_assert(function() NULL), class = "vctrs_error_scalar_type")
})

test_that("vec_typeof() handles all types", {
  for (i in seq_along(empty_types)) {
    expect_identical(vec_typeof(!!empty_types[[i]]), !!names(empty_types)[[i]])
  }
})

test_that("bare prototypes don't act as partial types", {
  expect_false(vec_is(foobar(1), dbl()))
  expect_error(vec_assert(foobar(1), dbl()), class = "vctrs_error_assert_ptype")
})

test_that("data frames are always classified as such even when dispatch is off", {
  expect_identical(vec_typeof_bare(mtcars), "dataframe")
})

test_that("assertion is not applied on proxy", {
  local_methods(
    vec_proxy.vctrs_foobar = function(x, ...) unclass(x),
    vec_restore.vctrs_foobar = function(x, ...) foobar(x),
    `[.vctrs_foobar` = function(x, i) vec_slice(x, i)
  )
  x <- foobar(list())

  expect_true(vec_is(x, x))
  expect_false(vec_is(x, list()))

  expect_error(vec_assert(x, list()), class = "vctrs_error_assert_ptype")
  expect_error(vec_assert(x, x), regexp = NA)
})

test_that("attributes of unclassed vectors are asserted", {
  x <- structure(FALSE, foo = "bar")
  y <- structure(TRUE, foo = "bar")
  expect_false(vec_is(x, FALSE))
  expect_false(vec_is(FALSE, x))
  expect_true(vec_is(y, x))
  expect_true(vec_is(x, y))
})

test_that("unspecified is finalised before assertion", {
  expect_true(vec_is(NA, TRUE))
  expect_true(vec_is(data.frame(x = NA), data.frame(x = lgl())))

  expect_error(regexp = NA, vec_assert(NA, TRUE))
  expect_error(regexp = NA, vec_assert(data.frame(x = NA), data.frame(x = lgl())))
})

test_that("assertion failures are explained", {
  local_no_stringsAsFactors()
  local_options(rlang_backtrace_on_error = "none")

  expect_snapshot(error = TRUE, vec_assert(lgl(), chr()))

  expect_snapshot(error = TRUE, vec_assert(lgl(), factor()))
  expect_snapshot(error = TRUE, vec_assert(lgl(), factor(levels = "foo")))
  expect_snapshot(error = TRUE, vec_assert(factor(levels = "bar"), factor(levels = "foo")))

  expect_snapshot(error = TRUE, vec_assert(factor(), chr()))

  expect_snapshot(error = TRUE, vec_assert(lgl(), data.frame()))
  expect_snapshot(error = TRUE, vec_assert(lgl(), data.frame(x = 1)))
  expect_snapshot(error = TRUE, vec_assert(lgl(), data.frame(x = 1, y = 2)))

  expect_snapshot(error = TRUE, vec_assert(data.frame(), chr()))

  expect_snapshot(error = TRUE, vec_assert(data.frame(x = 1), chr()))
  expect_snapshot(error = TRUE, vec_assert(data.frame(x = 1), data.frame(x = "foo")))
  expect_snapshot(error = TRUE, vec_assert(data.frame(x = 1), data.frame(x = "foo", y = 2)))

  expect_snapshot(error = TRUE, vec_assert(data.frame(x = 1, y = 2), chr()))

  expect_snapshot(error = TRUE, vec_assert(data.frame(x = 1, y = 2), data.frame(x = "foo")))
  expect_snapshot(error = TRUE, vec_assert(data.frame(x = 1, y = 2), data.frame(x = "foo", y = 2)))
})

test_that("vec_assert() validates `size` (#1470)", {
  expect_snapshot({
    (expect_error(vec_assert(1, size = c(2, 3))))
    (expect_error(vec_assert(1, size = 1.5)))
    (expect_error(vec_assert(1, size = "x")))
  })
})

test_that("NULL is not a vector", {
  expect_false(vec_is_vector(NULL))
  expect_false(vec_is(NULL))
})

test_that("names and row names do not influence type identity (#707)", {
  expect_true(vec_is(c(a = TRUE), logical()))
  expect_true(vec_is(TRUE, c(a = TRUE)))
  expect_true(vec_is(structure(mtcars, row.names = 1:32), mtcars))
  expect_true(vec_is(mtcars, structure(mtcars, row.names = 1:32)))
})

# vec_is_list -----------------------------------------------------------

test_that("bare lists are lists", {
  expect_true(vec_is_list(list()))
})

test_that("AsIs lists are lists (#1463)", {
  expect_true(vec_is_list(I(list())))
  expect_true(vec_is_list(I(list_of(1))))
  expect_false(vec_is_list(I(double())))
})

test_that("list_of are lists", {
  expect_true(vec_is_list(new_list_of()))
})

test_that("Vectors with a non-VECSXP type are not lists", {
  expect_false(vec_is_list(1))
  expect_false(vec_is_list("a"))
  expect_false(vec_is_list(quote(name)))
})

test_that("explicitly classed lists are lists", {
  x <- structure(list(), class = "list")

  expect_true(vec_is_list(x))
  expect_true(vec_is_list(subclass(x)))
})

test_that("explicit inheritance must be in the base class", {
  x <- structure(1:2, class = c("list", "foobar"))
  expect_false(vec_is_list(x))
})

test_that("POSIXlt are not considered a list", {
  expect_false(vec_is_list(as.POSIXlt(new_datetime())))
})

test_that("rcrd types are not lists", {
  expect_false(vec_is_list(new_rcrd(list(x = 1))))
})

test_that("scalars are not lists", {
  expect_false(vec_is_list(foobar()))
})

test_that("S3 types can't lie about their internal representation", {
  x <- structure(1:2, class = c("foobar", "list"))
  expect_false(vec_is_list(x))
})

test_that("data frames of all types are not lists", {
  expect_false(vec_is_list(data.frame()))
  expect_false(vec_is_list(subclass(data.frame())))
  expect_false(vec_is_list(tibble::tibble()))
})

test_that("S3 list with non-list proxy is still a list (#1208)", {
  x <- structure(list(), class = c("foobar", "list"))
  local_methods(vec_proxy.foobar = function(x) 1)
  # This used to be an error (#1003)
  # expect_error(vec_is_list(x), "`x` inherits")
  expect_true(vec_is_list(x))
})

test_that("list-rcrds with data frame proxies are considered lists (#1208)", {
  x <- structure(
    list(1:2, "x"),
    special = c("a", "b"),
    class = c("list_rcrd", "list")
  )

  local_methods(
    vec_proxy.list_rcrd = function(x) {
      special <- attr(x, "special")
      data <- unstructure(x)
      new_data_frame(list(data = data, special = special))
    }
  )

  expect_true(vec_is_list(x))
})

test_that("list_all_vectors() works", {
  expect_true(list_all_vectors(list(1)))
  expect_true(list_all_vectors(list_of(1)))
  expect_false(list_all_vectors(list(1, env())))
  expect_snapshot((expect_error(list_all_vectors(env()))))
})

test_that("vec_check_list() works", {
  expect_null(vec_check_list(list(1)))
  expect_null(vec_check_list(list_of(1)))
  expect_snapshot({
    my_function <- function(my_arg) vec_check_list(my_arg)
    (expect_error(my_function(env())))
  })
})

test_that("vec_check_list() uses a special error when `arg` is the empty string (#1604)", {
  expect_snapshot(error = TRUE, {
    vec_check_list(1, arg = "")
  })
})

test_that("vec_check_list() and list_check_all_vectors() work", {
  expect_null(list_check_all_vectors(list()))
  expect_null(list_check_all_vectors(list(1, mtcars)))
  expect_snapshot({
    my_function <- function(my_arg) list_check_all_vectors(my_arg)
    (expect_error(my_function(env())))
    (expect_error(my_function(list(1, env()))))
    (expect_error(my_function(list(1, name = env()))))
    (expect_error(my_function(list(1, foo = env()))))
  })
})

test_that("list_all_size() works", {
  expect_true(list_all_size(list(), 2))
  expect_true(list_all_size(list(integer()), 0))
  expect_true(list_all_size(list(NULL), 0))
  expect_true(list_all_size(list(1:2, 2:3), 2))

  expect_false(list_all_size(list(1:2, 1:3), 2))
  expect_false(list_all_size(list(NULL, 1:2), 2))

  expect_true(list_all_size(list_of(1:3, 2:4), 3))
  expect_false(list_all_size(list_of(1:3, 2:4), 4))
})

test_that("list_check_all_size() works", {
  expect_null(list_check_all_size(list(), 2))
  expect_null(list_check_all_size(list(integer()), 0))
  expect_null(list_check_all_size(list(NULL), 0))
  expect_null(list_check_all_size(list(1:2, 2:3), 2))

  expect_snapshot({
    my_function <- function(my_arg, size) list_check_all_size(my_arg, size)

    # Validates sizes
    (expect_error(list_check_all_size(list(1:2, 1:3), 2)))
    (expect_error(my_function(list(1:2, 1:3), 2)))

    # `NULL` is not ignored
    (expect_error(my_function(list(NULL, 1:2), 2)))
  })
})

test_that("list_all_size() and list_check_all_size() error on scalars", {
  x <- list(env())

  expect_snapshot({
    # Error considered internal to `list_all_size()`
    (expect_error(list_all_size(x, 2)))

    my_function <- function(my_arg, size) list_check_all_size(my_arg, size)
    (expect_error(my_function(x, 2)))
  })
})

test_that("list_all_size() and list_check_all_size() throw error using internal call on non-list input", {
  expect_snapshot({
    (expect_error(list_all_size(1, 2)))

    # `arg` and `call` are ignored
    (expect_error(list_check_all_size(1, 2, arg = "arg", call = call("foo"))))
  })
})

test_that("list_all_size() and list_check_all_size() validate `size`", {
  expect_snapshot({
    (expect_error(list_all_size(list(), size = "x")))
    (expect_error(list_check_all_size(list(), size = "x")))
  })
})

test_that("informative messages when 1d array doesn't match vector", {
  x <- array(1:3)
  expect_snapshot((expect_error(vec_assert(x, int()))))
})
