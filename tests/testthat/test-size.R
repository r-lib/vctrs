test_that("vec_as_short_length() checks inputs", {
  expect_equal(vec_as_short_length(0), 0)
  expect_equal(vec_as_short_length(1L), 1)

  my_function <- function(my_arg) vec_as_short_length(my_arg)
  expect_snapshot({
    (expect_error(my_function(-1)))
    (expect_error(my_function(1:2)))
    (expect_error(my_function(1.5)))
    (expect_error(my_function(NA)))
    (expect_error(my_function(na_int)))
    (expect_error(my_function("foo")))
    (expect_error(my_function(foobar(1:2))))
    (expect_error(my_function(.Machine$double.xmax)))
  })
})

test_that("vec_as_short_length() has a special error about long vector support", {
  # In particular, skips on 32-bit Windows where `r_ssize == int`
  skip_if(.Machine$sizeof.pointer < 8L, message = "No long vector support")

  my_function <- function(my_arg) vec_as_short_length(my_arg)
  expect_snapshot({
    (expect_error(my_function(.Machine$integer.max + 1)))
  })
})

# vec_size -----------------------------------------------------------------

test_that("vec_size must be called with vector", {
  expect_error(vec_size(mean), class = "vctrs_error_scalar_type")
})

test_that("length is number of rows", {
  expect_equal(vec_size(integer()), 0)
  expect_equal(vec_size(array(integer())), 0)

  expect_equal(vec_size(1:2), 2)
  expect_equal(vec_size(array(dim = 2)), 2)

  expect_equal(vec_size(matrix(nrow = 2, ncol = 3)), 2)
  expect_equal(vec_size(array(dim = c(2, 1, 5))), 2)
})

test_that("length of record is number of rows, not fields", {
  r <- new_rcrd(list(x = 1:10))
  expect_equal(vec_size(r), 10)
})

test_that("handles three types of data frame rownames", {
  df1 <- df2 <- df3 <- data.frame(x = 1:3)
  rownames(df1) <- NULL
  rownames(df2) <- 3:1
  rownames(df3) <- letters[1:3]

  expect_equal(vec_size(df1), 3)
  expect_equal(vec_size(df2), 3)
  expect_equal(vec_size(df3), 3)
})

test_that("handles positive short row names (#220)", {
  data <- structure(mtcars, row.names = c(NA, 32))
  expect_identical(vec_size(data), 32L)
})

test_that("size is proxied", {
  local_env_proxy()
  expect_size(new_proxy(1:3), 3)
  expect_size(new_proxy(list(1, 2, 3)), 3)
  expect_size(new_proxy(foobar(list(1, 2, 3))), 3)
})

test_that("`NULL` has size zero", {
  expect_identical(vec_size(NULL), 0L)
})

test_that("can take the size of unspecified objects", {
  expect_size(NA, 1)
  expect_size(c(NA, NA), 2)
  expect_size(unspecified(2), 2)
})

# vec_size_common ---------------------------------------------------------

test_that("vec_size_common() checks inputs", {
  expect_snapshot({
    (expect_error(vec_size_common(.size = "foo")))
    (expect_error(vec_size_common(.size = 1:2)))
  })
})

test_that("vec_size_common() mentions `arg` in errors", {
  my_function <- function(...) vec_size_common(..., .arg = "my_arg")
  expect_snapshot({
    (expect_error(my_function(this_arg = 1:2, that_arg = int())))
  })
})

test_that("vec_size_common with no input is 0L unless `.absent` is provided", {
  expect_identical(vec_size_common(), 0L)
  expect_identical(vec_size_common(NULL), 0L)
  expect_equal(vec_size_common(.absent = na_int), na_int)
})

test_that("`.absent` must be supplied when `...` is empty", {
  expect_snapshot({
    (expect_error(vec_size_common(.absent = NULL)))
  })
})

test_that("`.absent` must be a length 1 integer if provided", {
  expect_snapshot({
    (expect_error(vec_size_common(.absent = 1), "must be a single integer"))
    (expect_error(vec_size_common(.absent = c(1L, 2L)), "must be a single integer"))
  })
})

test_that("`NULL` is treated as the absence of input", {
  expect_equal(vec_size_common(1:5, NULL), vec_size_common(1:5))
})

test_that("size 1 is overshadowed by any other size", {
  expect_equal(vec_size_common(1, integer()), 0)
  expect_equal(vec_size_common(1, 1:5), 5)
})

test_that("if not size 1, sizes must be identical", {
  expect_equal(vec_size_common(integer(), integer()), 0)
  expect_error(vec_size_common(1:2, integer()), class = "vctrs_error_incompatible_size")
  expect_error(vec_size_common(1:2, 1:3), class = "vctrs_error_incompatible_size")
})

test_that("argument tags are forwarded", {
  expect_snapshot(error = TRUE, vec_size_common(1:2, 1, 1:4))
  expect_snapshot(error = TRUE, vec_size_common(foo = 1:2, 1, bar = 1:4))
})

test_that("can pass size", {
  expect_identical(vec_size_common(1:2, 1:3, .size = 5L), 5L)
})

test_that("provided size is cast to an integer", {
  expect_identical(vec_size_common(.size = 1), 1L)
})

# list_sizes --------------------------------------------------------------

test_that("only lists are allowed", {
  expect_error(list_sizes(mtcars), "must be a list")
  expect_error(list_sizes(1), "must be a list")
})

test_that("computes element sizes", {
  expect_identical(list_sizes(list(1, 1:3, c("a", "b"))), c(1L, 3L, 2L))
})

test_that("retains list names", {
  x <- list(1, x = 2, a = 3)
  expect_named(list_sizes(x), c("", "x", "a"))

  x <- list_of(y = 1, x = 2, a = 3)
  expect_named(list_sizes(x), c("y", "x", "a"))
})

test_that("retains names of empty lists", {
  x <- structure(list(), names = character())
  expect_named(list_sizes(x), character())
})

# sequences ---------------------------------------------------------------

test_that("vec_seq_along returns size-0 output for size-0 input", {
  expect_equal(vec_seq_along(character()), integer())
  expect_equal(vec_seq_along(data.frame()), integer())
})

test_that("vec_init_along can be called with single argument", {
  expect_equal(vec_init_along(1:3), rep(NA_integer_, 3))
})

# %0% --------------------------------------------------------------------

test_that("uses y when x is empty", {
  expect_equal(1 %0% 2, 1)
  expect_equal(1[0] %0% 2, 2)
})
