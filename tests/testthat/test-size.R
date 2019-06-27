context("test-size")

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
  scoped_env_proxy()
  expect_size(new_proxy(1:3), 3)
  expect_size(new_proxy(list(1, 2, 3)), 3)
  expect_size(new_proxy(foobar(list(1, 2, 3))), 3)
})

test_that("`NULL` has size zero", {
  expect_identical(vec_size(NULL), 0L)
})

# vec_size_common ---------------------------------------------------------

test_that("vec_size_common with no input is 0L unless `.absent` is provided", {
  expect_identical(vec_size_common(), 0L)
  expect_identical(vec_size_common(NULL), 0L)
  expect_equal(vec_size_common(.absent = na_int), na_int)
})

test_that("`.absent` must be a length 1 integer if provided", {
  expect_error(vec_size_common(.absent = 1), "must be a single integer")
  expect_error(vec_size_common(.absent = c(1L, 2L)), "must be a single integer")
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
  expect_known_output_nobang(file = test_path("test-type-vec-size-common-error.txt"), {
    try2(vec_size_common(1:2, 1, 1:4))
    try2(vec_size_common(foo = 1:2, 1, bar = 1:4))
  })
})

test_that("can pass size", {
  expect_identical(vec_size_common(1:2, 1:3, .size = 5L), 5L)
})


# sequences ---------------------------------------------------------------

test_that("vec_seq_along returns size-0 output for size-0 input", {
  expect_equal(vec_seq_along(character()), integer())
  expect_equal(vec_seq_along(data.frame()), integer())
})

test_that("vec_init_along can be called with single argument", {
  expect_equal(vec_init_along(1:3), rep(NA_integer_, 3))
})
