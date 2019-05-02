context("test-size")

# vec_size -----------------------------------------------------------------

test_that("vec_size must be called with vector", {
  expect_error(vec_size(mean), "not a vector")
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
  scoped_global_bindings(
    vec_proxy.vctrs_proxy = function(x) x[[1]]$x,
  )
  expect_size(new_proxy(1:3), 3)
})

test_that("identity of vec_size is 0", {
  expect_equal(vec_size(), 0)
})

test_that("`NULL` is treated as the absence of input", {
  expect_equal(vec_size(NULL), vec_size())
})

# vec_size_common ---------------------------------------------------------

test_that("identity of vec_size_common is 0", {
  expect_equal(vec_size_common(), 0)
})

test_that("`NULL` is treated as the absence of input", {
  expect_equal(vec_size_common(NULL), vec_size_common())
  expect_equal(vec_size_common(NULL, NULL), vec_size_common())
  expect_equal(vec_size_common(1:5, NULL), vec_size_common(1:5))
})

# sequences ---------------------------------------------------------------

test_that("vec_seq_along returns size-0 output for size-0 input", {
  expect_equal(vec_seq_along(character()), integer())
  expect_equal(vec_seq_along(data.frame()), integer())
})

test_that("vec_na_along can be called with single argument", {
  expect_equal(vec_na_along(1:3), rep(NA_integer_, 3))
})
