context("test-dim")

# vec_obs --------------------------------------------------------------

test_that("vec_obs must be called with vector", {
  expect_error(vec_obs(mean), "not a vector")
})

test_that("length is number of rows", {
  expect_equal(vec_obs(integer()), 0)
  expect_equal(vec_obs(array(integer())), 0)

  expect_equal(vec_obs(1:2), 2)
  expect_equal(vec_obs(array(dim = 2)), 2)

  expect_equal(vec_obs(matrix(nrow = 2, ncol = 3)), 2)
  expect_equal(vec_obs(array(dim = c(2, 1, 5))), 2)
})

test_that("length of record is number of rows, not fields", {
  r <- new_rcrd(list(x = 1:10))
  expect_equal(vec_obs(r), 10)
})

test_that("handles three types of data frame rownames", {
  df1 <- df2 <- df3 <- data.frame(x = 1:3)
  rownames(df1) <- NULL
  rownames(df2) <- 3:1
  rownames(df3) <- letters[1:3]

  expect_equal(vec_obs(df1), 3)
  expect_equal(vec_obs(df2), 3)
  expect_equal(vec_obs(df3), 3)
})

# vec_dim -----------------------------------------------------------------

test_that("dim is dimensions", {
  expect_equal(vec_dim(array(dim = c(1))), c(1))
  expect_equal(vec_dim(array(dim = c(1, 1))), c(1, 1))
  expect_equal(vec_dim(array(dim = c(1, 1, 1))), c(1, 1, 1))
})


test_that("dims is number of dimensions", {
  expect_equal(vec_dims(array(dim = c(1))), 1)
  expect_equal(vec_dims(array(dim = c(1, 1))), 2)
  expect_equal(vec_dims(array(dim = c(1, 1, 1))), 3)
})

test_that("vector and 1-d array are equivalent", {
  x1 <- 1:5
  x2 <- array(x1)

  expect_equal(vec_dim(x1), 5)
  expect_equal(vec_dim(x2), 5)

  expect_equal(vec_obs(x1), 5)
  expect_equal(vec_obs(x2), 5)
})
