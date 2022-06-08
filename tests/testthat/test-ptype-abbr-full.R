
test_that("input must be a vector", {
  expect_error(vec_ptype_abbr(sum), "Not a vector")
  expect_error(vec_ptype_full(sum), "Not a vector")
})

test_that("NULL has method", {
  expect_equal(vec_ptype_abbr(NULL), "NULL")
  expect_equal(vec_ptype_full(NULL), "NULL")
})

test_that("non objects default to type + shape", {
  expect_equal(vec_ptype_abbr(ones(10)), "dbl")
  expect_equal(vec_ptype_abbr(ones(0, 10)), "dbl[,10]")
  expect_equal(vec_ptype_abbr(ones(10, 0)), "dbl[,0]")

  expect_equal(vec_ptype_full(ones(10)), "double")
  expect_equal(vec_ptype_full(ones(0, 10)), "double[,10]")
  expect_equal(vec_ptype_full(ones(10, 0)), "double[,0]")

})

test_that("non objects can omit shape", {
  expect_equal(vec_ptype_abbr(ones(10), suffix_shape = FALSE), "dbl")
  expect_equal(vec_ptype_abbr(ones(0, 10), suffix_shape = FALSE), "dbl")
  expect_equal(vec_ptype_abbr(ones(10, 0), suffix_shape = FALSE), "dbl")
})

test_that("objects default to first class", {
  x <- structure(1, class = c("foofy", "goofy"))
  expect_equal(vec_ptype_full(x), "foofy")
  expect_equal(vec_ptype_abbr(x), "foofy")
})

test_that("atomic vectors and arrays as expected", {
  expect_equal(vec_ptype_full(1:5), "integer")

  dbl_mat <- array(double(), c(0, 3))
  expect_equal(vec_ptype_full(dbl_mat), "double[,3]")
})

test_that("complex and factor as expected (#323)", {
  expect_equal(vec_ptype_abbr(0i), "cpl")
  expect_equal(vec_ptype_abbr(factor()), "fct")
})

test_that("named lists are always tagged (#322)", {
  expect_identical(vec_ptype_abbr(list(x = 1, y = 2)), "named list")
  expect_identical(vec_ptype_abbr(list(x = 1, y = 2), prefix_named = TRUE), "named list")
})

test_that("named atomics are tagged optionally (#781)", {
  expect_identical(vec_ptype_abbr(c(x = 1, y = 2), prefix_named = TRUE), "named dbl")
  expect_identical(vec_ptype_abbr(c(x = 1L, y = 2L), prefix_named = TRUE), "named int")
})

test_that("vec_ptype_abbr() adds named tag in case of row names", {
  expect_equal(
    vec_ptype_abbr(mtcars, prefix_named = TRUE),
    "named df[,11]"
  )

  mat <- matrix(1:4, 2)
  rownames(mat) <- c("foo", "bar")
  expect_equal(
    vec_ptype_abbr(mat, prefix_named = TRUE),
    "named int[,2]"
  )
})

test_that("vec_ptype_abbr() and vec_ptype_full() are not inherited (#1549)", {
  foobar <- foobar(class = c("vctrs_bar", "vctrs_foo"))

  local_methods(
    vec_ptype_abbr.vctrs_foo = function(...) "foo_abbr",
    vec_ptype_full.vctrs_foo = function(...) "foo_full"
  )
  expect_equal(
    vec_ptype_abbr(foobar),
    vec_ptype_abbr.default(foobar)
  )
  expect_equal(
    vec_ptype_full(foobar),
    "vctrs_bar"
  )

  local_methods(
    vec_ptype_abbr.vctrs_bar = function(...) "bar_abbr",
    vec_ptype_full.vctrs_bar = function(...) "bar_full"
  )
  expect_equal(
    vec_ptype_abbr(foobar),
    "bar_abbr"
  )
  expect_equal(
    vec_ptype_full(foobar),
    "bar_full"
  )
})
