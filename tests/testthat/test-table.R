
test_that("can slice table", {
  expect_identical(tbl_slice(mtcars, 3), mtcars[3])
  expect_identical(tbl_slice(mtcars, 2:3), mtcars[2:3])
})

test_that("can take the table prototype", {
  expect_identical(tbl_ptype(mtcars), mtcars[integer(), integer()])
})

test_that("data frames are tabular", {
  expect_true(tbl_is(mtcars))
  expect_error_free(tbl_assert(mtcars))
})

test_that("vectors are not tabular", {
  expect_false(tbl_is(NA))
  expect_false(tbl_is(1:3))
  expect_error(tbl_assert(NA), "must be a data frame")
  expect_error(tbl_assert(1:3), "must be a data frame")
})
