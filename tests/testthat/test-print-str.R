test_that("show attributes", {
  # Printing of `z:'data.frame'` was corrected to `z: 'data.frame'`
  skip_if_not(getRversion() >= "4.7.0")
  x <- structure(1:100, x = "a string", y = 1:20, z = data.frame(x = 1:3))
  expect_snapshot(obj_str(x))
})

test_that("show data frame attributes", {
  expect_snapshot(obj_str(mtcars))
  expect_snapshot(obj_str(mtcars[0, 1:2]))
})
