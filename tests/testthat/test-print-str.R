
test_that("show attributes", {
  x <- structure(1:100, x = "a string", y = 1:20, z = data.frame(x = 1:3))
  expect_snapshot(obj_str(x))

  expect_snapshot(obj_str(mtcars))
})
