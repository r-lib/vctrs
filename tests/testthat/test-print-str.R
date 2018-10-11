context("test-print-str")

test_that("show attributes", {
  x <- structure(1:100, x = "a string", y = 1:20, z = data.frame(x = 1:3))
  expect_known_output(
    obj_str(x),
    test_path("test-print-str-attr.txt")
  )

  expect_known_output(
    obj_str(mtcars),
    test_path("test-print-str-mtcars.txt")
  )
})
