
test_that("show attributes", {
  x <- structure(1:100, x = "a string", y = 1:20, z = data.frame(x = 1:3))
  expect_snapshot(obj_str(x))

  expect_snapshot(obj_str(mtcars))
})

test_that("max argument (#1355)", {
  expect_snapshot({
    x <- vctrs::new_vctr(letters)
    print(x, max = 5)
    print(x, max = 30)
  })
})

test_that("small max.print option (#1355)", {
  local_options(max.print = 5)
  expect_snapshot({
    x <- vctrs::new_vctr(letters)
    print(x)
  })
})

test_that("large max.print option (#1355)", {
  local_options(max.print = 30)
  expect_snapshot({
    x <- vctrs::new_vctr(letters)
    print(x)
  })
})

test_that("both max argument and max.print option (#1355)", {
  local_options(max.print = 10)

  expect_snapshot({
    x <- vctrs::new_vctr(letters)
    print(x, max = 5)
    print(x, max = 20)
    print(x, max = 30)
  })
})
