
test_that("list_flatten() expects lists", {
  expect_error(list_flatten(""), "must be a list")
  expect_error(list_flatten(mtcars), "must be a list")
})

test_that("list_flatten() is generic over lists", {
  local_count_list_methods()

  x <- new_count_list(list(1, 2, 3))
  expect_identical(list_flatten(x), x)

  x <- new_count_list(list(1, list(2), new_count_list(list(3))))
  out <- list_flatten(x)
  exp <- new_count_list(list(1, 2, 3))
  expect_identical(out, exp)
})

test_that("list_flatten() flattens inner lists", {
  expect_identical(
    list_flatten(list(1L, list(2:3))),
    list(1L, 2:3)
  )
})

test_that("list_flatten() doesn't flatten data frames", {
  x <- list(1L, data.frame(x = 2:3))
  expect_identical(list_flatten(x), x)
})

test_that("list_flatten() creates output of type `ptype`", {
  local_count_list_methods()
  expect_identical(
    list_flatten(list(1, list(2)), ptype = new_count_list()),
    new_count_list(list(1, 2))
  )
})
