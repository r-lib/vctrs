test_that("basics", {
  x <- zando(10)

  expect_true(vec_is(x))
})
