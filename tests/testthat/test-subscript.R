
test_that("vec_as_subscript() coerces unspecified vectors", {
  expect_identical(
    vec_as_subscript(NA),
    NA
  )
  expect_identical(
    vec_as_subscript(NA, indicator = "error"),
    na_int
  )
  expect_identical(
    vec_as_subscript(NA, indicator = "error", location = "error"),
    na_chr
  )
})

test_that("vec_as_subscript() coerces subtypes and supertypes", {
  expect_identical(vec_as_subscript(factor("foo")), "foo")

  with_lgl_subtype({
    expect_identical(vec_as_subscript(new_lgl_subtype(TRUE)), TRUE)
  })
  with_lgl_supertype({
    expect_identical(vec_as_subscript(new_lgl_supertype(TRUE)), TRUE)
  })
})

test_that("subscript functions have informative error messages", {
  verify_output(test_path("error", "test-subscript.txt"), {
    "# vec_as_subscript() forbids subscript types"
    vec_as_subscript(1L, indicator = "error", location = "error")
    vec_as_subscript("foo", indicator = "error", name = "error")
    vec_as_subscript(TRUE, indicator = "error")
    vec_as_subscript("foo", name = "error")

    "# vec_as_subscript2() forbids subscript types"
    vec_as_subscript2(1L, location = "error", indicator = "error")
    vec_as_subscript2("foo", name = "error", indicator = "error")
    vec_as_subscript2(TRUE, indicator = "error")
  })
})
