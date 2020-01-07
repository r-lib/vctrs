
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

test_that("vec_as_subscript() forbids subscript types", {
  verify_output(test_path("out", "test-coerce-index-allow-types.txt"), {
    vec_as_subscript(1L, indicator = "error", location = "error")
    vec_as_subscript("foo", indicator = "error", name = "error")
    vec_as_subscript(TRUE, indicator = "error")
    vec_as_subscript("foo", name = "error")
  })
})

test_that("vec_as_subscript() coerces subtypes", {
  expect_identical(vec_as_subscript(factor("foo")), "foo")

  with_lgl_subtype({
    expect_identical(vec_as_subscript(new_lgl_subtype(TRUE)), TRUE)
  })

  with_lgl_supertype({
    expect_error(
      vec_as_subscript(new_lgl_supertype(TRUE)),
      class = "vctrs_error_subscript_bad_type"
    )
  })
})

test_that("vec_as_subscript2() handles `allow_types`", {
  skip("FIXME")
  verify_output(test_path("out", "test-coerce-position-allow-types.txt"), {
    vec_as_subscript2(1L, allow_types = "name")
    vec_as_subscript2("foo", allow_types = "location")
    vec_as_subscript2(TRUE, allow_types = c("location", "name"))
  })
})
