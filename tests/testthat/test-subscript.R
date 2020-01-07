
test_that("vec_as_subscript() handles `allow_types`", {
  expect_identical(vec_as_subscript(NA, allow_types = c("location", "name")), na_int)
  expect_identical(vec_as_subscript(NA, allow_types = "name"), na_chr)

  verify_output(test_path("out", "test-coerce-index-allow-types.txt"), {
    vec_as_subscript(1L, allow_types = "name")
    vec_as_subscript("foo", allow_types = "location")
    vec_as_subscript(TRUE, allow_types = c("location", "name"))
    vec_as_subscript("foo", allow_types = c("indicator", "location"))
  })
})

test_that("vec_as_subscript2() handles `allow_types`", {
  verify_output(test_path("out", "test-coerce-position-allow-types.txt"), {
    vec_as_subscript2(1L, allow_types = "name")
    vec_as_subscript2("foo", allow_types = "location")
    vec_as_subscript2(TRUE, allow_types = c("location", "name"))
  })
})
