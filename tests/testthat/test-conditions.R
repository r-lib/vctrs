context("conditions")

test_that("conditions inherit from `vctrs_error`", {
  expect_error(stop_incompatible(NULL, NULL), class = "vctrs_error")
  expect_error(stop_incompatible_type(NULL, NULL), class = "vctrs_error")
  expect_error(stop_incompatible_cast(NULL, NULL), class = "vctrs_error")
  expect_error(stop_incompatible_op("", NULL, NULL), class = "vctrs_error")
  expect_error(stop_incompatible_size(NULL, NULL, 0, 0), class = "vctrs_error")
  expect_error(maybe_lossy_cast(NULL, NULL, NULL, TRUE), class = "vctrs_error")
  expect_error(stop_unsupported("", ""), class = "vctrs_error")
  expect_error(stop_unimplemented("", ""), class = "vctrs_error")
  expect_error(stop_scalar_type(NULL), class = "vctrs_error")
  expect_error(stop_names("", NULL, 1), class = "vctrs_error")
  expect_error(stop_names_cannot_be_empty(1), class = "vctrs_error")
  expect_error(stop_names_cannot_be_dot_dot(1), class = "vctrs_error")
  expect_error(stop_names_must_be_unique(1), class = "vctrs_error")
})

test_that("can override arg in OOB conditions", {
  verify_errors({
    expect_error(
      with_subscript_data(
        vec_slice(set_names(letters), "foo"),
        NULL
      ),
      class = "vctrs_error_subscript_oob"
    )
    expect_error(
      with_subscript_data(
        vec_slice(set_names(letters), "foo"),
        quote(foo)
      ),
      class = "vctrs_error_subscript_oob"
    )
    expect_error(
      with_subscript_data(
        vec_slice(set_names(letters), "foo"),
        quote(foo(bar))
      ),
      class = "vctrs_error_subscript_oob"
    )
  })
})

test_that("scalar type errors are informative", {
  verify_errors({
    expect_error(
      vec_slice(foobar(list(1)), 1),
      class = "vctrs_error_scalar_type"
    )
    expect_error(
      stop_scalar_type(foobar(list(1)), arg = "foo"),
      class = "vctrs_error_scalar_type"
    )
  })
})

verify_output(test_path("error", "test-conditions.txt"), {
  "# can override arg in OOB conditions"
  with_subscript_data(
    vec_slice(set_names(letters), "foo"),
    NULL
  )
  with_subscript_data(
    vec_slice(set_names(letters), "foo"),
    "input"
  )
  with_subscript_data(
    vec_slice(set_names(letters), "foo"),
    quote(input)
  )
  with_subscript_data(
    vec_slice(set_names(letters), "foo"),
    quote(input[i])
  )

  "# scalar type errors are informative"
  vec_slice(foobar(list(1)), 1)
  stop_scalar_type(foobar(list(1)), arg = "foo")
})
