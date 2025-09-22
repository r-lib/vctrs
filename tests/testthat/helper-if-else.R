# Helpers for testing `vec_if_else()`
#
# These ensure that we have consistent output and errors across the
# atomic and generic paths. We use them when possible.
expect_identical_vec_if_else <- function(
  ...,
  condition,
  true,
  false,
  missing = NULL,
  ptype = NULL,
  condition_arg = "condition",
  true_arg = "true",
  false_arg = "false",
  missing_arg = "missing",
  expect
) {
  expect_something_vec_if_else(
    expect_fn = expect_identical,
    condition = condition,
    true = true,
    false = false,
    missing = missing,
    ptype = ptype,
    condition_arg = condition_arg,
    true_arg = true_arg,
    false_arg = false_arg,
    missing_arg = missing_arg,
    expect = expect
  )
}

expect_something_vec_if_else <- function(
  expect_fn,
  ...,
  condition,
  true,
  false,
  missing = NULL,
  ptype = NULL,
  condition_arg = "condition",
  true_arg = "true",
  false_arg = "false",
  missing_arg = "missing",
  expect
) {
  check_dots_empty0(...)

  expect_fn(
    vec_if_else(
      condition = condition,
      true = true,
      false = false,
      missing = missing,
      ptype = ptype,
      condition_arg = condition_arg,
      true_arg = true_arg,
      false_arg = false_arg,
      missing_arg = missing_arg
    ),
    expected = expect
  )

  true_vctr <- new_vctr(true)
  false_vctr <- new_vctr(false)

  if (is.null(missing)) {
    missing_vctr <- NULL
  } else {
    missing_vctr <- new_vctr(missing)
  }

  expect_vctr <- new_vctr(expect)

  expect_fn(
    vec_if_else(
      condition = condition,
      true = true_vctr,
      false = false_vctr,
      missing = missing_vctr,
      ptype = ptype,
      condition_arg = condition_arg,
      true_arg = true_arg,
      false_arg = false_arg,
      missing_arg = missing_arg
    ),
    expected = expect_vctr
  )
}

expect_snapshot_vec_if_else <- function(
  ...,
  condition,
  true,
  false,
  missing = NULL,
  ptype = NULL,
  condition_arg = "condition",
  true_arg = "true",
  false_arg = "false",
  missing_arg = "missing",
  error = FALSE
) {
  check_dots_empty0(...)

  expect_snapshot(error = error, {
    vec_if_else(
      condition = condition,
      true = true,
      false = false,
      missing = missing,
      ptype = ptype,
      condition_arg = condition_arg,
      true_arg = true_arg,
      false_arg = false_arg,
      missing_arg = missing_arg
    )
  })

  true_vctr <- new_vctr(true)
  false_vctr <- new_vctr(false)

  if (is.null(missing)) {
    missing_vctr <- NULL
  } else {
    missing_vctr <- new_vctr(missing)
  }

  expect_snapshot(error = error, {
    vec_if_else(
      condition = condition,
      true = true_vctr,
      false = false_vctr,
      missing = missing_vctr,
      ptype = ptype,
      condition_arg = condition_arg,
      true_arg = true_arg,
      false_arg = false_arg,
      missing_arg = missing_arg
    )
  })
}
