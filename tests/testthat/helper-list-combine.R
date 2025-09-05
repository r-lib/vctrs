# Helper for `list_combine()` tests that tests a `list_combine()` call across
# 3 variants:
#
# - Normal case
# - Homogenous fallback case (all S3 objects with the same class but no ptype2
#   method and no `c()` method. these end up using the "main" loop due to the
#   lack of `c()` method.)
# - `c()` fallback case (all S3 objects with the same class and a `c()` method.
#   these end up using the `base_c_invoke()` fallback path)
#
# It's important that all variants are fairly consistent
expect_identical_list_combine <- function(
  ...,
  x,
  indices,
  size,
  default = NULL,
  unmatched = "default",
  ptype = NULL,
  name_spec = NULL,
  name_repair = "minimal",
  x_arg = "x",
  indices_arg = "indices",
  default_arg = "default",
  expect
) {
  expect_something_list_combine(
    expect_identical,
    ...,
    x = x,
    indices = indices,
    size = size,
    default = default,
    unmatched = unmatched,
    ptype = ptype,
    name_spec = name_spec,
    name_repair = name_repair,
    x_arg = x_arg,
    indices_arg = indices_arg,
    default_arg = default_arg,
    expect = expect,
    foobar_expect = TRUE
  )
}

expect_named_list_combine <- function(
  ...,
  x,
  indices,
  size,
  default = NULL,
  unmatched = "default",
  ptype = NULL,
  name_spec = NULL,
  name_repair = "minimal",
  x_arg = "x",
  indices_arg = "indices",
  default_arg = "default",
  expect
) {
  expect_something_list_combine(
    expect_named,
    ...,
    x = x,
    indices = indices,
    size = size,
    default = default,
    unmatched = unmatched,
    ptype = ptype,
    name_spec = name_spec,
    name_repair = name_repair,
    x_arg = x_arg,
    indices_arg = indices_arg,
    default_arg = default_arg,
    expect = expect,
    foobar_expect = FALSE
  )
}

expect_snapshot_list_combine <- function(
  ...,
  x,
  indices,
  size,
  default = NULL,
  unmatched = "default",
  ptype = NULL,
  name_spec = NULL,
  name_repair = "minimal",
  x_arg = "x",
  indices_arg = "indices",
  default_arg = "default",
  error = FALSE
) {
  check_dots_empty0(...)

  expect_snapshot(error = error, {
    list_combine(
      x,
      indices = indices,
      size = size,
      default = default,
      unmatched = unmatched,
      ptype = ptype,
      name_spec = name_spec,
      name_repair = name_repair,
      x_arg = x_arg,
      indices_arg = indices_arg,
      default_arg = default_arg
    )
  })

  x_foobar <- lapply(x, function(elt) {
    if (is.null(elt)) {
      elt
    } else {
      foobar(elt)
    }
  })

  if (is.null(default)) {
    default_foobar <- NULL
  } else {
    default_foobar <- foobar(default)
  }

  # Homogeneous fallback
  expect_snapshot(error = error, {
    list_combine(
      x_foobar,
      indices = indices,
      size = size,
      default = default_foobar,
      unmatched = unmatched,
      ptype = ptype,
      name_spec = name_spec,
      name_repair = name_repair,
      x_arg = x_arg,
      indices_arg = indices_arg,
      default_arg = default_arg
    )
  })

  # So they show up differently in the snapshot
  x_foobar_c <- x_foobar
  default_foobar_c <- default_foobar

  # `c()` fallback
  with_c_foobar({
    expect_snapshot(error = error, {
      list_combine(
        x_foobar_c,
        indices = indices,
        size = size,
        default = default_foobar_c,
        unmatched = unmatched,
        ptype = ptype,
        name_spec = name_spec,
        name_repair = name_repair,
        x_arg = x_arg,
        indices_arg = indices_arg,
        default_arg = default_arg
      )
    })
  })
}

expect_something_list_combine <- function(
  expect_fn,
  ...,
  x,
  indices,
  size,
  default = NULL,
  unmatched = "default",
  ptype = NULL,
  name_spec = NULL,
  name_repair = "minimal",
  x_arg = "x",
  indices_arg = "indices",
  default_arg = "default",
  expect,
  foobar_expect = TRUE
) {
  check_dots_empty0(...)

  expect_fn(
    list_combine(
      x,
      indices = indices,
      size = size,
      default = default,
      unmatched = unmatched,
      ptype = ptype,
      name_spec = name_spec,
      name_repair = name_repair,
      x_arg = x_arg,
      indices_arg = indices_arg,
      default_arg = default_arg
    ),
    expected = expect
  )

  x_foobar <- lapply(x, function(elt) {
    if (is.null(elt)) {
      elt
    } else {
      foobar(elt)
    }
  })

  if (is.null(default)) {
    default_foobar <- NULL
  } else {
    default_foobar <- foobar(default)
  }

  if (foobar_expect) {
    expect_foobar <- foobar(expect)
  } else {
    expect_foobar <- expect
  }

  # Homogeneous fallback
  expect_fn(
    list_combine(
      x_foobar,
      indices = indices,
      size = size,
      default = default_foobar,
      unmatched = unmatched,
      ptype = ptype,
      name_spec = name_spec,
      name_repair = name_repair,
      x_arg = x_arg,
      indices_arg = indices_arg,
      default_arg = default_arg
    ),
    expected = expect_foobar
  )

  if (foobar_expect) {
    expect_foobar_c <- foobar_c(expect)
  } else {
    expect_foobar_c <- expect
  }

  # `c()` fallback
  with_c_foobar({
    expect_fn(
      list_combine(
        x_foobar,
        indices = indices,
        size = size,
        default = default_foobar,
        unmatched = unmatched,
        ptype = ptype,
        name_spec = name_spec,
        name_repair = name_repair,
        x_arg = x_arg,
        indices_arg = indices_arg,
        default_arg = default_arg
      ),
      expected = expect_foobar_c
    )
  })
}
