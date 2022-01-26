
test_that("vec_as_location2() returns a position", {
  expect_identical(vec_as_location2(2, 2L), 2L)
  expect_identical(vec_as_location2("foo", 2L, c("bar", "foo")), 2L)
  expect_identical(vec_as_location2("0", 4L, as.character(-1:2)), 2L)
})

test_that("vec_as_location2() requires integer or character inputs", {
  expect_snapshot({
    (expect_error(vec_as_location2(TRUE, 10L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location2(mtcars, 10L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location2(env(), 10L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location2(foobar(), 10L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location2(2.5, 10L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location2(Inf, 10L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location2(-Inf, 10L), class = "vctrs_error_subscript_type"))

    "Idem with custom `arg`"
    (expect_error(vec_as_location2(foobar(), 10L, arg = "foo", call = call("my_function")), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location2(2.5, 3L, arg = "foo", call = call("my_function")), class = "vctrs_error_subscript_type"))
    (expect_error(with_tibble_rows(vec_as_location2(TRUE)), class = "vctrs_error_subscript_type"))
  })
})

test_that("vec_as_location() requires integer, character, or logical inputs", {
  expect_snapshot({
    (expect_error(vec_as_location(mtcars, 10L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location(env(), 10L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location(foobar(), 10L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location(2.5, 10L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location(list(), 10L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location(function() NULL, 10L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location(Sys.Date(), 3L), class = "vctrs_error_subscript_type"))

    "Idem with custom `arg`"
    (expect_error(vec_as_location(env(), 10L, arg = "foo", call = call("my_function")), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location(foobar(), 10L, arg = "foo", call = call("my_function")), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location(2.5, 3L, arg = "foo", call = call("my_function")), class = "vctrs_error_subscript_type"))
  })
})

test_that("vec_as_location2() and vec_as_location() require integer- or character-like OO inputs", {
  expect_identical(vec_as_location2(factor("foo"), 2L, c("bar", "foo")), 2L)
  expect_identical(vec_as_location(factor("foo"), 2L, c("bar", "foo")), 2L)
  expect_error(vec_as_location2(foobar(1L), 10L), class = "vctrs_error_subscript_type")
  expect_error(vec_as_location(foobar(1L), 10L), class = "vctrs_error_subscript_type")

  # Define subtype of logical and integer
  local_methods(
    vec_ptype2.vctrs_foobar = function(x, y, ...) UseMethod("vec_ptype2.vctrs_foobar"),
    vec_ptype2.vctrs_foobar.logical = function(x, y, ...) logical(),
    vec_ptype2.vctrs_foobar.integer = function(x, y, ...) integer(),
    vec_ptype2.logical.vctrs_foobar = function(x, y, ...) logical(),
    vec_ptype2.integer.vctrs_foobar = function(x, y, ...) integer(),
    vec_cast.vctrs_foobar = function(x, to, ...) UseMethod("vec_cast.vctrs_foobar"),
    vec_cast.vctrs_foobar.integer = function(x, to, ...) foobar(x),
    vec_cast.integer.vctrs_foobar = function(x, to, ...) vec_cast(unclass(x), int()),
    vec_cast.logical.vctrs_foobar = function(x, to, ...) vec_cast(unclass(x), lgl())
  )
  expect_error(vec_as_location2(foobar(TRUE), 10L), class = "vctrs_error_subscript_type")
  expect_identical(vec_as_location(foobar(TRUE), 10L), 1:10)
  expect_identical(vec_as_location(foobar(FALSE), 10L), int())
})

test_that("vec_as_location() and variants check for OOB elements", {
  expect_snapshot({
    "Numeric indexing"
    (expect_error(vec_as_location(10L, 2L), class = "vctrs_error_subscript_oob"))
    (expect_error(vec_as_location(-10L, 2L), class = "vctrs_error_subscript_oob"))
    (expect_error(vec_as_location2(10L, 2L), class = "vctrs_error_subscript_oob"))

    "Character indexing"
    (expect_error(vec_as_location("foo", 1L, names = "bar"), class = "vctrs_error_subscript_oob"))
    (expect_error(vec_as_location2("foo", 1L, names = "bar"), class = "vctrs_error_subscript_oob"))
  })

  expect_error(num_as_location(10L, 2L), class = "vctrs_error_subscript_oob")
  expect_error(num_as_location2(10L, 2L), class = "vctrs_error_subscript_oob")
})

test_that("vec_as_location() doesn't require `n` for character indexing", {
  expect_identical(vec_as_location("b", NULL, names = letters), 2L)
})

test_that("vec_as_location2() requires length 1 inputs", {
  expect_snapshot({
    (expect_error(vec_as_location2(1:2, 2L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location2(c("foo", "bar"), 2L, c("foo", "bar")), class = "vctrs_error_subscript_type"))

    "Idem with custom `arg`"
    (expect_error(vec_as_location2(1:2, 2L, arg = "foo", call = call("my_function")), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location2(mtcars, 10L, arg = "foo", call = call("my_function")), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location2(1:2, 2L, arg = "foo", call = call("my_function")), class = "vctrs_error_subscript_type"))
  })
})

test_that("vec_as_location2() requires positive integers", {
  expect_snapshot({
    (expect_error(vec_as_location2(0, 2L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location2(-1, 2L), class = "vctrs_error_subscript_type"))

    "Idem with custom `arg`"
    (expect_error(vec_as_location2(0, 2L, arg = "foo", call = call("my_function")), class = "vctrs_error_subscript_type"))
  })
})

test_that("vec_as_location2() fails with NA", {
  expect_snapshot({
    (expect_error(vec_as_location2(na_int, 2L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location2(na_chr, 1L, names = "foo"), class = "vctrs_error_subscript_type"))

    "Idem with custom `arg`"
    (expect_error(vec_as_location2(na_int, 2L, arg = "foo", call = call("my_function")), class = "vctrs_error_subscript_type"))
  })
})

test_that("vec_as_location2() doesn't allow lossy casts", {
  expect_error(vec_as_location2(2^31, 3L), class = "vctrs_error_subscript_type")

  # Lossy casts generate missing values, which are disallowed
  expect_error(allow_lossy_cast(vec_as_location2(2^31, 3L)), class = "vctrs_error_subscript_type")
})

test_that("all subscript errors inherit from `vctrs_error_subscript`", {
  expect_error(vec_as_location(100, 2L), class = "vctrs_error_subscript")
  expect_error(vec_as_location("foo", 2L, names = c("bar", "baz")), class = "vctrs_error_subscript")
  expect_error(vec_as_location(foobar(1L), 2L), class = "vctrs_error_subscript")
  expect_error(vec_as_location(1.5, 2L), class = "vctrs_error_subscript")
  expect_error(vec_as_location2(TRUE, 2L), class = "vctrs_error_subscript")
  expect_error(vec_as_location2(1.5, 2L), class = "vctrs_error_subscript")
})

test_that("all OOB errors inherit from `vctrs_error_subscript_oob`", {
  expect_error(vec_as_location(100, 2L), class = "vctrs_error_subscript_oob")
  expect_error(vec_as_location("foo", 2L, names = c("bar", "baz")), class = "vctrs_error_subscript_oob")
})

test_that("vec_as_location() preserves names if possible", {
  expect_identical(vec_as_location(c(a = 1L, b = 3L), 3L), c(a = 1L, b = 3L))
  expect_identical(vec_as_location(c(a = 1, b = 3), 3L), c(a = 1L, b = 3L))
  expect_identical(vec_as_location(c(a = "z", b = "y"), 26L, letters), c(a = 26L, b = 25L))

  expect_identical(vec_as_location(c(foo = TRUE, bar = FALSE, baz = TRUE), 3L), c(foo = 1L, baz = 3L))
  expect_identical(vec_as_location(c(foo = TRUE), 3L), c(foo = 1L, foo = 2L, foo = 3L))
  expect_identical(vec_as_location(c(foo = NA), 3L), c(foo = na_int, foo = na_int, foo = na_int))

  # Names of negative selections are dropped
  expect_identical(vec_as_location(c(a = -1L, b = -3L), 3L), 2L)
})

test_that("vec_as_location2() optionally allows missing values", {
  expect_identical(vec_as_location2(NA, 2L, missing = "propagate"), na_int)
  expect_error(vec_as_location2(NA, 2L, missing = "error"), class = "vctrs_error_subscript_type")
})

test_that("num_as_location2() optionally allows missing and negative locations", {
  expect_identical(num_as_location2(na_dbl, 2L, missing = "propagate"), na_int)
  expect_identical(num_as_location2(-1, 2L, negative = "ignore"), -1L)
  expect_error(num_as_location2(-3, 2L, negative = "ignore"), class = "vctrs_error_subscript_oob")
  expect_error(num_as_location2(0, 2L, negative = "ignore"), class = "vctrs_error_subscript_type")
})

test_that("num_as_location() optionally allows negative indices", {
  expect_identical(num_as_location(dbl(1, -1), 2L, negative = "ignore"), int(1L, -1L))
  expect_error(num_as_location(c(1, -10), 2L, negative = "ignore"), class = "vctrs_error_subscript_oob")
})

test_that("num_as_location() optionally forbids negative indices", {
  expect_snapshot({
    (expect_error(num_as_location(dbl(1, -1), 2L, negative = "error"), class = "vctrs_error_subscript_type"))
  })
  expect_error(num_as_location(c(1, -10), 2L, negative = "error"), class = "vctrs_error_subscript_type")
})

test_that("num_as_location() optionally ignores zero indices", {
  expect_identical(num_as_location(c(1, 0), 2L, zero = "ignore"), c(1L, 0L))
})

test_that("num_as_location() optionally forbids zero indices", {
  expect_snapshot({
    (expect_error(
      num_as_location(0L, 1L, zero = "error"),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      num_as_location(c(0, 0, 0, 0, 0, 0), 1, zero = "error"),
      class = "vctrs_error_subscript_type"
    ))
  })
})

test_that("vec_as_location() handles NULL", {
  expect_identical(
    vec_as_location(NULL, 10),
    vec_as_location(int(), 10),
  )
})

test_that("vec_as_location() checks for mix of negative and missing locations", {
  expect_snapshot({
    (expect_error(
      vec_as_location(-c(1L, NA), 30),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      vec_as_location(-c(1L, rep(NA, 10)), 30),
      class = "vctrs_error_subscript_type"
    ))
  })
})

test_that("vec_as_location() checks for mix of negative and positive locations", {
  expect_snapshot({
    (expect_error(
      vec_as_location(c(-1L, 1L), 30),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      vec_as_location(c(-1L, rep(1L, 10)), 30),
      class = "vctrs_error_subscript_type"
    ))
  })
})

test_that("logical subscripts must match size of indexed vector", {
  expect_snapshot({
    (expect_error(
      vec_as_location(c(TRUE, FALSE), 3),
      class = "vctrs_error_subscript_size"
    ))
  })
})

test_that("character subscripts require named vectors", {
  expect_snapshot({
    (expect_error(vec_as_location(letters[1], 3), "unnamed vector"))
  })
})

test_that("arg is evaluated lazily (#1150)", {
  expect_silent(vec_as_location(1, 1, arg = { writeLines("oof"); "boo" }))
})

test_that("arg works for complex expressions (#1150)", {
  expect_error(vec_as_location(mean, 1, arg = paste0("foo", "bar")), "foobar")
})

test_that("can optionally extend beyond the end", {
  expect_error(num_as_location(1:5, 3), class = "vctrs_error_subscript_oob")

  expect_identical(num_as_location(1:5, 3, oob = "extend"), 1:5)
  expect_identical(num_as_location(4:5, 3, oob = "extend"), 4:5)

  expect_snapshot({
    (expect_error(
      num_as_location(3, 1, oob = "extend"),
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      num_as_location(c(1, 3), 1, oob = "extend"),
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      num_as_location(c(1:5, 7), 3, oob = "extend"),
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      num_as_location(c(1:5, 7, 1), 3, oob = "extend"),
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      class = "vctrs_error_subscript_oob",
      num_as_location(c(1:5, 7, 1, 10), 3, oob = "extend")
    ))
  })
})

test_that("can extend beyond the end consecutively but non-monotonically (#1166)", {
  expect_identical(num_as_location(6:4, 3, oob = "extend"), 6:4)
  expect_identical(num_as_location(c(1:5, 7, 6), 3, oob = "extend"), c(1:5, 7L, 6L))
  expect_identical(num_as_location(c(1, NA, 4, 3), 2, oob = "extend"), c(1L, NA, 4L, 3L))
})

test_that("missing values are supported in error formatters", {
  expect_snapshot({
    (expect_error(
      num_as_location(c(1, NA, 2, 3), 1),
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      num_as_location(c(1, NA, 3), 1, oob = "extend"),
      class = "vctrs_error_subscript_oob"
    ))
  })
})

test_that("can disallow missing values", {
  expect_snapshot({
    (expect_error(
      vec_as_location(c(1, NA), 2, missing = "error"),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      vec_as_location(c(1, NA, 2, NA), 2, missing = "error", arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      with_tibble_cols(vec_as_location(c(1, NA, 2, NA), 2, missing = "error")),
      class = "vctrs_error_subscript_type"
    ))
  })
})

test_that("can customise subscript type errors", {
  expect_snapshot({
    "With custom `arg`"
    (expect_error(
      num_as_location(-1, 2, negative = "error", arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      num_as_location2(-1, 2, negative = "error", arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      vec_as_location2(0, 2, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      vec_as_location2(na_dbl, 2, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      vec_as_location2(c(1, 2), 2, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      vec_as_location(c(TRUE, FALSE), 3, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_size"
    ))
    (expect_error(
      vec_as_location(c(-1, NA), 3, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      vec_as_location(c(-1, 1), 3, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      num_as_location(c(1, 4), 2, oob = "extend", arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      num_as_location(0, 1, zero = "error", arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"
    ))

    "With tibble columns"
    (expect_error(
      with_tibble_cols(num_as_location(-1, 2, negative = "error")),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      with_tibble_cols(num_as_location2(-1, 2, negative = "error")),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      with_tibble_cols(vec_as_location2(0, 2)),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      with_tibble_cols(vec_as_location2(na_dbl, 2)),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      with_tibble_cols(vec_as_location2(c(1, 2), 2)),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      with_tibble_cols(vec_as_location(c(TRUE, FALSE), 3)),
      class = "vctrs_error_subscript_size"
    ))
    (expect_error(
      with_tibble_cols(vec_as_location(c(-1, NA), 3)),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      with_tibble_cols(vec_as_location(c(-1, 1), 3)),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      with_tibble_cols(num_as_location(c(1, 4), 2, oob = "extend")),
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      with_tibble_cols(num_as_location(0, 1, zero = "error")),
      class = "vctrs_error_subscript_type"
    ))
  })
})

test_that("can customise OOB errors", {
  expect_snapshot({
    (expect_error(
      vec_slice(set_names(letters), "foo"),
      class = "vctrs_error_subscript_oob"
    ))

    "With custom `arg`"
    (expect_error(
      vec_as_location(30, length(letters), arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      vec_as_location("foo", NULL, letters, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_oob"
    ))

    "With tibble columns"
    (expect_error(
      with_tibble_cols(vec_slice(set_names(letters), "foo")),
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      with_tibble_cols(vec_slice(set_names(letters), 30)),
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      with_tibble_cols(vec_slice(set_names(letters), -30)),
      class = "vctrs_error_subscript_oob"
    ))

    "With tibble rows"
    (expect_error(
      with_tibble_rows(vec_slice(set_names(letters), c("foo", "bar"))),
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      with_tibble_rows(vec_slice(set_names(letters), 1:30)),
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      with_tibble_rows(vec_slice(set_names(letters), -(1:30))),
      class = "vctrs_error_subscript_oob"
    ))
  })
})

test_that("num_as_location() requires non-S3 inputs", {
  expect_error(num_as_location(factor("foo"), 2), "must be a numeric vector")
})

test_that("vec_as_location() checks dimensionality", {
  expect_snapshot({
    (expect_error(vec_as_location(matrix(TRUE, nrow = 1), 3L), class = "vctrs_error_subscript_type"))
    (expect_error(vec_as_location(array(TRUE, dim = c(1, 1, 1)), 3L), class = "vctrs_error_subscript_type"))
    (expect_error(with_tibble_rows(vec_as_location(matrix(TRUE, nrow = 1), 3L)), class = "vctrs_error_subscript_type"))
  })
})

test_that("vec_as_location() works with vectors of dimensionality 1", {
  expect_identical(vec_as_location(array(TRUE, dim = 1), 3L), 1:3)
})

test_that("vec_as_location() UI", {
  expect_snapshot(error = TRUE, vec_as_location(1, 1L, missing = "bogus"))
})

test_that("num_as_location() UI", {
  expect_snapshot(error = TRUE, num_as_location(1, 1L, missing = "bogus"))
  expect_snapshot(error = TRUE, num_as_location(1, 1L, negative = "bogus"))
  expect_snapshot(error = TRUE, num_as_location(1, 1L, oob = "bogus"))
  expect_snapshot(error = TRUE, num_as_location(1, 1L, zero = "bogus"))
})

test_that("vec_as_location2() UI", {
  expect_snapshot(error = TRUE, vec_as_location2(1, 1L, missing = "bogus"))
})
