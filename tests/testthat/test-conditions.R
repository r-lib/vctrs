
test_that("conditions inherit from `vctrs_error`", {
  expect_error(stop_incompatible(NULL, NULL), class = "vctrs_error")
  expect_error(stop_incompatible_type(NULL, NULL, x_arg = "x", y_arg = "y"), class = "vctrs_error")
  expect_error(stop_incompatible_cast(NULL, NULL, x_arg = "x", to_arg = "to"), class = "vctrs_error")
  expect_error(stop_incompatible_op("", NULL, NULL), class = "vctrs_error")
  expect_error(stop_incompatible_size(NULL, NULL, 0, 0, x_arg = "x", y_arg = "y"), class = "vctrs_error")
  expect_error(maybe_lossy_cast(NULL, NULL, NULL, TRUE, x_arg = "x", to_arg = "to"), class = "vctrs_error")
  expect_error(stop_unsupported("", ""), class = "vctrs_error")
  expect_error(stop_unimplemented("", ""), class = "vctrs_error")
  expect_error(stop_scalar_type(NULL), class = "vctrs_error")
  expect_error(stop_names(), class = "vctrs_error")
  expect_error(stop_names_cannot_be_empty(""), class = "vctrs_error")
  expect_error(stop_names_cannot_be_dot_dot("..1"), class = "vctrs_error")
  expect_error(stop_names_must_be_unique("x"), class = "vctrs_error")
})

test_that("incompatible cast throws an incompatible type error", {
  err <- expect_error(
    stop_incompatible_cast(1, 2, x_arg = "x", to_arg = "to"),
    class = "vctrs_error_incompatible_type"
  )

  expect_equal(err$x, 1)
  expect_equal(err$y, 2)
  expect_equal(err$x_arg, "x")
  expect_equal(err$y_arg, "to")

  # Convenience aliases
  expect_equal(err$to, err$y)
  expect_equal(err$to_arg, err$y_arg)
})

test_that("incompatible type error validates `action`", {
  expect_snapshot({
    (expect_error(stop_incompatible_type(1, 1, x_arg = "", y_arg = "", action = "c")))
    (expect_error(stop_incompatible_type(1, 1, x_arg = "", y_arg = "", action = 1)))
  })
})

test_that("can override arg in OOB conditions", {
  expect_snapshot({
    (expect_error(
      with_subscript_data(
        vec_slice(set_names(letters), "foo"),
        NULL
      ),
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      with_subscript_data(
        vec_slice(set_names(letters), "foo"),
        quote(foo)
      ),
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      with_subscript_data(
        vec_slice(set_names(letters), "foo"),
        quote(foo(bar))
      ),
      class = "vctrs_error_subscript_oob"
    ))
  })
})

test_that("scalar type errors are informative", {
  expect_snapshot({
    (expect_error(
      vec_slice(foobar(list(1)), 1),
      class = "vctrs_error_scalar_type"
    ))
    (expect_error(
      stop_scalar_type(foobar(list(1)), arg = "foo"),
      class = "vctrs_error_scalar_type"
    ))
  })
})

test_that("empty names errors are informative", {
  expect_snapshot({
    (expect_error(
      vec_as_names(c("x", "", "y"), repair = "check_unique"),
      class = "vctrs_error_names_cannot_be_empty"
    ))
    (expect_error(
      vec_as_names(c("x", "", "y", ""), repair = "check_unique"),
      class = "vctrs_error_names_cannot_be_empty"
    ))
    (expect_error(
      vec_as_names(rep("", 10), repair = "check_unique"),
      class = "vctrs_error_names_cannot_be_empty"
    ))
  })
})

test_that("dot dot names errors are informative", {
  expect_snapshot({
    (expect_error(
      vec_as_names(c("..1", "..1", "..1", "...", "z"), repair = "check_unique"),
      class = "vctrs_error_names_cannot_be_dot_dot"
    ))
    (expect_error(
      vec_as_names(c(rep("..1", 20), rep(c("..2", "..3", "..4", "...", "..5"), 2)), repair = "check_unique"),
      class = "vctrs_error_names_cannot_be_dot_dot"
    ))
  })
})

test_that("unique names errors are informative", {
  expect_snapshot({
    (expect_error(
      vec_as_names(c("x", "x", "x", "y", "y", "z"), repair = "check_unique"),
      class = "vctrs_error_names_must_be_unique"
    ))
    (expect_error(
      vec_as_names(c(rep("x", 20), rep(c("a", "b", "c", "d", "e"), 2)), repair = "check_unique"),
      class = "vctrs_error_names_must_be_unique"
    ))
  })
})

test_that("can't supply both `message` and `details`", {
  expect_error(
    stop_incompatible_type(1, 2, message = "my message", x_arg = "x", y_arg = "y"),
    "my message",
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    stop_incompatible_type(1, 2, message = "my message", details = "my details", x_arg = "x", y_arg = "y"),
    "Can't supply both `message` and `details`."
  )
})

test_that("lossy cast errors are internal", {
  # Should not trigger testthat warnings about untested class
  expect_error(vec_cast(mtcars, mtcars[1:3]), "convert")
  expect_error(vec_cast(1.5, int()), "convert")
})

test_that("lossy cast from character to factor mentions loss of generality", {
  expect_snapshot({
    (expect_error(vec_cast("a", factor("b")), class = "vctrs_error_cast_lossy"))
  })
})

test_that("ordered cast failures mention conversion", {
  expect_snapshot({
    (expect_error(
      vec_cast(ordered("x"), ordered("y")),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("incompatible size errors", {
  expect_snapshot({
    (expect_error(stop_incompatible_size(1:2, 3:5, 2L, 3L, x_arg = "", y_arg = "")))
    (expect_error(stop_incompatible_size(1:2, 3:5, 2L, 3L, x_arg = quote(foo), y_arg = "")))
    (expect_error(stop_incompatible_size(1:2, 3:5, 2L, 3L, x_arg = "", y_arg = "bar")))
    (expect_error(stop_incompatible_size(1:2, 3:5, 2L, 3L, x_arg = quote(foo), y_arg = quote(bar))))
  })
})

test_that("simplified backtraces include whole vctrs context", {
  skip_on_cran()

  top <- current_env()
  trace <- NULL
  expect_error(withCallingHandlers(vec_slice(1, 2), error = function(...) {
    trace <<- trace_back(top, sys.frame(-1L))
  }))

  trace_lines <- format(trace, simplify = "branch")
  expect_true(any(grepl("vec_slice", trace_lines)))
})
