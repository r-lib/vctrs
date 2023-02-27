testthat_import_from <- function(ns, names, env = caller_env()) {
  skip_if_not_installed(ns)
  import_from(ns, names, env = env)
}

shaped_int <- function(...) {
  array(NA_integer_, c(...))
}

set_rownames_recursively <- function(x, i = NULL) {
  n <- vec_size(x)
  stopifnot(n <= length(letters))

  for (j in seq_along(x)) {
    if (is.data.frame(x[[j]])) {
      x[[j]] <- set_rownames_recursively(x[[j]], i = i)
    }
  }

  row.names(x) <- paste0(letters[seq_len(n)], i)
  x
}

expect_waldo_equal <- function(type, act, exp, info, ...) {
  comp <- waldo::compare(act$val, exp$val, ..., x_arg = "actual", y_arg = "expected")
  expect(
    length(comp) == 0,
    sprintf(
      "`actual` (%s) not %s to `expected` (%s).\n\n%s",
      act$lab, type, exp$lab, paste0(comp, collapse = "\n\n")
    ),
    info = info
  )

  invisible(act$val)
}

expect_identical <- function(object, expected, info = NULL, label = NULL,
                             expected.label = NULL, ...) {
  act <- quasi_label(enquo(object), label, arg = "object")
  exp <- quasi_label(enquo(expected), expected.label, arg = "expected")

  expect_waldo_equal("identical", act, exp, info, ...)
}

expect_equal <- function(object, expected, ...,
                         tolerance = .Machine$double.eps ^ 0.5,
                         info = NULL, label = NULL,
                         expected.label = NULL) {
  act <- quasi_label(enquo(object), label, arg = "object")
  exp <- quasi_label(enquo(expected), expected.label, arg = "expected")

  expect_waldo_equal("equal", act, exp, info, ..., tolerance = tolerance)
}

raw2 <- function(...) {
  as.raw(list_unchop(list2(...), ptype = integer()))
}
