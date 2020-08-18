context("test-type-rcrd")


# constructor and accessors -----------------------------------------------

test_that("can construct and access components", {
  r <- new_rcrd(list(x = 1, y = 2))

  expect_equal(length(r), 1)
  expect_equal(n_fields(r), 2)

  expect_equal(names(r), NULL)
  expect_equal(fields(r), c("x", "y"))

  expect_error(r$x, class = "vctrs_error_unsupported")
  expect_equal(field(r, "x"), 1)
})

test_that("requires format method", {
  x <- new_rcrd(list(x = 1))
  expect_error(format(x), class = "vctrs_error_unimplemented")
})

test_that("vec_proxy() transforms records to data frames", {
  expect_identical(
    vec_proxy(new_rcrd(list(a = "1"))),
    new_data_frame(list(a = "1"))
  )
})


# coercion ----------------------------------------------------------------

test_that("can't cast list to rcrd", {
  l <- list(
    new_rcrd(list(a = "1", b = 3L)),
    new_rcrd(list(b = "4", a = 2))
  )
  expect_error(
    vec_cast(l, new_rcrd(list(a = 1L, b = 2L))),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("can recast rcrd from list", {
  r <- new_rcrd(list(x = integer(), y = numeric()))

  expect_equal(
    vec_restore(list(x = 1L, y = 1), r),
    new_rcrd(list(x = 1L, y = 1))
  )
})

test_that("can't cast rcrd to list", {
  r <- new_rcrd(list(x = 1:2, y = 2:3))
  expect_error(vec_cast(r, list()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(r, list()), class = "vctrs_error_incompatible_type")
})

test_that("default casts are implemented correctly", {
  r <- new_rcrd(list(x = 1, y = 1))

  expect_error(vec_cast(1, r), class = "vctrs_error_incompatible_type")
  expect_equal(vec_cast(NULL, r), NULL)
})

test_that("can't cast incompatible rcrd", {
  expect_error(
    vec_cast(
      new_rcrd(list(a = "1", b = 3L)),
      new_rcrd(list(a = "1"))
    ),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_cast(
      new_rcrd(list(a = "1", b = 3L)),
      new_rcrd(list(a = "1", c = 3L))
    ),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_cast(
      new_rcrd(list(a = "a", b = 3L)),
      new_rcrd(list(a = 1, b = 3L))
    ),
    class = "vctrs_error_incompatible_type"
  )
})

# input validation --------------------------------------------------------

test_that("must be list of equal length vectors", {
  expect_error(new_rcrd(list()), "list of length 1")
  expect_error(new_rcrd(list(x = environment())), class = "vctrs_error_scalar_type")
  expect_error(new_rcrd(list(x = 1:2, y = 1:3)), class = "vctrs_error_incompatible_size")
})

test_that("names must be unique", {
  expect_error(new_rcrd(list(1, 2)), class = "vctrs_error_names_cannot_be_empty")
  expect_error(new_rcrd(list(x = 1, 2)), class = "vctrs_error_names_cannot_be_empty")
  expect_error(new_rcrd(list(x = 1, x = 2)), class = "vctrs_error_names_must_be_unique")
  expect_error(new_rcrd(setNames(list(1, 2), "x")), "can't return `NA`")
})

test_that("subset assignment throws error", {
  x <- new_rcrd(list(x = 1))
  expect_error(
    x$y <- 2,
    class = "vctrs_error_unsupported"
  )
})

test_that("can supply data frame as fields", {
  expect_identical(
    new_rcrd(list(x = 1)),
    new_rcrd(tibble(x = 1))
  )
})

test_that("fields are recycled", {
  expect_identical(
    new_rcrd(list(x = 1, y = 1:2)),
    new_rcrd(list(x = c(1, 1), y = 1:2))
  )
})


# tuple class ----------------------------------------------------------
# use simple class to test essential features of rcrds

test_that("print and str use format", {
  local_tuple_methods()
  r <- tuple(1, 1:100)

  expect_known_output(
    file = test_path("test-rcrd-format.txt"),
    {
      print(r)
      cat("\n")
      str(r[1:10])
      cat("\n")
      str(list(list(list(r, 1:100))))
    }
  )
})

test_that("subsetting methods applied to each field", {
  local_tuple_methods()
  x <- tuple(1:2, 1)

  expect_equal(x[1], tuple(1, 1))
  expect_equal(x[[1]], tuple(1, 1))

  expect_equal(rep(tuple(1, 1), 2), tuple(c(1, 1), 1))

  length(x) <- 1
  expect_equal(x, tuple(1, 1))
})

test_that("subset assignment modifies each field", {
  local_tuple_methods()
  x <- tuple(c(1, 1), c(2, 2))

  x[[1]] <- tuple(3, 3)
  expect_equal(x, tuple(c(3, 1), c(3, 2)))

  x[1] <- tuple(4, 4)
  expect_equal(x, tuple(c(4, 1), c(4, 2)))
})

test_that("subset assignment recycles", {
  local_tuple_methods()

  x <- tuple(c(1, 1), c(2, 2))
  x[1:2] <- tuple(1, 1)
  expect_equal(x, tuple(c(1, 1), c(1, 1)))

  x[] <- tuple(2, 2)
  expect_equal(x, tuple(c(2, 2), c(2, 2)))
})

test_that("can sort rcrd", {
  local_tuple_methods()
  x <- tuple(c(1, 2, 1), c(3, 1, 2))
  expect_equal(xtfrm(x), c(2, 3, 1))
  expect_equal(order(x), c(3, 1, 2))
  expect_equal(sort(x), tuple(c(1, 1, 2), c(2, 3, 1)))
})

test_that("can use dictionary methods on a rcrd", {
  local_tuple_methods()
  x <- tuple(c(1, 2, 1), c(3, 1, 3))
  expect_equal(unique(x), x[1:2])
  expect_equal(duplicated(x), c(FALSE, FALSE, TRUE))
  expect_equal(anyDuplicated(x), TRUE)
})

test_that("cannot round trip through list", {
  local_tuple_methods()
  t <- tuple(1:2, 3:4)

  # Used to be allowed
  expect_error(vec_cast(t, list()), class = "vctrs_error_incompatible_type")
})

test_that("can convert to list using as.list() or vec_chop() (#1113)", {
  local_tuple_methods()
  t <- tuple(1:2, 3:4)

  expect <- list(tuple(1L, 3L), tuple(2L, 4L))

  expect_identical(as.list(t), expect)
  expect_identical(vec_chop(t), expect)
})

test_that("dangerous methods marked as unimplemented", {
  local_tuple_methods()
  t <- tuple()

  expect_error(mean(t), class = "vctrs_error_unsupported")
  expect_error(abs(t), class = "vctrs_error_unsupported")
  expect_error(is.finite(t), class = "vctrs_error_unsupported")
  expect_error(is.nan(t), class = "vctrs_error_unsupported")
})


# slicing -----------------------------------------------------------------

test_that("dots are forwarded", {
  expect_error(new_rcrd(list(foo = "foo"))[1, 2], "incorrect number of dimensions")
})

test_that("records are restored after slicing the proxy", {
  expect_identical(new_rcrd(list(x = 1:2))[1], new_rcrd(list(x = 1L)))
})

test_that("can supply df-cols as fields", {
  x <- new_rcrd(data_frame(x = data_frame(y = 1:2)))

  out <- vec_slice(x, 2)
  expect_identical(
    out,
    new_rcrd(data_frame(x = data_frame(y = 2L)))
  )
})
