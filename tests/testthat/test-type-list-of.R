test_that("list_of inherits from list", {
  x1 <- list_of(1, 1)
  expect_s3_class(x1, "list")
})

test_that("list_of works like list", {
  x1 <- list_of(1, 1)
  expect_type(x1, "list")
  expect_s3_class(x1, "vctrs_list_of")
  expect_equal(attr(x1, "ptype"), double())

  x2 <- list_of(1, 1, .ptype = integer())
  expect_equal(attr(x1, "ptype"), integer())

  x3 <- as_list_of(list(1, 1))
  expect_equal(x3, x1)

  x4 <- list_of(a = 1, b = 2)
  expect_equal(x4$b, 2)
  expect_error(x4$c, "Invalid index", fixed = TRUE)
})

test_that("list_of errors if it can't find common type", {
  expect_error(list_of(1, "a"), class = "vctrs_error_incompatible_type")
  expect_error(list_of(), "find common type")
})

test_that("can use as_list_of to change type", {
  x1 <- list_of(1)
  expect_equal(as_list_of(x1), x1)

  x2 <- as_list_of(x1, .ptype = integer())
  expect_identical(x2[[1]], 1L)
})

test_that("is_list_of as expected", {
  expect_false(is_list_of(list(1)))
  expect_true(is_list_of(list_of(1)))
})

test_that("print method gives human friendly output", {
  skip_on_cran() # Depends on tibble
  expect_snapshot(list_of(1, 2:3))
  expect_snapshot(tibble::tibble(x = list_of(1, 2:3)))
})

test_that("str method is reasonably correct", {
  x <- list_of(1, 2:3)

  expect_snapshot(str(x))
  expect_snapshot(str(list(list(x, y = 2:1))))

  expect_snapshot(str(x[0]))
  expect_snapshot(str(list(list(x[0], y = 2:1))))
})

test_that("constructor requires list input", {
  expect_error(new_list_of(1), "must be a list")
  expect_error(new_list_of(mtcars), "must be a list")
})

test_that("constructor requires size 0 ptype", {
  expect_error(new_list_of(ptype = 1), "must have size 0")
})

test_that("can combine a mix of named and unnamed list-ofs (#784)", {
  a <- new_list_of(list(x = 1L), ptype = integer())
  b <- new_list_of(list(2L), ptype = integer())

  expect <- new_list_of(list(x = 1L, 2L), ptype = integer())

  expect_identical(vec_c(a, b), expect)
})

# Subsetting --------------------------------------------------------------

test_that("[ preserves type", {
  x <- list_of(1)
  expect_equal(x[1], x)
})

test_that("[<-, [[<- and $<- coerce their input", {
  x <- list_of(x = 1, y = 1, z = 1, w = 1)

  x[1] <- list(FALSE)
  expect_identical(x, list_of(x = 0, y = 1, z = 1, w = 1))

  x[[2]] <- FALSE
  expect_identical(x, list_of(x = 0, y = 0, z = 1, w = 1))

  x$z <- FALSE
  expect_identical(x, list_of(x = 0, y = 0, z = 0, w = 1))

  x[3:4] <- c(TRUE, FALSE)
  expect_identical(x, list_of(x = 0, y = 0, z = 1, w = 0))

  x[[2]] <- NULL
  expect_equal(x, list_of(x = 0, y = NULL, z = 1, w = 0))

  expect_error(x[[2]] <- list(20), class = "vctrs_error_incompatible_type")
  expect_error(x$y <- list(20), class = "vctrs_error_incompatible_type")

  x[3:4] <- list(NULL)
  expect_equal(x, list_of(x = 0, y = NULL, z = NULL, w = NULL))

  expect_equal(is.na(x), c(FALSE, TRUE, TRUE, TRUE))
})

test_that("assingment can increase size of vector", {
  x <- list_of(x = 1)
  x[[2]] <- 2
  x$z <- 3
  x[4:5] <- c(4,5)

  expect_length(x, 5)
})


# Type system -------------------------------------------------------------

test_that("list_of() are vectors", {
  expect_true(obj_is_vector(list_of(1)))
  expect_true(vec_is(list_of(1)))
})

test_that("list coercions are symmetric and unchanging", {
  types <- list(
    list(),
    list_of(.ptype = integer()),
    list_of(.ptype = double()),
    list_of(.ptype = character())
  )
  mat <- maxtype_mat(types)

  expect_true(isSymmetric(mat))

  local_options(width = 200)
  expect_snapshot(print(mat))
})

test_that("max<list_of<a>, list_of<b>> is list_of<max<a, b>>", {
  r_int <- list_of(.ptype = integer())
  r_dbl <- list_of(.ptype = double())

  expect_equal(vec_ptype_common(r_int, r_int), r_int)
  expect_equal(vec_ptype_common(r_int, r_dbl), r_int)
})

test_that("can cast to self type", {
  x <- list_of(1)
  expect_identical(vec_cast(x, x), x)
})

test_that("can cast between different list_of types", {
  x <- list_of(1, 2)
  to <- list_of(.ptype = integer())
  expect_identical(vec_cast(x, to), list_of(1L, 2L))
})

test_that("list_of casting retains outer names", {
  x <- list_of(x = 1, 2, z = 3)
  to <- list_of(.ptype = integer())
  expect_named(vec_cast(x, to), c("x", "", "z"))
})

test_that("safe casts work as expected", {
  x <- list_of(1)
  expect_equal(vec_cast(NULL, x), NULL)
  expect_equal(vec_cast(NA, x), list_of(NULL, .ptype = double()))

  expect_identical(vec_cast(list(1), x), list_of(1))
  expect_identical(vec_cast(list(TRUE), x), list_of(1))
  expect_identical(vec_cast(x, list()), list(1))
  expect_identical(vec_cast(x, list()), list(1))

  expect_error(
    vec_cast(list_of(1), list_of("")),
    class = "vctrs_error_incompatible_type"
  )

  # These used to be allowed
  expect_error(vec_cast(1L, x), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(1, x), class = "vctrs_error_incompatible_type")
})

test_that("error call is passed to inner cast methods", {
  fn1 <- function() vec_cast(list_of(1), list_of(""))
  fn2 <- function() vec_cast(list(1), list_of(""))
  expect_snapshot({
    (expect_error(fn1()))
    (expect_error(fn2()))
  })
})

test_that("lossy casts generate warning (no longer the case)", {
  # This used to be a lossy cast warning
  expect_error(
    vec_cast(list(c(1.5, 1), 1L), to = list_of(1L)),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), list_of(1)), class = "vctrs_error_incompatible_type")
})

test_that("list_of() has as.character() method (tidyverse/tidyr#654)", {
  exp <- rep(paste0("<", vec_ptype_abbr(mtcars), ">"), 2)
  expect_identical(as.character(list_of(mtcars, mtcars)), exp)
})

test_that("vec_ptype2(<list_of<>>, NA) is symmetric (#687)", {
  lof <- list_of(1, 2, 3)
  expect_identical(vec_ptype2(lof, NA), vec_ptype(lof))
  expect_identical(vec_ptype2(NA, lof), vec_ptype(lof))
})

test_that("list_of() coerces to list() and list_of() (#1701)", {
  expect_equal(vec_ptype_common(list_of(1), list()), list())
  expect_equal(vec_cast_common(list_of(1), list()), list(list(1), list()))

  expect_equal(vec_ptype_common(list_of(1), list("")), list())
  expect_equal(vec_cast_common(list_of(1), list("")), list(list(1), list("")))

  expect_equal(
    vec_ptype_common(list_of(1), list_of("")),
    list()
  )
  expect_equal(
    vec_ptype_common(list_of(1), list(), list_of("")),
    list()
  )
})

test_that("can concatenate list and list-of (#1161)", {
  expect_equal(
    vec_c(list(1), list_of(2)),
    list(1, 2)
  )
  expect_equal(
    vec_c(list(""), list_of(2)),
    list("", 2)
  )
})
