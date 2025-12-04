# ------------------------------------------------------------------------------
# new_list_of

test_that("works without any arguments", {
  # Default is logical type, no size restriction
  expect_identical(
    new_list_of(),
    new_list_of(x = list(), ptype = logical(), size = NULL)
  )
})

test_that("constructor requires list input", {
  expect_snapshot(error = TRUE, {
    new_list_of(1)
  })
  expect_snapshot(error = TRUE, {
    new_list_of(mtcars)
  })
})

test_that("must lock at least one of ptype or size", {
  expect_snapshot(error = TRUE, {
    new_list_of(ptype = NULL, size = NULL)
  })
})

test_that("can lock ptype, size, or both", {
  x <- new_list_of(ptype = integer(), size = NULL)
  expect_identical(list_of_ptype(x), integer())
  expect_null(list_of_size(x))

  x <- new_list_of(ptype = NULL, size = 5L)
  expect_null(list_of_ptype(x))
  expect_identical(list_of_size(x), 5L)

  x <- new_list_of(ptype = integer(), size = 5L)
  expect_identical(list_of_ptype(x), integer())
  expect_identical(list_of_size(x), 5L)
})

test_that("validates `ptype`", {
  expect_snapshot(error = TRUE, {
    new_list_of(ptype = lm(1 ~ 1))
  })

  x <- new_list_of(ptype = 1:5)
  expect_identical(list_of_ptype(x), integer())
})

test_that("finalizes `ptype`", {
  x <- new_list_of(ptype = unspecified())
  expect_identical(list_of_ptype(x), logical())
})

test_that("validates `size`", {
  expect_snapshot(error = TRUE, {
    new_list_of(size = 1.1)
  })
  expect_snapshot(error = TRUE, {
    new_list_of(size = 1:2)
  })
  expect_snapshot(error = TRUE, {
    new_list_of(size = -5)
  })
})

test_that("has vctrs classes", {
  expect_identical(
    class(new_list_of()),
    c("vctrs_list_of", "vctrs_vctr", "list")
  )
})

test_that("can add extra class", {
  expect_s3_class(new_list_of(class = "foo_list_of"), "foo_list_of")
})

test_that("can add extra attributes", {
  x <- new_list_of(foo = "bar")
  expect_identical(attr(x, "foo"), "bar")
})

# ------------------------------------------------------------------------------
# is_list_of

test_that("is_list_of works as expected", {
  expect_false(is_list_of(list(1)))
  expect_true(is_list_of(list_of(1)))
})

test_that("can check for list of", {
  expect_snapshot(error = TRUE, {
    check_list_of(1)
  })
})

# ------------------------------------------------------------------------------
# list_of

test_that("default behavior infers ptype and doesn't lock size", {
  x <- list_of(1)
  expect_identical(list_of_ptype(x), double())
  expect_null(list_of_size(x))
})

test_that("errors if can't determine type", {
  expect_snapshot(error = TRUE, {
    list_of(.ptype = NULL)
  })
  expect_snapshot(error = TRUE, {
    list_of(1, "a")
  })
})

test_that("errors if can't determine size", {
  expect_snapshot(error = TRUE, {
    list_of(.ptype = zap(), .size = NULL)
  })
  expect_snapshot(error = TRUE, {
    list_of(1:2, 3:5, .ptype = zap(), .size = NULL)
  })
})

test_that("can specify ptype, size, or both", {
  x <- list_of(.ptype = integer(), .size = zap())
  expect_identical(list_of_ptype(x), integer())
  expect_null(list_of_size(x))

  x <- list_of(.ptype = zap(), .size = 5L)
  expect_null(list_of_ptype(x))
  expect_identical(list_of_size(x), 5L)

  x <- list_of(.ptype = integer(), .size = 5L)
  expect_identical(list_of_ptype(x), integer())
  expect_identical(list_of_size(x), 5L)
})

# ------------------------------------------------------------------------------
# as_list_of

test_that("can convert from list to list-of", {
  x <- list(1)
  expect_identical(as_list_of(x), list_of(1))
})

test_that("default behavior infers ptype and doesn't lock size", {
  x <- as_list_of(list(1))
  expect_identical(list_of_ptype(x), double())
  expect_null(list_of_size(x))
})

test_that("errors if can't determine type", {
  expect_snapshot(error = TRUE, {
    as_list_of(list())
  })
})

test_that("errors if can't determine size", {
  expect_snapshot(error = TRUE, {
    as_list_of(list(), .ptype = integer(), .size = NULL)
  })
})

test_that("can specify ptype, size, or both", {
  x <- as_list_of(list(), .ptype = integer(), .size = zap())
  expect_identical(list_of_ptype(x), integer())
  expect_null(list_of_size(x))

  x <- as_list_of(list(), .ptype = zap(), .size = 5L)
  expect_null(list_of_ptype(x))
  expect_identical(list_of_size(x), 5L)

  x <- as_list_of(list(), .ptype = integer(), .size = 5L)
  expect_identical(list_of_ptype(x), integer())
  expect_identical(list_of_size(x), 5L)
})

test_that("as_list_of on list_of is a no-op", {
  x <- list_of(1)
  expect_identical(as_list_of(x), x)

  # Used to be able to do this in `as_list_of.vctrs_list_of`, but we determined
  # you should just go through `list_of()` again.
  expect_identical(as_list_of(x, .ptype = integer()), x)
})

# ------------------------------------------------------------------------------
# as.list()

test_that("can convert to base list", {
  x <- list_of(1)
  expect_identical(as.list(x), list(1))
})

# ------------------------------------------------------------------------------
# as.character()

test_that("list_of() has as.character() method (tidyverse/tidyr#654)", {
  exp <- rep(paste0("<", vec_ptype_abbr(mtcars), ">"), 2)
  expect_identical(as.character(list_of(mtcars, mtcars)), exp)
})

# ------------------------------------------------------------------------------
# Formatting

test_that("can print empty list-of", {
  expect_snapshot(list_of(.ptype = integer(), .size = 5L))
})

test_that("print method gives human friendly output", {
  # Just ptype
  x <- list_of(1, 2:3, .ptype = double(), .size = zap())
  expect_snapshot(cat(vec_ptype_full(x)))
  expect_snapshot(cat(vec_ptype_abbr(x)))

  # Just size
  x <- list_of(1, 2:3, .ptype = zap(), .size = 2L)
  expect_snapshot(cat(vec_ptype_full(x)))
  expect_snapshot(cat(vec_ptype_abbr(x)))

  # Both ptype and size
  x <- list_of(1, 2:3, .ptype = double(), .size = 2L)
  expect_snapshot(cat(vec_ptype_full(x)))
  expect_snapshot(cat(vec_ptype_abbr(x)))
})

test_that("print method gives human friendly output for multi line types", {
  ptype <- data_frame(x = integer(), y = double(), z = character())

  # Just ptype
  x <- list_of(.ptype = ptype, .size = zap())
  expect_snapshot(cat(vec_ptype_full(x)))
  expect_snapshot(cat(vec_ptype_abbr(x)))

  # Just size
  x <- list_of(.ptype = zap(), .size = 2L)
  expect_snapshot(cat(vec_ptype_full(x)))
  expect_snapshot(cat(vec_ptype_abbr(x)))

  # Both ptype and size
  x <- list_of(.ptype = ptype, .size = 2L)
  expect_snapshot(cat(vec_ptype_full(x)))
  expect_snapshot(cat(vec_ptype_abbr(x)))
})

test_that("str method is reasonably correct", {
  x <- list_of(1, 2:3)

  expect_snapshot(str(x))
  expect_snapshot(str(list(list(x, y = 2:1))))

  expect_snapshot(str(x[0]))
  expect_snapshot(str(list(list(x[0], y = 2:1))))
})

# ------------------------------------------------------------------------------
# Subsetting

test_that("[ preserves type", {
  x <- list_of(1)
  expect_identical(x[1], x)

  x <- list_of(1, .ptype = double(), .size = 1)
  expect_identical(x[1], x)
})

test_that("[[ works", {
  x <- list_of(a = 1, b = 2)

  expect_identical(x[[1]], 1)
  expect_identical(x[["b"]], 2)

  expect_snapshot(error = TRUE, {
    x[[3]]
  })
  expect_snapshot(error = TRUE, {
    x[["c"]]
  })
})

test_that("$ works", {
  x <- list_of(a = 1, b = 2)

  expect_identical(x$b, 2)

  expect_snapshot(error = TRUE, {
    x$c
  })
})

test_that("[<- coerces and recycles", {
  # Just type
  x <- list_of(a = 1, b = 2, .ptype = double(), .size = zap())

  x[1] <- list(NULL)
  expect_identical(
    x,
    list_of(a = NULL, b = 2, .ptype = double(), .size = zap())
  )

  x[1] <- list(3)
  expect_identical(x[[1]], 3)

  # Casts automatically
  x[1] <- list(4:5)
  expect_identical(x[[1]], c(4, 5))

  expect_snapshot(error = TRUE, {
    x[1] <- list("5")
  })

  # Just size
  x <- list_of(a = 1:2, b = c("c", "d"), .ptype = zap(), .size = 2L)

  x[1] <- list(NULL)
  expect_identical(
    x,
    list_of(a = NULL, b = c("c", "d"), .ptype = zap(), .size = 2L)
  )

  x[1] <- list(c(5, 6))
  expect_identical(x[[1]], c(5, 6))

  # Recycles automatically
  x[1] <- list(7)
  expect_identical(x[[1]], c(7, 7))

  expect_snapshot(error = TRUE, {
    x[1] <- list(c(1, 2, 3))
  })
})

test_that("[[<- coerces and recycles", {
  # Just type
  x <- list_of(a = 1, b = 2, .ptype = double(), .size = zap())

  x[[1]] <- 3
  expect_identical(x[[1]], 3)

  # Casts automatically
  x[[1]] <- 4:5
  expect_identical(x[[1]], c(4, 5))

  expect_snapshot(error = TRUE, {
    x[[1]] <- "5"
  })

  # Just size
  x <- list_of(a = 1:2, b = c("c", "d"), .ptype = zap(), .size = 2L)

  x[[1]] <- c(5, 6)
  expect_identical(x[[1]], c(5, 6))

  # Recycles automatically
  x[[1]] <- 7
  expect_identical(x[[1]], c(7, 7))

  expect_snapshot(error = TRUE, {
    x[[1]] <- c(1, 2, 3)
  })
})

test_that("$<- coerces and recycles", {
  # Just type
  x <- list_of(a = 1, b = 2, .ptype = double(), .size = zap())

  x$a <- 3
  expect_identical(x$a, 3)

  # Casts automatically
  x$a <- 4:5
  expect_identical(x$a, c(4, 5))

  expect_snapshot(error = TRUE, {
    x$a <- "5"
  })

  # Just size
  x <- list_of(a = 1:2, b = c("c", "d"), .ptype = zap(), .size = 2L)

  x$a <- c(5, 6)
  expect_identical(x$a, c(5, 6))

  # Recycles automatically
  x$a <- 7
  expect_identical(x$a, c(7, 7))

  expect_snapshot(error = TRUE, {
    x$a <- c(1, 2, 3)
  })
})

test_that("[<- chops non list `value`s", {
  # Chops into `list(4, 5)` before casting or recycling
  x <- list_of(a = 1, b = 2, c = 3)
  x[2:3] <- c(4L, 5L)
  expect_identical(x, list_of(a = 1, b = 4, c = 5))

  # Chops into `list(7, 8)` before casting or recycling
  x <- list_of(a = 1:2, b = 2:3, c = 3:4, .size = 2L)
  x[2:3] <- c(7, 8)
  expect_identical(
    x,
    list_of(a = 1:2, b = c(7L, 7L), c = c(8L, 8L), .size = 2L)
  )
})

test_that("[<- can shorten with `NULL` (#2112)", {
  x <- list_of(a = 1, b = 2, c = 3)
  x[2] <- NULL
  expect_identical(x, list_of(a = 1, c = 3))

  x <- list_of(a = 1, b = 2, c = 3)
  x[2:3] <- NULL
  expect_identical(x, list_of(a = 1))

  # Like base lists
  x <- list(a = 1, b = 2, c = 3)
  x[2] <- NULL
  expect_identical(x, list(a = 1, c = 3))
})

test_that("[[<- can shorten with `NULL` (#2112)", {
  x <- list_of(a = 1, b = 2)
  x[[2]] <- NULL
  expect_identical(x, list_of(a = 1))

  x <- list_of(a = 1, b = 2)
  x[["b"]] <- NULL
  expect_identical(x, list_of(a = 1))

  # Like base lists
  x <- list(a = 1, b = 2)
  x[[2]] <- NULL
  expect_identical(x, list(a = 1))
})

test_that("$<- can shorten with `NULL` (#2112)", {
  x <- list_of(a = 1, b = 2)
  x$b <- NULL
  expect_identical(x, list_of(a = 1))

  # Like base lists
  x <- list(a = 1, b = 2)
  x$b <- NULL
  expect_identical(x, list(a = 1))
})

test_that("assignment can increase size of vector", {
  x <- list_of(x = 1)
  x[[2]] <- 2
  x$z <- 3
  x[4:5] <- c(4, 5)

  expect_length(x, 5)
})

# ------------------------------------------------------------------------------
# Type system

test_that("list_of() are vectors", {
  expect_true(obj_is_vector(list_of(1)))
  expect_true(vec_is(list_of(1)))
})

test_that("ptype2: list + list_of", {
  expect_identical(vec_ptype2(list(), new_list_of()), list())
  expect_identical(vec_ptype2(new_list_of(), list()), list())
})

test_that("ptype2: list_of + list_of", {
  # Self-self
  x <- list_of(.ptype = integer(), .size = zap())
  expect_identical(vec_ptype2(x, x), x)

  x <- list_of(.ptype = zap(), .size = 2L)
  expect_identical(vec_ptype2(x, x), x)

  x <- list_of(.ptype = integer(), .size = 2L)
  expect_identical(vec_ptype2(x, x), x)

  # All `size` variants with one fixed `ptype`
  x <- list_of(.ptype = integer(), .size = zap())
  y <- list_of(.ptype = zap(), .size = 2L)
  expect_identical(vec_ptype2(x, y), list())
  expect_identical(vec_ptype2(y, x), list())

  x <- list_of(.ptype = integer(), .size = 2L)
  y <- list_of(.ptype = zap(), .size = 2L)
  expect_identical(vec_ptype2(x, y), list_of(.ptype = zap(), .size = 2L))
  expect_identical(vec_ptype2(y, x), list_of(.ptype = zap(), .size = 2L))

  x <- list_of(.ptype = integer(), .size = 1L)
  y <- list_of(.ptype = zap(), .size = 2L)
  expect_identical(vec_ptype2(x, y), list_of(.ptype = zap(), .size = 2L))
  expect_identical(vec_ptype2(y, x), list_of(.ptype = zap(), .size = 2L))

  x <- list_of(.ptype = integer(), .size = 2L)
  y <- list_of(.ptype = zap(), .size = 3L)
  expect_identical(vec_ptype2(x, y), list())
  expect_identical(vec_ptype2(y, x), list())

  # All `ptype` variants with one fixed `size`
  x <- list_of(.ptype = zap(), .size = 1L)
  y <- list_of(.ptype = integer(), .size = zap())
  expect_identical(vec_ptype2(x, y), list())
  expect_identical(vec_ptype2(y, x), list())

  x <- list_of(.ptype = integer(), .size = 1L)
  y <- list_of(.ptype = integer(), .size = zap())
  expect_identical(vec_ptype2(x, y), list_of(.ptype = integer(), .size = zap()))
  expect_identical(vec_ptype2(y, x), list_of(.ptype = integer(), .size = zap()))

  x <- list_of(.ptype = integer(), .size = 1L)
  y <- list_of(.ptype = double(), .size = zap())
  expect_identical(vec_ptype2(x, y), list_of(.ptype = double(), .size = zap()))
  expect_identical(vec_ptype2(y, x), list_of(.ptype = double(), .size = zap()))

  x <- list_of(.ptype = integer(), .size = 1L)
  y <- list_of(.ptype = character(), .size = zap())
  expect_identical(vec_ptype2(x, y), list())
  expect_identical(vec_ptype2(y, x), list())

  # All variants with both `size` and `ptype` fixed
  x <- list_of(.ptype = integer(), .size = 1L)
  y <- list_of(.ptype = integer(), .size = 1L)
  expect_identical(vec_ptype2(x, y), x)
  expect_identical(vec_ptype2(y, x), x)

  x <- list_of(.ptype = integer(), .size = 2L)
  y <- list_of(.ptype = double(), .size = 1L)
  expect_identical(vec_ptype2(x, y), list_of(.ptype = double(), .size = 2L))
  expect_identical(vec_ptype2(y, x), list_of(.ptype = double(), .size = 2L))

  x <- list_of(.ptype = integer(), .size = 2L)
  y <- list_of(.ptype = double(), .size = 3L)
  expect_identical(vec_ptype2(x, y), list_of(.ptype = double(), .size = zap()))
  expect_identical(vec_ptype2(y, x), list_of(.ptype = double(), .size = zap()))

  x <- list_of(.ptype = integer(), .size = 2L)
  y <- list_of(.ptype = character(), .size = 1L)
  expect_identical(vec_ptype2(x, y), list_of(.ptype = zap(), .size = 2L))
  expect_identical(vec_ptype2(y, x), list_of(.ptype = zap(), .size = 2L))
})

test_that("cast: list_of to list", {
  x <- list_of(1, .ptype = double(), .size = zap())
  expect_identical(vec_cast(x, list()), list(1))

  x <- list_of(1, .ptype = zap(), .size = 1L)
  expect_identical(vec_cast(x, list()), list(1))

  x <- list_of(1, .ptype = double(), .size = 1L)
  expect_identical(vec_cast(x, list()), list(1))
})

test_that("cast: list to list_of", {
  # Just type
  to <- list_of(1, .ptype = double(), .size = zap())
  expect_identical(
    vec_cast(list(), to),
    list_of(.ptype = double(), .size = zap())
  )
  expect_snapshot(error = TRUE, {
    vec_cast(list("x"), to)
  })

  # Just size
  to <- list_of(1:2, .ptype = zap(), .size = 2L)
  expect_identical(
    vec_cast(list(), to),
    list_of(.ptype = zap(), .size = 2L)
  )
  expect_identical(
    vec_cast(list(1, "x"), to),
    list_of(c(1, 1), c("x", "x"), .ptype = zap(), .size = 2L)
  )
  expect_snapshot(error = TRUE, {
    vec_cast(list(1:3), to)
  })

  # Both type and size
  to <- list_of(1:2, .ptype = integer(), .size = 2L)
  expect_identical(
    vec_cast(list(), to),
    list_of(.ptype = integer(), .size = 2L)
  )
  expect_identical(
    vec_cast(list(1, 2:3), to),
    list_of(c(1L, 1L), 2:3, .ptype = integer(), .size = 2L)
  )
  expect_snapshot(error = TRUE, {
    vec_cast(list(1:3), to)
  })
  expect_snapshot(error = TRUE, {
    vec_cast(list("x"), to)
  })
})

test_that("cast: list_of to list_of", {
  # Starts just type restricted
  x <- list_of(1, 2)

  # To a different type restricted
  to <- list_of(.ptype = integer(), .size = zap())
  expect_identical(vec_cast(x, to), list_of(1L, 2L))

  to <- list_of(.ptype = character(), .size = zap())
  expect_snapshot(error = TRUE, {
    vec_cast(x, to)
  })

  # To a size restricted instead
  to <- list_of(.ptype = zap(), .size = 1L)
  expect_identical(vec_cast(x, to), list_of(1, 2, .ptype = zap(), .size = 1L))

  to <- list_of(.ptype = zap(), .size = 2L)
  expect_identical(
    vec_cast(x, to),
    list_of(c(1, 1), c(2, 2), .ptype = zap(), .size = 2L)
  )

  y <- list_of(1:2, 2:3)
  to <- list_of(.ptype = zap(), .size = 3L)
  expect_snapshot(error = TRUE, {
    vec_cast(y, to)
  })

  # To both type and size restricted
  to <- list_of(.ptype = integer(), .size = 1L)
  expect_identical(
    vec_cast(x, to),
    list_of(1L, 2L, .ptype = integer(), .size = 1L)
  )

  to <- list_of(.ptype = integer(), .size = 2L)
  expect_identical(
    vec_cast(x, to),
    list_of(c(1L, 1L), c(2L, 2L), .ptype = integer(), .size = 2L)
  )
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

  expect_identical(vec_ptype_common(r_int, r_int), r_int)
  expect_identical(vec_ptype_common(r_dbl, r_dbl), r_dbl)
  expect_identical(vec_ptype_common(r_int, r_dbl), r_dbl)
  expect_identical(vec_ptype_common(r_dbl, r_int), r_dbl)

  r_one <- list_of(.ptype = zap(), .size = 1L)
  r_two <- list_of(.ptype = zap(), .size = 2L)

  expect_identical(vec_ptype_common(r_one, r_one), r_one)
  expect_identical(vec_ptype_common(r_two, r_two), r_two)
  expect_identical(vec_ptype_common(r_one, r_two), r_two)
  expect_identical(vec_ptype_common(r_two, r_one), r_two)
})

test_that("can cast to self type", {
  x <- list_of(1, .ptype = double(), .size = zap())
  expect_identical(vec_cast(x, x), x)

  x <- list_of(c(1, 2), .ptype = zap(), .size = 2L)
  expect_identical(vec_cast(x, x), x)

  x <- list_of(c(1, 2), .ptype = double(), .size = 2L)
  expect_identical(vec_cast(x, x), x)
})

test_that("list_of casting retains outer names", {
  x <- list_of(x = 1, 2, z = 3)

  to <- list_of(.ptype = integer())
  expect_named(vec_cast(x, to), c("x", "", "z"))

  to <- list_of(.ptype = zap(), .size = 2L)
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
  expect_error(
    vec_cast(factor("a"), list_of(1)),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("vec_ptype2(<list_of<>>, NA) is symmetric (#687)", {
  lof <- list_of(1, 2, 3, .ptype = double(), .size = zap())
  expect_identical(vec_ptype2(lof, NA), vec_ptype(lof))
  expect_identical(vec_ptype2(NA, lof), vec_ptype(lof))

  lof <- list_of(1, 2, 3, .ptype = zap(), .size = 1L)
  expect_identical(vec_ptype2(lof, NA), vec_ptype(lof))
  expect_identical(vec_ptype2(NA, lof), vec_ptype(lof))
})

test_that("list_of() coerces to list() and list_of() (#1701)", {
  expect_equal(vec_ptype_common(list_of(1), list()), list())
  expect_equal(vec_cast_common(list_of(1), list()), list(list(1), list()))

  expect_equal(vec_ptype_common(list_of(1), list("")), list())
  expect_equal(vec_cast_common(list_of(1), list("")), list(list(1), list("")))

  # Fallback on common type failure
  expect_equal(
    vec_ptype_common(list_of(1), list_of("")),
    list()
  )
  expect_equal(
    vec_ptype_common(list_of(1), list(), list_of("")),
    list()
  )

  # Fallback on common size failure
  expect_equal(
    vec_ptype_common(
      list_of(1:2, .ptype = zap(), .size = 2L),
      list_of(1:3, .ptype = zap(), .size = 3L)
    ),
    list()
  )
  expect_equal(
    vec_ptype_common(
      list_of(1:2, .ptype = zap(), .size = 2L),
      list(),
      list_of(1:3, .ptype = zap(), .size = 3L)
    ),
    list()
  )
})

test_that("can concatenate list and list-of (#1161)", {
  # With fixed type
  expect_equal(
    vec_c(list(1), list_of(2)),
    list(1, 2)
  )
  expect_equal(
    vec_c(list(""), list_of(2)),
    list("", 2)
  )

  # With fixed size
  expect_equal(
    vec_c(list(1), list_of(2:3, .ptype = zap(), .size = 2)),
    list(1, 2:3)
  )
  expect_equal(
    vec_c(list(""), list_of(2:3, .ptype = zap(), .size = 2)),
    list("", 2:3)
  )
})

test_that("can combine a mix of named and unnamed list-ofs (#784)", {
  a <- new_list_of(list(x = 1L), ptype = integer())
  b <- new_list_of(list(2L), ptype = integer())

  expect <- new_list_of(list(x = 1L, 2L), ptype = integer())

  expect_identical(vec_c(a, b), expect)
})
