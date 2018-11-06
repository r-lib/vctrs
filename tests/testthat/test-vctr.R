context("test-vctr")

test_that("constructor sets attributes", {
  x <- new_vctr(1:4, class = "x", x = 1)
  expect_equal(x, structure(1:4, class = c("x", "vctrs_vctr"), x = 1))
})

test_that(".data must be a vector", {
  expect_error(new_vctr(mean), "vector type")
})

test_that(".data must not have attributes, apart from names", {
  expect_error(new_vctr(structure(1, a = 1)), "attributes")
  expect_error(new_vctr(c(a = 1)), NA)
})

test_that("default format method is internal", {
  x <- new_vctr(1, class = "x")
  expect_equal(format(x), format(x))
})

# Cast/restore ------------------------------------------------------------

test_that("cast to NULL returns x", {
  x <- new_vctr(1, class = "x")
  expect_equal(vec_cast(NULL, x), NULL)
})

test_that("cast succeeds if attributes equal", {
  x1 <- new_vctr(1, class = "x", a = 1, b = 2)
  x2 <- new_vctr(2, class = "x", a = 1, b = 2)

  expect_equal(vec_cast(x1, x2), x1)
  expect_equal(vec_cast(x2, x1), x2)
})

test_that("and fails if attributes are different", {
  x1 <- new_vctr(1, class = "x", a = 1, b = 2)
  x2 <- new_vctr(2, class = "x", a = 2, b = 2)

  expect_error(vec_cast(x1, x2), class = "error_incompatible_cast")
})

test_that("restoring to atomic vector of same type preserves attributes", {
  x1 <- new_vctr(1, class = "x")
  x2 <- new_vctr(2, class = "x")

  expect_equal(vec_restore(2, x1), x2)
})

test_that("restoring to atomic vector of different type throws error", {
  x1 <- new_vctr(1, class = "x")

  expect_error(vec_restore("x", x1), class = "error_incompatible_cast")
})

test_that("base coercion methods mapped to vec_cast", {
  x <- new_vctr(1)

  expect_error(as.logical(x), class = "error_incompatible_cast")
  expect_error(as.integer(x), class = "error_incompatible_cast")
  expect_error(as.logical(x), class = "error_incompatible_cast")
  expect_error(as.double(x), class = "error_incompatible_cast")
  expect_error(as.character(x), class = "error_incompatible_cast")
  expect_error(as.Date(x), class = "error_incompatible_cast")
  expect_error(as.POSIXct(x), class = "error_incompatible_cast")

  expect_equal(as.list(x), list(x))
})

test_that("as.data.frame creates data frame", {
  x <- new_vctr(1:3)
  df <- as.data.frame(x)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3)
  expect_named(df, "x")
})

# equality + comparison + arith + math ---------------------------------------

test_that("equality functions remapped", {
  x <- new_vctr(c(1, 1, NA))

  expect_error(x == 1, class = "error_incompatible_type")
  expect_error(x != 1, class = "error_incompatible_type")
  expect_equal(is.na(x), c(FALSE, FALSE, TRUE))
  expect_true(anyNA(x))

  expect_equal(unique(x), new_vctr(c(1, NA)))
  expect_equal(duplicated(x), c(FALSE, TRUE, FALSE))
  expect_true(anyDuplicated(x))
})

test_that("comparison functions remapped", {
  # Fails on 3.1 with `could not find function "vec_cast.vctrs_vctr"`
  skip_if_not(getRversion() >= "3.2")

  x1 <- new_vctr(c(1, 2), class = "bizzaro")
  x2 <- new_vctr(2, class = "bizzaro")

  vec_proxy_compare.bizzaro <- function(x) -vec_data(x)
  registerS3method("vec_proxy_compare", "bizzaro", vec_proxy_compare.bizzaro)

  expect_equal(order(x1), c(2L, 1L))
  expect_equal(x1 < x2, c(FALSE, FALSE))
  expect_equal(x1 <= x2, c(FALSE, TRUE))
  expect_equal(x1 > x2, c(TRUE, FALSE))
  expect_equal(x1 >= x2, c(TRUE, TRUE))
})

test_that("operators remapped", {
  x <- new_vctr(c(1, 2), class = "bizzaro")

  vec_arith.bizzaro <- function(op, x, y) 1L
  registerS3method("vec_arith", "bizzaro", vec_arith.bizzaro)

  expect_equal(x + 1, 1L)
  expect_equal(x - 1, 1L)
  expect_equal(x * 1, 1L)
  expect_equal(x / 1, 1L)
  expect_equal(x %% 1, 1L)
  expect_equal(x %/% 1, 1L)
  expect_equal(x & 1, 1L)
  expect_equal(x | 1, 1L)

  expect_equal(!x, 1L)
  expect_equal(+x, 1L)
  expect_equal(-x, 1L)
})

test_that("math functions overridden", {
  x <- new_vctr(c(1, NA), class = "bizzaro")

  vec_math.bizzaro <- function(fun, x, ...) vec_math_base(fun, 2L)
  registerS3method("vec_math", "bizzaro", vec_math.bizzaro)

  expect_equal(mean(x), 2L)
  expect_equal(sum(x), 2L)

  expect_equal(is.finite(x), TRUE)
  expect_equal(is.infinite(x), FALSE)
  expect_equal(is.nan(x), FALSE)
})

test_that("diff matches base R", {
  x1 <- cumsum(cumsum(1:10))
  x2 <- new_vctr(x1, class = "vctrs_minus")

  vec_arith.vctrs_minus <- function(op, x, y) vec_arith_base(op, x, y)
  registerS3method("vec_arith", "vctrs_minus", vec_arith.vctrs_minus)

  expect_equal(diff(x2), diff(x1))
  expect_equal(diff(x2, lag = 2L), diff(x1, lag = 2L))
  expect_equal(diff(x2, differences = 2L), diff(x1, differences = 2L))

  expect_equal(diff(x2, lag = 11), x2[0L])
  expect_equal(diff(x2, differences = 11), x2[0L])
})

# names -------------------------------------------------------------------

test_that("all elements must be named if any are named", {
  expect_error(new_vctr(setNames(1:2, c("a", NA))), "named")
  expect_error(new_vctr(setNames(1:2, c("a", ""))), "named")
})

test_that("can not provide invalid names", {
  x <- new_vctr(c(a = 1, b = 2))
  expect_error(names(x) <- "x", "length")
  expect_error(names(x) <- c("x", NA), "named")
  expect_error(names(x) <- c("x", ""), "named")
  expect_error(names(x) <- c("x", "y", "z"), "length")
  expect_error(names(x) <- NULL, NA)
})

test_that("can use [ and [[ with names", {
  x <- new_vctr(c(a = 1, b = 2))

  expect_equal(x["b"], new_vctr(c(b = 2)))
  expect_equal(x[["b"]], new_vctr(2)) # [[ drops names

  x[["c"]] <- 3
  expect_equal(x[["c"]], new_vctr(3))
  x["d"] <- 4
  expect_equal(x[["d"]], new_vctr(4))

  y <- new_vctr(list(a = 1, b = 2))
  y[["c"]] <- 3
  expect_equal(y[["c"]], 3)
  y["d"] <- list(4)
  expect_equal(y[["d"]], 4)
})

test_that("can use [[<- to replace n-dimensional elements", {
  x <- new_vctr(rep(1, times = 4), dim = c(2, 2), class = "vctrs_mtrx")
  vec_restore.vctrs_mtrx <- function(x, to) x
  s3_register("vctrs::vec_restore", "vctrs_mtrx", vec_restore.vctrs_mtrx)
  x[[2, 2]] <- 4
  expect_equal(x[[2, 2]], 4)
})

test_that("subsetting preserves attributes", {
  x <- new_vctr(c(a = 1, b = 2))
  attr(x, "extra") <- TRUE

  y <- x[1]
  expect_equal(attr(x, "extra"), TRUE)
})

test_that("$ inherits from underlying vector", {
  # Seems to be some bug in R 3.1 where NextMethod() called from $.vctr
  # causes an error "invalid subscript type 'promise'"
  skip_if_not(getRversion() >= "3.2")

  x1 <- new_vctr(c(a = 1, b = 2))
  expect_error(x1$a, "atomic vectors")
  expect_error(x1$a <- 2, "atomic vectors")

  x2 <- new_vctr(list(a = 1, b = 2))
  expect_equal(x2$a, 1)
  x2$a <- 10
  expect_equal(x2$a, 10)
})


# unsupported/unimplemented operations --------------------------------------

test_that("can't touch protected attributes", {
  x <- new_vctr(1:4)

  expect_error(is.na(x) <- TRUE, class = "error_unsupported")

  expect_error(dim(x) <- c(2, 2), class = "error_unsupported")
  expect_error(dimnames(x) <- list("x"), class = "error_unsupported")

  expect_error(levels(x), class = "error_unsupported")
  expect_error(levels(x) <- "x", class = "error_unsupported")

  # but it's ok to set names to NULL; this happens at least in vec_c
  # and maybe elsewhere. We may need to back off on this level of
  # strictness in the future
  expect_error(names(x) <- NULL, NA)
})

test_that("summary is unimplemented", {
  x <- new_vctr(1:4)
  expect_error(summary(x), class = "error_unimplemented")
})

# hidden class ------------------------------------------------------------
# We can't construct classes in test because the methods are not found
# when vctr generics call other generics. Instead we rely on a very simple
# class implemented in vctr.R

test_that("class preserved when subsetting", {
  h <- new_hidden(1:4)

  expect_s3_class(h, "hidden")
  expect_s3_class(h[1], "hidden")
  expect_s3_class(h[[1]], "hidden")
  expect_s3_class(rep(h[1], 2), "hidden")
  expect_s3_class(as.list(h)[[1]], "hidden")

  length(h) <- 3
  expect_s3_class(h, "hidden")
})

test_that("RHS cast when using subset assign", {
  h <- new_hidden(1)

  expect_error(h[[1]] <- "x", class = "error_incompatible_cast")
  expect_error(h[1] <- "x", class = "error_incompatible_cast")

  h[2] <- 1
  expect_equal(h, new_hidden(c(1, 1)))

  h[[2]] <- 2
  expect_equal(h, new_hidden(c(1, 2)))
})

test_that("numeric methods use vec_restore_numeric", {
  h <- new_hidden(1)

  expect_equal(abs(h), h)
  expect_equal(sum(h), h)
})

test_that("c passes on to vec_c", {
  h <- new_hidden(1)

  expect_equal(c(h), h)
  expect_equal(c(h, NULL), h)

  expect_equal(c(h, 1), rep(h, 2))
  expect_equal(c(h, h), rep(h, 2))
})

test_that("summaries preserve class", {
  h <- new_hidden(c(1, 2))

  expect_equal(sum(h), new_hidden(3))
  expect_equal(mean(h), new_hidden(1.5))
})

test_that("methods using vec_proxy_compare agree with base", {
  h <- new_hidden(c(1:10))

  expect_agree <- function(f, x) {
    f <- enexpr(f)
    expect_equal(vec_data((!!f)(x)), (!!f)(vec_data(x)))
  }

  expect_agree(min, h)
  expect_agree(max, h)
})

test_that("can put in data frame", {
  h <- new_hidden(1:4)

  expect_named(as.data.frame(h), "h")
  expect_named(data.frame(x = h), "x")
})

test_that("base coercions default to vec_cast", {
  h <- new_hidden(1)
  expect_error(as.character(h), class = "error_incompatible_cast")
  expect_error(as.integer(h), class = "error_incompatible_cast")
  expect_error(generics::as.factor(h), class = "error_incompatible_cast")
  expect_error(generics::as.ordered(h), class = "error_incompatible_cast")
  expect_error(generics::as.difftime(h), class = "error_incompatible_cast")
  expect_equal(as.logical(h), TRUE)
  expect_equal(as.double(h), 1)
})

test_that("default print and str methods are useful", {
  h <- new_hidden(1:4)

  expect_known_output(
    {
      print(h)
      cat("\n")
      print(h[0])
      cat("\n")
      str(h)
    },
    file = "test-vctr-print.txt",
  )
})

test_that("default print method shows names", {
  h <- new_hidden(c(A = 1, B = 2, C = 3))

  expect_known_output(
    {
      print(h)
    },
    file = "test-vctr-print-names.txt",
  )
})

test_that("can't transpose", {
  h <- new_hidden(1:4)
  expect_error(t(h), class = "error_unsupported")
})
