context("test-vctr")

test_that("constructor sets attributes", {
  x <- new_vctr(1:4, class = "x", x = 1)
  expect_equal(x, structure(1:4, class = c("x", "vctr"), x = 1))
})

test_that(".data must be a vector", {
  expect_error(new_vctr(mean), "vector type")
})

test_that("no default format method", {
  x <- new_vctr(1, class = "x")
  expect_error(format(x), "not implemented")
})

test_that("cast to NULL returns x", {
  x <- new_vctr(1, class = "x")
  expect_equal(vec_cast(NULL, x), NULL)
})

test_that("cast to atomic vector of same type preserves attributes", {
  x1 <- new_vctr(1, class = "x")
  x2 <- new_vctr(2, class = "x")

  expect_equal(vec_cast(2, x1), x2)

  # otherwise you get an error
  expect_error(vec_cast(2L, x1), class = "error_incompatible_cast")
  expect_error(vec_cast(new_date(), x1), class = "error_incompatible_cast")
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

test_that("group generics dispatch to vctr group generics", {
  h <- new_hidden(1)

  expect_equal(abs(h), h)
  expect_equal(+h, h)
  expect_equal(h * 2, new_hidden(2))
  expect_error(h & 1, "Boolean operator")

  expect_equal(h == 1, TRUE)
  expect_equal(h == c(1, 2), c(TRUE, FALSE))
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
  expect_equal(median(h), new_hidden(1.5))
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
  expect_equal(as.logical(h), TRUE)
  expect_equal(as.double(h), 1)
})

test_that("default print method is ok", {
  h <- new_hidden(1:4)

  expect_known_output(
    {
      print(h)
      cat("\n")
      print(h[0])
    },
    file = "test-vctr-print.txt",
  )
})

test_that("can't touch protected attributes", {
  h <- new_hidden(1:4)

  expect_error(names(h) <- "x", class = "error_unsupported")
  expect_error(dim(h) <- c(2, 2), class = "error_unsupported")
  expect_error(dimnames(h) <- list("x"), class = "error_unsupported")
  expect_error(h$x, class = "error_unsupported")
  expect_error(h$x <- 2, class = "error_unsupported")

  # but it's ok to set names to NULL; this happens at least in vec_c
  # and maybe elsewhere. We may need to back off on this level of
  # strictness in the future
  expect_error(names(h) <- NULL, NA)
})


# logical class -----------------------------------------------------------

test_that("logical group generics return bare logical", {
  v <- new_vctr(TRUE, class = "test")

  expect_equal(v == v, TRUE)
  expect_equal(v & v, TRUE)
  expect_equal(!v, FALSE)
})



