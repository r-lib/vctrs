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

test_that("Math and Ops generics preserve class (where sensible)", {
  h <- new_hidden(1)

  expect_equal(abs(h), h)
  expect_equal(+h, h)
  expect_equal(h * 2, new_hidden(2))
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
})

test_that("can put in data frame", {
  h <- new_hidden(1:4)

  expect_named(as.data.frame(h), "h")
  expect_named(data.frame(x = h), "x")
})

test_that("as.character defaults to format", {
  h <- new_hidden(1)
  expect_equal(as.character(h), "xxx")
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

  expect_error(names(h) <- "x", "Must not set")
  expect_error(dim(h) <- c(2, 2), "Must not set")
  expect_error(dimnames(h) <- list("x"), "Must not set")

  # but it's ok to set names to NULL; this happens at least in vec_c
  # and maybe elsewhere. We may need to back off on this level of
  # strictness in the future
  expect_error(names(h) <- NULL, NA)
})
