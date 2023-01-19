# vec_set_intersect -------------------------------------------------------

test_that("retains names of `x` elements", {
  x <- c(a = 1, b = 4, c = 1, d = 4, e = 2)
  y <- c(w = 3, x = 2, y = 1, z = 2)

  expect_identical(
    vec_set_intersect(x, y),
    c(a = 1, e = 2)
  )
})

test_that("returns elements in order they first appear in `x`", {
  expect_identical(vec_set_intersect(c(3, 1, 2, 3), c(2, 3)), c(3, 2))
})

test_that("returns unique elements", {
  expect_identical(vec_set_intersect(c(1, 2, 1), c(2, 2, 1)), c(1, 2))
})

test_that("works with character vectors of different encodings", {
  encs <- encodings()
  # Always returns UTF-8
  expect_identical(vec_set_intersect(encs$utf8, encs$latin1), encs$utf8)
  expect_identical(vec_set_intersect(encs$latin1, encs$utf8), encs$utf8)
})

test_that("has consistency with `NA` values", {
  expect_identical(vec_set_intersect(c(NA_real_, 1), NA_real_), NA_real_)
  expect_identical(vec_set_intersect(c(1, NA_real_), NA_real_), NA_real_)

  expect_identical(vec_set_intersect(c(NA_real_, NaN), NaN), NaN)
  expect_identical(vec_set_intersect(c(NaN, NA_real_), NaN), NaN)
})

test_that("works with complex missing values", {
  na <- complex(
    real = c(NA_real_, NA_real_, NaN, NaN),
    imaginary = c(NA_real_, NaN, NA_real_, NaN)
  )
  expect_identical(vec_set_intersect(na, na), na)
  expect_identical(vec_set_intersect(na, na[2]), na[2])
})

test_that("works correctly with unspecified logical vectors", {
  expect_identical(vec_set_intersect(NA, NA), NA)
})

test_that("returns a vector of the common type", {
  expect_identical(vec_set_intersect(1L, c(2, 1)), 1)
})

test_that("works with data frames", {
  x <- data_frame(
    a = c(1, 2, 1, 1),
    b = c("a", "b", "a", "d")
  )
  y <- data_frame(
    a = c(2, 3, 1, 2),
    b = c("b", "b", "d", "d")
  )

  expect_identical(vec_set_intersect(x, y), vec_slice(x, c(2, 4)))
  expect_identical(vec_set_intersect(y, x), vec_slice(y, c(1, 3)))
})

test_that("works with rcrds", {
  x <- new_rcrd(list(
    a = c(1, 2, 1, 1),
    b = c("a", "b", "a", "d")
  ))
  y <- new_rcrd(list(
    a = c(2, 3, 1, 2),
    b = c("b", "b", "d", "d")
  ))

  expect_identical(vec_set_intersect(x, y), vec_slice(x, c(2, 4)))
  expect_identical(vec_set_intersect(y, x), vec_slice(y, c(1, 3)))
})

# vec_set_difference ------------------------------------------------------

test_that("retains names of `x` elements", {
  x <- c(a = 1, b = 4, c = 1, d = 4, e = 5)
  y <- c(w = 3, x = 2, y = 1, z = 2)

  expect_identical(
    vec_set_difference(x, y),
    c(b = 4, e = 5)
  )
})

test_that("returns elements in order they first appear in `x`", {
  expect_identical(vec_set_difference(c(3, 1, 2, 3), 1), c(3, 2))
})

test_that("returns unique elements", {
  expect_identical(vec_set_difference(c(1, 2, 1, 4), c(4, 5)), c(1, 2))
})

test_that("works with character vectors of different encodings", {
  encs <- encodings()
  expect_identical(vec_set_difference(encs$utf8, encs$latin1), character())
  expect_identical(vec_set_difference(encs$latin1, encs$utf8), character())
})

test_that("has consistency with `NA` values", {
  expect_identical(vec_set_difference(c(NA_real_, 1), NA_real_), 1)
  expect_identical(vec_set_difference(c(1, NA_real_), NA_real_), 1)

  expect_identical(vec_set_difference(c(NA_real_, NaN), NaN), NA_real_)
  expect_identical(vec_set_difference(c(NaN, NA_real_), NaN), NA_real_)
})

test_that("works with complex missing values", {
  na <- complex(
    real = c(NA_real_, NA_real_, NaN, NaN),
    imaginary = c(NA_real_, NaN, NA_real_, NaN)
  )
  expect_identical(vec_set_difference(na, na), complex())
  expect_identical(vec_set_difference(na, na[2]), na[c(1, 3, 4)])
})

test_that("works correctly with unspecified logical vectors", {
  expect_identical(vec_set_difference(NA, NA), logical())
})

test_that("returns a vector of the common type", {
  expect_identical(vec_set_difference(c(3L, 1L), c(2, 1)), 3)
})

test_that("works with data frames", {
  x <- data_frame(
    a = c(1, 2, 1, 1),
    b = c("a", "b", "a", "d")
  )
  y <- data_frame(
    a = c(2, 3, 1, 2),
    b = c("b", "b", "d", "d")
  )

  expect_identical(vec_set_difference(x, y), vec_slice(x, 1))
  expect_identical(vec_set_difference(y, x), vec_slice(y, c(2, 4)))
})

test_that("works with rcrds", {
  x <- new_rcrd(list(
    a = c(1, 2, 1, 1),
    b = c("a", "b", "a", "d")
  ))
  y <- new_rcrd(list(
    a = c(2, 3, 1, 2),
    b = c("b", "b", "d", "d")
  ))

  expect_identical(vec_set_difference(x, y), vec_slice(x, 1))
  expect_identical(vec_set_difference(y, x), vec_slice(y, c(2, 4)))
})

# vec_set_union -----------------------------------------------------------

test_that("retains names of `x` and `y` elements", {
  x <- c(a = 1, b = 4, c = 1, d = 4, e = 5)
  y <- c(w = 3, x = 2, y = 1, z = 2)

  expect_identical(
    vec_set_union(x, y),
    c(a = 1, b = 4, e = 5, w = 3, x = 2)
  )
})

test_that("does minimal name repair on duplicate names", {
  x <- c(a = 1)
  y <- c(a = 2)

  expect_named(vec_set_union(x, y), c("a", "a"))
})

test_that("returns elements in order they first appear in `x` and `y`", {
  expect_identical(vec_set_union(c(3, 1, 2, 3), c(4, 2, 5, 4)), c(3, 1, 2, 4, 5))
})

test_that("returns unique elements", {
  expect_identical(vec_set_union(c(1, 2, 1, 4), c(4, 5, 5)), c(1, 2, 4, 5))
})

test_that("works with character vectors of different encodings", {
  encs <- encodings()
  # Always returns UTF-8
  expect_identical(vec_set_union(encs$utf8, encs$latin1), encs$utf8)
  expect_identical(vec_set_union(encs$latin1, encs$utf8), encs$utf8)
})

test_that("has consistency with `NA` values", {
  expect_identical(vec_set_union(c(NA_real_, 1), NA_real_), c(NA_real_, 1))
  expect_identical(vec_set_union(c(1, NA_real_), NA_real_), c(1, NA_real_))

  expect_identical(vec_set_union(NA_real_, NaN), c(NA_real_, NaN))
  expect_identical(vec_set_union(NaN, NA_real_), c(NaN, NA_real_))
})

test_that("works with complex missing values", {
  na <- complex(
    real = c(NA_real_, NA_real_, NaN, NaN),
    imaginary = c(NA_real_, NaN, NA_real_, NaN)
  )
  expect_identical(vec_set_union(na, na), na)
  expect_identical(vec_set_union(na[-2], na), na[c(1, 3, 4, 2)])
})

test_that("works correctly with unspecified logical vectors", {
  expect_identical(vec_set_union(NA, NA), NA)
})

test_that("returns a vector of the common type", {
  expect_identical(vec_set_union(c(3L, 1L), c(2, 1)), c(3, 1, 2))
})

test_that("works with data frames", {
  x <- data_frame(
    a = c(1, 2, 1, 1),
    b = c("a", "b", "a", "d")
  )
  y <- data_frame(
    a = c(2, 3, 1, 2),
    b = c("b", "b", "d", "d")
  )

  expect_identical(vec_set_union(x, y), vec_c(vec_slice(x, c(1, 2, 4)), vec_slice(y, c(2, 4))))
  expect_identical(vec_set_union(y, x), vec_c(vec_slice(y, c(1, 2, 3, 4)), vec_slice(x, 1)))
})

test_that("works with rcrds", {
  x <- new_rcrd(list(
    a = c(1, 2, 1, 1),
    b = c("a", "b", "a", "d")
  ))
  y <- new_rcrd(list(
    a = c(2, 3, 1, 2),
    b = c("b", "b", "d", "d")
  ))

  expect_identical(vec_set_union(x, y), vec_c(vec_slice(x, c(1, 2, 4)), vec_slice(y, c(2, 4))))
  expect_identical(vec_set_union(y, x), vec_c(vec_slice(y, c(1, 2, 3, 4)), vec_slice(x, 1)))
})

# vec_set_symmetric_difference --------------------------------------------

test_that("retains names of `x` and `y` elements", {
  x <- c(a = 1, b = 4, c = 1, d = 4, e = 5)
  y <- c(w = 3, x = 2, y = 1, z = 2)

  expect_identical(
    vec_set_symmetric_difference(x, y),
    c(b = 4, e = 5, w = 3, x = 2)
  )
})

test_that("returns elements in order they first appear in `x` and `y`", {
  expect_identical(vec_set_symmetric_difference(c(3, 1, 2, 3), c(4, 2, 5, 4)), c(3, 1, 4, 5))
})

test_that("returns unique elements", {
  expect_identical(vec_set_symmetric_difference(c(1, 2, 1, 4), c(4, 5, 5)), c(1, 2, 5))
})

test_that("works with character vectors of different encodings", {
  encs <- encodings()
  # Always returns UTF-8
  expect_identical(vec_set_symmetric_difference(encs$utf8, encs$latin1), character())
  expect_identical(vec_set_symmetric_difference(encs$latin1, encs$utf8), character())
})

test_that("has consistency with `NA` values", {
  expect_identical(vec_set_symmetric_difference(c(NA_real_, 1), NA_real_), 1)
  expect_identical(vec_set_symmetric_difference(c(1, NA_real_), NA_real_), 1)

  expect_identical(vec_set_symmetric_difference(c(NaN, 1), NaN), 1)
  expect_identical(vec_set_symmetric_difference(c(1, NaN), NaN), 1)

  expect_identical(vec_set_symmetric_difference(NA_real_, NaN), c(NA_real_, NaN))
  expect_identical(vec_set_symmetric_difference(NaN, NA_real_), c(NaN, NA_real_))
})

test_that("works with complex missing values", {
  na <- complex(
    real = c(NA_real_, NA_real_, NaN, NaN),
    imaginary = c(NA_real_, NaN, NA_real_, NaN)
  )
  expect_identical(vec_set_symmetric_difference(na, na), complex())
  expect_identical(vec_set_symmetric_difference(na[-2], na[-4]), na[c(2, 4)])
})

test_that("works correctly with unspecified logical vectors", {
  expect_identical(vec_set_symmetric_difference(NA, NA), logical())
})

test_that("returns a vector of the common type", {
  expect_identical(vec_set_symmetric_difference(c(3L, 1L), c(2, 1)), c(3, 2))
})

test_that("works with data frames", {
  x <- data_frame(
    a = c(1, 2, 1, 1),
    b = c("a", "b", "a", "d")
  )
  y <- data_frame(
    a = c(2, 3, 1, 2),
    b = c("b", "b", "d", "d")
  )

  expect_identical(
    vec_set_symmetric_difference(x, y),
    vec_c(vec_slice(x, 1), vec_slice(y, c(2, 4)))
  )
  expect_identical(
    vec_set_symmetric_difference(y, x),
    vec_c(vec_slice(y, c(2, 4)), vec_slice(x, 1))
  )
})

test_that("works with rcrds", {
  x <- new_rcrd(list(
    a = c(1, 2, 1, 1),
    b = c("a", "b", "a", "d")
  ))
  y <- new_rcrd(list(
    a = c(2, 3, 1, 2),
    b = c("b", "b", "d", "d")
  ))

  expect_identical(
    vec_set_symmetric_difference(x, y),
    vec_c(vec_slice(x, 1), vec_slice(y, c(2, 4)))
  )
  expect_identical(
    vec_set_symmetric_difference(y, x),
    vec_c(vec_slice(y, c(2, 4)), vec_slice(x, 1))
  )
})

# common ------------------------------------------------------------------

test_that("errors nicely if common type can't be taken", {
  expect_snapshot(error = TRUE, {
    vec_set_intersect(1, "x")
  })
  expect_snapshot(error = TRUE, {
    vec_set_difference(1, "x")
  })
  expect_snapshot(error = TRUE, {
    vec_set_union(1, "x")
  })
  expect_snapshot(error = TRUE, {
    vec_set_symmetric_difference(1, "x")
  })
})

test_that("dots must be empty", {
  expect_snapshot(error = TRUE, {
    vec_set_intersect(1, 2, 3)
  })
  expect_snapshot(error = TRUE, {
    vec_set_difference(1, 2, 3)
  })
  expect_snapshot(error = TRUE, {
    vec_set_union(1, 2, 3)
  })
  expect_snapshot(error = TRUE, {
    vec_set_symmetric_difference(1, 2, 3)
  })
})

test_that("`ptype` is respected", {
  expect_identical(vec_set_intersect(1, 1, ptype = integer()), 1L)
  expect_identical(vec_set_difference(1, 1, ptype = integer()), integer())
  expect_identical(vec_set_union(1, 2, ptype = integer()), c(1L, 2L))
  expect_identical(vec_set_symmetric_difference(1, 2, ptype = integer()), c(1L, 2L))

  expect_snapshot(error = TRUE, {
    vec_set_intersect(1, 1.5, ptype = integer())
  })
  expect_snapshot(error = TRUE, {
    vec_set_difference(1, 1.5, ptype = integer())
  })
  expect_snapshot(error = TRUE, {
    vec_set_union(1, 1.5, ptype = integer())
  })
  expect_snapshot(error = TRUE, {
    vec_set_symmetric_difference(1, 1.5, ptype = integer())
  })
})

test_that("`x_arg` and `y_arg` can be adjusted", {
  expect_snapshot(error = TRUE, {
    vec_set_intersect(1, "2", x_arg = "foo", y_arg = "bar")
  })
  expect_snapshot(error = TRUE, {
    vec_set_difference(1, "2", x_arg = "foo", y_arg = "bar")
  })
  expect_snapshot(error = TRUE, {
    vec_set_union(1, "2", x_arg = "foo", y_arg = "bar")
  })
  expect_snapshot(error = TRUE, {
    vec_set_symmetric_difference(1, "2", x_arg = "foo", y_arg = "bar")
  })

  expect_snapshot(error = TRUE, {
    vec_set_intersect(1, "2", x_arg = "", y_arg = "")
  })
})

test_that("`error_call` can be adjusted", {
  my_set_intersect <- function() {
    vec_set_intersect(1, "x", error_call = current_env())
  }
  my_set_difference <- function() {
    vec_set_difference(1, "x", error_call = current_env())
  }
  my_set_union <- function() {
    vec_set_union(1, "x", error_call = current_env())
  }
  my_set_symmetric_difference <- function() {
    vec_set_symmetric_difference(1, "x", error_call = current_env())
  }

  expect_snapshot(error = TRUE, {
    my_set_intersect()
  })
  expect_snapshot(error = TRUE, {
    my_set_difference()
  })
  expect_snapshot(error = TRUE, {
    my_set_union()
  })
  expect_snapshot(error = TRUE, {
    my_set_symmetric_difference()
  })
})
