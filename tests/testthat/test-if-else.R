test_that("`condition` must be a condition vector", {
  # No scalars
  expect_snapshot_vec_if_else(
    error = TRUE,
    condition = lm(1 ~ 1),
    true = 1,
    false = 2
  )

  # No casting
  expect_snapshot_vec_if_else(
    error = TRUE,
    condition = 1,
    true = 1,
    false = 2
  )

  # No objects
  expect_snapshot_vec_if_else(
    error = TRUE,
    condition = structure(TRUE, class = "foo"),
    true = 1,
    false = 2
  )

  # No dim
  expect_snapshot_vec_if_else(
    error = TRUE,
    condition = array(TRUE),
    true = 1,
    false = 2
  )
})

test_that("`true`, `false`, and `missing` must be vectors", {
  expect_snapshot(error = TRUE, {
    vec_if_else(
      condition = TRUE,
      true = lm(1 ~ 1),
      false = 2,
      missing = 0
    )
  })
  expect_snapshot(error = TRUE, {
    vec_if_else(
      condition = TRUE,
      true = 1,
      false = lm(1 ~ 1),
      missing = 0
    )
  })
  expect_snapshot(error = TRUE, {
    vec_if_else(
      condition = TRUE,
      true = 1,
      false = 2,
      missing = lm(1 ~ 1)
    )
  })
})

test_that("`true`, `false`, and `missing` must recycle to size of `condition`", {
  expect_snapshot_vec_if_else(
    error = TRUE,
    condition = TRUE,
    true = c(1, 2),
    false = 2,
    missing = 0
  )
  expect_snapshot_vec_if_else(
    error = TRUE,
    condition = TRUE,
    true = 1,
    false = c(1, 2),
    missing = 0
  )
  expect_snapshot_vec_if_else(
    error = TRUE,
    condition = TRUE,
    true = 1,
    false = 2,
    missing = c(1, 2)
  )
})

test_that("all combinations of `true`, `false`, and `missing` recycling (including names) are tested", {
  # 8 combinations
  expect_identical_vec_if_else(
    condition = c(TRUE, FALSE, NA, NA, TRUE, FALSE),
    true = set_names(1L),
    false = set_names(7L),
    missing = set_names(13L),
    expect = set_names(int(1, 7, 13, 13, 1, 7))
  )
  expect_identical_vec_if_else(
    condition = c(TRUE, FALSE, NA, NA, TRUE, FALSE),
    true = set_names(1:6),
    false = set_names(7L),
    missing = set_names(13L),
    expect = set_names(int(1, 7, 13, 13, 5, 7))
  )
  expect_identical_vec_if_else(
    condition = c(TRUE, FALSE, NA, NA, TRUE, FALSE),
    true = set_names(1L),
    false = set_names(7:12),
    missing = set_names(13L),
    expect = set_names(int(1, 8, 13, 13, 1, 12))
  )
  expect_identical_vec_if_else(
    condition = c(TRUE, FALSE, NA, NA, TRUE, FALSE),
    true = set_names(1L),
    false = set_names(7L),
    missing = set_names(13:18),
    expect = set_names(int(1, 7, 15, 16, 1, 7))
  )
  expect_identical_vec_if_else(
    condition = c(TRUE, FALSE, NA, NA, TRUE, FALSE),
    true = set_names(1:6),
    false = set_names(7:12),
    missing = set_names(13L),
    expect = set_names(int(1, 8, 13, 13, 5, 12))
  )
  expect_identical_vec_if_else(
    condition = c(TRUE, FALSE, NA, NA, TRUE, FALSE),
    true = set_names(1:6),
    false = set_names(7L),
    missing = set_names(13:18),
    expect = set_names(int(1, 7, 15, 16, 5, 7))
  )
  expect_identical_vec_if_else(
    condition = c(TRUE, FALSE, NA, NA, TRUE, FALSE),
    true = set_names(1L),
    false = set_names(7:12),
    missing = set_names(13:18),
    expect = set_names(int(1, 8, 15, 16, 1, 12))
  )
  expect_identical_vec_if_else(
    condition = c(TRUE, FALSE, NA, NA, TRUE, FALSE),
    true = set_names(1:6),
    false = set_names(7:12),
    missing = set_names(13:18),
    expect = set_names(int(1, 8, 15, 16, 5, 12))
  )
})

test_that("not all of `true`, `false`, and `missing` must be named", {
  expect_identical_vec_if_else(
    condition = c(TRUE, FALSE, NA, NA, TRUE, FALSE),
    true = 1L,
    false = set_names(7L),
    missing = set_names(13L),
    expect = set_names(int(1, 7, 13, 13, 1, 7), c("", "7", "13", "13", "", "7"))
  )
  expect_identical_vec_if_else(
    condition = c(TRUE, FALSE, NA, NA, TRUE, FALSE),
    true = set_names(1L),
    false = 7L,
    missing = set_names(13L),
    expect = set_names(int(1, 7, 13, 13, 1, 7), c("1", "", "13", "13", "1", ""))
  )
  expect_identical_vec_if_else(
    condition = c(TRUE, FALSE, NA, NA, TRUE, FALSE),
    true = set_names(1L),
    false = set_names(7L),
    missing = 13L,
    expect = set_names(int(1, 7, 13, 13, 1, 7), c("1", "7", "", "", "1", "7"))
  )
})

test_that("names are retained even with casting", {
  expect_named(
    vec_if_else(
      condition = c(TRUE, FALSE, NA, NA, TRUE, FALSE),
      true = set_names(1L),
      false = set_names(7L),
      missing = set_names(13L),
      ptype = double()
    ),
    as.character(c(1, 7, 13, 13, 1, 7))
  )

  # Generic
  expect_named(
    vec_if_else(
      condition = c(TRUE, FALSE, NA, NA, TRUE, FALSE),
      true = set_names(new_date(1)),
      false = set_names(new_date(7)),
      missing = set_names(new_date(13)),
      ptype = new_datetime(tzone = "UTC")
    ),
    as.character(new_date(c(1, 7, 13, 13, 1, 7)))
  )
})

test_that("`ptype` overrides common type", {
  expect_identical(
    vec_if_else(
      condition = TRUE,
      true = 1,
      false = 2,
      missing = 0,
      ptype = integer()
    ),
    1L
  )
  # Generic
  expect_identical(
    vec_if_else(
      condition = TRUE,
      true = new_date(0),
      false = new_date(1),
      missing = new_date(2),
      ptype = new_datetime(tzone = "UTC")
    ),
    new_datetime(0, tzone = "UTC")
  )

  expect_snapshot(error = TRUE, {
    vec_if_else(
      condition = TRUE,
      true = 1.5,
      false = 2,
      missing = 0,
      ptype = integer()
    )
  })
  expect_snapshot(error = TRUE, {
    vec_if_else(
      condition = TRUE,
      true = 1,
      false = 2.5,
      missing = 0,
      ptype = integer()
    )
  })
  expect_snapshot(error = TRUE, {
    vec_if_else(
      condition = TRUE,
      true = 1,
      false = 2,
      missing = 0.5,
      ptype = integer()
    )
  })

  # Generic
  expect_snapshot(error = TRUE, {
    vec_if_else(
      condition = TRUE,
      true = 1,
      false = new_date(2),
      missing = new_date(0),
      ptype = new_date()
    )
  })
  expect_snapshot(error = TRUE, {
    vec_if_else(
      condition = TRUE,
      true = new_date(1),
      false = 2,
      missing = new_date(0),
      ptype = new_date()
    )
  })
  expect_snapshot(error = TRUE, {
    vec_if_else(
      condition = TRUE,
      true = new_date(1),
      false = new_date(2),
      missing = 0,
      ptype = new_date()
    )
  })
})

test_that("gives expected output with `true` and `false`", {
  expect_identical_vec_if_else(
    condition = c(TRUE, FALSE, TRUE),
    true = c(1, 2, 3),
    false = c(4, 5, 6),
    expect = c(1, 5, 3)
  )
})

test_that("gives expected output with `missing`", {
  expect_identical_vec_if_else(
    condition = c(TRUE, FALSE, NA),
    true = c(1, 2, 3),
    false = c(4, 5, 6),
    expect = c(1, 5, NA)
  )
  expect_identical_vec_if_else(
    condition = c(TRUE, FALSE, NA),
    true = c(1, 2, 3),
    false = c(4, 5, 6),
    missing = c(7, 8, 9),
    expect = c(1, 5, 9)
  )
})

test_that("works with arrays of all types", {
  mat <- as.matrix

  expect_identical(
    vec_if_else(
      c(TRUE, FALSE, TRUE),
      mat(lgl(TRUE, FALSE, NA)),
      mat(lgl(FALSE, TRUE, FALSE))
    ),
    mat(lgl(TRUE, TRUE, NA))
  )
  expect_identical(
    vec_if_else(
      c(TRUE, FALSE, TRUE),
      mat(int(1, 2, 3)),
      mat(int(4, 5, 6))
    ),
    mat(int(1, 5, 3))
  )
  expect_identical(
    vec_if_else(
      c(TRUE, FALSE, TRUE),
      mat(dbl(1, 2, 3)),
      mat(dbl(4, 5, 6))
    ),
    mat(dbl(1, 5, 3))
  )
  expect_identical(
    vec_if_else(
      c(TRUE, FALSE, TRUE),
      mat(cpl(1, 2, 3)),
      mat(cpl(4, 5, 6))
    ),
    mat(cpl(1, 5, 3))
  )
  expect_identical(
    vec_if_else(
      c(TRUE, FALSE, TRUE),
      mat(as.raw(c(1, 2, 3))),
      mat(as.raw(c(4, 5, 6)))
    ),
    mat(as.raw(c(1, 5, 3)))
  )
  expect_identical(
    vec_if_else(
      c(TRUE, FALSE, TRUE),
      mat(chr("1", "2", "3")),
      mat(chr("4", "5", "6"))
    ),
    mat(chr("1", "5", "3"))
  )
  expect_identical(
    vec_if_else(
      c(TRUE, FALSE, TRUE),
      mat(list(1, 2, 3)),
      mat(list(4, 5, 6))
    ),
    mat(list(1, 5, 3))
  )
})

test_that("missing value fall through is right for all atomic types", {
  expect_identical(vec_if_else(NA, TRUE, TRUE), NA)
  expect_identical(vec_if_else(NA, 1L, 1L), NA_integer_)
  expect_identical(vec_if_else(NA, 1, 1), NA_real_)
  expect_identical(
    vec_if_else(NA, cpl(1), cpl(1)),
    complex(real = NA, imaginary = NA)
  )
  expect_identical(vec_if_else(NA, as.raw(1), as.raw(1)), as.raw(0))
  expect_identical(vec_if_else(NA, "1", "1"), NA_character_)
  expect_identical(vec_if_else(NA, list(1), list(1)), list(NULL))
})

test_that("extraneous attributes on `true`, `false`, and `missing` are dropped", {
  # We do not consider these as part of the `ptype`!
  expect_identical(
    vec_if_else(
      condition = c(TRUE, FALSE, TRUE, NA),
      true = structure(1:4, foo = "bar"),
      false = structure(5:8, foo = "bar"),
      missing = structure(9:12, foo = "bar")
    ),
    int(1, 6, 3, 12)
  )
})

test_that("`ptype` is finalized before being used", {
  # Without `vec_ptype_final()`, this could result in `unspecified()` as the `ptype`
  expect_identical(vec_if_else(TRUE, TRUE, FALSE, ptype = NA), TRUE)

  # We try not to leak <unspecified> when we can help it
  expect_identical(vec_if_else(TRUE, TRUE, FALSE, ptype = unspecified(1)), TRUE)
})

test_that("`vec_ptype2()`'s `left` is always set", {
  # This used to result in `left` being unset, so we'd hit an unreachable state
  expect_identical(
    vec_if_else(
      condition = FALSE,
      true = TRUE,
      false = NA,
      missing = FALSE
    ),
    NA
  )
})

# ------------------------------------------------------------------------------
# `dplyr::if_else()` tests

test_that("scalar true and false are vectorised", {
  x <- c(TRUE, TRUE, FALSE, FALSE)
  expect_equal(vec_if_else(x, 1, 2), c(1, 1, 2, 2))
})

test_that("vector true and false are ok", {
  x <- c(-1, 0, 1)

  expect_equal(vec_if_else(x < 0, x, 0), c(-1, 0, 0))
  expect_equal(vec_if_else(x > 0, x, 0), c(0, 0, 1))
})

test_that("missing values are missing", {
  expect_equal(vec_if_else(c(TRUE, NA, FALSE), -1, 1), c(-1, NA, 1))
})

test_that("works with lists", {
  x <- list(1, 2, 3)

  expect_equal(
    vec_if_else(c(TRUE, TRUE, FALSE), x, list(NULL)),
    list(1, 2, NULL)
  )
})

test_that("works with data frames", {
  true <- tibble(x = 1, y = 2)
  false <- tibble(x = 3, y = 4)

  expect_identical(
    vec_if_else(c(TRUE, FALSE, NA, TRUE), true, false),
    vec_c(true, false, NA, true)
  )
})

test_that("works with vctrs rcrd types", {
  true <- new_rcrd(list(x = 1, y = 2))
  false <- new_rcrd(list(x = 3, y = 4))

  expect_identical(
    vec_if_else(c(TRUE, FALSE, NA, TRUE), true, false),
    vec_c(true, false, NA, true)
  )
})

test_that("takes the common type of `true` and `false` (tidyverse/dplyr#6243)", {
  expect_identical(vec_if_else(TRUE, 1L, 1.5), 1)

  expect_snapshot(error = TRUE, {
    vec_if_else(TRUE, 1, "x")
  })
  expect_snapshot(error = TRUE, {
    vec_if_else(
      TRUE,
      1,
      "x",
      true_arg = "t",
      false_arg = "f",
      error_call = current_env()
    )
  })
})

test_that("includes `missing` in the common type computation if used", {
  expect_identical(vec_if_else(TRUE, 1L, 2L, missing = 3), 1)

  expect_snapshot(error = TRUE, {
    vec_if_else(TRUE, 1, 2, missing = "x")
  })
  expect_snapshot(error = TRUE, {
    vec_if_else(TRUE, 1L, 2, missing = "x")
  })
  expect_snapshot(error = TRUE, {
    vec_if_else(TRUE, 1, 2L, missing = "x")
  })
})

test_that("can recycle to size 0 `condition`", {
  expect_identical(vec_if_else(logical(), 1, 2, missing = 3), double())
})

test_that("accepts logical conditions with attributes (tidyverse/dplyr#6678)", {
  x <- structure(TRUE, label = "foo")
  expect_identical(vec_if_else(x, 1, 2), 1)
})

test_that("`condition` must be logical (and isn't cast to logical!)", {
  expect_snapshot(error = TRUE, {
    vec_if_else(1:10, 1, 2)
  })
})

test_that("`true`, `false`, and `missing` must recycle to the size of `condition`", {
  x <- 1:3
  bad <- 1:2

  expect_snapshot(error = TRUE, {
    vec_if_else(x < 2, bad, x)
  })
  expect_snapshot(error = TRUE, {
    vec_if_else(x < 2, x, bad)
  })
  expect_snapshot(error = TRUE, {
    vec_if_else(x < 2, x, x, missing = bad)
  })
})

test_that("must have empty dots", {
  expect_snapshot(error = TRUE, {
    vec_if_else(TRUE, 1, 2, missing = 3, 4)
  })
})

test_that("`ptype` overrides the common type", {
  expect_identical(vec_if_else(TRUE, 2, 1L, ptype = integer()), 2L)

  expect_snapshot(error = TRUE, {
    vec_if_else(TRUE, 1L, 2.5, ptype = integer())
  })
})
