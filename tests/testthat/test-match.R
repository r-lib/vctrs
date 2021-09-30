# ------------------------------------------------------------------------------
# vec_matches() - integers

test_that("can match in increasing order", {
  x <- vec_matches(1:2, 1:3)
  expect_identical(x$needles, 1:2)
  expect_identical(x$haystack, 1:2)
})

# ------------------------------------------------------------------------------
# vec_matches() - doubles

test_that("can match doubles", {
  x <- vec_matches(c(1, 2, 5), c(2, 2, 3, 1))

  expect_identical(x$needles, c(1L, 2L, 2L, 3L))
  expect_identical(x$haystack, c(4L, 1L, 2L, NA))
})

test_that("can match Inf and -Inf with all conditions", {
  x <- c(Inf, -Inf)
  y <- c(-Inf, 0, Inf)

  res <- vec_matches(x, y, condition = "==")
  expect_identical(res$needles, c(1L, 2L))
  expect_identical(res$haystack, c(3L, 1L))

  res <- vec_matches(x, y, condition = "<")
  expect_identical(res$needles, c(1L, 2L, 2L))
  expect_identical(res$haystack, c(NA, 2L, 3L))

  res <- vec_matches(x, y, condition = "<=")
  expect_identical(res$needles, c(1L, 2L, 2L, 2L))
  expect_identical(res$haystack, c(3L, 1L, 2L, 3L))

  res <- vec_matches(x, y, condition = ">")
  expect_identical(res$needles, c(1L, 1L, 2L))
  expect_identical(res$haystack, c(1L, 2L, NA))

  res <- vec_matches(x, y, condition = ">=")
  expect_identical(res$needles, c(1L, 1L, 1L, 2L))
  expect_identical(res$haystack, c(1L, 2L, 3L, 1L))
})

test_that("NA and NaN don't match numbers with equality conditions", {
  expect_identical(vec_matches(1, NA_real_)$haystack, NA_integer_)
  expect_identical(vec_matches(1, NaN)$haystack, NA_integer_)
  expect_identical(vec_matches(NA_real_, 1)$haystack, NA_integer_)
  expect_identical(vec_matches(NaN, 1)$haystack, NA_integer_)
})

test_that("NA and NaN are the same by default", {
  res <- vec_matches(NA_real_, NaN)
  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, 1L)

  res <- vec_matches(NaN, NA_real_)
  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, 1L)

  res <- vec_matches(c(NaN, NA, NaN), c(NA, NaN, NA), condition = "==")
  expect_identical(res$needles, rep(c(1L, 2L, 3L), each = 3))
  expect_identical(res$haystack, rep(c(1L, 2L, 3L), times = 3))

  res <- vec_matches(c(NaN, NA, NaN), c(NA, NaN, NA), condition = "==", multiple = "first")
  expect_identical(res$needles, c(1L, 2L, 3L))
  expect_identical(res$haystack, c(1L, 1L, 1L))
})

test_that("NA and NaN are distinct if requested", {
  res <- vec_matches(c(NaN, NA, NaN), c(NA, NaN, NA), condition = "==", nan_distinct = TRUE)
  expect_identical(res$needles, c(1L, 2L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 1L, 3L, 2L))

  res <- vec_matches(c(NaN, NA, NaN), c(NA, NaN, NA), condition = "==", multiple = "first", nan_distinct = TRUE)
  expect_identical(res$needles, c(1L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 1L, 2L))
})

test_that("NA and NaN match each other in non-equi conditions by default", {
  res <- vec_matches(c(NaN, NA, NaN, 1), c(NA, NaN, NA), condition = ">=", nan_distinct = FALSE)
  expect_identical(res$needles, c(rep(c(1L, 2L, 3L), each = 3), 4L))
  expect_identical(res$haystack, c(rep(c(1L, 2L, 3L), times = 3), NA))

  res <- vec_matches(c(NaN, NA, NaN, 1), c(NA, NaN, NA), condition = "<=", nan_distinct = FALSE)
  expect_identical(res$needles, c(rep(c(1L, 2L, 3L), each = 3), 4L))
  expect_identical(res$haystack, c(rep(c(1L, 2L, 3L), times = 3), NA))
})

test_that("NA and NaN never match each other in non-equi conditions if treated as distinct", {
  res <- vec_matches(c(NaN, NA, NaN), c(NA, NaN, NA), condition = ">=", nan_distinct = TRUE)
  expect_identical(res$needles, c(1L, 2L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 1L, 3L, 2L))

  res <- vec_matches(c(NaN, NA, NaN), c(NA, NaN, NA), condition = "<=", nan_distinct = TRUE)
  expect_identical(res$needles, c(1L, 2L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 1L, 3L, 2L))
})

test_that("NA and NaN are both considered incomplete no matter the value of `nan_distinct`", {
  res <- vec_matches(c(NA, NaN), c(NA, NaN), incomplete = NA, nan_distinct = FALSE)
  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, c(NA_integer_, NA_integer_))

  res <- vec_matches(c(NA, NaN), c(NA, NaN), incomplete = NA, nan_distinct = TRUE)
  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, c(NA_integer_, NA_integer_))
})

# ------------------------------------------------------------------------------
# vec_matches() - complex

test_that("complex can be matched", {
  x <- complex(real = 1, imaginary = c(1, 2))
  y <- complex(real = 1, imaginary = c(1, 1, 3))
  z <- complex(real = 2, imaginary = 1)

  res <- vec_matches(x, y)
  expect_identical(res$needles, c(1L, 1L, 2L))
  expect_identical(res$haystack, c(1L, 2L, NA))

  res <- vec_matches(x, z)
  expect_identical(res$needles, c(1L, 2L))
  expect_identical(res$haystack, c(NA_integer_, NA_integer_))
})

test_that("complex order lexicographically", {
  x <- complex(real = 1, imaginary = c(1, 2, 5))
  y <- complex(real = 1, imaginary = c(1, 4, 3))

  res <- vec_matches(x, y, condition = "<")
  expect_identical(res$needles, c(1L, 1L, 2L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 3L, 2L, 3L, NA))
})

test_that("complex incomplete values match correctly", {
  x <- complex(real = c(NA, NA, NaN, NaN), imaginary = c(NA, NaN, NA, NaN))
  y <- complex(real = c(NA, NA, NaN, NaN), imaginary = c(NA, NaN, NA, NaN))

  # Missings can match, and all missing values should be treated equally
  res <- vec_matches(x, y, condition = "==", incomplete = "match", nan_distinct = FALSE)
  expect_identical(res$needles, rep(1:4, each = 4))
  expect_identical(res$haystack, rep(1:4, times = 4))

  # Missings can match, but all combinations are different
  res <- vec_matches(x, y, condition = "==", incomplete = "match", nan_distinct = TRUE)
  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, 1:4)

  # Missings don't match
  res <- vec_matches(x, y, condition = "==", incomplete = NA)
  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, rep(NA_integer_, 4))

  # Missings don't match, but are never considered no-matches
  expect_identical(
    vec_matches(x, y, condition = "==", incomplete = NA, no_match = "error"),
    vec_matches(x, y, condition = "==", incomplete = NA)
  )
})

test_that("complex missing values are always grouped together (#1403)", {
  # Unlike data frames and rcrd types, for complex vectors if either element
  # is missing then the whole observation is normalised to have both components
  # be missing. This means `1+NAi` matches `2+NAi`. It also matches `2+NaNi`
  # unless `nan_distinct = TRUE`.
  x <- complex(real = c(1, 1, 2, 2, 2), imaginary = c(NA, 1, NA, 2, NaN))
  y <- x[-1]

  res <- vec_matches(x, y, condition = ">=")
  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 3L, 4L, 4L, 5L, 5L))
  expect_identical(res$haystack, c(2L, 4L, 1L, 2L, 4L, 1L, 3L, 2L, 4L))
})

test_that("behavior with complex missing values matches base R", {
  skip_if(getRversion() < "3.4.0", message = "`match()` is broken with complex missings")

  x <- complex(real = c(1, 1, 2, 2, 2), imaginary = c(NA, 1, NA, 2, NaN))

  expect_identical(
    vec_matches(x, x, nan_distinct = TRUE, multiple = "first")$haystack,
    match(x, x)
  )
})

# ------------------------------------------------------------------------------
# vec_matches() - characters

test_that("character ordering is done in the C locale", {
  x <- c("a", "A")
  y <- c("a", "A", "b", "B")

  # a < b, but a > A and a > B
  res <- vec_matches(x, y, condition = "<")
  expect_identical(res$needles, c(1L, 2L, 2L, 2L))
  expect_identical(res$haystack, c(3L, 1L, 3L, 4L))
})

test_that("`chr_transform` can affect the matching process", {
  x <- c("a", "A")
  y <- c("a", "A")

  res <- vec_matches(x, y, condition = "==")
  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, 1:2)

  res <- vec_matches(x, y, condition = "==", chr_transform = tolower)
  expect_identical(res$needles, c(1L, 1L, 2L, 2L))
  expect_identical(res$haystack, c(1L, 2L, 1L, 2L))
})

# ------------------------------------------------------------------------------
# vec_matches() - lists

test_that("lists can be matched", {
  x <- list(1, 2, 1, NULL)
  y <- list(1, 1, 3, NULL)

  res <- vec_matches(x, y)

  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 3L, 4L))
  expect_identical(res$haystack, c(1L, 2L, NA, 1L, 2L, 4L))
})

test_that("list incompleteness is detected", {
  res <- vec_matches(list(NULL), list(NULL), incomplete = NA)

  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, NA_integer_)
})

test_that("list ordering is by first appearance in `needles` (so non-equi joins don't make much sense)", {
  x <- list(3, 2, 1, NULL)
  y <- list(1, 3, 1, 3)

  res <- vec_matches(x, y, condition = ">")

  # x[1] appears first, so it isn't greater than anything
  # x[2] is greater than x[1] (when x[1] is in y)
  # and so on...
  # NULL still doesn't match anything
  expect_identical(res$needles, c(1L, 2L, 2L, 3L, 3L, 4L))
  expect_identical(res$haystack, c(NA, 2L, 4L, 2L, 4L, NA))

  # With data frame columns containing list-columns
  df1 <- data_frame(col = data_frame(x = x))
  df2 <- data_frame(col = data_frame(x = y))

  expect_identical(vec_matches(x, y, condition = ">"), res)
})

# ------------------------------------------------------------------------------
# vec_matches() - data frame

test_that("can match with 1 column data frames", {
  df1 <- data_frame(x = c(1L, 3L, 1L, 3L))
  df2 <- data_frame(x = c(1L, 3L, 1L))

  expect_identical(
    vec_matches(df1, df2),
    vec_matches(df1$x, df2$x)
  )
})

test_that("can match with >1 column data frames", {
  df1 <- data_frame(x = c(1L, 3L, 1L, 3L), y = c(1L, 4L, 1L, 2L))
  df2 <- data_frame(x = c(1L, 3L, 1L), y = c(1L, 2L, 1L))

  res <- vec_matches(df1, df2, condition = c("==", "=="))

  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 3L, 4L))
  expect_identical(res$haystack, c(1L, 3L, NA, 1L, 3L, 2L))
})

test_that("can match with df-cols of varying types", {
  y <- c(1L, 1L)

  expect_needles <- c(1L, 2L)
  expect_haystack <- c(NA, 1L)

  df1 <- data_frame(x = data_frame(x = c(2L, 1L), y = y))
  df2 <- data_frame(x = data_frame(x = c(1L, 3L), y = y))

  res <- vec_matches(df1, df2)
  expect_identical(res$needles, expect_needles)
  expect_identical(res$haystack, expect_haystack)

  df1 <- data_frame(x = data_frame(x = c(2, 1), y = y))
  df2 <- data_frame(x = data_frame(x = c(1, 3), y = y))

  res <- vec_matches(df1, df2)
  expect_identical(res$needles, expect_needles)
  expect_identical(res$haystack, expect_haystack)

  df1 <- data_frame(x = data_frame(x = c(TRUE, FALSE), y = y))
  df2 <- data_frame(x = data_frame(x = c(FALSE, NA), y = y))

  res <- vec_matches(df1, df2)
  expect_identical(res$needles, expect_needles)
  expect_identical(res$haystack, expect_haystack)

  df1 <- data_frame(x = data_frame(x = c("x", "y"), y = y))
  df2 <- data_frame(x = data_frame(x = c("y", "z"), y = y))

  res <- vec_matches(df1, df2)
  expect_identical(res$needles, expect_needles)
  expect_identical(res$haystack, expect_haystack)

  df1 <- data_frame(x = data_frame(x = complex(real = c(1, 2), imaginary = c(2, 1)), y = y))
  df2 <- data_frame(x = data_frame(x = complex(real = c(2, 3), imaginary = c(1, 1)), y = y))

  res <- vec_matches(df1, df2)
  expect_identical(res$needles, expect_needles)
  expect_identical(res$haystack, expect_haystack)
})

test_that("ensure that matching works if outer runs are present (i.e. `==` comes before non-equi condition)", {
  df1 <- data_frame(x = c(1, 2, 1, 1), y = c(2, 2, 3, 2))
  df2 <- data_frame(x = c(1, 1), y = c(2, 3))

  res <- vec_matches(df1, df2, condition = c("==", "<="))

  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 4L, 4L))
  expect_identical(res$haystack, c(1L, 2L, NA, 2L, 1L, 2L))

  df1$z <- c(1L, 2L, 1L, 3L)
  df2$z <- c(5L, 2L)

  res <- vec_matches(df1, df2, condition = c("==", "==", "<"))

  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, c(1L, NA, 2L, 1L))

  res <- vec_matches(df1, df2, condition = c("==", ">=", "<"))

  expect_identical(res$needles, c(1L, 2L, 3L, 3L, 4L))
  expect_identical(res$haystack, c(1L, NA, 1L, 2L, 1L))
})

test_that("df-cols propagate an NA if any columns are incomplete", {
  df <- data_frame(x = 1, y = data_frame(x = c(1, 1, NA), y = c(1, NA, 2)))

  res <- vec_matches(df, df, incomplete = "match")
  expect_identical(res$needles, 1:3)
  expect_identical(res$haystack, 1:3)

  # 2nd and 3rd rows aren't fully complete
  res <- vec_matches(df, df, incomplete = NA)
  expect_identical(res$needles, 1:3)
  expect_identical(res$haystack, c(1L, NA, NA))

  res <- vec_matches(df, df, incomplete = "drop")
  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, 1L)
})

test_that("df-cols aren't flattened, so `condition` is applied jointly on the df-col columns", {
  x <- data_frame(a = 1L, b = data_frame(x = 3L, y = 4L))
  y <- data_frame(a = 1L, b = data_frame(x = 2L, y = 5L))

  # In particular `x$b[1,] > y$b[1,]` because `3 > 4` and that breaks the tie
  # before any values of the `x$b$y` column are checked
  res <- vec_matches(x, y, condition = c("==", ">"))
  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, 1L)
})

# ------------------------------------------------------------------------------
# vec_matches() - rcrd

test_that("rcrd types can be matched", {
  x <- new_rcrd(list(x = c(1L, 3L), y = c(1L, 4L)))
  y <- new_rcrd(list(x = c(1L, 2L), y = c(1L, 5L)))

  res <- vec_matches(x, y, condition = "<=")
  expect_identical(res$needles, c(1L, 1L, 2L))
  expect_identical(res$haystack, c(1L, 2L, NA))

  # In particular: `(3, 4) > (2, 5)` since the first elt breaks the tie
  res <- vec_matches(x, y, condition = ">")
  expect_identical(res$needles, c(1L, 2L, 2L))
  expect_identical(res$haystack, c(NA, 1L, 2L))
})

test_that("rcrd type matching works with rcrd-cols", {
  x <- data_frame(a = c(1L, 1L), b = new_rcrd(list(x = c(1L, 3L), y = c(1L, 4L))))
  y <- data_frame(a = c(1L, 1L), b = new_rcrd(list(x = c(1L, 2L), y = c(1L, 5L))))

  res <- vec_matches(x, y, condition = c("==", "<="))
  expect_identical(res$needles, c(1L, 1L, 2L))
  expect_identical(res$haystack, c(1L, 2L, NA))

  res <- vec_matches(x, y, condition = c("==", ">"))
  expect_identical(res$needles, c(1L, 2L, 2L))
  expect_identical(res$haystack, c(NA, 1L, 2L))
})

test_that("rcrd type incompleteness is handled correctly", {
  x <- new_rcrd(list(x = c(1L, NA), y = c(NA_integer_, NA_integer_)))
  y <- new_rcrd(list(x = c(1L, 2L, NA), y = c(NA, 5L, NA)))

  # When `incomplete = "match"`, the types of incompleteness still must
  # match exactly to have a match. i.e. (x=1L, y=NA) doesn't match (x=NA, y=1L).
  # This is the same as the rule for data frames.
  res <- vec_matches(x, y, condition = "==", incomplete = "match")
  expect_identical(res$needles, c(1L, 2L))
  expect_identical(res$haystack, c(1L, 3L))

  # If any field contains NA, the entire observation is incomplete.
  res <- vec_matches(x, y, condition = "==", incomplete = NA)
  expect_identical(res$needles, c(1L, 2L))
  expect_identical(res$haystack, c(NA_integer_, NA_integer_))
})

# ------------------------------------------------------------------------------
# vec_matches() - missing values

test_that("integer missing values can match with equality condition", {
  res <- vec_matches(NA_integer_, c(1L, NA, 2L, NA), condition = "==")

  expect_identical(res$needles, c(1L, 1L))
  expect_identical(res$haystack, c(2L, 4L))
})

test_that("integer missing values can match with <= and >= condition, but don't match any other value", {
  res <- vec_matches(NA_integer_, c(1L, NA, 2L, NA), condition = "<=")

  expect_identical(res$needles, c(1L, 1L))
  expect_identical(res$haystack, c(2L, 4L))

  res <- vec_matches(NA_integer_, c(1L, NA, 2L, NA), condition = ">=")

  expect_identical(res$needles, c(1L, 1L))
  expect_identical(res$haystack, c(2L, 4L))
})

test_that("integer missing values don't match anything with < and > conditions", {
  res <- vec_matches(NA_integer_, c(1L, NA, 2L, NA), condition = "<")

  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, NA_integer_)

  res <- vec_matches(NA_integer_, c(1L, NA, 2L, NA), condition = ">")

  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, NA_integer_)
})

test_that("missing values match within columns", {
  df1 <- data_frame(x = c(1L, 2L, 1L), y = rep(NA_integer_, 3))
  df2 <- data_frame(x = c(2L, 1L, 1L), y = c(1L, NA, NA))

  res <- vec_matches(df1, df2, condition = c("==", "=="))

  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 3L))
  expect_identical(res$haystack, c(2L, 3L, NA, 2L, 3L))

  expect_identical(
    vec_matches(df1, df2, condition = c("<=", ">=")),
    vec_matches(df1, df2, condition = c("==", "=="))
  )

  res <- vec_matches(df1, df2, condition = c("<", ">"))

  expect_identical(res$needles, 1:3)
  expect_identical(res$haystack, rep(NA_integer_, 3))
})

test_that("integer needles can't match NAs in the haystack", {
  # At the C level, 1L > NA_integer_ (INT_MIN),
  # but we are careful to work around this
  res <- vec_matches(1L, c(1L, NA, 2L, NA), condition = ">=")

  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, 1L)

  res <- vec_matches(1L, c(1L, NA, 2L, NA), condition = ">")

  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, NA_integer_)
})

test_that("double needles can't match NAs or NaNs in the haystack", {
  # At the C level, our helpers assumg NA and NaN are the smallest values,
  # so we are careful to avoid including them with >= and > conditions
  res <- vec_matches(1, c(1, NA, 2, NaN), condition = ">=")

  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, 1L)

  res <- vec_matches(1, c(1, NA, 2, NaN), condition = ">")

  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, NA_integer_)
})

# ------------------------------------------------------------------------------
# vec_matches() - `incomplete`

test_that("can handle incomplete needles with `incomplete = <integer>`", {
  x <- c(1L, NA, 2L)
  y <- c(NA, 1L, 1L)

  res <- vec_matches(x, y, condition = "==", incomplete = NA)

  expect_identical(res$needles, c(1L, 1L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 3L, NA, NA))

  res <- vec_matches(x, y, condition = "<=", incomplete = 0L)

  expect_identical(res$needles, c(1L, 1L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 3L, 0L, NA))

  res <- vec_matches(x, y, condition = ">=", incomplete = -1L)

  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 3L))
  expect_identical(res$haystack, c(2L, 3L, -1L, 2L, 3L))
})

test_that("can drop incomplete needle rows with `incomplete = 'drop'", {
  x <- c(1L, NA, 2L)
  y <- c(NA, 1L, 1L)

  res <- vec_matches(x, y, condition = "==", incomplete = "drop")
  expect_identical(res$needles, c(1L, 1L, 3L))
  expect_identical(res$haystack, c(2L, 3L, NA))
})

test_that("if `incomplete = <integer>`, an NA in any column results in the value", {
  df1 <- data_frame(x = c(1L, NA, 2L, 1L, 1L), y = c(2L, 2L, NA, 1L, 1L))
  df2 <- data_frame(x = c(1L, 1L, 2L), y = c(1L, 1L, NA))

  res <- vec_matches(df1, df2, condition = c("==", "=="), incomplete = NA)

  expect_identical(res$needles, c(1L, 2L, 3L, 4L, 4L, 5L, 5L))
  expect_identical(res$haystack, c(NA, NA, NA, 1L, 2L, 1L, 2L))

  res <- vec_matches(df1, df2, condition = c(">=", ">="), incomplete = NA)

  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 4L, 4L, 5L, 5L))
  expect_identical(res$haystack, c(1L, 2L, NA, NA, 1L, 2L, 1L, 2L))
})

test_that("`incomplete = <integer> / 'drop'` still handles NAs in future columns when an earlier column has no matches", {
  df1 <- data_frame(x = c(1, 1, 2, 3), y = c(1, NA, NA, 4))
  df2 <- data_frame(x = c(1, 3), y = c(1, 5))

  # The 2 in row 3 of df1 has no match, but the NA in the 2nd column still propagates
  res <- vec_matches(df1, df2, incomplete = NA, no_match = -1L)

  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, c(1L, NA, NA, -1L))

  res <- vec_matches(df1, df2, incomplete = "drop", no_match = -1L)

  expect_identical(res$needles, c(1L, 4L))
  expect_identical(res$haystack, c(1L, -1L))

  # The 1 in row 1 and 2 of df1 have no match, but the NA in row 2 of the 2nd column propagates
  res <- vec_matches(df1, df2, incomplete = NA, no_match = -1L, condition = ">")

  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, c(-1L, NA, NA, 1L))

  res <- vec_matches(df1, df2, incomplete = "drop", no_match = -1L, condition = ">")

  expect_identical(res$needles, c(1L, 4L))
  expect_identical(res$haystack, c(-1L, 1L))
})

test_that("`incomplete` can error informatively", {
  expect_snapshot(error = TRUE, vec_matches(NA, 1, incomplete = "error"))
  expect_snapshot(error = TRUE, vec_matches(NA, 1, incomplete = "error", needles_arg = "foo"))
})

test_that("`incomplete` error is classed", {
  expect_error(vec_matches(NA, 1, incomplete = "error"), class = "vctrs_error_matches_incomplete")
})

test_that("`incomplete` is validated", {
  expect_snapshot(error = TRUE, vec_matches(1, 2, incomplete = 1.5))
  expect_snapshot(error = TRUE, vec_matches(1, 2, incomplete = c("match", "drop")))
  expect_snapshot(error = TRUE, vec_matches(1, 2, incomplete = "x"))
})

# ------------------------------------------------------------------------------
# vec_matches() - `condition`

test_that("multiple matches from a non-equi condition are returned in first appearance order", {
  res <- vec_matches(0L, c(1L, 0L, -1L, 0L), condition = "<=")

  expect_identical(res$needles, rep(1L, 3))
  expect_identical(res$haystack, c(1L, 2L, 4L))

  # Checking equi for good measure
  res <- vec_matches(0L, c(1L, 0L, -1L, 0L), condition = "==")

  expect_identical(res$needles, rep(1L, 2))
  expect_identical(res$haystack, c(2L, 4L))
})

test_that("multiple matches from a non-equi condition are returned in first appearance order when the matches are in different nesting containers", {
  df <- data_frame(x = 0, y = 0)
  df2 <- data_frame(x = 2:1, y = 1:2)

  res <- vec_matches(df, df2, condition = c("<=", "<="))

  expect_identical(res$needles, c(1L, 1L))
  expect_identical(res$haystack, c(1L, 2L))
})

test_that("`condition` is validated", {
  expect_error(vec_matches(1, 2, condition = 1), "`condition` must be a character vector, or `NULL`")
  expect_error(vec_matches(1, 2, condition = "x"), 'must only contain "==", ">", ">=", "<", or "<="')
  expect_error(vec_matches(1, 2, condition = c("==", "==")), "must be length 1, or the same length as the number of columns of the input")
})

# ------------------------------------------------------------------------------
# vec_matches() - `multiple`

test_that("can get all matches", {
  x <- vec_matches(c(1L, 3L), c(1L, 3L, 1L, 3L), multiple = "all")

  expect_identical(x$needles, c(1L, 1L, 2L, 2L))
  expect_identical(x$haystack, c(1L, 3L, 2L, 4L))
})

test_that("can get first match", {
  x <- vec_matches(c(1L, 3L), c(1L, 3L, 1L, 3L), multiple = "first")

  expect_identical(x$needles, 1:2)
  expect_identical(x$haystack, 1:2)
})

test_that("can get last match", {
  x <- vec_matches(c(1L, 3L), c(1L, 3L, 1L, 3L), multiple = "last")

  expect_identical(x$needles, 1:2)
  expect_identical(x$haystack, 3:4)
})

test_that("duplicate needles match the same haystack locations", {
  x <- vec_matches(c(1L, 3L, 1L, 3L), c(1L, 3L, 1L), multiple = "all")

  expect_identical(x$needles, c(1L, 1L, 2L, 3L, 3L, 4L))
  expect_identical(x$haystack, c(1L, 3L, 2L, 1L, 3L, 2L))
})

test_that("`multiple` can error informatively", {
  expect_snapshot(error = TRUE, vec_matches(1L, c(1L, 1L), multiple = "error"))
  expect_snapshot(error = TRUE, vec_matches(1L, c(1L, 1L), multiple = "error", needles_arg = "foo"))
  expect_snapshot(error = TRUE, vec_matches(1L, c(1L, 1L), multiple = "error", needles_arg = "foo", haystack_arg = "bar"))
  expect_snapshot(error = TRUE, vec_matches(1, 1:2, multiple = "error", condition = NULL))
})

test_that("`multiple` can warn informatively", {
  expect_snapshot(vec_matches(1L, c(1L, 1L), multiple = "warning"))
  expect_snapshot(vec_matches(1L, c(1L, 1L), multiple = "warning", needles_arg = "foo"))
  expect_snapshot(vec_matches(1L, c(1L, 1L), multiple = "warning", needles_arg = "foo", haystack_arg = "bar"))
  expect_snapshot(vec_matches(1, 1:2, multiple = "warning", condition = NULL))
})

test_that("warning falls back to 'all'", {
  expect_warning(
    result <- vec_matches(c(1L, 3L, 1L, 3L), c(1L, 3L, 1L), multiple = "warning"),
    class = "vctrs_warning_matches_multiple"
  )

  expect_identical(
    result,
    vec_matches(c(1L, 3L, 1L, 3L), c(1L, 3L, 1L), multiple = "all")
  )
})

test_that("errors on multiple matches that come from different nesting containers", {
  df <- data_frame(x = 0, y = 0)
  df2 <- data_frame(x = 1:2, y = 2:1)

  expect_error(
    vec_matches(df, df2, condition = c("<=", "<="), multiple = "error"),
    "multiple matches"
  )
})

test_that("`multiple = 'error'` doesn't error errneously on the last observation", {
  expect_error(res <- vec_matches(1:2, 1:2, multiple = "error"), NA)
  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, 1:2)
})

test_that("`multiple` is validated", {
  expect_error(vec_matches(1, 2, multiple = 1.5), "`multiple` must be a string")
  expect_error(vec_matches(1, 2, multiple = c("first", "last")), "`multiple` must be a string")
  expect_error(vec_matches(1, 2, multiple = "x"), '`multiple` must be one of "all", "first", "last", "warning", or "error"')
})

# ------------------------------------------------------------------------------
# vec_matches() - `no_match`

test_that("can control `no_match`", {
  x <- vec_matches(1:3, 1L)
  expect_identical(x$haystack, c(1L, NA, NA))

  x <- vec_matches(1:3, 1L, no_match = 0L)
  expect_identical(x$haystack, c(1L, 0L, 0L))
})

test_that("can drop unmatched needles", {
  x <- vec_matches(1:3, 2L, no_match = "drop")
  expect_identical(x$needles, 2L)
  expect_identical(x$haystack, 1L)
})

test_that("can drop unmatched missings when `incomplete = 'match'`", {
  x <- vec_matches(c(NaN, 2, NA), 2, no_match = "drop")
  expect_identical(x$needles, 2L)
  expect_identical(x$haystack, 1L)

  x <- vec_matches(c(NaN, 2, NA), NA, no_match = "drop", nan_distinct = FALSE)
  expect_identical(x$needles, c(1L, 3L))
  expect_identical(x$haystack, c(1L, 1L))

  x <- vec_matches(c(NaN, 2, NA), NA, no_match = "drop", nan_distinct = TRUE)
  expect_identical(x$needles, 3L)
  expect_identical(x$haystack, 1L)
})

test_that("can differentiate between `no_match` and `incomplete`", {
  res <- vec_matches(c(1, NA), 2, incomplete = NA, no_match = -1L)

  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, c(-1L, NA))
})

test_that("`no_match` can error informatively", {
  expect_snapshot(error = TRUE, vec_matches(1, 2, no_match = "error"))
  expect_snapshot(error = TRUE, vec_matches(1, 2, no_match = "error", needles_arg = "foo"))
  expect_snapshot(error = TRUE, vec_matches(1, 2, no_match = "error", needles_arg = "foo", haystack_arg = "bar"))
  expect_snapshot(error = TRUE, vec_matches(1, double(), no_match = "error", condition = NULL))
})

test_that("`no_match = 'error'` doesn't error on handled incomplete values", {
  res <- vec_matches(c(NA, NaN, NA, 1), c(NA, 1), incomplete = NA, no_match = "error")

  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, c(rep(NA, 3), 2L))
})

test_that("`no_match = 'drop'` doesn't drop handled incomplete values", {
  res <- vec_matches(c(NA, NaN, NA, 1), c(NA, 1), incomplete = NA, no_match = "drop")

  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, c(rep(NA, 3), 2L))
})

test_that("`no_match` is validated", {
  expect_snapshot(error = TRUE, vec_matches(1, 2, no_match = 1.5))
  expect_snapshot(error = TRUE, vec_matches(1, 2, no_match = c(1L, 2L)))
  expect_snapshot(error = TRUE, vec_matches(1, 2, no_match = "x"))
})

# ------------------------------------------------------------------------------
# vec_matches() - `remaining`

test_that("`remaining` can retain `haystack` values that `needles` didn't match", {
  res <- vec_matches(1, 0:2, remaining = NA)
  expect_identical(res$needles, c(1L, NA, NA))
  expect_identical(res$haystack, c(2L, 1L, 3L))

  res <- vec_matches(1, 0:2, remaining = NA, condition = ">=")
  expect_identical(res$needles, c(1L, 1L, NA))
  expect_identical(res$haystack, c(1L, 2L, 3L))

  res <- vec_matches(1, 0:2, remaining = NA, condition = "<")
  expect_identical(res$needles, c(1L, NA, NA))
  expect_identical(res$haystack, c(3L, 1L, 2L))
})

test_that("`incomplete` affects `needles` but not `haystack`", {
  # Matches NA to NA, so nothing remaining
  res <- vec_matches(c(1, NA), c(NA, 1), incomplete = "match", remaining = NA)
  expect_identical(res$needles, c(1L, 2L))
  expect_identical(res$haystack, c(2L, 1L))

  # `needles` NA value is propagated, so `haystack` is left with a remaining value
  res <- vec_matches(c(1, NA), c(NA, 1), incomplete = NA, remaining = NA)
  expect_identical(res$needles, c(1L, 2L, NA))
  expect_identical(res$haystack, c(2L, NA, 1L))

  # `needles` NA value is dropped, so `haystack` is left with a remaining value
  res <- vec_matches(c(1, NA), c(NA, 1), incomplete = "drop", remaining = NA)
  expect_identical(res$needles, c(1L, NA))
  expect_identical(res$haystack, c(2L, 1L))
})

test_that("`remaining` works with `condition = NULL` and empty `needles`", {
  res <- vec_matches(integer(), 1:5, condition = NULL, remaining = NA)
  expect_identical(res$needles, rep(NA_integer_, 5))
  expect_identical(res$haystack, 1:5)
})

test_that("`remaining` combined with `multiple = 'first/last'` treats non-first/last matches as remaining", {
  x <- c(1, 2)
  y <- c(1, 2, 2)

  res <- vec_matches(x, y, remaining = NA, multiple = "first")
  expect_identical(res$needles, c(1L, 2L, NA))
  expect_identical(res$haystack, c(1L, 2L, 3L))

  res <- vec_matches(x, y, remaining = NA, multiple = "last")
  expect_identical(res$needles, c(1L, 2L, NA))
  expect_identical(res$haystack, c(1L, 3L, 2L))
})

test_that("`remaining` combined with the haystack reordering retains appearance order", {
  x <- data_frame(a = 1, b = 4)
  y <- data_frame(a = c(2, 1, 0), b = c(2, 1, 0))

  # Appearance order for the haystack locations
  res <- vec_matches(x, y, condition = c("<=", ">="))
  expect_identical(res$needles, c(1L, 1L))
  expect_identical(res$haystack, c(1L, 2L))

  # Retain that appearance order of the matches, with remaining values appended
  res <- vec_matches(x, y, condition = c("<=", ">="), remaining = NA)
  expect_identical(res$needles, c(1L, 1L, NA))
  expect_identical(res$haystack, c(1L, 2L, 3L))
})

test_that("`remaining` can error informatively", {
  expect_snapshot(error = TRUE, vec_matches(1, 2, remaining = "error"))
  expect_snapshot(error = TRUE, vec_matches(1, 2, remaining = "error", needles_arg = "foo"))
  expect_snapshot(error = TRUE, vec_matches(1, 2, remaining = "error", needles_arg = "foo", haystack_arg = "bar"))
  expect_snapshot(error = TRUE, vec_matches(double(), c(1, 2), remaining = "error", condition = NULL))
})

test_that("`remaining` is validated", {
  expect_snapshot(error = TRUE, vec_matches(1, 2, remaining = 1.5))
  expect_snapshot(error = TRUE, vec_matches(1, 2, remaining = c(1L, 2L)))
  expect_snapshot(error = TRUE, vec_matches(1, 2, remaining = "x"))
})

# ------------------------------------------------------------------------------
# vec_matches() - filter

test_that("simple `filter`s work", {
  needles <- c(1, 2, 4)
  haystack <- c(2, 1, 3, 0)

  res <- vec_matches(needles, haystack, condition = "<", filter = "max")
  expect_identical(res$haystack, c(3L, 3L, NA))

  res <- vec_matches(needles, haystack, condition = "<", filter = "min")
  expect_identical(res$haystack, c(1L, 3L, NA))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max")
  expect_identical(res$haystack, c(2L, 1L, 3L))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "min")
  expect_identical(res$haystack, c(4L, 4L, 4L))
})

test_that("haystack duplicates are preserved", {
  needles <- c(1, 2, 4)
  haystack <- c(2, 1, 2, 3, 0, 1, 0)

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max")
  expect_identical(res$needles, c(1L, 1L, 2L, 2L, 3L))
  expect_identical(res$haystack, c(2L, 6L, 1L, 3L, 4L))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "min")
  expect_identical(res$needles, c(1L, 1L, 2L, 2L, 3L, 3L))
  expect_identical(res$haystack, c(5L, 7L, 5L, 7L, 5L, 7L))
})

test_that("haystack duplicates can be controlled by `multiple`", {
  needles <- c(1, 2, 4)
  haystack <- c(2, 1, 2, 3, 0, 1, 0)

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max", multiple = "first")
  expect_identical(res$needles, 1:3)
  expect_identical(res$haystack, c(2L, 1L, 4L))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max", multiple = "last")
  expect_identical(res$needles, 1:3)
  expect_identical(res$haystack, c(6L, 3L, 4L))
})

test_that("`filter` works when valid matches are in different nesting containers", {
  needles <- data_frame(x = 0L, y = 1L, z = 2L)
  haystack <- data_frame(x = c(1L, 2L, 1L, 0L), y = c(2L, 1L, 2L, 3L), z = c(3L, 3L, 2L, 2L))

  info <- compute_nesting_container_info(haystack, c("<=", "<=", "<="), "all")
  haystack_order <- info[[1]]
  container_ids <- info[[2]]

  # Rows 1 and 2 of haystack are in different nesting containers, but
  # both have the "max" filter value of `z=3` so both should be in the result.
  # Row 4 is in its own container, so it will be considered the "max"
  # of its group, but it is less than rows 1 and 2 so it will ultimately be
  # filtered out.
  expect_identical(container_ids, c(1L, 2L, 1L, 0L))
  expect_identical(haystack_order, c(4L, 3L, 1L, 2L))

  res <- vec_matches(needles, haystack, condition = c("<=", "<=", "<="), filter = c("none", "none", "max"))
  expect_identical(res$needles, c(1L, 1L))
  expect_identical(res$haystack, c(1L, 2L))

  res <- vec_matches(needles, haystack, condition = c("<=", "<=", "<="), filter = c("none", "none", "max"), multiple = "first")
  expect_identical(res$haystack, 1L)

  res <- vec_matches(needles, haystack, condition = c("<=", "<=", "<="), filter = c("none", "none", "max"), multiple = "last")
  expect_identical(res$haystack, 2L)
})

test_that("single filter is applied to all columns", {
  needles <- data_frame(x = 5L, y = 8L, z = 4L)
  haystack <- data_frame(x = c(1L, 3L, 2L, 2L), y = c(1L, 3L, 2L, 3L), z = c(1L, 2L, 3L, 3L))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max")
  expect_identical(res$haystack, 2L)

  res <- vec_matches(needles, haystack, condition = ">=", filter = "min")
  expect_identical(res$haystack, 1L)
})

test_that("different `filter`s can be used per column", {
  needles <- data_frame(x = c(0, 2, 1, 1), y = c(2, 0, 0, 4))
  haystack <- data_frame(x = c(2, 2, 2, 1, 1), y = c(1, 1, 2, 2, 1))

  res <- vec_matches(needles, haystack, condition = c(">=", "<"), filter = c("max", "min"))
  expect_identical(res$needles, c(1L, 2L, 2L, 3L, 4L))
  expect_identical(res$haystack, c(NA, 1L, 2L, 5L, NA))
})

test_that("`filter` works with incomplete values", {
  needles <- c(1, NA, 4, NA)
  haystack <- c(NA, 1, NA, 1, 3)

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max", incomplete = "match")
  expect_identical(res$needles, c(1L, 1L, 2L, 2L, 3L, 4L, 4L))
  expect_identical(res$haystack, c(2L, 4L, 1L, 3L, 5L, 1L, 3L))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max", incomplete = "match", multiple = "first")
  expect_identical(res$needles, 1:4)
  expect_identical(res$haystack, c(2L, 1L, 5L, 1L))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "max", incomplete = NA)
  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 4L))
  expect_identical(res$haystack, c(2L, 4L, NA, 5L, NA))
})

test_that("`filter` works with mixed NA and NaN", {
  needles <- c(1, NA, 4, NaN)
  haystack <- c(NA, 1, NaN, 1, 3)

  res <- vec_matches(needles, haystack, condition = ">=", filter = "min", incomplete = "match", nan_distinct = FALSE)
  expect_identical(res$needles, c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L))
  expect_identical(res$haystack, c(2L, 4L, 1L, 3L, 2L, 4L, 1L, 3L))

  res <- vec_matches(needles, haystack, condition = ">=", filter = "min", incomplete = "match", nan_distinct = TRUE)
  expect_identical(res$needles, c(1L, 1L, 2L, 3L, 3L, 4L))
  expect_identical(res$haystack, c(2L, 4L, 1L, 2L, 4L, 3L))
})

test_that("`filter` is validated", {
  expect_error(vec_matches(1, 2, filter = 1.5), "character vector")
  expect_error(vec_matches(1, 2, filter = "x"), 'one of "none", "min", or "max"')
  expect_error(vec_matches(1, 2, filter = c("min", "max")), "length 1, or the same length as")
})

# ------------------------------------------------------------------------------
# vec_matches() - edge cases

test_that("zero row `needles` results in zero row data frame output", {
  res <- vec_matches(integer(), 1:3)

  expect_identical(res$needles, integer())
  expect_identical(res$haystack, integer())

  res <- vec_matches(integer(), 1:3, condition = "<")

  expect_identical(res$needles, integer())
  expect_identical(res$haystack, integer())
})

test_that("zero row `haystack` results in no-matches for all needles", {
  res <- vec_matches(1:3, integer())

  expect_identical(res$needles, 1:3)
  expect_identical(res$haystack, rep(NA_integer_, 3))

  res <- vec_matches(1:3, integer(), condition = "<")

  expect_identical(res$needles, 1:3)
  expect_identical(res$haystack, rep(NA_integer_, 3))
})

test_that("zero row `haystack` still allows needle incomplete handling", {
  res <- vec_matches(c(1, NA), integer(), incomplete = NA, no_match = 0L)

  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, c(0L, NA))

  res <- vec_matches(c(1, NA), integer(), incomplete = NA, no_match = 0L, condition = "<")

  expect_identical(res$needles, 1:2)
  expect_identical(res$haystack, c(0L, NA))
})

test_that("`condition = NULL` is correct in all possible cases", {
  matches <- function(needles, haystack, multiple, no_match = NA) {
    vec_matches(needles, haystack, condition = NULL, multiple = multiple, no_match = no_match)
  }
  exp <- function(needles, haystack) {
    data_frame(needles = as.integer(needles), haystack = as.integer(haystack))
  }

  zero <- data_frame(.size = 0L)
  one <- data_frame(.size = 1L)
  two <- data_frame(.size = 2L)

  multiples <- c("all", "warning", "error", "first", "last")

  for (multiple in multiples) {
    # `zero` haystack
    expect_identical(matches(zero, zero, multiple = multiple), exp(integer(), integer()))
    expect_identical(matches(one, zero, multiple = multiple), exp(1, NA))
    expect_identical(matches(two, zero, multiple = multiple), exp(1:2, c(NA, NA)))
  }

  for (multiple in multiples) {
    # `one` haystack
    expect_identical(matches(zero, one, multiple = multiple), exp(integer(), integer()))
    expect_identical(matches(one, one, multiple = multiple), exp(1, 1))
    expect_identical(matches(two, one, multiple = multiple), exp(1:2, c(1, 1)))
  }

  # `two` haystack
  expect_identical(matches(zero, two, multiple = "all"), exp(integer(), integer()))
  expect_identical(matches(one, two, multiple = "all"), exp(c(1, 1), c(1, 2)))
  expect_identical(matches(two, two, multiple = "all"), exp(c(1, 1, 2, 2), c(1, 2, 1, 2)))

  expect_identical(matches(zero, two, multiple = "warning"), exp(integer(), integer()))
  expect_warning(expect_identical(matches(one,  two, multiple = "warning"), exp(c(1, 1), c(1, 2))))
  expect_warning(expect_identical(matches(two,  two, multiple = "warning"), exp(c(1, 1, 2, 2), c(1, 2, 1, 2))))

  expect_identical(matches(zero, two, multiple = "error"), exp(integer(), integer()))
  expect_error(matches(one, two, multiple = "error"), "multiple matches")
  expect_error(matches(two, two, multiple = "error"), "multiple matches")

  expect_identical(matches(zero, two, multiple = "first"), exp(integer(), integer()))
  expect_identical(matches(one, two, multiple = "first"), exp(1, 1))
  expect_identical(matches(two, two, multiple = "first"), exp(1:2, c(1, 1)))

  expect_identical(matches(zero, two, multiple = "last"), exp(integer(), integer()))
  expect_identical(matches(one, two, multiple = "last"), exp(1, 2))
  expect_identical(matches(two, two, multiple = "last"), exp(1:2, c(2, 2)))

  for (multiple in multiples) {
    # `zero` haystack with `no_match = "error"`
    expect_identical(matches(zero, zero, multiple = multiple, no_match = "error"), exp(integer(), integer()))
    expect_error(matches(one, zero, multiple = multiple, no_match = "error"), "must have a match")
    expect_error(matches(two, zero, multiple = multiple, no_match = "error"), "must have a match")
  }
})

test_that("zero column data frames are not allowed if `condition != NULL`", {
  expect_error(
    vec_matches(data_frame(.size = 2L), data_frame(.size = 2L)),
    "at least 1 column"
  )
})

test_that("zero column input still checks `condition` correctness", {
  x <- data_frame(.size = 2)
  y <- data_frame(.size = 3)

  expect_error(
    vec_matches(x, y, condition = c("==", "<=")),
    "length 1, or the same length as the number of columns"
  )
})

test_that("`multiple = 'first'/'last'` returns the first/last by appearance", {
  x <- c(1, 2, 3)
  y <- c(2, 1, 0)

  res <- vec_matches(x, y, condition = ">=", multiple = "first")
  expect_identical(res$haystack, c(2L, 1L, 1L))

  res <- vec_matches(x, y, condition = ">=", multiple = "last")
  expect_identical(res$haystack, c(3L, 3L, 3L))
})

test_that("NA adjustment of `>` and `>=` conditions is protected from empty haystack", {
  res <- vec_matches(1L, integer(), condition = ">")
  expect_identical(res$needles, 1L)
  expect_identical(res$haystack, NA_integer_)
})

test_that("`condition = NULL` works with `no_match = 'drop'`", {
  # All needles are unmatched with an empty haystack
  res <- vec_matches(1:2, integer(), condition = NULL, no_match = "drop")
  expect_identical(res$needles, integer())
  expect_identical(res$haystack, integer())
})

test_that("potential overflow on large output size is caught informatively", {
  # Windows 32-bit doesn't support long vectors of this size, and the
  # intermediate `r_ssize` will be too large
  skip_if(.Machine$sizeof.pointer < 8L, message = "No long vector support")

  expect_snapshot(error = TRUE, vec_matches(1:1e7, 1:1e7, condition = ">="))
  expect_snapshot(error = TRUE, vec_matches(1:1e7, 1:1e7, condition = NULL))
})

# ------------------------------------------------------------------------------
# vec_matches() - nesting containers

test_that("`multiple = 'first' doesn't require nesting containers if completely ordered", {
  # Single nesting container.
  # It is already completely ordered (in ascending order by value and row number).
  df <- data_frame(x = c(1L, 1L, 2L, 3L))
  res <- compute_nesting_container_info(df, ">=", "first")
  container_ids <- res[[2]]
  expect_identical(container_ids, integer())
})

test_that("`multiple = 'first'` requires increasing row order, and looks at group starts", {
  # 2 different containers:
  # - We are doing multiple = "first", so we are looking at group starts
  # - Look at the 1 at location 2, it becomes the first group along with its
  #   group partner at location 4.
  # - Look at the 2 at location 3, it is greater than the 1 at location 1 in
  #   both size and row number. It joins that group.
  # - Look at the 3 at location 1, it is greater than the 1 at location 1,
  #   but is smaller in row number. It becomes a new group with its partner at
  #   location 5.
  df <- data_frame(x = c(3L, 1L, 2L, 1L, 3L))
  res <- compute_nesting_container_info(df, ">=", "first")
  container_ids <- res[[2]]
  expect_identical(container_ids, c(1L, 0L, 0L, 0L, 1L))
})

test_that("`multiple = 'last'` requires increasing row order, and looks at group ends", {
  # 2 different containers:
  # - We are doing multiple = "last", so we are looking at group ends
  # - Look at the 1 at location 4, it becomes the first group along with its
  #   group partner at location 2.
  # - Look at the 2 at location 3, it is greater than the 1 at location 4,
  #   but is smaller in row number. It becomes a new group.
  # - Look at the 3 at location 5, it is greater than the 1 at location 4 in
  #   both size and row number. It joins that group.
  df <- data_frame(x = c(3L, 1L, 2L, 1L, 3L))
  res <- compute_nesting_container_info(df, ">=", "last")
  container_ids <- res[[2]]
  expect_identical(container_ids, c(0L, 0L, 1L, 0L, 0L))
})

