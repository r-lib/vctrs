# group_id_and_loc ----------------------------------------------------------------

expect_matches_separate_calls <- function(x) {
  expect_equal(
    as.numeric(vec_group_id_and_loc(x)),
    as.numeric(vec_group_id(x))
  )
  expect_equal(
    attr(vec_group_id_and_loc(x), "unique_loc"),
    vec_unique_loc(x)
  )
}

test_that("vec_group_id_and_loc matches vec_group_id and vec_unique_loc", {
  x <- c(2, 4, 2, 1, 4)
  expect_matches_separate_calls(x)
})

test_that("vec_group_id_and_loc works for size 0 input", {
  expect <- structure(integer(), n = 0L, unique_loc=integer())
  expect_equal(vec_group_id_and_loc(NULL), expect)
  expect_equal(vec_group_id_and_loc(numeric()), expect)
})

test_that("vec_group_id_and_loc works on base S3 objects", {
  x <- factor(c("x", "y", "x"))
  expect_matches_separate_calls(x)

  x <- new_date(c(0, 1, 0))
  expect_matches_separate_calls(x)
})

test_that("vec_group_id_and_loc works on data frames", {
  df <- data.frame(x = c(1, 2, 1, 1), y = c(2, 3, 2, 3))
  expect_matches_separate_calls(df)
})

test_that("vec_group_id_and_loc works on arrays", {
  x <- array(c(1, 1, 1, 2, 4, 2), c(3, 2))
  expect_matches_separate_calls(x)
})

test_that("vec_group_id takes the equality proxy", {
  local_comparable_tuple()
  x <- tuple(c(1, 2, 1, 1), c(1, 1, 1, 2))
  # Compares on only the first field
  expect_matches_separate_calls(x)
})

test_that("vec_group_id takes the equality proxy recursively", {
  local_comparable_tuple()

  x <- tuple(c(1, 2, 1, 1), 1:4)
  df <- data_frame(x = x)
  expect_matches_separate_calls(df)
})


# vec_deduplicate ---------------------------------------------------------

test_that("vec_deduplicate(f) runs only on deduplicated values", {
  ncalls <<- 0
  f <- function(ii) for(i in ii) ncalls <<- ncalls + 1

  x <- c(1, 1, 1, 2, 3)
  vec_deduplicate(f)(x)
  expect_equal(ncalls, 3)

  ncalls <<- 0
  x <- 1:5
  vec_deduplicate(f)(x)
  expect_equal(ncalls, 5)
})
