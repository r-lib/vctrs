#' Count unique values in a vector
#'
#' Count the number of unique values in a vector. `vec_count()` has two
#' important differences to `table()`: it returns a data frame, and when
#' given multiple inputs (as a data frame), it only counts combinations that
#' appear in the input.
#'
#' @param x A vector
#' @param sort One of "count", "key", "location", or "none".
#'  * "count", the default, puts most frequent values at top
#'  * "key", orders by the output key column (i.e. unique values of `x`)
#'  * "location", orders by location where key first seen. This is useful
#'     if you want to match the counts up to other unique/duplicated functions.
#'  * "none", leaves unordered.
#' @return A data frame with columns `key` (same type as `x`) and
#'   `count` (an integer vector).
#' @export
#' @examples
#' vec_count(mtcars$vs)
#' vec_count(iris$Species)
#'
#' # If you count a data frame you'll get a data frame
#' # column in the output
#' str(vec_count(mtcars[c("vs", "am")]))
#'
#' # Sorting ---------------------------------------
#'
#' x <- letters[rpois(100, 6)]
#' # default is to sort by frequency
#' vec_count(x)
#'
#' # by can sort by key
#' vec_count(x, sort = "key")
#'
#' # or location of first value
#' vec_count(x, sort = "location")
#' head(x)
#'
#' # or not at all
#' vec_count(x, sort = "none")
vec_count <- function(x, sort = c("count", "key", "location", "none")) {
  sort <- match.arg(sort)

  # Returns key-value pair giving index of first occurence value and count
  kv <- .Call(vctrs_count, vec_proxy_equality(x))

  df <- data.frame(key = 0, count = kv$val)
  df$key <- vec_subset(x, kv$key) # might be a dataframe

  if (sort == "none")
    return(df)

  idx <- switch(sort,
    location = order(kv$key),
    key = order(df$key),
    count = order(-kv$val)
  )

  df <- df[idx, , drop = FALSE]
  reset_rownames(df)
}

reset_rownames <- function(x) {
  rownames(x) <- NULL

  is_df <- map_lgl(x, is.data.frame)
  x[is_df] <- lapply(x[is_df], `rownames<-`, NULL)

  x
}

# Duplicates --------------------------------------------------------------

#' Find duplicated values
#'
#' * `vec_duplicate_any()`: detects the presence of any duplicated values,
#'   a la [anyDuplicated()].
#' * `vec_duplicate_detect()`: returns a logical vector describing if each
#'   element of the vector is duplicated elsewhere. Unlike [duplicated()], it
#'   reports all duplicated values, not just the second and subsequent
#'   repetitions.
#' * `vec_duplicate_id()`: returns an integer vector given the location of
#'   the first occurence of the value
#'
#' @param x A vector (including data frames).
#' @return
#'   * `vec_duplicate_any()`: a logical vector of length 1.
#'   * `vec_duplicate_detect()`: a logical vector the same length as `x`
#'   * `vec_duplicate_id()`: an integer vector the same length as `x`
#' @seealso [vec_unique()] for functions that work with the dual of duplicated
#'   values: unique values.
#' @name vec_duplicate
#' @examples
#' vec_duplicate_any(1:10)
#' vec_duplicate_any(c(1, 1:10))
#'
#' x <- c(10, 10, 20, 30, 30, 40)
#' vec_duplicate_detect(x)
#' # Note that `duplicated()` doesn't consider the first instance to
#' # be a duplicate
#' duplicated(x)
#'
#' # Identify elements of a vector by the location of the first element that
#' # they're equal to:
#' vec_duplicate_id(x)
#'
#' # Equivalent to `duplicated()`:
#' vec_duplicate_id(x) == seq_along(x)
NULL

#' @rdname vec_duplicate
#' @export
vec_duplicate_any <- function(x) {
  x <- vec_proxy_equality(x)
  .Call(vctrs_duplicated_any, x)
}

#' @rdname vec_duplicate
#' @export
vec_duplicate_detect <- function(x) {
  x <- vec_proxy_equality(x)
  .Call(vctrs_duplicated, x)
}

#' @rdname vec_duplicate
#' @export
vec_duplicate_id <- function(x) {
  x <- vec_proxy_equality(x)
  .Call(vctrs_id, x)
}

# Unique values -----------------------------------------------------------

vec_unique <- function(x) {
  x <- vec_proxy_equality(x)
  vec_subset(x, vec_unique_loc(x))
}

vec_unique_loc <- function(x) {
  x <- vec_proxy_equality(x)
  .Call(vctrs_unique_loc, x)
}

vec_unique_count <- function(x) {
  x <- vec_proxy_equality(x)
  .Call(vctrs_n_distinct, x)
}


# Matching ----------------------------------------------------------------

vec_in <- function(needles, haystack) {
  v <- vec_coerce(needles = needles, haystack = haystack)
  .Call(vctrs_in, vec_proxy_equality(v$needles), vec_proxy_equality(v$haystack))
}

vec_match <- function(needles, haystack) {
  v <- vec_coerce(needles = needles, haystack = haystack)
  .Call(vctrs_match, vec_proxy_equality(v$needles), vec_proxy_equality(v$haystack))
}

