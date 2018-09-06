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

vec_match <- function(needles, haystack) {
  v <- vec_coerce(needles = needles, haystack = haystack)
  .Call(vctrs_match, v$needles, v$haystack)
}

reset_rownames <- function(x) {
  rownames(x) <- NULL

  is_df <- map_lgl(x, is.data.frame)
  x[is_df] <- lapply(x[is_df], `rownames<-`, NULL)

  x
}

# Duplicates --------------------------------------------------------------

vec_duplicated <- function(x) {
  x <- vec_proxy_equality(x)
  .Call(vctrs_duplicated, x)
}

vec_duplicated_any <- function(x) {
  x <- vec_proxy_equality(x)
  .Call(vctrs_duplicated_any, x)
}

vec_unique <- function(x) {
  vec_subset(x, vec_unique_loc(x))
}

vec_unique_loc <- function(x) {
  .Call(vctrs_unique_loc, x)
}

vec_n_distinct <- function(x) {
  x <- vec_proxy_equality(x)
  .Call(vctrs_n_distinct, x)
}

vec_id <- function(x) {
  x <- vec_proxy_equality(x)
  .Call(vctrs_id, x)
}
