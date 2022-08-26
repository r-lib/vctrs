#' Locate observations matching specified conditions
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `vec_locate_matches()` is a more flexible version of [vec_match()] used to
#' identify locations where each observation of `needles` matches one or
#' multiple observations in `haystack`. Unlike `vec_match()`,
#' `vec_locate_matches()` returns all matches by default, and can match on
#' binary conditions other than equality, such as `>`, `>=`, `<`, and `<=`.
#'
#' @details
#' [vec_match()] is identical to (but often slightly faster than):
#'
#' ```
#' vec_locate_matches(
#'   needles,
#'   haystack,
#'   condition = "==",
#'   multiple = "first",
#'   nan_distinct = TRUE
#' )
#' ```
#'
#' `vec_locate_matches()` is extremely similar to a SQL join between `needles`
#' and `haystack`, with the default being most similar to a left join.
#'
#' Be very careful when specifying match `condition`s. If a condition is
#' mis-specified, it is very easy to accidentally generate an exponentially
#' large number of matches.
#'
#' @section Dependencies of `vec_locate_matches()`:
#' * [vec_order_radix()]
#' * [vec_detect_complete()]
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#' @inheritParams order-radix
#'
#' @param needles,haystack Vectors used for matching.
#'   - `needles` represents the vector to search for.
#'   - `haystack` represents the vector to search in.
#'
#'   Prior to comparison, `needles` and `haystack` are coerced to the same type.
#'
#' @param condition Condition controlling how `needles` should be compared
#'   against `haystack` to identify a successful match.
#'   - One of: `"=="`, `">"`, `">="`, `"<"`, or `"<="`.
#'   - For data frames, a length `1` or `ncol(needles)` character vector
#'     containing only the above options, specifying how matching is determined
#'     for each column.
#'
#' @param filter Filter to be applied to the matched results.
#'   - `"none"` doesn't apply any filter.
#'   - `"min"` returns only the minimum haystack value matching the current
#'     needle.
#'   - `"max"` returns only the maximum haystack value matching the current
#'     needle.
#'   - For data frames, a length `1` or `ncol(needles)` character vector
#'     containing only the above options, specifying a filter to apply to
#'     each column.
#'
#'   Filters don't have any effect on `"=="` conditions, but are useful for
#'   computing "rolling" matches with other conditions.
#'
#'   A filter can return multiple haystack matches for a particular needle
#'   if the maximum or minimum haystack value is duplicated in `haystack`. These
#'   can be further controlled with `multiple`.
#'
#' @param incomplete Handling of missing values and
#'   [incomplete][vec_detect_complete] observations in `needles`.
#'   - `"compare"` uses `condition` to determine whether or not a missing value
#'     in `needles` matches a missing value in `haystack`. If `condition` is
#'     `==`, `>=`, or `<=`, then missing values will match.
#'   - `"match"` always allows missing values in `needles` to match missing
#'     values in `haystack`, regardless of the `condition`.
#'   - `"drop"` drops incomplete observations in `needles` from the result.
#'   - `"error"` throws an error if any `needles` are incomplete.
#'   - If a single integer is provided, this represents the value returned
#'     in the `haystack` column for observations of `needles` that are
#'     incomplete. If `no_match = NA`, setting `incomplete = NA` forces
#'     incomplete observations in `needles` to be treated like unmatched values.
#'
#'   `nan_distinct` determines whether a `NA` is allowed to match a `NaN`.
#'
#' @param no_match Handling of `needles` without a match.
#'   - `"drop"` drops `needles` with zero matches from the result.
#'   - `"error"` throws an error if any `needles` have zero matches.
#'   - If a single integer is provided, this represents the value returned in
#'     the `haystack` column for observations of `needles` that have zero
#'     matches. The default represents an unmatched needle with `NA`.
#'
#' @param remaining Handling of `haystack` values that `needles` never matched.
#'   - `"drop"` drops remaining `haystack` values from the result.
#'     Typically, this is the desired behavior if you only care when `needles`
#'     has a match.
#'   - `"error"` throws an error if there are any remaining `haystack`
#'     values.
#'   - If a single integer is provided (often `NA`), this represents the value
#'     returned in the `needles` column for the remaining `haystack` values
#'     that `needles` never matched. Remaining `haystack` values are always
#'     returned at the end of the result.
#'
#' @param multiple Handling of `needles` with multiple matches. For each needle:
#'   - `"all"` returns all matches detected in `haystack`.
#'   - `"any"` returns any match detected in `haystack` with no guarantees on
#'     which match will be returned. It is often faster than `"first"` and
#'     `"last"` if you just need to detect if there is at least one match.
#'   - `"first"` returns the first match detected in `haystack`.
#'   - `"last"` returns the last match detected in `haystack`.
#'   - `"warning"` throws a warning if multiple matches are detected, but
#'     otherwise falls back to `"all"`.
#'   - `"error"` throws an error if multiple matches are detected.
#'
#' @param needles_arg,haystack_arg Argument tags for `needles` and `haystack`
#'   used in error messages.
#'
#' @return A two column data frame containing the locations of the matches.
#'   - `needles` is an integer vector containing the location of
#'     the needle currently being matched.
#'   - `haystack` is an integer vector containing the location of the
#'     corresponding match in the haystack for the current needle.
#'
#' @export
#' @examples
#' x <- c(1, 2, NA, 3, NaN)
#' y <- c(2, 1, 4, NA, 1, 2, NaN)
#'
#' # By default, for each element of `x`, all matching locations in `y` are
#' # returned
#' matches <- vec_locate_matches(x, y)
#' matches
#'
#' # The result can be used to slice the inputs to align them
#' data_frame(
#'   x = vec_slice(x, matches$needles),
#'   y = vec_slice(y, matches$haystack)
#' )
#'
#' # If multiple matches are present, control which is returned with `multiple`
#' vec_locate_matches(x, y, multiple = "first")
#' vec_locate_matches(x, y, multiple = "last")
#' vec_locate_matches(x, y, multiple = "any")
#' try(vec_locate_matches(x, y, multiple = "error"))
#'
#' # By default, NA is treated as being identical to NaN.
#' # Using `nan_distinct = TRUE` treats NA and NaN as different values, so NA
#' # can only match NA, and NaN can only match NaN.
#' vec_locate_matches(x, y, nan_distinct = TRUE)
#'
#' # If you never want missing values to match, set `incomplete = NA` to return
#' # `NA` in the `haystack` column anytime there was an incomplete observation
#' # in `needles`.
#' vec_locate_matches(x, y, incomplete = NA)
#'
#' # `no_match` allows you to specify the returned value for a needle with
#' # zero matches. Note that this is different from an incomplete value,
#' # so specifying `no_match` allows you to differentiate between incomplete
#' # values and unmatched values.
#' vec_locate_matches(x, y, incomplete = NA, no_match = 0L)
#'
#' # If you want to require that every `needle` has at least 1 match, set
#' # `no_match` to `"error"`:
#' try(vec_locate_matches(x, y, incomplete = NA, no_match = "error"))
#'
#' # By default, `vec_locate_matches()` detects equality between `needles` and
#' # `haystack`. Using `condition`, you can detect where an inequality holds
#' # true instead. For example, to find every location where `x[[i]] >= y`:
#' matches <- vec_locate_matches(x, y, condition = ">=")
#'
#' data_frame(
#'   x = vec_slice(x, matches$needles),
#'   y = vec_slice(y, matches$haystack)
#' )
#'
#' # You can limit which matches are returned with a `filter`. For example,
#' # with the above example you can filter the matches returned by `x[[i]] >= y`
#' # down to only the ones containing the maximum `y` value of those matches.
#' matches <- vec_locate_matches(x, y, condition = ">=", filter = "max")
#'
#' # Here, the matches for the `3` needle value have been filtered down to
#' # only include the maximum haystack value of those matches, `2`. This is
#' # often referred to as a rolling join.
#' data_frame(
#'   x = vec_slice(x, matches$needles),
#'   y = vec_slice(y, matches$haystack)
#' )
#'
#' # In the very rare case that you need to generate locations for a
#' # cross match, where every observation of `x` is forced to match every
#' # observation of `y` regardless of what the actual values are, you can
#' # replace `x` and `y` with integer vectors of the same size that contain
#' # a single value and match on those instead.
#' x_proxy <- vec_rep(1L, vec_size(x))
#' y_proxy <- vec_rep(1L, vec_size(y))
#' nrow(vec_locate_matches(x_proxy, y_proxy))
#' vec_size(x) * vec_size(y)
#'
#' # By default, missing values will match other missing values when using
#' # `==`, `>=`, or `<=` conditions, but not when using `>` or `<` conditions.
#' # This is similar to how `vec_compare(x, y, na_equal = TRUE)` works.
#' x <- c(1, NA)
#' y <- c(NA, 2)
#'
#' vec_locate_matches(x, y, condition = "<=")
#' vec_locate_matches(x, y, condition = "<")
#'
#' # You can force missing values to match regardless of the `condition`
#' # by using `incomplete = "match"`
#' vec_locate_matches(x, y, condition = "<", incomplete = "match")
#'
#' # You can also use data frames for `needles` and `haystack`. The
#' # `condition` will be recycled to the number of columns in `needles`, or
#' # you can specify varying conditions per column. In this example, we take
#' # a vector of date `values` and find all locations where each value is
#' # between lower and upper bounds specified by the `haystack`.
#' values <- as.Date("2019-01-01") + 0:9
#' needles <- data_frame(lower = values, upper = values)
#'
#' set.seed(123)
#' lower <- as.Date("2019-01-01") + sample(10, 10, replace = TRUE)
#' upper <- lower + sample(3, 10, replace = TRUE)
#' haystack <- data_frame(lower = lower, upper = upper)
#'
#' # (values >= lower) & (values <= upper)
#' matches <- vec_locate_matches(needles, haystack, condition = c(">=", "<="))
#'
#' data_frame(
#'   lower = vec_slice(lower, matches$haystack),
#'   value = vec_slice(values, matches$needle),
#'   upper = vec_slice(upper, matches$haystack)
#' )
vec_locate_matches <- function(needles,
                               haystack,
                               ...,
                               condition = "==",
                               filter = "none",
                               incomplete = "compare",
                               no_match = NA_integer_,
                               remaining = "drop",
                               multiple = "all",
                               nan_distinct = FALSE,
                               chr_proxy_collate = NULL,
                               needles_arg = "",
                               haystack_arg = "",
                               call = current_env()) {
  check_dots_empty0(...)
  frame <- environment()

  .Call(
    ffi_locate_matches,
    needles,
    haystack,
    condition,
    filter,
    incomplete,
    no_match,
    remaining,
    multiple,
    nan_distinct,
    chr_proxy_collate,
    needles_arg,
    haystack_arg,
    frame
  )
}

# ------------------------------------------------------------------------------

#' Internal FAQ - Implementation of `vec_locate_matches()`
#'
#' ```{r, child = "man/faq/internal/matches-algorithm.Rmd"}
#' ```
#'
#' @name internal-faq-matches-algorithm
NULL

# ------------------------------------------------------------------------------

# Helper used for testing and in the internal FAQ.
# It needs to live in R/ to be usable by the FAQ Rmd.
compute_nesting_container_info <- function(x, condition) {
  .Call(ffi_compute_nesting_container_info, x, condition)
}

# ------------------------------------------------------------------------------

stop_matches <- function(class = NULL, ...) {
  stop_vctrs(
    class = c(class, "vctrs_error_matches"),
    ...
  )
}

warn_matches <- function(message, class = NULL, ...) {
  warn_vctrs(
    message = message,
    class = c(class, "vctrs_warning_matches"),
    ...
  )
}

# ------------------------------------------------------------------------------

stop_matches_nothing <- function(i, needles_arg, haystack_arg, call) {
  stop_matches(
    class = "vctrs_error_matches_nothing",
    i = i,
    needles_arg = needles_arg,
    haystack_arg = haystack_arg,
    call = call
  )
}

#' @export
cnd_header.vctrs_error_matches_nothing <- function(cnd, ...) {
  if (nzchar(cnd$needles_arg)) {
    needles_name <- glue::glue(" of `{cnd$needles_arg}` ")
  } else {
    needles_name <- " "
  }

  if (nzchar(cnd$haystack_arg)) {
    haystack_name <- glue::glue(" in `{cnd$haystack_arg}`")
  } else {
    haystack_name <- ""
  }

  glue::glue("Each element{needles_name}must have a match{haystack_name}.")
}

#' @export
cnd_body.vctrs_error_matches_nothing <- function(cnd, ...) {
  bullet <- glue::glue("The element at location {cnd$i} does not have a match.")
  bullet <- c(x = bullet)
  format_error_bullets(bullet)
}

# ------------------------------------------------------------------------------

stop_matches_remaining <- function(i, needles_arg, haystack_arg, call) {
  stop_matches(
    class = "vctrs_error_matches_remaining",
    i = i,
    needles_arg = needles_arg,
    haystack_arg = haystack_arg,
    call = call
  )
}

#' @export
cnd_header.vctrs_error_matches_remaining <- function(cnd, ...) {
  if (nzchar(cnd$haystack_arg)) {
    haystack_name <- glue::glue(" of `{cnd$haystack_arg}` ")
  } else {
    haystack_name <- " "
  }

  if (nzchar(cnd$needles_arg)) {
    needles_name <- glue::glue(" by `{cnd$needles_arg}`")
  } else {
    needles_name <- ""
  }

  glue::glue("Each haystack value{haystack_name}must be matched{needles_name}.")
}

#' @export
cnd_body.vctrs_error_matches_remaining <- function(cnd, ...) {
  bullet <- glue::glue("The value at location {cnd$i} was not matched.")
  bullet <- c(x = bullet)
  format_error_bullets(bullet)
}

# ------------------------------------------------------------------------------

stop_matches_incomplete <- function(i, needles_arg, call) {
  stop_matches(
    class = "vctrs_error_matches_incomplete",
    i = i,
    needles_arg = needles_arg,
    call = call
  )
}

#' @export
cnd_header.vctrs_error_matches_incomplete <- function(cnd, ...) {
  if (nzchar(cnd$needles_arg)) {
    needles_name <- glue::glue(" of `{cnd$needles_arg}` ")
  } else {
    needles_name <- " "
  }

  glue::glue("No element{needles_name}can contain missing values.")
}

#' @export
cnd_body.vctrs_error_matches_incomplete <- function(cnd, ...) {
  bullet <- glue::glue("The element at location {cnd$i} contains missing values.")
  bullet <- c(x = bullet)
  format_error_bullets(bullet)
}

# ------------------------------------------------------------------------------

stop_matches_multiple <- function(i, needles_arg, haystack_arg, call) {
  stop_matches(
    class = "vctrs_error_matches_multiple",
    i = i,
    needles_arg = needles_arg,
    haystack_arg = haystack_arg,
    call = call
  )
}

#' @export
cnd_header.vctrs_error_matches_multiple <- function(cnd, ...) {
  cnd_matches_multiple_header(cnd$needles_arg, cnd$haystack_arg)
}
cnd_matches_multiple_header <- function(needles_arg, haystack_arg) {
  if (nzchar(needles_arg)) {
    needles_name <- glue::glue(" of `{needles_arg}` ")
  } else {
    needles_name <- " "
  }

  if (nzchar(haystack_arg)) {
    haystack_name <- glue::glue(" from `{haystack_arg}`")
  } else {
    haystack_name <- ""
  }

  glue::glue("Each element{needles_name}can match at most 1 observation{haystack_name}.")
}

#' @export
cnd_body.vctrs_error_matches_multiple <- function(cnd, ...) {
  cnd_matches_multiple_body(cnd$i)
}
cnd_matches_multiple_body <- function(i) {
  bullet <- glue::glue("The element at location {i} has multiple matches.")
  bullet <- c(x = bullet)
  format_error_bullets(bullet)
}

# ------------------------------------------------------------------------------

warn_matches_multiple <- function(i, needles_arg, haystack_arg) {
  message <- paste(
    cnd_matches_multiple_header(needles_arg, haystack_arg),
    cnd_matches_multiple_body(i),
    sep = "\n"
  )

  warn_matches(
    message = message,
    class = "vctrs_warning_matches_multiple",
    i = i,
    needles_arg = needles_arg,
    haystack_arg = haystack_arg
  )
}
