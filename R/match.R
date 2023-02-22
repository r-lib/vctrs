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
#'
#' @param relationship Handling of the expected relationship between
#'   `needles` and `haystack`. If the expectations chosen from the list below
#'   are invalidated, an error is thrown.
#'
#'   - `"none"` doesn't perform any relationship checks.
#'
#'   - `"one_to_one"` expects:
#'     - Each value in `needles` matches at most 1 value in `haystack`.
#'     - Each value in `haystack` matches at most 1 value in `needles`.
#'
#'   - `"one_to_many"` expects:
#'     - Each value in `needles` matches any number of values in `haystack`.
#'     - Each value in `haystack` matches at most 1 value in `needles`.
#'
#'   - `"many_to_one"` expects:
#'     - Each value in `needles` matches at most 1 value in `haystack`.
#'     - Each value in `haystack` matches any number of values in `needles`.
#'
#'   - `"many_to_many"` expects:
#'     - Each value in `needles` matches any number of values in `haystack`.
#'     - Each value in `haystack` matches any number of values in `needles`.
#'
#'     This performs no checks, and is identical to `"none"`, but is provided to
#'     allow you to be explicit about this relationship if you know it exists.
#'
#'   - `"warn_many_to_many"` doesn't assume there is any known relationship, but
#'     will warn if `needles` and `haystack` have a many-to-many relationship
#'     (which is typically unexpected), encouraging you to either take a closer
#'     look at your inputs or make this relationship explicit by specifying
#'     `"many_to_many"`.
#'
#'   `relationship` is applied after `filter` and `multiple` to allow potential
#'   multiple matches to be filtered out first.
#'
#'   `relationship` doesn't handle cases where there are zero matches. For that,
#'   see `no_match` and `remaining`.
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
#'
#' # Use `relationship` to add constraints and error on multiple matches if
#' # they aren't expected
#' try(vec_locate_matches(x, y, relationship = "one_to_one"))
#'
#' # In this case, the `NA` in `y` matches two rows in `x`
#' try(vec_locate_matches(x, y, relationship = "one_to_many"))
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
#' # Using `incomplete = NA` allows us to enforce the one-to-many relationship
#' # that we couldn't before
#' vec_locate_matches(x, y, relationship = "one_to_many", incomplete = NA)
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
                               relationship = "none",
                               nan_distinct = FALSE,
                               chr_proxy_collate = NULL,
                               needles_arg = "needles",
                               haystack_arg = "haystack",
                               error_call = current_env()) {
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
    relationship,
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

stop_matches <- function(class = NULL, ..., call = caller_env()) {
  stop_vctrs(
    class = c(class, "vctrs_error_matches"),
    ...,
    call = call
  )
}

warn_matches <- function(message, class = NULL, ..., call = caller_env()) {
  warn_vctrs(
    message = message,
    class = c(class, "vctrs_warning_matches"),
    ...,
    call = call
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
  glue::glue("Each value of `{cnd$needles_arg}` must have a match in `{cnd$haystack_arg}`.")
}

#' @export
cnd_body.vctrs_error_matches_nothing <- function(cnd, ...) {
  bullet <- glue::glue("Location {cnd$i} of `{cnd$needles_arg}` does not have a match.")
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
  glue::glue("Each value of `{cnd$haystack_arg}` must be matched by `{cnd$needles_arg}`.")
}

#' @export
cnd_body.vctrs_error_matches_remaining <- function(cnd, ...) {
  bullet <- glue::glue("Location {cnd$i} of `{cnd$haystack_arg}` was not matched.")
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
  glue::glue("`{cnd$needles_arg}` can't contain missing values.")
}

#' @export
cnd_body.vctrs_error_matches_incomplete <- function(cnd, ...) {
  bullet <- glue::glue("Location {cnd$i} contains missing values.")
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

#' @export
cnd_body.vctrs_error_matches_multiple <- function(cnd, ...) {
  cnd_matches_multiple_body(cnd$i, cnd$needles_arg)
}

# ------------------------------------------------------------------------------

warn_matches_multiple <- function(i, needles_arg, haystack_arg, call) {
  message <- paste(
    cnd_matches_multiple_header(needles_arg, haystack_arg),
    cnd_matches_multiple_body(i, needles_arg),
    sep = "\n"
  )

  warn_matches(
    message = message,
    class = "vctrs_warning_matches_multiple",
    i = i,
    needles_arg = needles_arg,
    haystack_arg = haystack_arg,
    call = call
  )
}

# ------------------------------------------------------------------------------

stop_matches_relationship_one_to_one <- function(i, which, needles_arg, haystack_arg, call) {
  stop_matches_relationship(
    class = "vctrs_error_matches_relationship_one_to_one",
    i = i,
    which = which,
    needles_arg = needles_arg,
    haystack_arg = haystack_arg,
    call = call
  )
}

#' @export
cnd_header.vctrs_error_matches_relationship_one_to_one <- function(cnd, ...) {
  if (cnd$which == "needles") {
    cnd_matches_multiple_header(cnd$needles_arg, cnd$haystack_arg)
  } else {
    cnd_matches_multiple_header(cnd$haystack_arg, cnd$needles_arg)
  }
}

#' @export
cnd_body.vctrs_error_matches_relationship_one_to_one <- function(cnd, ...) {
  if (cnd$which == "needles") {
    cnd_matches_multiple_body(cnd$i, cnd$needles_arg)
  } else {
    cnd_matches_multiple_body(cnd$i, cnd$haystack_arg)
  }
}


stop_matches_relationship_one_to_many <- function(i, needles_arg, haystack_arg, call) {
  stop_matches_relationship(
    class = "vctrs_error_matches_relationship_one_to_many",
    i = i,
    needles_arg = needles_arg,
    haystack_arg = haystack_arg,
    call = call
  )
}

#' @export
cnd_header.vctrs_error_matches_relationship_one_to_many <- function(cnd, ...) {
  cnd_matches_multiple_header(cnd$haystack_arg, cnd$needles_arg)
}

#' @export
cnd_body.vctrs_error_matches_relationship_one_to_many <- function(cnd, ...) {
  cnd_matches_multiple_body(cnd$i, cnd$haystack_arg)
}


stop_matches_relationship_many_to_one <- function(i, needles_arg, haystack_arg, call) {
  stop_matches_relationship(
    class = "vctrs_error_matches_relationship_many_to_one",
    i = i,
    needles_arg = needles_arg,
    haystack_arg = haystack_arg,
    call = call
  )
}

#' @export
cnd_header.vctrs_error_matches_relationship_many_to_one <- function(cnd, ...) {
  cnd_matches_multiple_header(cnd$needles_arg, cnd$haystack_arg)
}

#' @export
cnd_body.vctrs_error_matches_relationship_many_to_one <- function(cnd, ...) {
  cnd_matches_multiple_body(cnd$i, cnd$needles_arg)
}


stop_matches_relationship <- function(class = NULL, ..., call = caller_env()) {
  stop_matches(
    class = c(class, "vctrs_error_matches_relationship"),
    ...,
    call = call
  )
}

cnd_matches_multiple_header <- function(x_arg, y_arg) {
  glue::glue("Each value of `{x_arg}` can match at most 1 value from `{y_arg}`.")
}

cnd_matches_multiple_body <- function(i, name) {
  bullet <- glue::glue("Location {i} of `{name}` matches multiple values.")
  bullet <- c(x = bullet)
  format_error_bullets(bullet)
}

# ------------------------------------------------------------------------------

warn_matches_relationship_many_to_many <- function(i, j, needles_arg, haystack_arg, call) {
  message <- paste(
    glue::glue("Detected an unexpected many-to-many relationship between `{needles_arg}` and `{haystack_arg}`."),
    cnd_matches_multiple_body(i, needles_arg),
    cnd_matches_multiple_body(j, haystack_arg),
    sep = "\n"
  )

  warn_matches_relationship(
    message = message,
    class = "vctrs_warning_matches_relationship_many_to_many",
    i = i,
    j = j,
    needles_arg = needles_arg,
    haystack_arg = haystack_arg,
    call = call
  )
}

warn_matches_relationship <- function(message, class = NULL, ..., call = caller_env()) {
  warn_matches(
    message = message,
    class = c(class, "vctrs_warning_matches_relationship"),
    ...,
    call = call
  )
}
