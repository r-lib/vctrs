#' Group overlapping intervals
#'
#' @description
#' These functions are used to group together any overlaps that are present
#' within a set of vector intervals. When multiple overlapping intervals are
#' grouped together they result in a wider interval containing the smallest
#' `start` and the largest `end` of the overlaps.
#'
#' - `vec_interval_groups()` merges all overlapping intervals found within
#' `start` and `end`. The resulting intervals are known as the interval
#' "groups".
#'
#' - `vec_interval_locate_groups()` returns a two column data frame with a `key`
#' column containing the result of `vec_interval_groups()` and a `loc`
#' list-column containing integer vectors that map each interval in `start` and
#' `end` to the group that it falls in.
#'
#' These functions require that `start < end`. Additionally, intervals are
#' treated as if they are right-open, i.e. `[start, end)`.
#'
#' @section Assumptions:
#' For performance and simplicity, these functions make a few assumptions about
#' `start` and `end` that are not checked internally:
#'
#' - `start < end` must be true, with an exception for missing intervals.
#'
#' - If the i-th observation of `start` is missing, then the i-th observation
#' of `end` must also be missing.
#'
#' - Each observation of `start` and `end` must be either
#' [complete][vec_detect_complete] or [missing][vec_detect_missing]. Partially
#' complete values such as `start = data_frame(x = 1, y = NA)` are not allowed.
#'
#' If any of these assumptions are invalid, then the result is undefined.
#'
#' Developer note: These assumptions stem from the idea that if these functions
#' were in ivs itself, then we could safely make these assumptions in the C
#' code, because the `iv()` helper would assert them for us ahead of time.
#' Trying to re-assert these checks in the C code here is wasteful and makes the
#' code more complex.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param start,end
#'   A pair of vectors representing the starts and ends of the intervals.
#'
#'   It is required that `start < end`.
#'
#'   `start` and `end` will be cast to their common type, and must have the same
#'   size.
#'
#' @param abutting
#'   A single logical controlling whether or not abutting intervals should be
#'   grouped together. If `TRUE`, `[a, b)` and `[b, c)` will be grouped.
#'
#' @param missing
#'   Handling of missing intervals.
#'
#'   - `"group"`: Group all missing intervals together.
#'
#'   - `"drop"`: Drop all missing intervals from the result.
#'
#' @return
#' - `vec_interval_groups()` returns a data frame with two columns, `start` and
#' `end`, which contain vectors matching the types of `start` and `end`.
#'
#' - `vec_interval_locate_groups()` returns a data frame with two columns, `key`
#' and `loc`. `key` contains the result of `vec_interval_groups()` and `loc` is
#' a list of integer vectors.
#'
#' @name interval-groups
#'
#' @examples
#' bounds <- data_frame(
#'   start = c(1, 2, NA, 5, NA, 9, 12),
#'   end = c(5, 3, NA, 6, NA, 12, 14)
#' )
#' bounds
#'
#' # Group overlapping intervals together
#' vec_interval_groups(bounds$start, bounds$end)
#'
#' # You can choose not to group abutting intervals if you want to retain
#' # those boundaries
#' vec_interval_groups(bounds$start, bounds$end, abutting = FALSE)
#'
#' # You can also choose to drop all missing intervals if you don't consider
#' # them part of the result
#' vec_interval_groups(bounds$start, bounds$end, missing = "drop")
#'
#' # You can also locate the groups, which allows you to map each original
#' # interval to its corresponding group
#' vec_interval_locate_groups(bounds$start, bounds$end)
#'
#' @noRd
vec_interval_groups <- function(start,
                                end,
                                ...,
                                abutting = TRUE,
                                missing = "group") {
  check_dots_empty0(...)
  .Call(ffi_interval_groups, start, end, abutting, missing)
}

#' @noRd
#' @rdname interval-groups
vec_interval_locate_groups <- function(start,
                                       end,
                                       ...,
                                       abutting = TRUE,
                                       missing = "group") {
  check_dots_empty0(...)
  .Call(ffi_interval_locate_groups, start, end, abutting, missing)
}

# ------------------------------------------------------------------------------

#' Interval complement
#'
#' @description
#' `vec_interval_complement()` takes the complement of the intervals defined by
#' `start` and `end`. The complement can also be thought of as the "gaps"
#' between the intervals. By default, the minimum of `start` and the maximum of
#' `end` define the bounds to take the complement over, but this can be adjusted
#' with `lower` and `upper`. Missing intervals are always dropped from the
#' complement.
#'
#' These functions require that `start < end`. Additionally, intervals are
#' treated as if they are right-open, i.e. `[start, end)`.
#'
#' @inheritSection interval-groups Assumptions
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param start,end
#'   A pair of vectors representing the starts and ends of the intervals.
#'
#'   It is required that `start < end`.
#'
#'   `start` and `end` will be cast to their common type, and must have the same
#'   size.
#'
#' @param lower,upper
#'   Bounds for the universe over which to compute the complement. These should
#'   be singular values with the same type as `start` and `end`.
#'
#' @return
#' A two column data frame with a `start` column containing a vector of the
#' same type as `start` and an `end` column containing a vector of the same
#' type as `end`.
#'
#' @examples
#' x <- data_frame(
#'   start = c(10, 0, NA, 3, -5, NA),
#'   end = c(12, 5, NA, 6, -2, NA)
#' )
#' x
#'
#' # The complement contains any values from `[-5, 12)` that aren't represented
#' # in these intervals. Missing intervals are dropped.
#' vec_interval_complement(x$start, x$end)
#'
#' # Expand out the "universe" of possible values
#' vec_interval_complement(x$start, x$end, lower = -Inf)
#' vec_interval_complement(x$start, x$end, lower = -Inf, upper = Inf)
#'
#' @noRd
vec_interval_complement <- function(start,
                                    end,
                                    ...,
                                    lower = NULL,
                                    upper = NULL) {
  check_dots_empty0(...)
  .Call(ffi_interval_complement, start, end, lower, upper)
}

# ------------------------------------------------------------------------------

#' Interval containers
#'
#' @description
#' `vec_interval_locate_containers()` locates interval _containers_. Containers
#' are defined as the widest intervals that aren't contained by any other
#' interval. The returned locations will arrange the containers in ascending
#' order.
#'
#' For example, with the following vector of intervals: `[1, 5), [2, 6), [3, 4),
#' [5, 9), [5, 8)`, the containers are: `[1, 5), [2, 6), [5, 9)`. The intervals
#' `[3, 4)` and `[5, 8)` aren't containers because they are completely contained
#' within at least one other interval. Note that containers can partially
#' overlap, i.e. `[1, 5)` and `[2, 6)`, and multiple containers can contain the
#' same intervals, i.e. both `[1, 5)` and `[2, 6)` contain `[3, 4)`.
#'
#' Missing intervals are placed into their own container at the end, separate
#' from all other intervals.
#'
#' These functions require that `start < end`. Additionally, intervals are
#' treated as if they are right-open, i.e. `[start, end)`.
#'
#' @inheritSection interval-groups Assumptions
#'
#' @param start,end
#'   A pair of vectors representing the starts and ends of the intervals.
#'
#'   It is required that `start < end`.
#'
#'   `start` and `end` will be cast to their common type, and must have the same
#'   size.
#'
#' @return
#' An integer vector that represents the locations of the containers in `start`
#' and `end`.
#'
#' @examples
#' x <- data_frame(
#'   start = c(10, 0, NA, 3, 2, 2, NA, 11),
#'   end = c(12, 5, NA, 5, 6, 6, NA, 12)
#' )
#' x
#'
#' loc <- vec_interval_locate_containers(x$start, x$end)
#' loc
#'
#' vec_slice(x, loc)
#'
#' @noRd
vec_interval_locate_containers <- function(start, end) {
  .Call(ffi_interval_locate_containers, start, end)
}

# ------------------------------------------------------------------------------

# Experimental shims of interval functions used by other packages (mainly, ivs).
#
# This gives us the freedom to experiment with the signature of these functions
# while being backwards compatible with ivs in the meantime.
#
# We can remove these after:
# - The interval functions are exported
# - ivs updates to use them directly
# - A short deprecation period goes by that allows users time to update their
#   version of ivs

exp_vec_interval_groups <- function(start,
                                    end,
                                    ...,
                                    abutting = TRUE,
                                    missing = "group") {
  vec_interval_groups(
    start = start,
    end = end,
    ...,
    abutting = abutting,
    missing = missing
  )
}

exp_vec_interval_locate_groups <- function(start,
                                           end,
                                           ...,
                                           abutting = TRUE,
                                           missing = "group") {
  vec_interval_locate_groups(
    start = start,
    end = end,
    ...,
    abutting = abutting,
    missing = missing
  )
}

exp_vec_interval_complement <- function(start,
                                        end,
                                        ...,
                                        lower = NULL,
                                        upper = NULL) {
  vec_interval_complement(
    start = start,
    end = end,
    ...,
    lower = lower,
    upper = upper
  )
}

exp_vec_interval_locate_containers <- function(start, end) {
  vec_interval_locate_containers(
    start = start,
    end = end
  )
}
