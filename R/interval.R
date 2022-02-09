#' Merge intervals
#'
#' @description
#' These functions are used to merge overlaps present within a set of vector
#' intervals. They take a parallel set of `start` and `end` vectors of any type
#' that define the bounds of the intervals.
#'
#' - `vec_locate_interval_merge_bounds()` returns locations to slice `start`
#' and `end` with to generate the set of intervals that remain after all
#' overlaps are merged.
#'
#' - `vec_locate_interval_merge_groups()` returns a two column data frame
#' with a `key` column containing the result of
#' `vec_locate_interval_merge_bounds()` and a `loc` list-column containing
#' integer vectors that map each original interval to one of the resulting
#' merge bounds.
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
#' [complete][vec_detect_complete] or [missing][vec_equal_na]. Partially
#' complete values such as `start = data_frame(x = 1, y = NA)` are not allowed.
#'
#' If any of these assumptions are invalid, then the result is undefined.
#'
#' Developer note: These assumptions stem from the idea that if these functions
#' were in iv itself, then we could safely make these assumptions in the C code,
#' because the `iv()` helper would assert them for us ahead of time. Trying to
#' re-assert these checks in the C code here is wasteful and makes the code
#' more complex.
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
#'   merged together. If `TRUE`, `[a, b)` and `[b, c)` will be merged.
#'
#' @param missing
#'   Handling of missing intervals.
#'
#'   - `"merge"`: Merge all missing intervals together.
#'
#'   - `"drop"`: Drop all missing intervals from the result.
#'
#' @return
#' - `vec_locate_interval_merge_bounds()` returns a data frame with two columns,
#' `start` and `end`, both of which are integer vectors.
#'
#' - `vec_locate_interval_merge_groups()` returns a data frame with two columns,
#' `key` and `loc`. `key` contains the result of
#' `vec_locate_interval_merge_bounds()` and `loc` is a list of integer vectors.
#'
#' @name interval-merge
#'
#' @examples
#' bounds <- data_frame(
#'   start = c(1, 2, NA, 5, NA, 9, 12),
#'   end = c(5, 3, NA, 6, NA, 12, 14)
#' )
#' bounds
#'
#' # Locate the bounds that will generate the merged intervals
#' loc <- vec_locate_interval_merge_bounds(bounds$start, bounds$end)
#' loc
#'
#' data_frame(
#'   start = vec_slice(bounds$start, loc$start),
#'   end = vec_slice(bounds$end, loc$end)
#' )
#'
#' # You can choose not to merge abutting intervals if you want to retain
#' # those boundaries
#' loc <- vec_locate_interval_merge_bounds(
#'   bounds$start,
#'   bounds$end,
#'   abutting = FALSE
#' )
#'
#' data_frame(
#'   start = vec_slice(bounds$start, loc$start),
#'   end = vec_slice(bounds$end, loc$end)
#' )
#'
#' # You can also choose to drop all missing intervals if you don't consider
#' # them part of the merged result
#' loc <- vec_locate_interval_merge_bounds(
#'   bounds$start,
#'   bounds$end,
#'   missing = "drop"
#' )
#'
#' data_frame(
#'   start = vec_slice(bounds$start, loc$start),
#'   end = vec_slice(bounds$end, loc$end)
#' )
#'
#' # You can also locate the merge groups, which allow you to map each original
#' # interval to its corresponding merged interval
#' vec_locate_interval_merge_groups(bounds$start, bounds$end)
#'
#' @noRd
vec_locate_interval_merge_bounds <- function(start,
                                             end,
                                             ...,
                                             abutting = TRUE,
                                             missing = "merge") {
  check_dots_empty0(...)
  .Call(ffi_locate_interval_merge_bounds, start, end, abutting, missing)
}

#' @noRd
#' @rdname interval-merge
vec_locate_interval_merge_groups <- function(start,
                                             end,
                                             ...,
                                             abutting = TRUE,
                                             missing = "merge") {
  check_dots_empty0(...)
  .Call(ffi_locate_interval_merge_groups, start, end, abutting, missing)
}

# ------------------------------------------------------------------------------

# Experimental shims of interval functions used by other packages (mainly, iv).
#
# This gives us the freedom to experiment with the signature of these functions
# while being backwards compatible with iv in the meantime.
#
# We can remove these after:
# - The interval functions are exported
# - iv updates to use them directly
# - A short deprecation period goes by that allows users time to update their
#   version of iv

exp_vec_locate_interval_merge_bounds <- function(start,
                                                 end,
                                                 ...,
                                                 abutting = TRUE,
                                                 missing = "merge") {
  vec_locate_interval_merge_bounds(
    start = start,
    end = end,
    ...,
    abutting = abutting,
    missing = missing
  )
}

exp_vec_locate_interval_merge_groups <- function(start,
                                                 end,
                                                 ...,
                                                 abutting = TRUE,
                                                 missing = "merge") {
  vec_locate_interval_merge_groups(
    start = start,
    end = end,
    ...,
    abutting = abutting,
    missing = missing
  )
}
