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
#' @param incomplete
#'   Merging of missing and [incomplete][vec_detect_complete] intervals. An
#'   interval is considered incomplete if either `start` or `end` contain any
#'   missing values.
#'
#'   - `"merge"`: Merge all incomplete intervals together. The bound location
#'   returned for an incomplete interval is `NA`.
#'
#'   - `"drop"`: Drop incomplete intervals from the result.
#'
#'   - `"error"`: Error if any incomplete intervals are detected.
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
#'   start = c(1, 2, NA, 5, 6, 9, 12),
#'   end = c(5, 3, 2, 6, NA, 12, 14)
#' )
#' bounds
#'
#' # Locate the bounds that will generate the merged intervals
#' loc <- vec_locate_interval_merge_bounds(bounds$start, bounds$end)
#' loc
#'
#' # Notice that the incomplete intervals are standardized in the merged result
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
#' # You can also locate the merge groups, which allow you to map each original
#' # interval to its corresponding merged interval
#' vec_locate_interval_merge_groups(bounds$start, bounds$end)
#'
#' @noRd
NULL

# ------------------------------------------------------------------------------

vec_locate_interval_merge_bounds <- function(start,
                                             end,
                                             ...,
                                             abutting = TRUE,
                                             incomplete = "merge") {
  check_dots_empty0(...)
  .Call(ffi_locate_interval_merge_bounds, start, end, abutting, incomplete)
}

vec_locate_interval_merge_groups <- function(start,
                                             end,
                                             ...,
                                             abutting = TRUE,
                                             incomplete = "merge") {
  check_dots_empty0(...)
  .Call(ffi_locate_interval_merge_groups, start, end, abutting, incomplete)
}

