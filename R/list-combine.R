#' Combine a list of vectors
#'
#' @description
#' `list_combine()` is a more powerful version of [vec_c()]. While `vec_c()` is
#' used for sequential combination, `list_combine()` takes a list of `indices`
#' that specify where to place each element in the output.
#'
#' If you have a list of vectors and just need to combine them sequentially,
#' you'll still want to use `vec_c(!!!x)`.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#' @inheritParams name_spec
#' @inheritParams vec_as_names
#'
#' @param x A list of vectors.
#'
#'   If `slice_x = FALSE`, each element must be size 1 or the same size as its
#'   corresponding index in `indices` after that index has been converted to
#'   a positive integer location vector with [vec_as_location()].
#'
#'   If `slice_x = TRUE`, each element must be size 1 or size `size`.
#'
#' @param indices A list of indices.
#'
#'   Indices can be provided in one of two forms:
#'
#'   - Positive integer vectors of locations less than or equal to `size`. Each
#'     vector can be any size.
#'
#'   - Logical vectors of size `size` where `TRUE` denotes the location in the
#'     output to assign to, and the location from the `x` element to pull from.
#'     Both `NA` and `FALSE` are considered unmatched.
#'
#'   The size of `indices` must match the size of `x`.
#'
#' @param size The output size.
#'
#' @param default If `NULL`, a missing value is used for locations unmatched by
#'   `indices`, otherwise the provided `default` is used.
#'
#'   If provided, `default` must be size 1 or size `size`.
#'
#'   Can only be set when `unmatched = "default"`.
#'
#' @param unmatched Handling of locations in the output unmatched by `indices`.
#'   One of:
#'
#'   - `"default"` to use `default` in unmatched locations.
#'
#'   - `"error"` to error when there are unmatched locations.
#'
#' @param slice_x A boolean.
#'
#'   If `TRUE`, each element of `x` is sliced by its corresponding index from
#'   `indices` before being assigned into the output, which is effectively the
#'   same as `map2(list(x, indices), function(x, index) vec_slice(x, index))`,
#'   but is optimized to avoid materializing the slices.
#'
#'   See the `slice_value` argument of [vec_assign()] for more examples.
#'
#' @param ptype If `NULL`, the output type is determined by computing the common
#'   type across all elements of `x` and `default`. Alternatively, you can
#'   supply `ptype` to give the output a known type.
#'
#' @param x_arg,indices_arg,default_arg An argument name as a string. This
#'   argument will be mentioned in error messages as the input that is at the
#'   origin of a problem.
#'
#' @returns
#' A vector of type `vec_ptype_common(!!!x)`, or `ptype`, if specified.
#'
#' The size of the output is determined by `size`.
#'
#' @examples
#' # Combine a list of vectors using
#' # a list of `indices`
#' x <- list(
#'   1:3,
#'   4:6,
#'   7:8
#' )
#' indices <- list(
#'   c(1, 3, 7),
#'   c(8, 6, 5),
#'   c(2, 4)
#' )
#' list_combine(x, indices = indices, size = 8)
#'
#' # Overlapping `indices` are allowed. The output size is
#' # computed as `sum(list_sizes(indices))` and the last
#' # index "wins"
#' x <- list(
#'   1:3,
#'   4:6
#' )
#' indices <- list(
#'   c(1, 2, 3),
#'   c(1, 2, 6)
#' )
#' list_combine(x, indices = indices, size = 6)
#'
#' # Works with data frames as well.
#' # Now how index 2 is not assigned to.
#' x <- list(
#'   data.frame(x = 1:2, y = c("a", "b")),
#'   data.frame(x = 3:4, y = c("c", "d"))
#' )
#' indices <- list(
#'   c(4, 1),
#'   c(3, NA)
#' )
#' list_combine(x, indices = indices, size = 4)
#'
#' # You can use `size` to combine into a larger object than you have values for
#' list_combine(list(1:2, 4:5), indices = list(1:2, 4:5), size = 8)
#'
#' # Additionally specifying `default` allows you to control the value used in
#' # unfilled locations
#' list_combine(
#'   list(1:2, 4:5),
#'   indices = list(1:2, 4:5),
#'   size = 8,
#'   default = 0L
#' )
#'
#' # Alternatively, if you'd like to assert that you've covered all output
#' # locations through `indices`, set `unmatched = "error"`.
#' # Here, we've set the size to 5 but missed location 3:
#' try(list_combine(
#'   list(1:2, 4:5),
#'   indices = list(1:2, 4:5),
#'   size = 5,
#'   unmatched = "error"
#' ))
#' @noRd
list_combine <- function(
  x,
  ...,
  indices,
  size,
  default = NULL,
  unmatched = "default",
  slice_x = FALSE,
  ptype = NULL,
  name_spec = NULL,
  name_repair = c(
    "minimal",
    "unique",
    "check_unique",
    "universal",
    "unique_quiet",
    "universal_quiet"
  ),
  x_arg = "x",
  indices_arg = "indices",
  default_arg = "default",
  error_call = current_env()
) {
  check_dots_empty0(...)
  .Call(
    ffi_list_combine,
    x,
    indices,
    size,
    default,
    unmatched,
    slice_x,
    ptype,
    name_spec,
    name_repair,
    environment()
  )
}
list_combine <- fn_inline_formals(list_combine, "name_repair")

# ------------------------------------------------------------------------------

stop_combine <- function(message = NULL, class = NULL, ..., call = caller_env()) {
  stop_vctrs(
    message = message,
    class = c(class, "vctrs_error_combine"),
    ...,
    call = call
  )
}

# ------------------------------------------------------------------------------

stop_combine_unmatched <- function(loc, call) {
  stop_combine(
    class = "vctrs_error_combine_unmatched",
    loc = loc,
    call = call
  )
}

#' @export
cnd_header.vctrs_error_combine_unmatched <- function(cnd, ...) {
  "Each location must be matched."
}

#' @export
cnd_body.vctrs_error_combine_unmatched <- function(cnd, ...) {
  # cli's pluralization length feature only kicks in on character vectors
  loc <- as.character(cnd$loc)
  bullet <- cli::format_inline("Location{?s} {loc} {?is/are} unmatched.")
  bullet <- c(x = bullet)
  format_error_bullets(bullet)
}

# ------------------------------------------------------------------------------

# Called from C
base_c_invoke <- function(xs) {
  local_options("vctrs:::base_c_in_progress" = TRUE)

  # Remove all `NULL` arguments which prevent dispatch if in first
  # position and might not be handled correctly by methods
  xs <- compact(xs)

  unspecified <- map_lgl(xs, fallback_is_unspecified)
  if (!any(unspecified)) {
    return(base_c(xs))
  }

  # First concatenate without the unspecified chunks. This way the
  # `c()` method doesn't need to handle unspecified inputs correctly,
  # and we're guaranteed to dispatch on the correct class even if the
  # first input is unspecified.
  out <- base_c(xs[!unspecified])

  # Create index vector with `NA` locations for unspecified chunks
  locs <- c_locs(xs)
  locs[unspecified] <- map(locs[unspecified], rep_along, na_int)
  locs[!unspecified] <- c_locs(xs[!unspecified])
  locs <- vec_c(!!!locs, .ptype = int())

  # Expand the concatenated vector with unspecified chunks
  out[locs]
}

base_c <- function(xs) {
  # Dispatch in the base namespace which inherits from the global env
  exec("c", !!!xs, .env = ns_env("base"))
}

# FIXME: Should be unnecessary in the future. We currently attach an
# attribute to unspecified columns initialised in `df_cast()`. We
# can't use an unspecified vector because we (unnecessarily but for
# convenience) go through `vec_assign()` before falling back in
# `vec_rbind()`.
fallback_is_unspecified <- function(x) {
  is_unspecified(x) || is_true(attr(x, "vctrs:::unspecified"))
}

c_locs <- function(xs) {
  locs <- reduce(lengths(xs), .init = list(0), function(output, input) {
    n <- last(last(output))
    c(output, list(seq(n + 1, n + input)))
  })

  locs[-1]
}
