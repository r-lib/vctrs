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
#'   Each element must be length 1 or the same length as its corresponding index
#'   in `indices`.
#'
#' @param indices A list of indices.
#'
#'   Each index must be a positive integer vector specifying the locations to
#'   place elements of `x`.
#'
#'   The size of `indices` must match the size of `x`.
#'
#' @param ptype If `NULL`, the default, the output type is determined by
#'   computing the common type across all elements of `x`. Alternatively, you
#'   can supply `ptype` to give the output a known type.
#'
#' @param x_arg,indices_arg An argument name as a string. This argument will be
#'   mentioned in error messages as the input that is at the origin of a
#'   problem.
#'
#' @returns
#' A vector of type `vec_ptype_common(!!!x)`, or `ptype`, if specified.
#'
#' The size is computed as `vec_size_common(!!!indices)`.
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
#' list_combine(x, indices)
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
#' list_combine(x, indices)
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
#' list_combine(x, indices)
#'
#' @noRd
list_combine <- function(
  x,
  indices,
  ...,
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
  error_call = current_env()
) {
  check_dots_empty0(...)
  .Call(
    ffi_list_combine,
    x,
    indices,
    ptype,
    name_spec,
    name_repair,
    environment()
  )
}
list_combine <- fn_inline_formals(list_combine, "name_repair")

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
