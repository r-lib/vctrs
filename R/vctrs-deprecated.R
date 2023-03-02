#' Is a vector empty
#'
#' @description
#'
#' `r lifecycle::badge("defunct")`
#'
#' This function is defunct, please use [vec_is_empty()].
#'
#' @param x An object.
#'
#' @keywords internal
#' @export
vec_empty <- function(x) {
  # Defunct: 2019-06
  lifecycle::deprecate_stop(
    when = "0.2.0",
    what = "vec_empty()",
    with = "vec_is_empty()"
  )
}

#' Deprecated type functions
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' These functions have been renamed:
#'
#' * `vec_type()` => [vec_ptype()]
#' * `vec_type2()` => [vec_ptype2()]
#' * `vec_type_common()` => [vec_ptype_common()]
#'
#' @param x,y,...,.ptype Arguments for deprecated functions.
#'
#' @keywords internal
#' @export
vec_type <- function(x) {
  # Deprecated: 2019-06
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "vec_type()",
    with = "vec_ptype()",
    always = TRUE
  )
  vec_ptype(x)
}
#' @rdname vec_type
#' @export
vec_type_common <- function(..., .ptype = NULL) {
  # Deprecated: 2019-06
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "vec_type_common()",
    with = "vec_ptype_common()",
    always = TRUE
  )
  vec_ptype_common(..., .ptype = .ptype)
}
#' @rdname vec_type
#' @export
vec_type2 <- function(x, y, ...) {
  # Deprecated: 2019-06
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "vec_type2()",
    with = "vec_ptype2()",
    always = TRUE
  )
  vec_ptype2(x, y, ...)
}

#' Convert to an index vector
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `vec_as_index()` has been renamed to [vec_as_location()] and is
#' deprecated as of vctrs 0.2.2.
#'
#' @inheritParams vec_as_location
#'
#' @keywords internal
#' @export
vec_as_index <- function(i, n, names = NULL) {
  # Soft-deprecated: 2020-01
  lifecycle::deprecate_soft(
    when = "0.2.2",
    what = "vec_as_index()",
    with = "vec_as_location()"
  )
  n <- vec_cast(n, integer())
  vec_check_size(n, size = 1L)
  i <- vec_as_subscript(i)

  # Picked up from the environment at the C level
  arg <- NULL

  .Call(
    ffi_as_location,
    i = i,
    n = n,
    names = names,
    loc_negative = "invert",
    loc_oob = "error",
    loc_zero = "remove",
    missing = "propagate",
    env = environment()
  )
}

#' Expand the length of a vector
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `vec_repeat()` has been replaced with [vec_rep()] and [vec_rep_each()] and is
#' deprecated as of vctrs 0.3.0.
#'
#' @param x A vector.
#' @param each Number of times to repeat each element of `x`.
#' @param times Number of times to repeat the whole vector of `x`.
#' @return A vector the same type as `x` with size `vec_size(x) * times * each`.
#' @keywords internal
#' @export
vec_repeat <- function(x, each = 1L, times = 1L) {
  # Soft-deprecated: 2020-03
  lifecycle::deprecate_soft(
    when = "0.3.0",
    what = "vec_repeat()",
    with = I("either `vec_rep()` or `vec_rep_each()`")
  )

  vec_check_size(each, size = 1L)
  vec_check_size(times, size = 1L)

  idx <- rep(vec_seq_along(x), times = times, each = each)
  vec_slice(x, idx)
}

#' Chopping
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `vec_unchop()` has been renamed to [list_unchop()] and is deprecated as of
#' vctrs 0.5.0.
#'
#' @inheritParams list_unchop
#' @inherit list_unchop return
#'
#' @keywords internal
#' @export
vec_unchop <- function(x,
                       indices = NULL,
                       ptype = NULL,
                       name_spec = NULL,
                       name_repair = c("minimal", "unique", "check_unique", "universal")) {
  # Soft-deprecated: 2022-09
  lifecycle::deprecate_soft("0.5.0", "vec_unchop()", "list_unchop()")

  list_unchop(
    x = x,
    indices = indices,
    ptype = ptype,
    name_spec = name_spec,
    name_repair = name_repair
  )
}

#' Missing values
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `vec_equal_na()` has been renamed to [vec_detect_missing()] and is deprecated
#' as of vctrs 0.5.0.
#'
#' @inheritParams vec_detect_missing
#'
#' @return
#' A logical vector the same size as `x`.
#'
#' @keywords internal
#' @export
vec_equal_na <- function(x) {
  # Soft-deprecated: 2022-09
  lifecycle::deprecate_soft("0.5.0", "vec_equal_na()", "vec_detect_missing()")
  vec_detect_missing(x)
}
