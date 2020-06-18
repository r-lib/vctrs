#' Comparison proxy
#'
#' Returns a proxy object (i.e. an atomic vector or data frame of atomic
#' vectors). For [vctr]s, this determines the behaviour of [order()] and
#' [sort()] (via [xtfrm()]); `<`, `>`, `>=` and `<=` (via [vec_compare()]);
#' and [min()], [max()], [median()], and [quantile()].
#'
#' The default method assumes that all classes built on top of atomic
#' vectors or records are orderable. If your class is not, you will need
#' to provide a `vec_proxy_compare()` method that throws an error.
#'
#' @param x A vector x.
#' @param relax If `TRUE`, and `x` is otherwise non-comparable, will return
#'   `vec_seq_along(x)`. This allows a data frame to be orderable, even if
#'   one of its components is not. This is experimental and may change in the
#'   future.
#' @inheritParams ellipsis::dots_empty
#' @return A 1d atomic vector or a data frame.
#'
#' @section Dependencies:
#' - [vec_proxy()] called by default
#'
#' @keywords internal
#' @export
vec_proxy_compare <- function(x, ..., relax = FALSE) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  UseMethod("vec_proxy_compare")
}

#' @export
vec_proxy_compare.default <- function(x, ..., relax = FALSE) {
  if (vec_dim_n(x) > 1) {
    # The conversion to data frame is only a stopgap, in the long
    # term, we'll hash arrays natively. Note that hashing functions
    # similarly convert to data frames.
    as.data.frame(x)
  } else {
    vec_proxy_compare_default(x, relax)
  }
}

vec_proxy_compare_default <- function(x, relax = FALSE) {
  if (is_bare_list(x)) {
    if (relax) {
      vec_seq_along(x)
    } else {
      stop_unsupported(x, "vec_proxy_compare")
    }
  } else {
    vec_proxy_equal(x)
  }
}

#' Compare two vectors
#'
#' @section S3 dispatch:
#' `vec_compare()` is not generic for performance; instead it uses
#' [vec_proxy_compare()] to
#'
#' @param x,y Vectors with compatible types and lengths.
#' @param na_equal Should `NA` values be considered equal?
#' @param .ptype Override to optionally specify common type
#' @return An integer vector with values -1 for `x < y`, 0 if `x == y`,
#'    and 1 if `x > y`. If `na_equal` is `FALSE`, the result will be `NA`
#'    if either `x` or `y` is `NA`.
#'
#' @section Dependencies:
#' - [vec_cast_common()] with fallback
#' - [vec_recycle_common()]
#' - [vec_proxy_compare()]
#'
#' @export
#' @examples
#' vec_compare(c(TRUE, FALSE, NA), FALSE)
#' vec_compare(c(TRUE, FALSE, NA), FALSE, na_equal = TRUE)
#'
#' vec_compare(1:10, 5)
#' vec_compare(runif(10), 0.5)
#' vec_compare(letters[1:10], "d")
#'
#' df <- data.frame(x = c(1, 1, 1, 2), y = c(0, 1, 2, 1))
#' vec_compare(df, data.frame(x = 1, y = 1))
vec_compare <- function(x, y, na_equal = FALSE, .ptype = NULL) {
  vec_assert(x)
  vec_assert(y)
  vec_assert(na_equal, ptype = logical(), size = 1L)

  args <- vec_recycle_common(x, y)
  args <- vec_cast_common_params(
    !!!args,
    .to = .ptype,
    .df_fallback = DF_FALLBACK_quiet
  )

  .Call(vctrs_compare, vec_proxy_compare(args[[1]]), vec_proxy_compare(args[[2]]), na_equal)
}

# Helpers -----------------------------------------------------------------

# Used for testing
cmp <- function(x, y) (x > y) - (x < y)
