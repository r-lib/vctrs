#' Cast a vector to a specified type
#'
#' @description
#'
#' `vec_cast()` provides directional conversions from one type of
#' vector to another. Along with [vec_ptype2()], this generic forms
#' the foundation of type coercions in vctrs.
#'
#' @includeRmd man/faq/developer/links-coercion.Rmd
#'
#' @param x Vectors to cast.
#' @param ... For `vec_cast_common()`, vectors to cast. For
#'   `vec_cast()`, `vec_cast_default()`, and `vec_restore()`, these
#'   dots are only for future extensions and should be empty.
#' @param to,.to Type to cast to. If `NULL`, `x` will be returned as is.
#' @param x_arg,to_arg Argument names for `x` and `to`. These are used
#'   in error messages to inform the user about the locations of
#'   incompatible types (see [stop_incompatible_type()]).
#' @return A vector the same length as `x` with the same type as `to`,
#'   or an error if the cast is not possible. An error is generated if
#'   information is lost when casting between compatible types (i.e. when
#'   there is no 1-to-1 mapping for a specific value).
#'
#' @section Dependencies of `vec_cast_common()`:
#'
#' ## vctrs dependencies
#'
#' - [vec_ptype2()]
#' - [vec_cast()]
#'
#'
#' ## base dependencies
#'
#' Some functions enable a `data.frame` fallback for
#' `vec_cast_common()`. In that case the inputs are deemed compatible
#' when they are all data frames with compatible columns.
#'
#' @seealso Call [stop_incompatible_cast()] when you determine from the
#' attributes that an input can't be cast to the target type.
#' @export
#' @examples
#' # x is a double, but no information is lost
#' vec_cast(1, integer())
#'
#' # When information is lost the cast fails
#' try(vec_cast(c(1, 1.5), integer()))
#' try(vec_cast(c(1, 2), logical()))
#'
#' # You can suppress this error and get the partial results
#' allow_lossy_cast(vec_cast(c(1, 1.5), integer()))
#' allow_lossy_cast(vec_cast(c(1, 2), logical()))
#'
#' # By default this suppress all lossy cast errors without
#' # distinction, but you can be specific about what cast is allowed
#' # by supplying prototypes
#' allow_lossy_cast(vec_cast(c(1, 1.5), integer()), to_ptype = integer())
#' try(allow_lossy_cast(vec_cast(c(1, 2), logical()), to_ptype = integer()))
#'
#' # No sensible coercion is possible so an error is generated
#' try(vec_cast(1.5, factor("a")))
#'
#' # Cast to common type
#' vec_cast_common(factor("a"), factor(c("a", "b")))
vec_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  return(.Call(vctrs_cast, x, to, x_arg, to_arg))
  UseMethod("vec_cast", to)
}
vec_cast_dispatch <- function(x, to, ..., x_arg = "", to_arg = "") {
  UseMethod("vec_cast", to)
}

vec_cast_no_fallback <- function(x, to) {
  vec_cast_common_params(x = x, .to = to, .df_fallback = DF_FALLBACK_NONE)$x
}

#' @export
#' @rdname vec_cast
vec_cast_common <- function(..., .to = NULL) {
  .External2(vctrs_cast_common, .to)
}
vec_cast_common_opts <- function(...,
                                 .to = NULL,
                                 .opts = ptype2_opts()) {
  .External2(vctrs_cast_common_opts, .to, .opts)
}
vec_cast_common_params <- function(...,
                                   .to = NULL,
                                   .df_fallback = NULL,
                                   .s3_fallback = NULL) {
  opts <- ptype2_opts(
    df_fallback = .df_fallback,
    s3_fallback = .s3_fallback
  )
  vec_cast_common_opts(..., .to = .to, .opts = opts)
}

#' @rdname vec_default_ptype2
#' @inheritParams vec_cast
#' @export
vec_default_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  if (is_asis(x)) {
    return(vec_cast_from_asis(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  if (is_asis(to)) {
    return(vec_cast_to_asis(x, to, x_arg = x_arg, to_arg = to_arg))
  }

  if (inherits(to, "vctrs_vctr") && !inherits(to, c("vctrs_rcrd", "vctrs_list_of"))) {
    return(vctr_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }

  # Compatibility for sfc lists (#989)
  if (inherits(x, "sfc") || inherits(to, "sfc")) {
    return(UseMethod("vec_cast", to))
  }

  opts <- match_ptype2_opts(...)

  # If both data frames, first find the `to` type of columns before
  # the same-type fallback
  if (df_needs_normalisation(x, to, opts)) {
    x <- vec_cast_df_fallback_normalise(x, to)
  }

  if (is_same_type(x, to)) {
    return(x)
  }

  if (has_df_fallback(opts$df_fallback) && is_df_subclass(x)) {
    out <- df_cast_opts(
      x,
      to,
      ...,
      opts = opts,
      x_arg = x_arg,
      to_arg = to_arg
    )

    if (inherits(to, "tbl_df")) {
      out <- df_as_tibble(out)
    }

    return(out)
  }

  stop_incompatible_cast(
    x,
    to,
    x_arg = x_arg,
    to_arg = to_arg,
    `vctrs:::from_dispatch` = match_from_dispatch(...)
  )
}

is_informative_error.vctrs_error_cast_lossy <- function(x, ...) {
  FALSE
}
