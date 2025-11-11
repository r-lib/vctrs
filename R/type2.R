#' Find the common type for a pair of vectors
#'
#' @description
#'
#' `vec_ptype2()` defines the coercion hierarchy for a set of related
#' vector types. Along with [vec_cast()], this generic forms the
#' foundation of type coercions in vctrs.
#'
#' `vec_ptype2()` is relevant when you are implementing vctrs methods
#' for your class, but it should not usually be called directly. If
#' you need to find the common type of a set of inputs, call
#' [vec_ptype_common()] instead. This function supports multiple
#' inputs and [finalises][vec_ptype_finalise] the common type.
#'
#' @includeRmd man/faq/developer/links-coercion.Rmd
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#' @param x,y Vector types.
#' @param x_arg,y_arg Argument names for `x` and `y`. These are used
#'   in error messages to inform the user about the locations of
#'   incompatible types (see [stop_incompatible_type()]).
#'
#' @seealso [stop_incompatible_type()] when you determine from the
#'   attributes that an input can't be cast to the target type.
#'
#' @section Dependencies:
#' - [vec_ptype()] is applied to `x` and `y`
#'
#' @export
vec_ptype2 <- function(
  x,
  y,
  ...,
  x_arg = caller_arg(x),
  y_arg = caller_arg(y),
  call = caller_env()
) {
  if (!missing(...)) {
    check_ptype2_dots_empty(...)
    return(vec_ptype2_opts(
      x,
      y,
      opts = match_fallback_opts(...),
      x_arg = x_arg,
      y_arg = y_arg,
      call = call
    ))
  }
  return(.Call(ffi_ptype2, x, y, environment()))
  UseMethod("vec_ptype2")
}
vec_ptype2_dispatch_s3 <- function(
  x,
  y,
  ...,
  x_arg = "",
  y_arg = "",
  call = caller_env()
) {
  UseMethod("vec_ptype2")
}

vec_ptype2_dispatch_native <- function(
  x,
  y,
  ...,
  x_arg = "",
  y_arg = "",
  call = caller_env()
) {
  fallback_opts <- match_fallback_opts(...)
  .Call(
    ffi_ptype2_dispatch_native,
    x,
    y,
    fallback_opts,
    frame = environment()
  )
}

#' Default cast and ptype2 methods
#'
#' @description
#'
#' These functions are automatically called when no [vec_ptype2()] or
#' [vec_cast()] method is implemented for a pair of types.
#'
#' * They apply special handling if one of the inputs is of type
#'   `AsIs` or `sfc`.
#'
#' * They attempt a number of fallbacks in cases where it would be too
#'   inconvenient to be strict:
#'
#'   - If the class and attributes are the same they are considered
#'     compatible. `vec_default_cast()` returns `x` in this case.
#'
#'   - In case of incompatible data frame classes, they fall back to
#'     `data.frame`. If an incompatible subclass of tibble is
#'     involved, they fall back to `tbl_df`.
#'
#' * Otherwise, an error is thrown with [stop_incompatible_type()] or
#'   [stop_incompatible_cast()].
#'
#' @keywords internal
#' @export
vec_default_ptype2 <- function(
  x,
  y,
  ...,
  x_arg = "",
  y_arg = "",
  call = caller_env()
) {
  if (is_asis(x)) {
    return(vec_ptype2_asis_left(
      x,
      y,
      x_arg = x_arg,
      y_arg = y_arg,
      call = call
    ))
  }
  if (is_asis(y)) {
    return(vec_ptype2_asis_right(
      x,
      y,
      x_arg = x_arg,
      y_arg = y_arg,
      call = call
    ))
  }

  opts <- match_fallback_opts(...)

  if (opts$s3_fallback && can_fall_back_2(x, y)) {
    common <- common_class_suffix(x, y)
    if (length(common)) {
      return(new_common_class_fallback(x, common))
    }
  }

  if (is.data.frame(x) && is.data.frame(y)) {
    out <- vec_ptype2_df_fallback(
      x,
      y,
      opts,
      x_arg = x_arg,
      y_arg = y_arg,
      call = call
    )

    if (identical(non_df_attrib(x), non_df_attrib(y))) {
      attributes(out) <- c(df_attrib(out), non_df_attrib(x))
    }

    return(out)
  }

  if (is_same_type(x, y)) {
    return(vec_ptype(x, x_arg = x_arg))
  }

  # The from-dispatch parameter is set only when called from our S3
  # dispatch mechanism, when no method is found to dispatch to. It
  # indicates whether the error message should provide advice about
  # diverging attributes.
  withRestarts(
    stop_incompatible_type(
      x,
      y,
      x_arg = x_arg,
      y_arg = y_arg,
      `vctrs:::from_dispatch` = match_from_dispatch(...),
      call = call
    ),
    vctrs_restart_ptype2 = function(ptype) {
      ptype
    }
  )
}

# This wrapper for `stop_incompatible_type()` matches error context
# arguments. It is useful to pass ptype2 arguments through dots
# without risking unknown arguments getting stored as condition fields.
vec_incompatible_ptype2 <- function(
  x,
  y,
  ...,
  x_arg = "",
  y_arg = "",
  call = caller_env()
) {
  stop_incompatible_type(
    x,
    y,
    x_arg = x_arg,
    y_arg = y_arg,
    call = call
  )
}

# We can't check for a proxy or ptype2 method to determine whether a
# class is foreign, because we implement these generics for many base
# classes and we still need to allow base fallbacks with subclasses.
can_fall_back_2 <- function(x, y) {
  if (!identical(typeof(x), typeof(y))) {
    return(FALSE)
  }

  if (!can_fall_back(x) || !can_fall_back(y)) {
    return(FALSE)
  }

  TRUE
}

can_fall_back <- function(x) {
  UseMethod("can_fall_back")
}

#' @export
can_fall_back.vctrs_vctr <- function(x) {
  # Work around bad interaction when `c()` method calls back into `vec_c()`
  FALSE
}
#' @export
can_fall_back.ts <- function(x) {
  # Work around bug with hard-coded `tsp` attribute in Rf_setAttrib()
  FALSE
}
#' @export
can_fall_back.data.frame <- function(x) {
  # The `c()` fallback is only for 1D vectors
  FALSE
}

#' @export
`can_fall_back.vctrs:::common_class_fallback` <- function(x) {
  TRUE
}

#' @export
can_fall_back.default <- function(x) {
  # Don't fall back for classes that directly implement a proxy.
  #
  # NOTE: That's suboptimal. For instance this forces us to override
  # `can_fall_back()` for `vctrs_vctr` to avoid recursing into
  # `vec_c()` through `c()`. Maybe we want to avoid falling back for
  # any vector that inherits a `vec_proxy()` method implemented
  # _outside_ of vctrs, i.e. not for a base class?
  is_null(s3_get_method(class(x)[[1]], "vec_proxy", ns = "vctrs"))
}

new_common_class_fallback <- function(x, fallback_class) {
  structure(
    vec_ptype(x),
    class = "vctrs:::common_class_fallback",
    fallback_class = fallback_class
  )
}
#' @export
`vec_proxy.vctrs:::common_class_fallback` <- function(x, ...) {
  x
}

is_common_class_fallback <- function(x) {
  inherits(x, "vctrs:::common_class_fallback")
}
common_class_suffix <- function(x, y) {
  vec_common_suffix(fallback_class(x), fallback_class(y))
}
fallback_class <- function(x) {
  if (is_common_class_fallback(x)) {
    attr(x, "fallback_class")
  } else {
    class(x)
  }
}

check_ptype2_dots_empty <- function(
  ...,
  `vctrs:::from_dispatch`,
  `vctrs:::s3_fallback`
) {
  check_dots_empty0(...)
}
match_fallback_opts <- function(..., `vctrs:::s3_fallback` = NULL) {
  fallback_opts(
    s3_fallback = `vctrs:::s3_fallback`
  )
}
match_from_dispatch <- function(..., `vctrs:::from_dispatch` = FALSE) {
  `vctrs:::from_dispatch`
}

fallback_opts <- function(s3_fallback = NULL) {
  # Order is important for the C side
  list(
    s3_fallback = s3_fallback %||% s3_fallback_default()
  )
}

enabled_fallback_opts <- function() {
  fallback_opts(
    s3_fallback = S3_FALLBACK_true
  )
}

vec_ptype2_opts <- function(
  x,
  y,
  ...,
  opts,
  x_arg = "",
  y_arg = "",
  call = caller_env()
) {
  .Call(ffi_ptype2_opts, x, y, opts, environment())
}
vec_ptype2_params <- function(
  x,
  y,
  ...,
  s3_fallback = NULL,
  x_arg = "",
  y_arg = "",
  call = caller_env()
) {
  opts <- fallback_opts(
    s3_fallback = s3_fallback
  )
  vec_ptype2_opts(
    x,
    y,
    opts = opts,
    x_arg = x_arg,
    y_arg = y_arg,
    call = call
  )
}

vec_ptype2_no_fallback <- function(
  x,
  y,
  ...,
  x_arg = "",
  y_arg = "",
  call = caller_env()
) {
  opts <- fallback_opts(
    s3_fallback = S3_FALLBACK_false
  )
  vec_ptype2_opts(
    x,
    y,
    ...,
    ,
    opts = opts,
    x_arg = x_arg,
    y_arg = y_arg,
    call = call
  )
}

s3_fallback_default <- function() 0L
S3_FALLBACK_false <- 0L
S3_FALLBACK_true <- 1L


vec_typeof2 <- function(x, y) {
  .Call(ffi_typeof2, x, y)
}

vec_typeof2_s3 <- function(x, y) {
  .Call(ffi_typeof2_s3, x, y)
}

# https://github.com/r-lib/vctrs/issues/571
vec_is_coercible <- function(
  x,
  y,
  ...,
  opts = fallback_opts(),
  x_arg = "",
  y_arg = "",
  call = caller_env()
) {
  check_dots_empty0(...)

  .Call(
    ffi_is_coercible,
    x,
    y,
    opts,
    environment()
  )
}

vec_is_subtype <- function(x, super, ..., x_arg = "", super_arg = "") {
  tryCatch(
    vctrs_error_incompatible_type = function(...) FALSE,
    {
      common <- vctrs::vec_ptype2(
        x,
        super,
        ...,
        x_arg = x_arg,
        y_arg = super_arg
      )
      vec_is(common, super)
    }
  )
}

vec_implements_ptype2 <- function(x) {
  .Call(vctrs_implements_ptype2, x)
}
