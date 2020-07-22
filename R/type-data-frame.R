#' Data frame class
#'
#' A `data.frame` [data.frame()] is a list with "row.names" attribute. Each
#' element of the list must be named, and of the same length. These functions
#' help the base data.frame classes fit in to the vctrs type system by
#' providing constructors, coercion functions, and casting functions.
#'
#' @param x A named list of equal-length vectors. The lengths are not
#'   checked; it is responsibility of the caller to make sure they are
#'   equal.
#' @param n Number of rows. If `NULL`, will be computed from the length of
#'   the first element of `x`.
#' @param ...,class Additional arguments for creating subclasses.
#'   The `"names"` and `"row.names"` attributes override input in `x` and `n`,
#'   respectively:
#'
#'   - `"names"` is used if provided, overriding existing names in `x`
#'   - `"row.names"` is used if provided, if `n` is provided it must be
#'     consistent.
#'
#' @export
#' @keywords internal
#' @examples
#' new_data_frame(list(x = 1:10, y = 10:1))
new_data_frame <- function(x = list(), n = NULL, ..., class = NULL) {
  .External(vctrs_new_data_frame, x, n, class, ...)
}
new_data_frame <- fn_inline_formals(new_data_frame, "x")


data_frame <- function(...,
                       .size = NULL,
                       .name_repair = c("minimal", "unique", "universal", "check_unique")) {
  .Call(vctrs_data_frame, list2(...), .size, .name_repair)
}
data_frame <- fn_inline_formals(data_frame, ".name_repair")

#' @export
vec_ptype_full.data.frame <- function(x, ...) {
  if (length(x) == 0) {
    return(paste0(class(x)[[1]], "<>"))
  } else if (length(x) == 1) {
    return(paste0(class(x)[[1]], "<", names(x), ":", vec_ptype_full(x[[1]]), ">"))
  }

  # Needs to handle recursion with indenting
  types <- map_chr(x, vec_ptype_full)
  needs_indent <- grepl("\n", types)
  types[needs_indent] <- map(types[needs_indent], function(x) indent(paste0("\n", x), 4))

  names <- paste0("  ", format(names(x)))

  paste0(
    class(x)[[1]], "<\n",
    paste0(names, ": ", types, collapse = "\n"),
    "\n>"
  )
}

#' @export
vec_ptype_abbr.data.frame <- function(x, ...) {
  if (inherits_only(x, "data.frame")) {
    abbr <- "df"
  } else {
    abbr <- class(x)[[1]]
  }
  paste0(abbr, vec_ptype_shape(x))
}

#' @export
vec_proxy_equal.data.frame <- function(x, ...) {
  df_proxy(x, VCTRS_PROXY_KIND_equal)
}

#' @export
vec_proxy_compare.data.frame <- function(x, ...) {
  df_proxy(x, VCTRS_PROXY_KIND_compare)
}

#' @export
vec_proxy_order.data.frame <- function(x, ...) {
  df_proxy(x, VCTRS_PROXY_KIND_order)
}

# Keep in sync with `enum vctrs_proxy_kind` in `vctrs.h`
VCTRS_PROXY_KIND_default <- 0L
VCTRS_PROXY_KIND_equal <- 1L
VCTRS_PROXY_KIND_compare <- 2L
VCTRS_PROXY_KIND_order <- 3L

df_proxy <- function(x, kind) {
  .Call(vctrs_df_proxy, x, kind)
}

df_is_coercible <- function(x, y, opts) {
  vec_is_coercible(
    new_data_frame(x),
    new_data_frame(y),
    opts = opts
  )
}


# Coercion ----------------------------------------------------------------

#' Coercion between two data frames
#'
#' `df_ptype2()` and `df_cast()` are the two functions you need to
#' call from `vec_ptype2()` and `vec_cast()` methods for data frame
#' subclasses. See [?howto-faq-coercion-data-frame][howto-faq-coercion-data-frame].
#' Their main job is to determine the common type of two data frames,
#' adding and coercing columns as needed, or throwing an incompatible
#' type error when the columns are not compatible.
#'
#' @param x,y,to Subclasses of data frame.
#' @param ... If you call `df_ptype2()` or `df_cast()` from a
#'   `vec_ptype2()` or `vec_cast()` method, you must forward the dots
#'   passed to your method on to `df_ptype2()` or `df_cast()`.
#' @inheritParams vec_ptype2
#' @inheritParams vec_cast
#'
#' @return
#' * When `x` and `y` are not compatible, an error of class
#'   `vctrs_error_incompatible_type` is thrown.
#' * When `x` and `y` are compatible, `df_ptype2()` returns the common
#'   type as a bare data frame. `tib_ptype2()` returns the common type
#'   as a bare tibble.
#'
#' @export
df_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  .Call(vctrs_df_ptype2_opts, x, y, opts = match_fallback_opts(...), x_arg, y_arg)
}
#' @rdname df_ptype2
#' @export
df_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  .Call(vctrs_df_cast_opts, x, to, opts = match_fallback_opts(...), x_arg, to_arg)
}

df_ptype2_opts <- function(x, y, ..., opts, x_arg = "", y_arg = "") {
  .Call(vctrs_df_ptype2_opts, x, y, opts = opts, x_arg, y_arg)
}

df_cast_opts <- function(x,
                         to,
                         ...,
                         opts = fallback_opts(),
                         x_arg = "",
                         to_arg = "") {
  .Call(vctrs_df_cast_opts, x, to, opts, x_arg, to_arg)
}
df_cast_params <- function(x,
                           to,
                           ...,
                           x_arg = "",
                           to_arg = "",
                           df_fallback = NULL,
                           s3_fallback = NULL) {
  opts <- fallback_opts(
    df_fallback = df_fallback,
    s3_fallback = s3_fallback
  )
  df_cast_opts(x, to, opts = opts, x_arg = x_arg, to_arg = to_arg)
}

#' @rdname new_data_frame
#' @export vec_ptype2.data.frame
#' @method vec_ptype2 data.frame
#' @export
vec_ptype2.data.frame <- function(x, y, ...) {
  UseMethod("vec_ptype2.data.frame")
}
#' @method vec_ptype2.data.frame data.frame
#' @export
vec_ptype2.data.frame.data.frame <- function(x, y, ...) {
  df_ptype2(x, y, ...)
}

vec_ptype2_df_fallback_normalise <- function(x, y, opts) {
  x_orig <- x
  y_orig <- y

  ptype <- df_ptype2_opts(x, y, opts = opts)

  x <- x[0, , drop = FALSE]
  y <- y[0, , drop = FALSE]

  x[seq_along(ptype)] <- ptype
  y[seq_along(ptype)] <- ptype

  # Names might have been repaired by `[<-`
  names(x) <- names(ptype)
  names(y) <- names(ptype)

  # Restore attributes if no `[` method is implemented
  if (df_has_base_subset(x)) {
    x <- vec_restore(x, x_orig)
  }
  if (df_has_base_subset(y)) {
    y <- vec_restore(y, y_orig)
  }

  list(x = x, y = y)
}
vec_cast_df_fallback_normalise <- function(x, to, opts) {
  orig <- x
  cast <- df_cast_opts(x, to, opts = opts)

  # Seq-assign should be more widely implemented than empty-assign?
  x[seq_along(to)] <- cast

  # Names might have been repaired by `[<-`
  names(x) <- names(cast)

  # Restore attributes if no `[` method is implemented
  if (df_has_base_subset(x)) {
    x <- vec_restore(x, orig)
  }

  x
}

df_needs_normalisation <- function(x, y, opts) {
  is.data.frame(x) && is.data.frame(y) && df_is_coercible(x, y, opts)
}

# Fallback for data frame subclasses (#981)
vec_ptype2_df_fallback <- function(x, y, opts, x_arg = "", y_arg = "") {
  seen_tibble <- inherits(x, "tbl_df") || inherits(y, "tbl_df")

  ptype <- vec_ptype2_params(
    new_data_frame(x),
    new_data_frame(y),
    df_fallback = opts$df_fallback,
    s3_fallback = opts$s3_fallback
  )

  classes <- NULL
  if (is_df_fallback(x)) {
    classes <- c(classes, known_classes(x))
    x <- df_fallback_as_df(x)
  }
  if (is_df_fallback(y)) {
    classes <- c(classes, known_classes(y))
    y <- df_fallback_as_df(y)
  }
  x_class <- class(x)[[1]]
  y_class <- class(y)[[1]]

  if (needs_fallback_warning(opts$df_fallback) &&
      !all(c(x_class, y_class) %in% c(classes, "tbl_df"))) {
    fallback_class <- if (seen_tibble) "<tibble>" else "<data.frame>"
    msg <- cnd_type_message(
      x, y,
      x_arg, y_arg,
      NULL,
      "combine",
      NULL,
      fallback = fallback_class
    )

    if (identical(x_class, y_class)) {
      msg <- c(
        msg,
        incompatible_attrib_bullets()
      )
    }

    warn(msg)
  }

  # Return a fallback class so we don't warn multiple times. This
  # fallback class is stripped in `vec_ptype_finalise()`.
  new_fallback_df(
    ptype,
    known_classes = unique(c(classes, x_class, y_class)),
    seen_tibble = seen_tibble
  )
}

is_df_subclass <- function(x) {
  inherits(x, "data.frame") && !identical(class(x), "data.frame")
}
is_df_fallback <- function(x) {
  inherits(x, "vctrs:::df_fallback")
}
new_fallback_df <- function(x, known_classes, seen_tibble = FALSE, n = nrow(x)) {
  class <- "vctrs:::df_fallback"
  if (seen_tibble) {
    class <- c(class, "tbl_df", "tbl")
  }

  new_data_frame(
    x,
    n = n,
    known_classes = known_classes,
    seen_tibble = seen_tibble,
    class = class
  )
}
df_fallback_as_df <- function(x) {
  if (inherits(x, "tbl_df")) {
    new_data_frame(x, class = c("tbl_df", "tbl", "data.frame"))
  } else {
    new_data_frame(x)
  }
}
known_classes <- function(x) {
  if (is_df_fallback(x)) {
    attr(x, "known_classes")
  }
}


# Cast --------------------------------------------------------------------

#' @rdname new_data_frame
#' @export vec_cast.data.frame
#' @method vec_cast data.frame
#' @export
vec_cast.data.frame <- function(x, to, ...) {
  UseMethod("vec_cast.data.frame")
}
#' @export
#' @method vec_cast.data.frame data.frame
vec_cast.data.frame.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_restore.data.frame <- function(x, to, ..., n = NULL) {
  .Call(vctrs_bare_df_restore, x, to, n)
}

# Helpers -----------------------------------------------------------------

df_size <- function(x) {
  .Call(vctrs_df_size, x)
}

df_lossy_cast <- function(out, x, to, ..., x_arg = "", to_arg = "") {
  extra <- setdiff(names(x), names(to))

  maybe_lossy_cast(
    result = out,
    x = x,
    to = to,
    lossy = length(extra) > 0,
    locations = int(),
    x_arg = x_arg,
    to_arg = to_arg,
    details = inline_list("Dropped variables: ", extra, quote = "`"),
    class = "vctrs_error_cast_lossy_dropped"
  )
}

is_informative_error.vctrs_error_cast_lossy_dropped <- function(x, ...) {
  FALSE
}
