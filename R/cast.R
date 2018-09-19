#' Cast a vector to specified type
#'
#' `vec_cast()` provides general coercions from one type of vector to another,
#' and along with [vec_type2()] forms the foundation of the vctrs type system.
#' It should generally not be called by R users, but is important for R
#' developers. `vec_restore()` is designed specifically for casting a bare
#' vector to the original type; it's useful when relying `NextMethod()` for
#' the actual implementation.
#'
#' @section Casting rules:
#' Casting is more flexible than coercion, and allows for the possibility of
#' information loss. This diagram summarises possible coercions. `vec_cast()`
#' from any type connected to another type, provided that the arrows are
#' followed in only one direction. For example you can cast from logical to
#' character, and list to time, but you can not cast from logical to datetime.
#'
#' \figure{cast.png}
#'
#' Most casts are not symmetric: you can cast all integers to doubles, but you
#' can only cast a subset of doubles back to integers. If a cast is potentially
#' lossy, a warning message will be shown whenever an actual loss occurs
#' (which may only be for some elements of a vector).
#'
#' The rules for coercing from a list are fairly strict: each component of the
#' list must be of length 1, and must be coercible to type `to`. This ensures
#' that a round-trip to and form list is possible, without opening the door
#' to very flexible list flattening (which should be the job of a more
#' specialised function).
#'
#' @section S3 dispatch:
#' `vec_cast()` dispatches on both arguments because casting depends on both
#' the type of `x` and of `to`. This is implemented by having methods of
#' `vec_cast()`, e.g. `vec_cast.integer()` also be S3 generics, which call
#' e.g. `vec_cast.integer.double()`.
#'
#' Note that `vec_cast()` dispatch on its second argument, so that the name
#' of the final method uses the same convention as `as.xyz()` methods, i.e.
#' `vec_cast.integer.double()` casts double to integers, in the same way
#' that `as.integer.double()` would.
#'
#' See `vignette("s3-vector")` on how to extend to your own S3
#' vector classes.
#'
#' @section Restoring attributes:
#'
#' A restore is a specialised type of cast, primarily used in conjunction
#' with `NextMethod()` or a C-level function that works on the underlying
#' data structure. A `vec_restore()` method can assume that `x` has the
#' correct type (although the length may be different) but all attributes
#' have been lost and need to be restored. In other words,
#' `vec_restore(vec_data(x), x)` should yield `x`.
#'
#' To understand the difference between `vec_cast()` and `vec_restore()`
#' think about factors: it doesn't make sense to cast an integer to a factor,
#' but if `NextMethod()` or other low-level function has stripped attributes,
#' you still need to be able to restore them.
#'
#' The default method copies across all attributes so you only need to
#' provide your own method if your attributes require special care
#' (i.e. they are dependent on the data in somew way). When implementing
#' your own method, bear in mind that many R users add attributes to track
#' additional metadat that is important to them, so you should preserve any
#' attributes that don't require special handling for your class.
#'
#' @param x Vector to cast.
#' @param to Type to cast to.
#' @return A vector the same length as `x` with the same type as `to`,
#'   or an error if the cast is not possible. A warning is generated if
#'   information is lost when casting between compatible types (i.e. when
#'   there is no 1-to-1 mapping for a specific value).
#' @export
#' @keywords internal
#' @examples
#' # x is a double, but no information is lost
#' vec_cast(1, integer())
#'
#' # Information is lost so a warning is generated
#' vec_cast(1.5, integer())
#'
#' # No sensible coercion is possible so an error is generated
#' \dontrun{
#' vec_cast(1.5, factor("a"))
#' }
#'
vec_cast <- function(x, to) {
  UseMethod("vec_cast", to)
}

#' @export
vec_cast.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @rdname vec_cast
vec_restore <- function(x, to) {
  UseMethod("vec_restore", to)
}

#' @export
vec_restore.default <- function(x, to) {
  attributes(x) <- attributes(to)
  x
}

#' @export
vec_restore.data.frame <- function(x, to) {
  # Copy attribute, preserving existing names & recreating rownames
  attr_to <- attributes(to)
  attr_to[["names"]] <- names(x)
  attr_to[["row.names"]] <- .set_row_names(df_length(x))
  attributes(x) <- attr_to

  x
}

# Base vectors --------------------------------------------------------------

#' @export
#' @export vec_cast.NULL
#' @method vec_cast NULL
#' @rdname vec_cast
vec_cast.NULL <- function(x, to) UseMethod("vec_cast.NULL")
#' @export
#' @method vec_cast.NULL default
vec_cast.NULL.default <- function(x, to) x

#' @export
#' @rdname vec_cast
#' @export vec_cast.logical
#' @method vec_cast logical
vec_cast.logical <- function(x, to) UseMethod("vec_cast.logical")
#' @export
#' @method vec_cast.logical NULL
vec_cast.logical.NULL <- function(x, to) {
  NULL
}
#' @export
#' @method vec_cast.logical logical
vec_cast.logical.logical <- function(x, to) {
  shape_recycle(x, to)
}
#' @export
#' @method vec_cast.logical integer
vec_cast.logical.integer <- function(x, to) {
  lossy <- !x %in% c(0L, 1L)
  if (any(lossy)) {
    warn_lossy_cast(x, to, locations = which(lossy))
  }
  x <- vec_coerce_bare(x, "logical")
  shape_recycle(x, to)
}
#' @export
#' @method vec_cast.logical double
vec_cast.logical.double <- function(x, to) {
  lossy <- !x %in% c(0, 1)
  if (any(lossy)) {
    warn_lossy_cast(x, to, locations = which(lossy))
  }
  x <- vec_coerce_bare(x, "logical")
  shape_recycle(x, to)
}
#' @export
#' @method vec_cast.logical character
vec_cast.logical.character <- function(x, to) {
  lossy <- !toupper(x) %in% c("T", "F", "TRUE", "FALSE")
  if (any(lossy)) {
    warn_lossy_cast(x, to, locations = which(lossy))
  }
  x <- vec_coerce_bare(x, "logical")
  shape_recycle(x, to)
}
#' @export
#' @method vec_cast.logical list
vec_cast.logical.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.logical default
vec_cast.logical.default <- function(x, to) stop_incompatible_cast(x, to)

#' @export
#' @rdname vec_cast
#' @export vec_cast.integer
#' @method vec_cast integer
vec_cast.integer <- function(x, to) {
  UseMethod("vec_cast.integer")
}
#' @export
#' @method vec_cast.integer NULL
vec_cast.integer.NULL  <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.integer logical
vec_cast.integer.logical <- function(x, to) {
  x <- vec_coerce_bare(x, "integer")
  shape_recycle(x, to)
}
#' @export
#' @method vec_cast.integer integer
vec_cast.integer.integer <- function(x, to) {
  shape_recycle(x, to)
}
#' @export
#' @method vec_cast.integer double
vec_cast.integer.double <- function(x, to) {
  out <- suppressWarnings(vec_coerce_bare(x, "integer"))

  lossy <- (out != x) | xor(is.na(x), is.na(out))
  if (any(lossy)) {
    warn_lossy_cast(x, to, locations = which(lossy))
  }

  shape_recycle(out, to)
}
#' @export
#' @method vec_cast.integer character
vec_cast.integer.character <- vec_cast.integer.double
#' @export
#' @method vec_cast.integer list
vec_cast.integer.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.integer default
vec_cast.integer.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @rdname vec_cast
#' @export vec_cast.double
#' @method vec_cast double
vec_cast.double <- function(x, to) {
  UseMethod("vec_cast.double")
}
#' @export
#' @method vec_cast.double NULL
vec_cast.double.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.double logical
vec_cast.double.logical <- function(x, to) {
  x <- vec_coerce_bare(x, "double")
  shape_recycle(x, to)
}
#' @export
#' @method vec_cast.double integer
vec_cast.double.integer <- vec_cast.double.logical
#' @export
#' @method vec_cast.double character
vec_cast.double.character <- function(x, to) {
  out <- suppressWarnings(vec_coerce_bare(x, "double"))

  lossy <- (out != x) | xor(is.na(x), is.na(out))
  if (any(lossy)) {
    warn_lossy_cast(x, to, locations = which(lossy))
  }

  out <- shape_recycle(out, to)
  out
}
#' @export
#' @method vec_cast.double double
vec_cast.double.double <- function(x, to) {
  shape_recycle(x, to)
}
#' @export
#' @method vec_cast.double list
vec_cast.double.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.double default
vec_cast.double.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @rdname vec_cast
#' @export vec_cast.character
#' @method vec_cast character
vec_cast.character <- function(x, to) {
  UseMethod("vec_cast.character")
}
#' @export
#' @method vec_cast.character NULL
vec_cast.character.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.character logical
vec_cast.character.logical <- function(x, to) {
  x <- vec_coerce_bare(x, "character")
  shape_recycle(x, to)
}
#' @export
#' @method vec_cast.character integer
vec_cast.character.integer <- vec_cast.character.logical
#' @export
#' @method vec_cast.character double
vec_cast.character.double <- vec_cast.character.logical
#' @export
#' @method vec_cast.character character
vec_cast.character.character <- function(x, to) {
  shape_recycle(x, to)
}
#' @export
#' @method vec_cast.character difftime
vec_cast.character.difftime <- function(x, to) {
  x <- paste(x, units(x))
  shape_recycle(x, to)
}
#' @export
#' @method vec_cast.character list
vec_cast.character.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.character default
vec_cast.character.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @rdname vec_cast
#' @export vec_cast.list
#' @method vec_cast list
#' @export
vec_cast.list <- function(x, to) {
  UseMethod("vec_cast.list")
}
#' @export
#' @method vec_cast.list NULL
vec_cast.list.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.list vctrs_list_of
vec_cast.list.vctrs_list_of <- function(x, to) {
  warn_lossy_cast(x, to)
  shape_recycle(as.list(x), to)
}
#' @export
#' @method vec_cast.list list
vec_cast.list.list <- function(x, to) {
  shape_recycle(x, to)
}
#' @export
#' @method vec_cast.list default
vec_cast.list.default <- function(x, to) {
  lapply(seq_along(x), function(i) x[[i]])
}

# S3 vectors --------------------------------------------------------------

#' @rdname vec_cast
#' @export vec_cast.factor
#' @method vec_cast factor
#' @export
vec_cast.factor <- function(x, to) {
  UseMethod("vec_cast.factor")
}
#' @export
#' @method vec_cast.factor NULL
vec_cast.factor.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.factor factor
vec_cast.factor.factor <- function(x, to) {
  if (length(levels(to)) == 0L) {
    factor(as.character(x), levels = unique(x), ordered = is.ordered(to))
  } else {
    lossy <- !x %in% levels(to)
    if (any(lossy)) {
      warn_lossy_cast(x, to, locations = which(lossy))
    }

    factor(x, levels = levels(to), ordered = is.ordered(to))
  }
}
#' @export
#' @method vec_cast.factor character
vec_cast.factor.character <- vec_cast.factor.factor
#' @export
#' @method vec_cast.factor list
vec_cast.factor.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.factor default
vec_cast.factor.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @rdname vec_cast
#' @export vec_cast.Date
#' @method vec_cast Date
#' @export
vec_cast.Date <- function(x, to) {
  UseMethod("vec_cast.Date")
}
#' @export
#' @method vec_cast.Date NULL
vec_cast.Date.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.Date double
vec_cast.Date.double <- function(x, to) {
  as.Date(x, origin = "1970-01-01")
}
#' @export
#' @method vec_cast.Date character
vec_cast.Date.character <- function(x, to) {
  as.Date(x, format = "%Y-%m-%d")
}
#' @export
#' @method vec_cast.Date Date
vec_cast.Date.Date <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.Date POSIXt
vec_cast.Date.POSIXt <- function(x, to) {
  out <- as.Date(x)

  lossy <- abs(x - as.POSIXct(out)) > 1e-9
  if (any(lossy)) {
    warn_lossy_cast(x, to, locations = which(lossy))
  }

  out
}
#' @export
#' @method vec_cast.Date list
vec_cast.Date.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.Date default
vec_cast.Date.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @rdname vec_cast
#' @export vec_cast.POSIXt
#' @method vec_cast POSIXt
#' @export
vec_cast.POSIXt <- function(x, to) {
  UseMethod("vec_cast.POSIXt")
}
#' @export
#' @method vec_cast.POSIXt NULL
vec_cast.POSIXt.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.POSIXt double
vec_cast.POSIXt.double <- function(x, to) {
  x <- as.POSIXct(x, origin = "1970-01-01")
  attr(x, "tzone") <- attr(to, "tzone")
  x
}
#' @export
#' @method vec_cast.POSIXt character
vec_cast.POSIXt.character <- function(x, to) {
  as.POSIXct(x, tz = attr(to, "tzone") %||% "")
}
#' @export
#' @method vec_cast.POSIXt Date
vec_cast.POSIXt.Date <- function(x, to) {
  as.POSIXct(as.character(x), tz = attr(to, "tzone") %||% "")
}
#' @export
#' @method vec_cast.POSIXt POSIXt
vec_cast.POSIXt.POSIXt <- function(x, to) {
  attr(x, "tzone") <- attr(to, "tzone")
  x
}
#' @export
#' @method vec_cast.POSIXt list
vec_cast.POSIXt.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.POSIXt default
vec_cast.POSIXt.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @rdname vec_cast
#' @export vec_cast.difftime
#' @method vec_cast difftime
#' @export
vec_cast.difftime <- function(x, to) {
  UseMethod("vec_cast.difftime")
}
#' @export
#' @method vec_cast.difftime NULL
vec_cast.difftime.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.difftime double
vec_cast.difftime.double <- function(x, to) {
  structure(
    as.double(x), # strip attributes
    class = "difftime",
    units = units(to)
  )
}
#' @export
#' @method vec_cast.difftime difftime
vec_cast.difftime.difftime <- function(x, to) {
  if (identical(units(x), units(to))) {
    x
  } else {
    # Hack: I can't see any obvious way of changing the units
    origin <- as.POSIXct(0, origin = "1970-01-01")
    difftime(origin, origin - x, units = units(to))
  }
}
#' @export
#' @method vec_cast.difftime list
vec_cast.difftime.list <- function(x, to) {
  vec_list_cast(x, to)
}
#' @export
#' @method vec_cast.difftime default
vec_cast.difftime.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @rdname vec_cast
#' @export vec_cast.data.frame
#' @method vec_cast data.frame
#' @export
vec_cast.data.frame <- function(x, to) {
  UseMethod("vec_cast.data.frame")
}
#' @export
#' @method vec_cast.data.frame NULL
vec_cast.data.frame.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.data.frame data.frame
vec_cast.data.frame.data.frame <- function(x, to) {
  df <- df_col_cast(x, to)
  vec_restore(df, to)
}
#' @export
#' @method vec_cast.data.frame default
vec_cast.data.frame.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @rdname vec_cast
#' @export vec_cast.tbl_df
#' @method vec_cast tbl_df
#' @export
vec_cast.tbl_df <- function(x, to) {
  UseMethod("vec_cast.tbl_df")
}
#' @export
#' @method vec_cast.tbl_df NULL
vec_cast.tbl_df.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.tbl_df data.frame
vec_cast.tbl_df.data.frame <- function(x, to) {
  tibble::as_tibble(df_col_cast(x, to))
}
#' @export
#' @method vec_cast.data.frame default
vec_cast.tbl_df.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}
