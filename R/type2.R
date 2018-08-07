#' Find the common type for a pair of vector types
#'
#' `vec_type2()` finds the common type for a pair of vectors, or dies trying.
#' It forms the foundation of the vctrs type system, along with [vec_cast()].
#' It should generally not be called by R users, but is important for R
#' developers.
#'
#' @section Coercion rules:
#' vctrs thinks of the vector types as forming a partially ordered set, or
#' poset. Then finding the common type from a set of types is a matter of
#' finding the least-upper-bound; if the least-upper-bound does not exist,
#' there is no common type. This is the case for many pairs of 1d vectors.
#'
#' The poset of the most important base vectors is shown below:
#' (where datetime stands for `POSIXt`, and date for `Date`)
#'
#' \figure{coerce.png}
#'
#' @section S3 dispatch:
#' `vec_type2()` dispatches on both arguments. This is implemented by having
#' methods of `vec_type2()`, e.g. `vec_type2.integer()` also be S3 generics,
#' which call e.g. `vec_type2.integer.double()`. `vec_type2.x.y()` must
#' return the same value as `vec_type2.y.x()`; this is currently not enforced,
#' only checked in unit tests.
#'
#' See `vignette("extending-vctrs")` on how to extend to your own S3
#' vector classes.
#' @keywords internal
#' @param x,y Either vector types produced by [vec_type()], or actual vectors.
#' @export
vec_type2 <- function(x, y) {
  UseMethod("vec_type2")
}

#' @export
vec_type2.default <- function(x, y) {
  abort_no_max_type(x, y)
}

#' @export
vec_type2.vec_type <- function(x, y) {
  y <- as_vec_type(y)
  vec_type(vec_type2(x$prototype, y$prototype))
}

#' @export
vec_type2.NULL <- function(x, y) {
  vec_subset(y, 0L)
}

# Numeric-ish ----------------------------------------------------------

#' @rdname vec_type2
#' @export vec_type2.logical
#' @method vec_type2 logical
#' @export
vec_type2.logical <- function(x, y) UseMethod("vec_type2.logical", y)
#' @rdname vec_type2
#' @export vec_type2.integer
#' @method vec_type2 integer
#' @export
vec_type2.integer <- function(x, y) UseMethod("vec_type2.integer", y)
#' @rdname vec_type2
#' @export vec_type2.double
#' @method vec_type2 double
#' @export
vec_type2.double  <- function(x, y) UseMethod("vec_type2.double", y)

#' @method vec_type2.logical NULL
#' @export
vec_type2.logical.NULL    <- function(x, y) logical()
#' @method vec_type2.logical logical
#' @export
vec_type2.logical.logical <- function(x, y) logical()

#' @method vec_type2.integer NULL
vec_type2.integer.NULL    <- function(x, y) integer()
#' @export
#' @method vec_type2.logical integer
vec_type2.logical.integer <- function(x, y) integer()
#' @export
#' @method vec_type2.integer logical
vec_type2.integer.logical <- function(x, y) integer()
#' @export
#' @method vec_type2.integer integer
vec_type2.integer.integer <- function(x, y) integer()

#' @method vec_type2.double NULL
vec_type2.double.NULL     <- function(x, y) double()
#' @export
#' @method vec_type2.logical double
vec_type2.logical.double  <- function(x, y) double()
#' @export
#' @method vec_type2.double logical
vec_type2.double.logical  <- function(x, y) double()
#' @export
#' @method vec_type2.integer double
vec_type2.integer.double  <- function(x, y) double()
#' @export
#' @method vec_type2.double integer
vec_type2.double.integer  <- function(x, y) double()
#' @export
#' @method vec_type2.double double
vec_type2.double.double   <- function(x, y) double()

#' @method vec_type2.logical default
#' @export
vec_type2.logical.default <- function(x, y) abort_no_max_type(x, y)
#' @method vec_type2.integer default
#' @export
vec_type2.integer.default <- function(x, y) abort_no_max_type(x, y)
#' @method vec_type2.double default
#' @export
vec_type2.double.default  <- function(x, y) abort_no_max_type(x, y)

# Characters and factors --------------------------------------------------

#' @rdname vec_type2
#' @export vec_type2.character
#' @method vec_type2 character
#' @export
vec_type2.character <- function(x, y) UseMethod("vec_type2.character", y)
#' @rdname vec_type2
#' @export vec_type2.factor
#' @method vec_type2 factor
#' @export
vec_type2.factor    <- function(x, y) UseMethod("vec_type2.factor", y)
#' @rdname vec_type2
#' @export vec_type2.ordered
#' @method vec_type2 ordered
#' @export
vec_type2.ordered   <- function(x, y) UseMethod("vec_type2.ordered", y)

#' @method vec_type2.ordered NULL
#' @export
vec_type2.ordered.NULL        <- function(x, y) new_ordered(levels = levels(x))
#' @method vec_type2.ordered ordered
#' @export
vec_type2.ordered.ordered     <- function(x, y) new_ordered(levels = levels_union(x, y))

#' @method vec_type2.factor NULL
#' @export
vec_type2.factor.NULL         <- function(x, y) new_factor(levels = levels(x))
#' @method vec_type2.factor factor
#' @export
vec_type2.factor.factor       <- function(x, y) new_factor(levels = levels_union(x, y))
#' @method vec_type2.ordered factor
#' @export
vec_type2.ordered.factor      <- function(x, y) new_factor(levels = levels_union(x, y))

#' @method vec_type2.character NULL
#' @export
vec_type2.character.NULL      <- function(x, y) character()
#' @method vec_type2.ordered character
#' @export
vec_type2.ordered.character   <- function(x, y) character()
#' @method vec_type2.character ordered
#' @export
vec_type2.character.ordered   <- function(x, y) character()
#' @method vec_type2.character factor
#' @export
vec_type2.character.factor    <- function(x, y) character()
#' @method vec_type2.factor character
#' @export
vec_type2.factor.character    <- function(x, y) character()
#' @method vec_type2.character character
#' @export
vec_type2.character.character <- function(x, y) character()

#' @method vec_type2.character default
#' @export
vec_type2.character.default <- function(x, y) abort_no_max_type(x, y)
#' @method vec_type2.factor default
#' @export
vec_type2.factor.default    <- function(x, y) abort_no_max_type(x, y)
#' @method vec_type2.ordered default
#' @export
vec_type2.ordered.default   <- function(x, y) abort_no_max_type(x, y)

# Date-times --------------------------------------------------------------

#' @rdname vec_type2
#' @export vec_type2.Date
#' @method vec_type2 Date
#' @export
vec_type2.Date   <- function(x, y) UseMethod("vec_type2.Date", y)
#' @rdname vec_type2
#' @export vec_type2.POSIXt
#' @method vec_type2 POSIXt
#' @export
vec_type2.POSIXt <- function(x, y) UseMethod("vec_type2.POSIXt", y)

#' @method vec_type2.Date NULL
#' @export
vec_type2.Date.NULL      <- function(x, y) new_date()
#' @method vec_type2.Date Date
#' @export
vec_type2.Date.Date      <- function(x, y) new_date()

#' @method vec_type2.POSIXt NULL
#' @export
vec_type2.POSIXt.NULL    <- function(x, y) new_datetime(tzone = tzone(x))
#' @method vec_type2.POSIXt Date
#' @export
vec_type2.POSIXt.Date    <- function(x, y) new_datetime(tzone = tzone(x))
#' @method vec_type2.Date POSIXt
#' @export
vec_type2.Date.POSIXt    <- function(x, y) new_datetime(tzone = tzone(y))
#' @method vec_type2.POSIXt POSIXt
#' @export
vec_type2.POSIXt.POSIXt  <- function(x, y) new_datetime(tzone = tzone_union(x, y))

#' @method vec_type2.Date default
#' @export
vec_type2.Date.default   <- function(x, y) abort_no_max_type(x, y)
#' @method vec_type2.POSIXt default
#' @export
vec_type2.POSIXt.default <- function(x, y) abort_no_max_type(x, y)

# difftime ----------------------------------------------------------------

#' @rdname vec_type2
#' @export vec_type2.difftime
#' @method vec_type2 difftime
#' @export
vec_type2.difftime <- function(x, y) UseMethod("vec_type2.difftime", y)

#' @method vec_type2.difftime NULL
#' @export
vec_type2.difftime.NULL     <- function(x, y) new_difftime(units = units(x))
#' @method vec_type2.difftime difftime
#' @export
vec_type2.difftime.difftime <- function(x, y) new_difftime(units = units_union(x, y))

#' @method vec_type2.difftime default
#' @export
vec_type2.difftime.default  <- function(x, y) abort_no_max_type(x, y)

# Lists -------------------------------------------------------------------

#' @rdname vec_type2
#' @export vec_type2.list
#' @method vec_type2 list
#' @export
vec_type2.list    <- function(x, y) UseMethod("vec_type2.list", y)

#' @method vec_type2.list NULL
#' @export
vec_type2.list.NULL <- function(x, y) list()
#' @method vec_type2.list list
#' @export
vec_type2.list.list <- function(x, y) list()

#' @method vec_type2.list default
#' @export
vec_type2.list.default  <- function(x, y) abort_no_max_type(x, y)

# Data frames -------------------------------------------------------------

#' @rdname vec_type2
#' @export vec_type2.data.frame
#' @method vec_type2 data.frame
#' @export
vec_type2.data.frame <- function(x, y) UseMethod("vec_type2.data.frame", y)
#' @rdname vec_type2
#' @export vec_type2.tbl_df
#' @method vec_type2 tbl_df
#' @export
vec_type2.tbl_df     <- function(x, y) UseMethod("vec_type2.tbl_df", y)

#' @method vec_type2.data.frame NULL
#' @export
vec_type2.data.frame.NULL <- function(x, y) vec_subset(x, 0L)
#' @method vec_type2.data.frame data.frame
#' @export
vec_type2.data.frame.data.frame <- function(x, y) new_data_frame(df_col_type2(x, y), n = 0)

#' @method vec_type2.tbl_df NULL
#' @export
vec_type2.tbl_df.NULL <- function(x, y) vec_subset(x, 0L)
#' @method vec_type2.tbl_df data.frame
#' @export
vec_type2.tbl_df.data.frame <- function(x, y) new_tibble(df_col_type2(x, y), n = 0)
#' @method vec_type2.data.frame tbl_df
#' @export
vec_type2.data.frame.tbl_df <- function(x, y) new_tibble(df_col_type2(x, y), n = 0)

#' @method vec_type2.data.frame default
#' @export
vec_type2.data.frame.default <- function(x, y) abort_no_max_type(x, y)
#' @method vec_type2.tbl_df default
#' @export
vec_type2.tbl_df.default <- function(x, y) abort_no_max_type(x, y)

# Helpers -----------------------------------------------------------------

abort_no_max_type <- function(x, y) {
  type_x <- as_vec_type(x)
  type_y <- as_vec_type(y)

  msg <- glue::glue("No common type for {type_x} and {type_y}")
  abort(
    "error_no_max_type",
    message = msg,
    type_x = type_x,
    type_y = type_y,
  )
}
