#' Assemble attributes for data frame construction
#'
#' `new_data_frame()` constructs a new data frame from an existing list. It is
#' meant to be performant, and does not check the inputs for correctness in any
#' way. It is only safe to use after a call to [df_list()], which collects and
#' validates the columns used to construct the data frame.
#'
#' @seealso
#' [df_list()] for a way to safely construct a data frame's underlying
#' data structure from individual columns. This can be used to create a
#' named list for further use by `new_data_frame()`.
#'
#' @param x A named list of equal-length vectors. The lengths are not
#'   checked; it is responsibility of the caller to make sure they are
#'   equal.
#' @param n Number of rows. If `NULL`, will be computed from the length of
#'   the first element of `x`.
#' @param ...,class Additional arguments for creating subclasses.
#'
#'   The following attributes have special behavior:
#'   - `"names"` is preferred if provided, overriding existing names in `x`.
#'   - `"row.names"` is preferred if provided, overriding both `n` and the size
#'     implied by `x`.
#'
#' @export
#' @examples
#' new_data_frame(list(x = 1:10, y = 10:1))
new_data_frame <- function(x = list(), n = NULL, ..., class = NULL) {
  .External(ffi_new_data_frame, x, n, class, ...)
}
new_data_frame <- fn_inline_formals(new_data_frame, "x")

#' Collect columns for data frame construction
#'
#' `df_list()` constructs the data structure underlying a data
#' frame, a named list of equal-length vectors. It is often used in
#' combination with [new_data_frame()] to safely and consistently create
#' a helper function for data frame subclasses.
#'
#' @section Properties:
#'
#' - Inputs are [recycled][theory-faq-recycling] to a common size with
#'   [vec_recycle_common()].
#'
#' - With the exception of data frames, inputs are not modified in any way.
#'   Character vectors are never converted to factors, and lists are stored
#'   as-is for easy creation of list-columns.
#'
#' - Unnamed data frame inputs are automatically unpacked. Named data frame
#'   inputs are stored unmodified as data frame columns.
#'
#' - `NULL` inputs are completely ignored.
#'
#' - The dots are dynamic, allowing for splicing of lists with `!!!` and
#'   unquoting.
#'
#' @seealso
#' [new_data_frame()] for constructing data frame subclasses from a validated
#' input. [data_frame()] for a fast data frame creation helper.
#'
#' @inheritParams rlang::args_error_context
#'
#' @param ... Vectors of equal-length. When inputs are named, those names
#'   are used for names of the resulting list.
#' @param .size The common size of vectors supplied in `...`. If `NULL`, this
#'   will be computed as the common size of the inputs.
#' @param .unpack Should unnamed data frame inputs be unpacked? Defaults to
#'   `TRUE`.
#' @param .name_repair One of `"check_unique"`, `"unique"`, `"universal"`,
#'   `"minimal"`, `"unique_quiet"`, or `"universal_quiet"`. See [vec_as_names()]
#'   for the meaning of these options.
#'
#' @export
#' @examples
#' # `new_data_frame()` can be used to create custom data frame constructors
#' new_fancy_df <- function(x = list(), n = NULL, ..., class = NULL) {
#'   new_data_frame(x, n = n, ..., class = c(class, "fancy_df"))
#' }
#'
#' # Combine this constructor with `df_list()` to create a safe,
#' # consistent helper function for your data frame subclass
#' fancy_df <- function(...) {
#'   data <- df_list(...)
#'   new_fancy_df(data)
#' }
#'
#' df <- fancy_df(x = 1)
#' class(df)
df_list <- function(...,
                    .size = NULL,
                    .unpack = TRUE,
                    .name_repair = c("check_unique", "unique", "universal", "minimal", "unique_quiet", "universal_quiet"),
                    .error_call = current_env()) {
  .Call(ffi_df_list, list2(...), .size, .unpack, .name_repair, environment())
}
df_list <- fn_inline_formals(df_list, ".name_repair")

#' Construct a data frame
#'
#' @description
#' `data_frame()` constructs a data frame. It is similar to
#' [base::data.frame()], but there are a few notable differences that make it
#' more in line with vctrs principles. The Properties section outlines these.
#'
#' @details
#' If no column names are supplied, `""` will be used as a default name for all
#' columns. This is applied before name repair occurs, so the default name
#' repair of `"check_unique"` will error if any unnamed inputs are supplied and
#' `"unique"` (or `"unique_quiet"`) will repair the empty string column names
#' appropriately. If the column names don't matter, use a `"minimal"` name
#' repair for convenience and performance.
#'
#' @inheritSection df_list Properties
#'
#' @seealso
#' [df_list()] for safely creating a data frame's underlying data structure from
#' individual columns. [new_data_frame()] for constructing the actual data
#' frame from that underlying data structure. Together, these can be useful
#' for developers when creating new data frame subclasses supporting
#' standard evaluation.
#'
#' @inheritParams rlang::args_error_context
#'
#' @param ... Vectors to become columns in the data frame. When inputs are
#'   named, those names are used for column names.
#' @param .size The number of rows in the data frame. If `NULL`, this will
#'   be computed as the common size of the inputs.
#' @param .name_repair One of `"check_unique"`, `"unique"`, `"universal"`,
#'   `"minimal"`, `"unique_quiet"`, or `"universal_quiet"`. See [vec_as_names()]
#'   for the meaning of these options.
#'
#' @export
#' @examples
#' data_frame(x = 1, y = 2)
#'
#' # Inputs are recycled using tidyverse recycling rules
#' data_frame(x = 1, y = 1:3)
#'
#' # Strings are never converted to factors
#' class(data_frame(x = "foo")$x)
#'
#' # List columns can be easily created
#' df <- data_frame(x = list(1:2, 2, 3:4), y = 3:1)
#'
#' # However, the base print method is suboptimal for displaying them,
#' # so it is recommended to convert them to tibble
#' if (rlang::is_installed("tibble")) {
#'   tibble::as_tibble(df)
#' }
#'
#' # Named data frame inputs create data frame columns
#' df <- data_frame(x = data_frame(y = 1:2, z = "a"))
#'
#' # The `x` column itself is another data frame
#' df$x
#'
#' # Again, it is recommended to convert these to tibbles for a better
#' # print method
#' if (rlang::is_installed("tibble")) {
#'   tibble::as_tibble(df)
#' }
#'
#' # Unnamed data frame input is automatically unpacked
#' data_frame(x = 1, data_frame(y = 1:2, z = "a"))
data_frame <- function(...,
                       .size = NULL,
                       .name_repair = c("check_unique", "unique", "universal", "minimal", "unique_quiet", "universal_quiet"),
                       .error_call = current_env()) {
  .Call(ffi_data_frame, list2(...), .size, .name_repair, environment())
}
data_frame <- fn_inline_formals(data_frame, ".name_repair")

#' @export
vec_ptype_abbr.data.frame <- function(x, ...) {
  "df"
}

# For testing
# Keep in sync with `enum vctrs_proxy_kind` in `vctrs.h`
df_proxy <- function(x, kind) {
  .Call(ffi_df_proxy, x, kind)
}
VCTRS_PROXY_KIND_equal <- 0L
VCTRS_PROXY_KIND_compare <- 1L
VCTRS_PROXY_KIND_order <- 2L

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
df_ptype2 <- function(x,
                      y,
                      ...,
                      x_arg = "",
                      y_arg = "",
                      call = caller_env()) {
  .Call(
    ffi_df_ptype2_opts,
    x,
    y,
    opts = match_fallback_opts(...),
    environment()
  )
}
#' @rdname df_ptype2
#' @export
df_cast <- function(x,
                    to,
                    ...,
                    x_arg = "",
                    to_arg = "",
                    call = caller_env()) {
  .Call(
    ffi_df_cast_opts,
    x,
    to,
    opts = match_fallback_opts(...),
    environment()
  )
}

df_ptype2_opts <- function(x,
                           y,
                           ...,
                           opts,
                           x_arg = "",
                           y_arg = "",
                           call = caller_env()) {
  .Call(ffi_df_ptype2_opts, x, y, opts = opts, environment())
}

df_cast_opts <- function(x,
                         to,
                         ...,
                         opts = fallback_opts(),
                         x_arg = "",
                         to_arg = "",
                         call = caller_env()) {
  .Call(
    ffi_df_cast_opts,
    x,
    to,
    opts,
    environment()
  )
}
df_cast_params <- function(x,
                           to,
                           ...,
                           x_arg = "",
                           to_arg = "",
                           s3_fallback = NULL) {
  opts <- fallback_opts(s3_fallback = s3_fallback)
  df_cast_opts(x, to, opts = opts, x_arg = x_arg, to_arg = to_arg)
}

#' vctrs methods for data frames
#'
#' These functions help the base data.frame class fit into the vctrs type system
#' by providing coercion and casting functions.
#'
#' @keywords internal
#' @name vctrs-data-frame
NULL

#' @rdname vctrs-data-frame
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

# Fallback for data frame subclasses (#981)
vec_ptype2_df_fallback <- function(x,
                                   y,
                                   opts,
                                   x_arg = "",
                                   y_arg = "",
                                   call = caller_env()) {
  vec_ptype2_params(
    as_base_df(x),
    as_base_df(y),
    s3_fallback = opts$s3_fallback,
    x_arg = x_arg,
    y_arg = y_arg,
    call = call
  )
}
as_base_df <- function(x) {
  if (inherits(x, "tbl_df")) {
    new_data_frame(x, class = c("tbl_df", "tbl"))
  } else {
    new_data_frame(x)
  }
}


# Cast --------------------------------------------------------------------

#' @rdname vctrs-data-frame
#' @export vec_cast.data.frame
#' @method vec_cast data.frame
#' @export
vec_cast.data.frame <- function(x, to, ...) {
  UseMethod("vec_cast.data.frame")
}
#' @export
#' @method vec_cast.data.frame data.frame
vec_cast.data.frame.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_restore.data.frame <- function(x, to, ...) {
  .Call(ffi_vec_bare_df_restore, x, to)
}

# Helpers -----------------------------------------------------------------

df_size <- function(x) {
  .Call(vctrs_df_size, x)
}

df_lossy_cast <- function(out,
                          x,
                          to,
                          ...,
                          x_arg = "",
                          to_arg = "",
                          call = caller_env()) {
  extra <- setdiff(names(x), names(to))

  maybe_lossy_cast(
    result = out,
    x = x,
    to = to,
    lossy = length(extra) > 0,
    locations = int(),
    x_arg = x_arg,
    to_arg = to_arg,
    call = call,
    details = inline_list("Dropped variables: ", extra, quote = "`"),
    class = "vctrs_error_cast_lossy_dropped"
  )
}

is_informative_error_vctrs_error_cast_lossy_dropped <- function(x, ...) {
  FALSE
}

df_attrib <- function(x) {
  attributes(x)[c("row.names", "names")]
}
non_df_attrib <- function(x) {
  attrib <- attributes(x)
  attrib <- attrib[!names(attrib) %in% c("row.names", "names")]

  # Sort to allow comparison
  attrib[order(names(attrib))]
}
