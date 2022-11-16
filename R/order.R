# TODO: Use this NEWS bullet when we move to the new `vec_order()` algorithm
#
# * `vec_order()` and `vec_sort()` now use a custom radix sort algorithm, rather
#    than relying on `order()`. The implementation is based on data.table’s
#    `forder()` and their earlier contribution to R’s `order()`. There are four
#    major changes, outlined below, the first two of which are breaking changes.
#    If you need to retain the old ordering behavior, use `vec_order_base()`.
#
#    * Character vectors now order in the C locale by default, which is _much_
#      faster than ordering in the system's locale. To order in a specific locale,
#      you can provide a character proxy function through `chr_proxy_collate`,
#      such as `stringi::stri_sort_key()`.
#
#    * Optional arguments, such as `direction` and `na_value`, must now be
#      specified by name. Specifying by position will result in an error.
#
#    * When ordering data frames, you can now control the behavior of `direction`
#      and `na_value` on a per column basis.
#
#    * There is a new `nan_distinct` argument for differentiating between `NaN`
#      and `NA` in double and complex vectors.


#' Order and sort vectors
#'
#' @description
#' `vec_order_radix()` computes the order of `x`. For data frames, the order is
#' computed along the rows by computing the order of the first column and
#' using subsequent columns to break ties.
#'
#' `vec_sort_radix()` sorts `x`. It is equivalent to `vec_slice(x, vec_order_radix(x))`.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x A vector
#' @param direction Direction to sort in.
#'   - A single `"asc"` or `"desc"` for ascending or descending order
#'     respectively.
#'   - For data frames, a length `1` or `ncol(x)` character vector containing
#'     only `"asc"` or `"desc"`, specifying the direction for each column.
#' @param na_value Ordering of missing values.
#'   - A single `"largest"` or `"smallest"` for ordering missing values as the
#'     largest or smallest values respectively.
#'   - For data frames, a length `1` or `ncol(x)` character vector containing
#'     only `"largest"` or `"smallest"`, specifying how missing values should
#'     be ordered within each column.
#' @param nan_distinct A single logical specifying whether or not `NaN` should
#'   be considered distinct from `NA` for double and complex vectors. If `TRUE`,
#'   `NaN` will always be ordered between `NA` and non-missing numbers.
#' @param chr_proxy_collate A function generating an alternate representation
#'   of character vectors to use for collation, often used for locale-aware
#'   ordering.
#'   - If `NULL`, no transformation is done.
#'   - Otherwise, this must be a function of one argument. If the input contains
#'     a character vector, it will be passed to this function after it has been
#'     translated to UTF-8. This function should return a character vector with
#'     the same length as the input. The result should sort as expected in the
#'     C-locale, regardless of encoding.
#'
#'   For data frames, `chr_proxy_collate` will be applied to all character
#'   columns.
#'
#'   Common transformation functions include: `tolower()` for case-insensitive
#'   ordering and `stringi::stri_sort_key()` for locale-aware ordering.
#'
#' @return
#' * `vec_order_radix()` an integer vector the same size as `x`.
#' * `vec_sort_radix()` a vector with the same size and type as `x`.
#'
#' @section Differences with `order()`:
#'
#' Unlike the `na.last` argument of `order()` which decides the positions of
#' missing values irrespective of the `decreasing` argument, the `na_value`
#' argument of `vec_order_radix()` interacts with `direction`. If missing values
#' are considered the largest value, they will appear last in ascending order,
#' and first in descending order.
#'
#' Character vectors are ordered in the C-locale. This is different from
#' `base::order()`, which respects `base::Sys.setlocale()`. Sorting in a
#' consistent locale can produce more reproducible results between different
#' sessions and platforms, however, the results of sorting in the C-locale
#' can be surprising. For example, capital letters sort before lower case
#' letters. Sorting `c("b", "C", "a")` with `vec_sort_radix()` will return
#' `c("C", "a", "b")`, but with `base::order()` will return `c("a", "b", "C")`
#' unless `base::order(method = "radix")` is explicitly set, which also uses
#' the C-locale. While sorting with the C-locale can be useful for
#' algorithmic efficiency, in many real world uses it can be the cause of
#' data analysis mistakes. To balance these trade-offs, you can supply a
#' `chr_proxy_collate` function to transform character vectors into an
#' alternative representation that orders in the C-locale in a less surprising
#' way. For example, providing [base::tolower()] as a transform will order the
#' original vector in a case-insensitive manner. Locale-aware ordering can be
#' achieved by providing `stringi::stri_sort_key()` as a transform, setting the
#' collation options as appropriate for your locale.
#'
#' Character vectors are always translated to UTF-8 before ordering, and before
#' any transform is applied by `chr_proxy_collate`.
#'
#' For complex vectors, if either the real or imaginary component is `NA` or
#' `NaN`, then the entire observation is considered missing.
#'
#' @section Dependencies of `vec_order_radix()`:
#' * [vec_proxy_order()]
#'
#' @section Dependencies of `vec_sort_radix()`:
#' * [vec_order_radix()]
#' * [vec_slice()]
#'
#' @name order-radix
#' @keywords internal
#'
#' @examples
#' if (FALSE) {
#'
#' x <- round(sample(runif(5), 9, replace = TRUE), 3)
#' x <- c(x, NA)
#'
#' vec_order_radix(x)
#' vec_sort_radix(x)
#' vec_sort_radix(x, direction = "desc")
#'
#' # Can also handle data frames
#' df <- data.frame(g = sample(2, 10, replace = TRUE), x = x)
#' vec_order_radix(df)
#' vec_sort_radix(df)
#' vec_sort_radix(df, direction = "desc")
#'
#' # For data frames, `direction` and `na_value` are allowed to be vectors
#' # with length equal to the number of columns in the data frame
#' vec_sort_radix(
#'   df,
#'   direction = c("desc", "asc"),
#'   na_value = c("largest", "smallest")
#' )
#'
#' # Character vectors are ordered in the C locale, which orders capital letters
#' # below lowercase ones
#' y <- c("B", "A", "a")
#' vec_sort_radix(y)
#'
#' # To order in a case-insensitive manner, provide a `chr_proxy_collate`
#' # function that transforms the strings to all lowercase
#' vec_sort_radix(y, chr_proxy_collate = tolower)
#'
#' }
NULL

#' @rdname order-radix
vec_order_radix <- function(x,
                            ...,
                            direction = "asc",
                            na_value = "largest",
                            nan_distinct = FALSE,
                            chr_proxy_collate = NULL) {
  check_dots_empty0(...)
  .Call(vctrs_order, x, direction, na_value, nan_distinct, chr_proxy_collate)
}

#' @rdname order-radix
vec_sort_radix <- function(x,
                           ...,
                           direction = "asc",
                           na_value = "largest",
                           nan_distinct = FALSE,
                           chr_proxy_collate = NULL) {
  check_dots_empty0(...)

  idx <- vec_order_radix(
    x = x,
    direction = direction,
    na_value = na_value,
    nan_distinct = nan_distinct,
    chr_proxy_collate = chr_proxy_collate
  )

  vec_slice(x, idx)
}

# ------------------------------------------------------------------------------

#' Locate sorted groups
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `vec_locate_sorted_groups()` returns a data frame containing a `key` column
#' with sorted unique groups, and a `loc` column with the locations of each
#' group in `x`.
#'
#' `vec_locate_sorted_groups()` is very similar to [vec_group_loc()], except
#' the groups are typically sorted by value rather than by first appearance.
#' If `appearance = TRUE`, then the two functions are roughly identical, with
#' the main difference being that `vec_locate_sorted_groups(appearance = TRUE)`
#' computes the groups using a sort-based approach, and `vec_group_loc()`
#' computes them using a hash-based approach. One may be faster than the other
#' depending on the structure of the input data.
#'
#' @details
#' `vec_locate_sorted_groups(x)` is equivalent to, but faster than:
#'
#' ```
#' info <- vec_group_loc(x)
#' vec_slice(info, vec_order(info$key))
#' ```
#'
#' @inheritParams order-radix
#'
#' @param appearance Ordering of returned group keys.
#'
#'   If `FALSE`, the default, group keys are returned sorted by value.
#'
#'   If `TRUE`, group keys are returned sorted by first appearance in `x`. This
#'   means `direction`, `na_value`, and `chr_proxy_collate` no longer have any
#'   effect.
#'
#' @return
#' A two column data frame with size equal to `vec_size(vec_unique(x))`.
#' * A `key` column of type `vec_ptype(x)`.
#' * A `loc` column of type list, with elements of type integer.
#'
#' @section Dependencies of `vec_locate_sorted_groups()`:
#' * [vec_proxy_order()]
#'
#' @export
#' @keywords internal
#' @examples
#' df <- data.frame(
#'   g = sample(2, 10, replace = TRUE),
#'   x = c(NA, sample(5, 9, replace = TRUE))
#' )
#'
#' # `vec_locate_sorted_groups()` is similar to `vec_group_loc()`, except keys
#' # are returned ordered rather than by first appearance by default.
#' vec_locate_sorted_groups(df)
#' vec_group_loc(df)
#'
#' # Setting `appearance = TRUE` makes `vec_locate_sorted_groups()` mostly
#' # equivalent to `vec_group_loc()`, but their underlying algorithms are very
#' # different.
#' vec_locate_sorted_groups(df, appearance = TRUE)
vec_locate_sorted_groups <- function(x,
                                     ...,
                                     direction = "asc",
                                     na_value = "largest",
                                     nan_distinct = FALSE,
                                     chr_proxy_collate = NULL,
                                     appearance = FALSE) {
  check_dots_empty0(...)

  .Call(
    vctrs_locate_sorted_groups,
    x,
    direction,
    na_value,
    nan_distinct,
    chr_proxy_collate,
    appearance
  )
}

# ------------------------------------------------------------------------------

vec_order_info <- function(x,
                           ...,
                           direction = "asc",
                           na_value = "largest",
                           nan_distinct = FALSE,
                           chr_proxy_collate = NULL,
                           appearance = FALSE) {
  check_dots_empty0(...)
  .Call(vctrs_order_info, x, direction, na_value, nan_distinct, chr_proxy_collate, appearance)
}

# ------------------------------------------------------------------------------

#' Order and sort vectors
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x A vector
#' @param direction Direction to sort in. Defaults to `asc`ending.
#' @param na_value Should `NA`s be treated as the largest or smallest values?
#' @return
#' * `vec_order()` an integer vector the same size as `x`.
#' * `vec_sort()` a vector with the same size and type as `x`.
#'
#' @section Differences with `order()`:
#' Unlike the `na.last` argument of `order()` which decides the
#' positions of missing values irrespective of the `decreasing`
#' argument, the `na_value` argument of `vec_order()` interacts with
#' `direction`. If missing values are considered the largest value,
#' they will appear last in ascending order, and first in descending
#' order.
#'
#' @section Dependencies of `vec_order()`:
#' * [vec_proxy_order()]
#'
#' @section Dependencies of `vec_sort()`:
#' * [vec_proxy_order()]
#' * [vec_order()]
#' * [vec_slice()]
#' @export
#' @examples
#' x <- round(c(runif(9), NA), 3)
#' vec_order(x)
#' vec_sort(x)
#' vec_sort(x, direction = "desc")
#'
#' # Can also handle data frames
#' df <- data.frame(g = sample(2, 10, replace = TRUE), x = x)
#' vec_order(df)
#' vec_sort(df)
#' vec_sort(df, direction = "desc")
#'
#' # Missing values interpreted as largest values are last when
#' # in increasing order:
#' vec_order(c(1, NA), na_value = "largest", direction = "asc")
#' vec_order(c(1, NA), na_value = "largest", direction = "desc")
vec_order <- function(x,
                      ...,
                      direction = c("asc", "desc"),
                      na_value = c("largest", "smallest")) {
  check_dots_empty0(...)

  direction <- arg_match0(direction, c("asc", "desc"))
  na_value <- arg_match0(na_value, c("largest", "smallest"))

  decreasing <- !identical(direction, "asc")
  na.last <- identical(na_value, "largest")
  if (decreasing) {
    na.last <- !na.last
  }

  proxy <- vec_proxy_order(x)

  if (is.data.frame(proxy)) {
    if (length(proxy) == 0L) {
      # Work around type-instability in `base::order()`
      return(vec_seq_along(proxy))
    }
    args <- map(unstructure(proxy), function(.x) {
      if (is.data.frame(.x)) {
        .x <- order(vec_order(.x, direction = direction, na_value = na_value))
      }
      .x
    })
    exec("order", !!!args, decreasing = decreasing, na.last = na.last)
  } else if (is_character(proxy) || is_logical(proxy) || is_integer(proxy) || is_double(proxy) || is.complex(proxy)) {
    if (is.object(proxy)) {
      proxy <- unstructure(proxy)
    }
    order(proxy, decreasing = decreasing, na.last = na.last)
  } else {
    abort("Invalid type returned by `vec_proxy_order()`.")
  }
}

#' @export
#' @rdname vec_order
vec_sort <- function(x,
                     ...,
                     direction = c("asc", "desc"),
                     na_value = c("largest", "smallest")) {
  check_dots_empty0(...)

  direction <- arg_match0(direction, c("asc", "desc"))
  na_value <- arg_match0(na_value, c("largest", "smallest"))

  idx <- vec_order(x, direction = direction, na_value = na_value)
  vec_slice(x, idx)
}
