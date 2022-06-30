#' Recycling rules used by r-lib and the tidyverse
#'
#' @description
#' Recycling describes the concept of repeating elements of one vector to match
#' the size of another vector. There are two rules that guide the recycling
#' process used by packages in r-lib and the tidyverse:
#'
#' - Vectors of size 1 will be recycled to the size of any other vector
#'
#' - Otherwise, all vectors must have the same size
#'
#' @section Examples:
#'
#' Vectors of size 1 are recycled to the size of any other vector:
#'
#' ```
#' tibble(x = 1:3, y = 1L)
#' #> # A tibble: 3 × 2
#' #>       x     y
#' #>   <int> <int>
#' #> 1     1     1
#' #> 2     2     1
#' #> 3     3     1
#' ```
#'
#' This includes vectors of size 0:
#'
#' ```
#' tibble(x = integer(), y = 1L)
#' #> # A tibble: 0 × 2
#' #> # … with 2 variables: x <int>, y <int>
#' ```
#'
#' If vectors aren't size 1, they must all be the same size. Otherwise, an error
#' is thrown:
#'
#' ```
#' tibble(x = 1:3, y = 4:7)
#' #> Error:
#' #> ! Tibble columns must have compatible sizes.
#' #> • Size 3: Existing data.
#' #> • Size 4: Column `y`.
#' #> ℹ Only values of size one are recycled.
#' ```
#'
#' @section vctrs backend:
#'
#' Packages in r-lib and the tidyverse generally use [vec_size_common()] and
#' [vec_recycle_common()] as the backends for handling recycling rules.
#'
#' - `vec_size_common()` returns the common size of multiple vectors, after
#'   applying the recycling rules
#'
#' - `vec_recycle_common()` goes one step further, and actually recycles the
#'   vectors to their common size
#'
#' ```
#' vec_size_common(1:3, "x")
#' #> [1] 3
#'
#' vec_recycle_common(1:3, "x")
#' #> [[1]]
#' #> [1] 1 2 3
#' #>
#' #> [[2]]
#' #> [1] "x" "x" "x"
#'
#' vec_size_common(1:3, c("x", "y"))
#' #> Error:
#' #> ! Can't recycle `..1` (size 3) to match `..2` (size 2).
#' ```
#'
#' @section Differences with base R:
#'
#' The recycling rules described here are stricter than the ones generally used
#' by base R. With base R, the rules are usually:
#'
#' - If any vector is length 0, the output will be length 0
#'
#' - Otherwise, the output will be length `max(length_x, length_y)`, and a
#'   warning will be thrown if the length of the longer vector is not an integer
#'   multiple of the length of the shorter vector.
#'
#' ```
#' # `max(2, 4) == 4`
#' # `1:2` is fully recycled to `c(1:2, 1:2)`
#' 1:2 + 1:4
#' #> [1] 2 4 4 6
#'
#' # `max(3, 4) == 4`, with a warning
#' # `1:3` is partially recycled to `c(1:3, 1)`
#' 1:3 + 1:4
#' #> [1] 2 4 6 5
#' #> Warning message:
#' #>   In 1:3 + 1:4 :
#' #>   longer object length is not a multiple of shorter object length
#'
#' # Length 0 vector overrides any other length
#' 1 + numeric()
#' #> numeric(0)
#' 1:3 + numeric()
#' #> numeric(0)
#' ```
#'
#' These rules come from the [R Language
#' Definition](https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Recycling-rules)
#' and are mainly focused on arithmetic. Outside of arithmetic, these rules are
#' not followed consistently:
#'
#' ```
#' # Silent recycling
#' atan2(1:3, 1:2)
#' #> [1] 0.7853982 0.7853982 1.2490458
#'
#' # `cbind()` is fairly consistent
#' cbind(1:3, 1:2)
#' #>      [,1] [,2]
#' #> [1,]    1    1
#' #> [2,]    2    2
#' #> [3,]    3    1
#' #> Warning message:
#' #>   In cbind(1:3, 1:2) :
#' #>   number of rows of result is not a multiple of vector length (arg 2)
#'
#' # But it doesn't recycle to length 0. Instead, it takes `max(3, 0) == 3`.
#' cbind(1:3, integer())
#' #>      [,1]
#' #> [1,]    1
#' #> [2,]    2
#' #> [3,]    3
#'
#' # `paste()` also takes `max(3, 0)` rather than recycling to length 0
#' paste(1:3, integer())
#' #> [1] "1 " "2 " "3 "
#'
#' # R 4.0.1 added `recycle0` for this case
#' paste(1:3, integer(), recycle0 = TRUE)
#' #> character(0)
#'
#' # Erroring rather than recycling
#' data.frame(1:3, 1:2)
#' #> Error in base::data.frame(..., stringsAsFactors = stringsAsFactors):
#' #> arguments imply differing number of rows: 3, 2
#' ```
#'
#' @name vector_recycling_rules
#' @keywords internal
NULL

#' Vector recycling
#'
#' `vec_recycle(x, size)` recycles a single vector to a given size.
#' `vec_recycle_common(...)` recycles multiple vectors to their common size. All
#' functions obey the [vctrs recycling rules][vector_recycling_rules], and will
#' throw an error if recycling is not possible. See [vec_size()] for the precise
#' definition of size.
#'
#' @inheritParams rlang::args_error_context
#'
#' @param x A vector to recycle.
#' @param ... Depending on the function used:
#'   * For `vec_recycle_common()`, vectors to recycle.
#'   * For `vec_recycle()`, these dots should be empty.
#' @param size Desired output size.
#' @param .size Desired output size. If omitted,
#'   will use the common size from [vec_size_common()].
#' @param x_arg Argument name for `x`. These are used in error
#'   messages to inform the user about which argument has an
#'   incompatible size.
#'
#' @section Dependencies:
#' - [vec_slice()]
#'
#' @export
#' @examples
#' # Inputs with 1 observation are recycled
#' vec_recycle_common(1:5, 5)
#' vec_recycle_common(integer(), 5)
#' \dontrun{
#' vec_recycle_common(1:5, 1:2)
#' }
#'
#' # Data frames and matrices are recycled along their rows
#' vec_recycle_common(data.frame(x = 1), 1:5)
#' vec_recycle_common(array(1:2, c(1, 2)), 1:5)
#' vec_recycle_common(array(1:3, c(1, 3, 1)), 1:5)
vec_recycle <- function(x, size, ..., x_arg = "", call = caller_env()) {
  check_dots_empty0(...)
  .Call(ffi_recycle, x, size, environment())
}

#' @export
#' @rdname vec_recycle
vec_recycle_common <- function(...,
                               .size = NULL,
                               .arg = "",
                               .call = caller_env()) {
  .External2(ffi_recycle_common, .size)
}
