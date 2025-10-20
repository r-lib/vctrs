#' Combine many vectors into one vector
#'
#' Combine all arguments into a new vector of common type.
#'
#' @section Invariants:
#' * `vec_size(vec_c(x, y)) == vec_size(x) + vec_size(y)`
#' * `vec_ptype(vec_c(x, y)) == vec_ptype_common(x, y)`.
#'
#' @section Dependencies:
#'
#' ## vctrs dependencies
#'
#' - [vec_cast_common()] with fallback
#' - [vec_proxy()]
#' - [vec_restore()]
#'
#'
#' ## base dependencies
#'
#' - [base::c()]
#'
#' If inputs inherit from a common class hierarchy, `vec_c()` falls
#' back to `base::c()` if there exists a `c()` method implemented for
#' this class hierarchy.
#'
#' @inheritParams rlang::args_error_context
#' @inheritParams vec_ptype_show
#' @inheritParams name_spec
#' @inheritParams vec_as_names
#'
#' @param ... Vectors to coerce.
#' @param .name_repair How to repair names, see `repair` options in
#'   [vec_as_names()].
#' @return A vector with class given by `.ptype`, and length equal to the
#'   sum of the `vec_size()` of the contents of `...`.
#'
#'   The vector will have names if the individual components have names
#'   (inner names) or if the arguments are named (outer names). If both
#'   inner and outer names are present, an error is thrown unless a
#'   `.name_spec` is provided.
#'
#' @seealso [vec_cbind()]/[vec_rbind()] for combining data frames by rows
#'   or columns.
#' @export
#' @examples
#' vec_c(FALSE, 1L, 1.5)
#'
#' # Date/times --------------------------
#' c(Sys.Date(), Sys.time())
#' c(Sys.time(), Sys.Date())
#'
#' vec_c(Sys.Date(), Sys.time())
#' vec_c(Sys.time(), Sys.Date())
#'
#' # Factors -----------------------------
#' c(factor("a"), factor("b"))
#' vec_c(factor("a"), factor("b"))
#'
#'
#' # By default, named inputs must be length 1:
#' vec_c(name = 1)
#' try(vec_c(name = 1:3))
#'
#' # Pass a name specification to work around this:
#' vec_c(name = 1:3, .name_spec = "{outer}_{inner}")
#'
#' # See `?name_spec` for more examples of name specifications.
vec_c <- function(
  ...,
  .ptype = NULL,
  .name_spec = NULL,
  .name_repair = c(
    "minimal",
    "unique",
    "check_unique",
    "universal",
    "unique_quiet",
    "universal_quiet"
  ),
  .error_arg = "",
  .error_call = current_env()
) {
  .External2(ffi_vec_c, list2(...), .ptype, .name_spec, .name_repair)
}
vec_c <- fn_inline_formals(vec_c, ".name_repair")
