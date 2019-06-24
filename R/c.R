#' Combine many vectors into one vector
#'
#' Combine all arguments into a new vector of common type.
#'
#' @section Invariants:
#' * `vec_size(vec_c(x, y)) == vec_size(x) + vec_size(y)`
#' * `vec_type(vec_c(x, y)) == vec_type_common(x, y)`.
#'
#' @param ... Vectors to coerce.
#' @param .name_spec A name specification for combining outer and
#'   inner names. This is relevant for inputs passed with a name, when
#'   these inputs are themselves named, like `outer = c(inner = 1)`,
#'   or when they have length greater than 1: `outer = 1:2`. By
#'   default, these cases trigger an error. You can resolve the error
#'   by providing a specification that describes how to combine the
#'   names or the indices of the inner vector with the name of the
#'   input. This specification can be:
#'
#'   * A function of two arguments. The exterior name is passed as a
#'     string to the first argument, and the inner names or positions
#'     are passed as second argument.
#'
#'   * An anonymous function as a purrr-style formula.
#' @param .name_repair How to repair names, see `repair` options in [vec_as_names()].
#' @return A vector with class given by `.ptype`, and length equal to the
#'   sum of the `vec_size()` of the contents of `...`.
#'
#'   The vector will have names if the individual components have names
#'   (inner names) or if the arguments are named (outer names). If both
#'   inner and outer names are present, they are combined with a `.`.
#' @inheritParams vec_ptype
#' @seealso [vec_cbind()]/[vec_rbind()] for combining data frames by rows
#'   or columns.
#' @export
#' @examples
#' vec_c(FALSE, 1L, 1.5)
#' vec_c(FALSE, 1L, "x", .ptype = character())
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
vec_c <- function(...,
                  .ptype = NULL,
                  .name_spec = NULL,
                  .name_repair = c("minimal", "unique", "check_unique", "universal")) {
  .External2(vctrs_c, .ptype, .name_spec, .name_repair)
}
vec_c <- fn_inline_formals(vec_c, ".name_repair")
