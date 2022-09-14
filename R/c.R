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
vec_c <- function(...,
                  .ptype = NULL,
                  .name_spec = NULL,
                  .name_repair = c("minimal", "unique", "check_unique", "universal"),
                  .call = current_env()) {
  .External2(ffi_vec_c, .ptype, .name_spec, .name_repair)
}
vec_c <- fn_inline_formals(vec_c, ".name_repair")

base_c <- function(xs) {
  # Dispatch in the base namespace which inherits from the global env
  exec("c", !!!xs, .env = ns_env("base"))
}

base_c_invoke <- function(xs) {
  # Remove all `NULL` arguments which prevent dispatch if in first
  # position and might not be handled correctly by methods
  xs <- compact(xs)

  unspecified <- map_lgl(xs, fallback_is_unspecified)
  if (!any(unspecified)) {
    return(base_c(xs))
  }

  # First concatenate without the unspecified chunks. This way the
  # `c()` method doesn't need to handle unspecified inputs correctly,
  # and we're guaranteed to dispatch on the correct class even if the
  # first input is unspecified.
  out <- base_c(xs[!unspecified])

  # Create index vector with `NA` locations for unspecified chunks
  locs <- c_locs(xs)
  locs[unspecified] <- map(locs[unspecified], rep_along, na_int)
  locs[!unspecified] <- c_locs(xs[!unspecified])
  locs <- vec_c(!!!locs, .ptype = int())

  # Expand the concatenated vector with unspecified chunks
  out[locs]
}

# FIXME: Should be unnecessary in the future. We currently attach an
# attribute to unspecified columns initialised in `df_cast()`. We
# can't use an unspecified vector because we (unnecessarily but for
# convenience) go through `vec_assign()` before falling back in
# `vec_rbind()`.
fallback_is_unspecified <- function(x) {
  is_unspecified(x) || is_true(attr(x, "vctrs:::unspecified"))
}

c_locs <- function(xs) {
  locs <- reduce(lengths(xs), .init = list(0), function(output, input) {
    n <- last(last(output))
    c(output, list(seq(n + 1, n + input)))
  })

  locs[-1]
}
