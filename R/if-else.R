# TODO!: Export or remove.
#
# `data.table::fifelse()` is wickedly fast due to not allocating anything along
# the way besides the final output container. We can at least be as fast in the
# atomic case and fall back to `vec_case_when()` in the generic case.
vec_if_else <- function(
  condition,
  true,
  false,
  ...,
  missing = NULL,
  ptype = NULL,
  condition_arg = "condition",
  true_arg = "true",
  false_arg = "false",
  missing_arg = "missing",
  call = current_env()
) {
  check_dots_empty0(...)
  .Call(
    ffi_vec_if_else,
    condition,
    true,
    false,
    missing,
    ptype,
    environment()
  )
}

vec_if_else2 <- function(
  condition,
  true,
  false,
  ...,
  missing = NULL,
  ptype = NULL,
  call = current_env()
) {
  check_dots_empty0(...)

  check_logical(condition, call = call)
  if (has_dim(condition)) {
    abort("`condition` can't be an array.", call = call)
  }

  cases <- list(
    condition,
    !condition
  )
  values <- list(
    true = true,
    false = false
  )

  # `cases` is fully checked already, shouldn't have errors
  # related to it
  cases_arg <- ""

  # `values` elements are named, we don't want an arg level name
  # in error messages
  values_arg <- ""

  vec_case_when(
    cases = cases,
    values = values,
    default = missing,
    cases_arg = cases_arg,
    values_arg = values_arg,
    default_arg = "missing",
    ptype = ptype,
    call = call
  )
}
