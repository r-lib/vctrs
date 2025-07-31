vec_case_when <- function(
  cases,
  values,
  ...,
  default = NULL,
  unmatched = "default",
  cases_arg = "cases",
  values_arg = "values",
  default_arg = "default",
  ptype = NULL,
  size = NULL,
  call = current_env()
) {
  check_dots_empty0(...)
  .Call(
    ffi_vec_case_when,
    cases,
    values,
    default,
    unmatched,
    ptype,
    size,
    environment()
  )
}

vec_replace_when <- function(
  x,
  cases,
  values,
  ...,
  x_arg = "x",
  cases_arg = "cases",
  values_arg = "values",
  call = current_env()
) {
  check_dots_empty0(...)

  obj_check_vector(x, arg = x_arg, call = call)

  default <- x
  default_arg <- x_arg
  size <- vec_size(x)
  ptype <- vec_ptype_finalise(vec_ptype(x))

  out <- vec_case_when(
    cases = cases,
    values = values,
    default = default,
    cases_arg = cases_arg,
    values_arg = values_arg,
    default_arg = default_arg,
    ptype = ptype,
    size = size,
    call = call
  )

  # `vec_case_when()` creates a new vector and names come from any of
  # `values` or `default`, but `vec_update_when()` updates an existing
  # vector and should act like `[<-`, retaining existing names.
  out <- vec_set_names(out, vec_names(x))

  out
}

vec_if_else <- function(
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
