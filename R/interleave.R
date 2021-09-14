vec_interleave <- function(...,
                           .ptype = NULL,
                           .name_spec = NULL,
                           .name_repair = c("minimal", "unique", "check_unique", "universal")) {
  args <- list2(...)

  n <- length(args)
  size <- vec_size_common(!!!args)

  indices <- vec_interleave_indices(n, size)

  vec_unchop(
    x = args,
    indices = indices,
    ptype = .ptype,
    name_spec = .name_spec,
    name_repair = .name_repair
  )
}

vec_interleave_indices <- function(n, size) {
  .Call(vctrs_interleave_indices, n, size)
}
