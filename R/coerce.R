coerce <- function(..., .strict = TRUE) {
  args <- list2(...)
  if (length(args) == 0)
    return(list())

  type <- reduce(args, vectype_max, strict = .strict)

  # Should return ListOf<type>
  map(args, vectype_coerce, x = type)
}

vec_c <- function(..., .strict = TRUE) {
  args <- list2(...)

  type <- reduce(args, vectype_max, strict = .strict)
  if (is.null(type))
    return(NULL)

  ns <- map_int(args, length)
  out <- vec_rep(type, sum(ns))

  pos <- 1
  for (i in seq_along(ns)) {
    n <- ns[[i]]
    if (n == 0L)
      next

    out[pos:(pos + n - 1)] <- vectype_coerce(type, args[[i]])
    pos <- pos + n
  }

  out
}

vec_unlist <- function(x, .strict = TRUE) {
  vec_c(!!!x, .strict = .strict)
}
