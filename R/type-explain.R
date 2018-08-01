vec_type_explain <- function(...) {
  args <- list2(...)
  n <- length(args)
  if (n == 0L) {
    stop("No types to explain", call. = FALSE)
  }

  types <- map(args, vec_type)
  accum <- vec_na(character(), n = n)

  cur <- args[[1L]]
  accum[[1L]] <- format(vec_type(cur))
  for (i in seq2(2, n)) {
    cur <- tryCatch(
      error_no_max_type = function(e) NA_character_,
      vectype_max(cur, args[[i]])
    )

    if (identical(cur, NA_character_)) {
      break
    }

    accum[[i]] <- format(vec_type(cur))
  }

  data.frame(
    id = names(args) %||% seq_along(args),
    type = map_chr(types, format),
    accum,
    row.names = NULL
  )
}
