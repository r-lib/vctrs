coerces_to <- function(x, y, using = "strict") {
  type_max <- switch(using,
    strict = vectype_max,
    flex = function(x, y) vectype_max(x, y, strict = FALSE),
    base_c = c,
    base_unlist = function(x, y) unlist(list(x, y)),
    base_modify = function(x, y) `[<-`(x, 2, value = y)
  )

  tryCatch({
    type <- type_max(x, y)
    vec_type(type)
  }, error = function(e) {
    NA_character_
  })
}

base <- function(x, y) {
  df_x <- data.frame(x = x, stringsAsFactors = FALSE)
  df_y <- data.frame(x = y, stringsAsFactors = FALSE)

  list(
    c = c(x, y),
    unlist = unlist(list(x, y)),
    modify = tryish(`[<-`(x, 2, value = y)),
    rbind = tryish(rbind(df_x, df_y)$x)
  )
}


maxtype_mat <- function(types, using = "strict") {
  names(types) <- map_chr(types, vec_type)

  grid <- expand.grid(x = types, y = types)
  grid$max <- map2_chr(grid$x, grid$y, coerces_to, using = using)

  matrix(
    grid$max,
    nrow = length(types),
    dimnames = list(names(types), names(types))
  )
}
