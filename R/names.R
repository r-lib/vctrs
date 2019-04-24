
minimal_names <- function(x) {
  names <- names(x)

  if (is.null(names)) {
    rep_along(x, "")
  } else {
    as_minimal_names(names)
  }
}
as_minimal_names <- function(names) {
  if (!is_character(names)) {
    abort("`names` must be a character vector")
  }
  names %|% ""
}

as_unique_names <- function(names, ..., quiet = FALSE, transform = identity) {
  ellipsis::check_dots_empty()

  min_names <- as_minimal_names(names)

  naked_names <- strip_pos(two_to_three_dots(min_names))
  naked_needs_suffix <- (naked_names %in% c("", "..."))

  new_names <- rep_along(naked_names, "")
  new_names[!naked_needs_suffix] <- transform(naked_names[!naked_needs_suffix])

  duped_after <- vec_duplicate_detect(new_names)
  new_names <- append_pos(new_names, needs_suffix = naked_needs_suffix | duped_after)

  if (!quiet) {
    describe_repair(names, new_names)
  }

  new_names
}

as_universal_names <- function(names, ..., quiet = FALSE) {
  ellipsis::check_dots_empty()
  as_unique_names(names, quiet = quiet, transform = make_syntactic)
}

set_minimal_names <- function(x) {
  set_names(x, minimal_names(x))
}
set_unique_names <- function(x, ..., quiet = FALSE) {
  ellipsis::check_dots_empty()
  set_names(x, as_unique_names(minimal_names(x), quiet = quiet))
}
set_universal_names <- function(x, ..., quiet = FALSE) {
  ellipsis::check_dots_empty()
  set_names(x, as_universal_names(minimal_names(x), quiet = quiet))
}

two_to_three_dots <- function(names) {
  sub("(^[.][.][1-9][0-9]*$)", ".\\1", names)
}
append_pos <- function(names, needs_suffix) {
  need_append_pos <- which(needs_suffix)
  names[need_append_pos] <- paste0(names[need_append_pos], "...", need_append_pos)
  names
}
strip_pos <- function(names) {
  rx <- "([.][.][.][1-9][0-9]*)+$"
  gsub(rx, "", names) %|% ""
}

# Makes each individual name syntactic but does not enforce unique-ness
make_syntactic <- function(names) {
  names[is.na(names)]       <- ""
  names[names == ""]        <- "."
  names[names == "..."]     <- "...."
  names <- sub("^_", "._", names)

  new_names <- make.names(names)

  X_prefix <- grepl("^X", new_names) & !grepl("^X", names)
  new_names[X_prefix] <- sub("^X", "", new_names[X_prefix])

  dot_suffix <- which(new_names == paste0(names, "."))
  new_names[dot_suffix] <- sub("^(.*)[.]$", ".\\1", new_names[dot_suffix])
  # Illegal characters have been replaced with '.' via make.names()
  # however, we have:
  #   * Declined its addition of 'X' prefixes.
  #   * Turned its '.' suffixes to '.' prefixes.

  regex <- paste0(
    "^(?<leading_dots>[.]{0,2})",
    "(?<numbers>[0-9]*)",
    "(?<leftovers>[^0-9]?.*$)"
  )

  re <- re_match(new_names, pattern = regex)
  needs_dots <- which(re$numbers != "")
  needs_third_dot <- (re$leftovers[needs_dots] == "")
  re$leading_dots[needs_dots] <- ifelse(needs_third_dot, "...", "..")
  new_names <- paste0(re$leading_dots, re$numbers, re$leftovers)

  new_names
}

# From rematch2, except we don't add tbl_df or tbl classes to the return value
re_match <- function(text, pattern, perl = TRUE, ...) {
  stopifnot(
    is.character(pattern),
    length(pattern) == 1,
    !is.na(pattern)
  )
  text <- as.character(text)

  match <- regexpr(pattern, text, perl = perl, ...)

  start  <- as.vector(match)
  length <- attr(match, "match.length")
  end    <- start + length - 1L

  matchstr <- substring(text, start, end)
  matchstr[ start == -1 ] <- NA_character_

  res <- data.frame(
    stringsAsFactors = FALSE,
    .text = text,
    .match = matchstr
  )

  if (!is.null(attr(match, "capture.start"))) {

    gstart  <- attr(match, "capture.start")
    glength <- attr(match, "capture.length")
    gend    <- gstart + glength - 1L

    groupstr <- substring(text, gstart, gend)
    groupstr[ gstart == -1 ] <- NA_character_
    dim(groupstr) <- dim(gstart)

    res <- cbind(groupstr, res, stringsAsFactors = FALSE)
  }

  names(res) <- c(attr(match, "capture.names"), ".text", ".match")
  res
}


describe_repair <- function(orig_names, names) {
  stopifnot(length(orig_names) == length(names))

  new_names <- names != as_minimal_names(orig_names)
  if (any(new_names)) {
    msg <- bullets(
      "New names:",
      paste0(
        tick_if_needed(orig_names[new_names]),
        " -> ",
        tick_if_needed(names[new_names]),
        .problem = ""
      )
    )
    message(msg)
  }
}

bullets <- function(header, ..., .problem) {
  problems <- c(...)
  MAX_BULLETS <- 6L
  if (length(problems) >= MAX_BULLETS) {
    n_more <- length(problems) - MAX_BULLETS + 1L
    problems[[MAX_BULLETS]] <- "..."
    length(problems) <- MAX_BULLETS
  }

  paste0(
    header, "\n",
    paste0("* ", problems, collapse = "\n")
  )
}

tick <- function(x) {
  ifelse(is.na(x), "NA", encodeString(x, quote = "`"))
}

is_syntactic <- function(x) {
  ret <- (make_syntactic(x) == x)
  ret[is.na(x)] <- FALSE
  ret
}

tick_if_needed <- function(x) {
  needs_ticks <- !is_syntactic(x)
  x[needs_ticks] <- tick(x[needs_ticks])
  x
}
