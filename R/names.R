#' Retrieve and repair names
#'
#' @description
#'
#' Like `base::names()`, `vec_names()` retrieves the names of a
#' vector. It has the following differences:
#'
#' * It always returns __minimal names__. Whereas `names()` returns
#'   `NULL` when a vector doesn't have names, `vec_names()` returns a
#'   character vector of empty strings by default. `NA` names are also
#'   converted to `""`. You can control how names are repaired with
#'   the `repair` argument.
#'
#' * With arrays it returns the row names. With data frames it returns
#'   an empty character vector as long as the number of rows.
#'
#' `vec_as_names()` takes a character vector of names and repairs it
#' according to the `repair` argument. It is the r-lib and tidyverse
#' equivalent of [base::make.names()].
#'
#' @param x A vector.
#' @param ... These dots are for future extensions and must be empty.
#' @param repair
#'   * Minimal names are never `NULL` or `NA`. When an element doesn't
#'     have a name, its minimal name is an empty string.
#'
#'   * Unique names are unique. A suffix is appended to duplicate
#'     names to make them unique.
#'
#'   * Universal names are unique and syntactic, meaning that you can
#'     safely use the names as variables without causing a syntax
#'     error.
#'
#'   See [vec_repair_names()] for a complete overview of names repair.
#' @param quiet By default, the user is informed of any renaming
#'   caused by repairing the names. This only concerns unique and
#'   universal repairing. Set `quiet` to `TRUE` to silence the
#'   messages.
#'
#' @examples
#' # By default, `vec_names()` returns minimal names:
#' vec_names(1:3)
#'
#' # You can make them unique:
#' vec_names(1:3, repair = "unique")
#'
#' # Universal repairing fixes any non-syntactic name:
#' x <- c("_foo" = 1, "+" = 2)
#' vec_names(x, repair = "universal")
#'
#' @export
vec_names <- function(x,
                      ...,
                      repair = c("minimal", "unique", "universal"),
                      quiet = FALSE) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  switch(arg_match(repair),
    minimal = minimal_names(x),
    unique = as_unique_names(minimal_names(x), quiet = quiet),
    universal = as_universal_names(minimal_names(x), quiet = quiet)
  )
}
#' @rdname vec_names
#' @param names A character vector.
#' @export
vec_as_names <- function(names,
                         ...,
                         repair = c("minimal", "unique", "universal"),
                         quiet = FALSE) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  switch(arg_match(repair),
    minimal = as_minimal_names(names),
    unique = as_unique_names(as_minimal_names(names), quiet = quiet),
    universal = as_universal_names(as_minimal_names(names), quiet = quiet)
  )
}

#' Repair the names of a vector
#'
#' @description
#'
#' vctrs deals with a few levels of name repair:
#'
#' * `minimal` names exist. The `names` attribute is not `NULL`. The
#'   name of an unnamed element is `""` and never `NA`. For instance,
#'   [vec_names()] always returns minimal names and data frames
#'   created by the tibble package have names that are, at least,
#'   `minimal`.
#'
#' * `unique` names are `minimal`, have no duplicates, and can be used
#'   where a variable name is expected.  Empty names, and `...` or
#'   `..` followed by a sequence of digits are banned.
#'
#'   - All columns can be accessed by name via `df[["name"]]` and
#'     ``df$`name` `` and ``with(df, `name`)``.
#'
#' * `universal` names are `unique` and syntactic (see Details for
#'   more).
#'
#'   - Names work everywhere, without quoting: `df$name` and `with(df,
#'     name)` and `lm(name1 ~ name2, data = df)` and
#'     `dplyr::select(df, name)` all work.
#'
#' `universal` implies `unique`, `unique` implies `minimal`. These
#' levels are nested.
#'
#'
#' @inheritParams vec_names
#'
#' @section `minimal` names:
#'
#' `minimal` names exist. The `names` attribute is not `NULL`. The
#' name of an unnamed element is `""` and never `NA`.
#'
#' Examples:
#'
#' ```
#' Original names of a vector with length 3: NULL
#'                            minimal names: "" "" ""
#'
#'                           Original names: "x" NA
#'                            minimal names: "x" ""
#' ```
#'
#'
#' @section `unique` names:
#'
#' `unique` names are `minimal`, have no duplicates, and can be used
#'  (possibly with backticks) in contexts where a variable is
#'  expected. Empty names, and `...` or `..` followed by a sequence of
#'  digits are banned If a data frame has `unique` names, you can
#'  index it by name, and also access the columns by name.  In
#'  particular, `df[["name"]]` and `` df$`name` `` and also ``with(df,
#'  `name`)`` always work.
#'
#' There are many ways to make names `unique`. We append a suffix of the form
#' `...j` to any name that is `""` or a duplicate, where `j` is the position.
#' We also change `..#` and `...` to `...#`.
#'
#' Example:
#'
#' ```
#' Original names:     ""     "x"     "" "y"     "x"  "..2"  "..."
#'   unique names: "...1" "x...2" "...3" "y" "x...5" "...6" "...7"
#' ```
#'
#' Pre-existing suffixes of the form `...j` are always stripped, prior
#' to making names `unique`, i.e. reconstructing the suffixes. If this
#' interacts poorly with your names, you should take control of name
#' repair.
#'
#'
#' @section `universal` names:
#'
#' `universal` names are `unique` and syntactic, meaning they:
#'
#'   * Are never empty (inherited from `unique`).
#'   * Have no duplicates (inherited from `unique`).
#'   * Are not `...`. Do not have the form `..i`, where `i` is a
#'     number (inherited from `unique`).
#'   * Consist of letters, numbers, and the dot `.` or underscore `_`
#'     characters.
#'   * Start with a letter or start with the dot `.` not followed by a
#'     number.
#'   * Are not a [reserved] word, e.g., `if` or `function` or `TRUE`.
#'
#' If a vector has `universal` names, variable names can be used
#' "as is" in code. They work well with nonstandard evaluation, e.g.,
#' `df$name` works.
#'
#' vctrs has a different method of making names syntactic than
#' [base::make.names()]. In general, vctrs prepends one or more dots
#' `.` until the name is syntactic.
#'
#' Examples:
#'
#' ```
#'  Original names:     ""     "x"    NA      "x"
#' universal names: "...1" "x...2" "...3" "x...4"
#'
#'   Original names: "(y)"  "_z"  ".2fa"  "FALSE"
#'  universal names: ".y." "._z" "..2fa" ".FALSE"
#' ```
#'
#' @seealso [rlang::names2()] returns the names of an object, after
#'   making them `minimal`.
#'
#' The [Names attribute](https://principles.tidyverse.org/names-attribute.html)
#' section in the "tidyverse package development principles".
#'
#' @export
vec_repair_names <- function(x,
                             repair = c("minimal", "unique", "universal"),
                             ...,
                             quiet = FALSE) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  names <- switch(arg_match(repair),
    minimal = minimal_names(x),
    unique = as_unique_names(minimal_names(x), quiet = quiet),
    universal = as_universal_names(minimal_names(x), quiet = quiet)
  )
  set_bare_names(x, names)
}

minimal_names <- function(x) {
  names <- vec_bare_names(x)

  if (is.null(names)) {
    rep_len("", vec_size(x))
  } else {
    as_minimal_names(names)
  }
}

vec_bare_names <- function(x) {
  if (vec_dims(x) == 1) {
    names(x)
  } else if (is.data.frame(x)) {
    NULL
  } else {
    rownames(x)
  }
}
`vec_bare_names<-` <- function(x, value) {
  if (vec_dims(x) == 1) {
    names(x) <- value
  } else if (is.data.frame(x)) {
    # Do not update row names
  } else {
    rownames(x) <- value
  }
  x
}
set_bare_names <- `vec_bare_names<-`

as_minimal_names <- function(names) {
  if (!is_character(names)) {
    abort("`names` must be a character vector")
  }
  names %|% ""
}
as_unique_names <- function(names, quiet = FALSE) {
  as_unique_names_impl(names, quiet, FALSE)
}
as_universal_names <- function(names, quiet = FALSE) {
  as_unique_names_impl(names, quiet, TRUE)
}

as_unique_names_impl <- function(names, quiet, syntactic) {
  new_names <- rep_along(names, "")

  naked_names <- strip_pos(two_to_three_dots(names))
  empty <- naked_names %in% c("", "...")

  if (syntactic) {
    new_names[!empty] <- make_syntactic(naked_names[!empty])
  } else {
    new_names[!empty] <- naked_names[!empty]
  }

  needs_suffix <- empty | vec_duplicate_detect(new_names)
  new_names <- append_pos(new_names, needs_suffix = needs_suffix)

  if (!quiet) {
    describe_repair(names, new_names)
  }

  new_names
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
