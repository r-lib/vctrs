#' Retrieve and repair names
#'
#' @description
#'
#' `vec_as_names()` takes a character vector of names and repairs it
#' according to the `repair` argument. It is the r-lib and tidyverse
#' equivalent of [base::make.names()].
#'
#' vctrs deals with a few levels of name repair:
#'
#' * `minimal` names exist. The `names` attribute is not `NULL`. The
#'   name of an unnamed element is `""` and never `NA`. For instance,
#'   `vec_as_names()` always returns minimal names and data frames
#'   created by the tibble package have names that are, at least,
#'   `minimal`.
#'
#' * `unique` names are `minimal`, have no duplicates, and can be used
#'   where a variable name is expected. Empty names, `...`, and
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
#' @inheritParams rlang::args_error_context
#' @inheritParams rlang::args_dots_empty
#'
#' @param names A character vector.

#' @param repair Either a string or a function. If a string, it must be one of
#'   `"check_unique"`, `"minimal"`, `"unique"`, `"universal"`, `"unique_quiet"`,
#'   or `"universal_quiet"`. If a function, it is invoked with a vector of
#'   minimal names and must return minimal names, otherwise an error is thrown.
#'
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
#'   The `"check_unique"` option doesn't perform any name repair.
#'   Instead, an error is raised if the names don't suit the
#'   `"unique"` criteria.
#'
#'   The options `"unique_quiet"` and `"universal_quiet"` are here to help the
#'   user who calls this function indirectly, via another function which exposes
#'   `repair` but not `quiet`. Specifying `repair = "unique_quiet"` is like
#'   specifying `repair = "unique", quiet = TRUE`. When the `"*_quiet"` options
#'   are used, any setting of `quiet` is silently overridden.
#' @param repair_arg If specified and `repair = "check_unique"`, any errors
#'   will include a hint to set the `repair_arg`.
#' @param quiet By default, the user is informed of any renaming
#'   caused by repairing the names. This only concerns unique and
#'   universal repairing. Set `quiet` to `TRUE` to silence the
#'   messages.
#'
#'   Users can silence the name repair messages by setting the
#'   `"rlib_name_repair_verbosity"` global option to `"quiet"`.
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
#'  expected. Empty names, `...`, and `..` followed by a sequence of
#'  digits are banned. If a data frame has `unique` names, you can
#'  index it by name, and also access the columns by name. In
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
#' @examples
#' # By default, `vec_as_names()` returns minimal names:
#' vec_as_names(c(NA, NA, "foo"))
#'
#' # You can make them unique:
#' vec_as_names(c(NA, NA, "foo"), repair = "unique")
#'
#' # Universal repairing fixes any non-syntactic name:
#' vec_as_names(c("_foo", "+"), repair = "universal")
#' @export
vec_as_names <- function(
  names,
  ...,
  repair = c(
    "minimal",
    "unique",
    "universal",
    "check_unique",
    "unique_quiet",
    "universal_quiet"
  ),
  repair_arg = NULL,
  quiet = FALSE,
  call = caller_env()
) {
  check_dots_empty0(...)
  .Call(
    ffi_vec_as_names,
    names,
    repair,
    quiet,
    environment()
  )
}

# TODO! Error calls
validate_name_repair_arg <- function(repair) {
  .Call(vctrs_validate_name_repair_arg, repair)
}
validate_minimal_names <- function(names, n = NULL) {
  .Call(vctrs_validate_minimal_names, names, n)
}
validate_unique <- function(names, arg = "", n = NULL, call = caller_env()) {
  validate_minimal_names(names, n)

  empty_names <- detect_empty_names(names)
  if (has_length(empty_names)) {
    stop_names_cannot_be_empty(names, call = call)
  }

  dot_dot_name <- detect_dot_dot(names)
  if (has_length(dot_dot_name)) {
    stop_names_cannot_be_dot_dot(names, call = call)
  }

  if (anyDuplicated(names)) {
    stop_names_must_be_unique(names, arg, call = call)
  }

  invisible(names)
}
detect_empty_names <- function(names) {
  which(names == "")
}
detect_dot_dot <- function(names) {
  grep("^[.][.](?:[.]|[1-9][0-9]*)$", names)
}

#' Get or set the names of a vector
#'
#' @description
#' These functions work like [rlang::names2()], [names()] and [names<-()],
#' except that they return or modify the the rowwise names of the vector. These are:
#' * The usual `names()` for atomic vectors and lists
#' * The row names for data frames and matrices
#' * The names of the first dimension for arrays
#' Rowwise names are size consistent: the length of the names always equals
#' [vec_size()].
#'
#' `vec_names2()` returns the repaired names from a vector, even if it is unnamed.
#' See [vec_as_names()] for details on name repair.
#'
#' `vec_names()` is a bare-bones version that returns `NULL` if the vector is
#' unnamed.
#'
#' `vec_set_names()` sets the names or removes them.
#'
#' @param x A vector with names
#' @param names A character vector, or `NULL`.
#' @inheritParams vec_as_names
#'
#' @return
#'   `vec_names2()` returns the names of `x`, repaired.
#'   `vec_names()` returns the names of `x` or `NULL` if unnamed.
#'   `vec_set_names()` returns `x` with names updated.
#'
#' @name vec_names
#' @export
#' @examples
#' vec_names2(1:3)
#' vec_names2(1:3, repair = "unique")
#' vec_names2(c(a = 1, b = 2))
#'
#' # `vec_names()` consistently returns the rowwise names of data frames and arrays:
#' vec_names(data.frame(a = 1, b = 2))
#' names(data.frame(a = 1, b = 2))
#' vec_names(mtcars)
#' names(mtcars)
#' vec_names(Titanic)
#' names(Titanic)
#'
#' vec_set_names(1:3, letters[1:3])
#' vec_set_names(data.frame(a = 1:3), letters[1:3])
vec_names2 <- function(
  x,
  ...,
  repair = c(
    "minimal",
    "unique",
    "universal",
    "check_unique",
    "unique_quiet",
    "universal_quiet"
  ),
  quiet = FALSE
) {
  check_dots_empty0(...)
  repair <- validate_name_repair_arg(repair)

  if (is_function(repair)) {
    names <- minimal_names(x)
    new_names <- validate_minimal_names(repair(names), n = length(names))

    if (!quiet) {
      describe_repair(names, new_names)
    }

    return(new_names)
  }

  switch(
    repair,
    minimal = minimal_names(x),
    unique = unique_names(x, quiet = quiet),
    universal = as_universal_names(minimal_names(x), quiet = quiet),
    check_unique = validate_unique(minimal_names(x)),
    unique_quiet = unique_names(x, quiet = TRUE),
    universal_quiet = as_universal_names(minimal_names(x), quiet = TRUE)
  )
}
vec_repair_names <- function(
  x,
  repair = c(
    "minimal",
    "unique",
    "universal",
    "check_unique",
    "unique_quiet",
    "universal_quiet"
  ),
  ...,
  quiet = FALSE
) {
  if (is.data.frame(x)) {
    x
  } else {
    vec_set_names(x, vec_names2(x, ..., repair = repair, quiet = quiet))
  }
}

minimal_names <- function(x) {
  .Call(ffi_minimal_names, x)
}
unique_names <- function(x, quiet = FALSE) {
  .Call(ffi_unique_names, x, quiet)
}

#' @rdname vec_names
#' @export
vec_names <- function(x) {
  .Call(vctrs_names, x)
}

as_minimal_names <- function(names) {
  .Call(ffi_as_minimal_names, names)
}
as_unique_names <- function(names, quiet = FALSE) {
  .Call(vctrs_as_unique_names, names, quiet)
}
as_universal_names <- function(names, quiet = FALSE) {
  new_names <- names
  new_names[] <- ""

  naked_names <- strip_pos(two_to_three_dots(names))
  empty <- naked_names %in% c("", "...")

  new_names[!empty] <- make_syntactic(naked_names[!empty])

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
  names[need_append_pos] <- paste0(
    names[need_append_pos],
    "...",
    need_append_pos
  )
  names
}
strip_pos <- function(names) {
  rx <- "([.][.][.][1-9][0-9]*)+$"
  gsub(rx, "", names) %|% ""
}

# Makes each individual name syntactic but does not enforce unique-ness
make_syntactic <- function(names) {
  names[is.na(names)] <- ""
  names[names == ""] <- "."
  names[names == "..."] <- "...."
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

  start <- as.vector(match)
  length <- attr(match, "match.length")
  end <- start + length - 1L

  matchstr <- substring(text, start, end)
  matchstr[start == -1] <- NA_character_

  res <- data.frame(
    stringsAsFactors = FALSE,
    .text = text,
    .match = matchstr
  )

  if (!is.null(attr(match, "capture.start"))) {
    gstart <- attr(match, "capture.start")
    glength <- attr(match, "capture.length")
    gend <- gstart + glength - 1L

    groupstr <- substring(text, gstart, gend)
    groupstr[gstart == -1] <- NA_character_
    dim(groupstr) <- dim(gstart)

    res <- cbind(groupstr, res, stringsAsFactors = FALSE)
  }

  names(res) <- c(attr(match, "capture.names"), ".text", ".match")
  res
}


describe_repair <- function(orig_names, names) {
  names_inform_repair(orig_names, names)
}

bullets <- function(..., header = NULL) {
  problems <- c(...)
  MAX_BULLETS <- 6L
  if (length(problems) >= MAX_BULLETS) {
    n_more <- length(problems) - MAX_BULLETS + 1L
    problems[[MAX_BULLETS]] <- "..."
    length(problems) <- MAX_BULLETS
  }

  info <- paste0("* ", problems, collapse = "\n")

  if (!is.null(header)) {
    info <- paste0(header, "\n", info)
  }

  info
}

# Used in names.c
set_rownames_dispatch <- function(x, names) {
  rownames(x) <- names
  x
}

# Used in names.c
set_names_dispatch <- function(x, names) {
  names(x) <- names
  x
}

#' @rdname vec_names
#' @export
vec_set_names <- function(x, names) {
  .Call(ffi_vec_set_names, x, names)
}

#' Repair names with legacy method
#'
#' This standardises names with the legacy approach that was used in
#' tidyverse packages (such as tibble, tidyr, and readxl) before
#' [vec_as_names()] was implemented. This tool is meant to help
#' transitioning to the new name repairing standard and will be
#' deprecated and removed from the package some time in the future.
#'
#' @inheritParams vec_as_names
#' @param prefix,sep Prefix and separator for repaired names.
#'
#' @examples
#' if (rlang::is_installed("tibble")) {
#'
#' library(tibble)
#'
#' # Names repair is turned off by default in tibble:
#' try(tibble(a = 1, a = 2))
#'
#' # You can turn it on by supplying a repair method:
#' tibble(a = 1, a = 2, .name_repair = "universal")
#'
#' # If you prefer the legacy method, use `vec_as_names_legacy()`:
#' tibble(a = 1, a = 2, .name_repair = vec_as_names_legacy)
#'
#' }
#' @keywords internal
#' @export
vec_as_names_legacy <- function(names, prefix = "V", sep = "") {
  if (length(names) == 0) {
    return(character())
  }

  blank <- names == ""
  names[!blank] <- make.unique(names[!blank], sep = sep)

  new_nms <- setdiff(paste(prefix, seq_along(names), sep = sep), names)
  names[blank] <- new_nms[seq_len(sum(blank))]

  names
}


#' Name specifications
#'
#' @description
#'
#' A name specification describes how to combine an inner and outer
#' names. This sort of name combination arises when concatenating
#' vectors or flattening lists. There are two possible cases:
#'
#' * Named vector:
#'
#'   ```
#'   vec_c(outer = c(inner1 = 1, inner2 = 2))
#'   ```
#'
#' * Unnamed vector:
#'
#'   ```
#'   vec_c(outer = 1:2)
#'   ```
#'
#' In r-lib and tidyverse packages, these cases are errors by default,
#' because there's no behaviour that works well for every case.
#' Instead, you can provide a name specification that describes how to
#' combine the inner and outer names of inputs. Name specifications
#' can refer to:
#'
#' * `outer`: The external name recycled to the size of the input
#'   vector.
#'
#' * `inner`: Either the names of the input vector, or a sequence of
#'   integer from 1 to the size of the vector if it is unnamed.
#'
#' @param name_spec,.name_spec A name specification for combining
#'   inner and outer names. This is relevant for inputs passed with a
#'   name, when these inputs are themselves named, like `outer =
#'   c(inner = 1)`, or when they have length greater than 1: `outer =
#'   1:2`. By default, these cases trigger an error. You can resolve
#'   the error by providing a specification that describes how to
#'   combine the names or the indices of the inner vector with the
#'   name of the input. This specification can be:
#'
#'   * A function of two arguments. The outer name is passed as a
#'     string to the first argument, and the inner names or positions
#'     are passed as second argument.
#'
#'   * An anonymous function as a purrr-style formula.
#'
#'   * A glue specification of the form `"{outer}_{inner}"`.
#'
#'   * `"inner"`, in which case outer names are ignored, and inner
#'     names are used if they exist. Note that outer names may still
#'     be used to provide informative error messages.
#'
#'   * An [rlang::zap()] object, in which case both outer and inner
#'     names are ignored and the result is unnamed.
#'
#'   See the [name specification topic][name_spec].
#'
#' @examples
#' # By default, named inputs must be length 1:
#' vec_c(name = 1)         # ok
#' try(vec_c(name = 1:3))  # bad
#'
#' # They also can't have internal names, even if scalar:
#' try(vec_c(name = c(internal = 1)))  # bad
#'
#' # Pass a name specification to work around this. A specification
#' # can be a glue string referring to `outer` and `inner`:
#' vec_c(name = 1:3, other = 4:5, .name_spec = "{outer}")
#' vec_c(name = 1:3, other = 4:5, .name_spec = "{outer}_{inner}")
#'
#' # They can also be functions:
#' my_spec <- function(outer, inner) paste(outer, inner, sep = "_")
#' vec_c(name = 1:3, other = 4:5, .name_spec = my_spec)
#'
#' # Or purrr-style formulas for anonymous functions:
#' vec_c(name = 1:3, other = 4:5, .name_spec = ~ paste0(.x, .y))
#'
#' # Or the string `"inner"` to only use inner names
#' vec_c(name = 1:3, outer = 4:5, .name_spec = "inner")
#' vec_c(name = c(a = 1, b = 2, c = 3), outer = 4:5, .name_spec = "inner")
#' # This can be useful when you want outer names mentioned in error messages,
#' # but you don't want them interfering with the result
#' try(vec_c(x = c(a = 1), y = c(b = "2"), .name_spec = "inner"))
#'
#' # Or `rlang::zap()` to ignore both outer and inner names entirely
#' vec_c(name = c(a = 1, b = 2), outer = c(c = 3), .name_spec = rlang::zap())
#' @name name_spec
NULL

apply_name_spec <- function(name_spec, outer, inner, n = length(inner)) {
  .Call(ffi_apply_name_spec, name_spec, outer, inner, n)
}

glue_as_name_spec <- function(`_spec`) {
  function(inner, outer) {
    glue::glue(`_spec`)
  }
}

# Evaluate glue specs in a child of base for now
environment(glue_as_name_spec) <- baseenv()
