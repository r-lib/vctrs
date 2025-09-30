local_name_repair_quiet()

# vec_names() ---------------------------------------------------------

test_that("vec_names() retrieves names", {
  expect_null(vec_names(letters))
  expect_identical(vec_names(set_names(letters)), letters)

  expect_identical(vec_names(mtcars), row.names(mtcars))
  expect_null(vec_names(unrownames(mtcars)))

  expect_identical(vec_names(Titanic), dimnames(Titanic)[[1]])

  x <- matrix(1L, dimnames = list("row", "col"))
  expect_identical(vec_names(x), dimnames(x)[[1]])
})

test_that("vec_names() dispatches", {
  local_methods(
    names.vctrs_foobar = function(x) "dispatched!"
  )
  expect_identical(vec_names(foobar()), "dispatched!")
})


# vec_names2() -------------------------------------------------------------

test_that("vec_names2() repairs names", {
  expect_identical(vec_names2(1:2), c("", ""))
  expect_identical(vec_names2(1:2, repair = "unique"), c("...1", "...2"))
  expect_identical(
    vec_names2(set_names(1:2, c("_foo", "_bar")), repair = "universal"),
    c("._foo", "._bar")
  )
})

test_that("vec_names2() treats data frames and arrays as vectors", {
  expect_identical(vec_names2(mtcars), row.names(mtcars))
  expect_identical(vec_names2(as.matrix(mtcars)), row.names(mtcars))

  df <- unrownames(mtcars)
  exp <- rep_len("", nrow(mtcars))
  expect_identical(vec_names2(df), exp)
  expect_identical(vec_names2(as.matrix(df)), exp)
})

test_that("vec_names2() accepts and checks repair function", {
  expect_identical(
    vec_names2(1:2, repair = function(nms) rep_along(nms, "foo")),
    c("foo", "foo")
  )
  expect_error(
    vec_names2(1:2, repair = function(nms) "foo"),
    "length 1 instead of length 2"
  )
})

test_that("vec_names2() repairs names before invoking repair function", {
  x <- set_names(1:2, c(NA, NA))
  expect_identical(vec_names2(x, repair = identity), c("", ""))
})

test_that("vec_names2() result is correct for *_quiet repair", {
  expect_identical(
    vec_names2(1:2, repair = "unique"),
    vec_names2(1:2, repair = "unique_quiet")
  )
  expect_identical(
    vec_names2(1:2, repair = "universal"),
    vec_names2(1:2, repair = "universal_quiet")
  )
})

# vec_as_names() -----------------------------------------------------------

test_that("vec_as_names() requires character vector", {
  expect_error(vec_as_names(NULL), "`names` must be a character vector")
})

test_that("vec_as_names() validates `repair`", {
  expect_snapshot({
    (expect_error(my_vec_as_names("x", my_repair = "foo"), "can't be \"foo\""))
    (expect_error(my_vec_as_names(1, my_repair = 1), "string or a function"))
  })
})

test_that("vec_as_names() repairs names", {
  expect_identical(vec_as_names(chr(NA, NA)), c("", ""))
  expect_identical(
    vec_as_names(chr(NA, NA), repair = "unique"),
    c("...1", "...2")
  )
  expect_identical(
    vec_as_names(chr("_foo", "_bar"), repair = "universal"),
    c("._foo", "._bar")
  )
  expect_identical(
    vec_as_names(chr("a", "b"), repair = "check_unique"),
    c("a", "b")
  )
})

test_that("vec_as_names() checks unique names", {
  expect_snapshot({
    (expect_error(my_vec_as_names(chr(NA), my_repair = "check_unique")))
    (expect_error(my_vec_as_names(chr(""), my_repair = "check_unique")))
    (expect_error(my_vec_as_names(chr("a", "a"), my_repair = "check_unique")))
    (expect_error(my_vec_as_names(chr("..1"), my_repair = "check_unique")))
    (expect_error(my_vec_as_names(chr("..."), my_repair = "check_unique")))
  })
})

test_that("vec_as_names() result is correct for *_quiet repair", {
  expect_identical(
    vec_as_names(chr("_foo", "_bar"), repair = "unique"),
    vec_as_names(chr("_foo", "_bar"), repair = "unique_quiet")
  )
  expect_identical(
    vec_as_names(chr("_foo", "_bar"), repair = "universal"),
    vec_as_names(chr("_foo", "_bar"), repair = "universal_quiet")
  )
})

test_that("vec_as_names() keeps the names of a named vector", {
  x_unnamed <- c(NA, "", "..1", "...2")
  x_names <- letters[1:4]
  x <- set_names(x_unnamed, x_names)

  expect_identical(
    set_names(vec_as_names(x_unnamed, repair = "minimal"), x_names),
    vec_as_names(x, repair = "minimal")
  )
  expect_identical(
    set_names(vec_as_names(x_unnamed, repair = "unique"), x_names),
    vec_as_names(x, repair = "unique")
  )
  expect_identical(
    set_names(vec_as_names(x_unnamed, repair = "universal"), x_names),
    vec_as_names(x, repair = "universal")
  )
})

test_that("vec_as_names() accepts and checks repair function", {
  f <- local({
    local_obj <- "foo"
    ~ rep_along(.x, local_obj)
  })
  expect_identical(vec_as_names(c("", ""), repair = f), c("foo", "foo"))
  expect_snapshot(
    error = TRUE,
    my_vec_as_names(c("", ""), my_repair = function(nms) "foo")
  )
})

test_that("vec_as_names() repairs names before invoking repair function", {
  expect_identical(vec_as_names(chr(NA, NA), repair = identity), c("", ""))
})

test_that("vec_as_names() is noisy by default", {
  local_name_repair_verbose()

  expect_snapshot({
    # Noisy name repair
    vec_as_names(c("x", "x"), repair = "unique")

    # Quiet name repair
    vec_as_names(c("x", "x"), repair = "unique", quiet = TRUE)

    # Hint at repair argument, if known
    (expect_error(
      my_vec_as_names(c("x", "x"), my_repair = "check_unique")
    ))

    # request quiet via name repair string, don't specify `quiet`
    vec_as_names(c("1", "1"), repair = "unique_quiet")
    vec_as_names(c("1", "1"), repair = "universal_quiet")

    # request quiet via name repair string, specify `quiet` = TRUE
    vec_as_names(c("1", "1"), repair = "unique_quiet", quiet = TRUE)
    vec_as_names(c("1", "1"), repair = "universal_quiet", quiet = TRUE)

    # request quiet via name repair string, specify `quiet` = FALSE
    vec_as_names(c("1", "1"), repair = "unique_quiet", quiet = FALSE)
    vec_as_names(c("1", "1"), repair = "universal_quiet", quiet = FALSE)
  })
})

test_that("validate_minimal_names() checks names", {
  expect_snapshot({
    (expect_error(validate_minimal_names(1), "must return a character vector"))
    (expect_error(validate_minimal_names(NULL), "can't return `NULL`"))
    (expect_error(validate_minimal_names(chr(NA)), "can't return `NA` values"))
  })
})

test_that("validate_unique() checks unique names", {
  expect_snapshot({
    (expect_error(validate_unique(chr(NA)), "`NA`"))
    (expect_error(
      validate_unique(chr("")),
      class = "vctrs_error_names_cannot_be_empty"
    ))
    (expect_error(
      validate_unique(chr("a", "a")),
      class = "vctrs_error_names_must_be_unique"
    ))
    (expect_error(
      validate_unique(chr("..1")),
      class = "vctrs_error_names_cannot_be_dot_dot"
    ))
    (expect_error(
      validate_unique(chr("...")),
      class = "vctrs_error_names_cannot_be_dot_dot"
    ))
  })
})

test_that("vec_as_names_validate() validates repair arguments", {
  expect_identical(
    validate_name_repair_arg(c("unique", "check_unique")),
    "unique"
  )
  expect_identical(
    validate_name_repair_arg(~ toupper(.))(letters),
    LETTERS
  )
})

test_that("vec_as_names() is quiet when function is supplied (#1018)", {
  expect_silent(
    vctrs::vec_as_names(
      c("a", "b"),
      repair = function(x) paste0(x, "a"),
      quiet = FALSE
    )
  )
})

test_that("vec_as_names() evaluates repair_arg lazily", {
  expect_silent(vec_as_names(letters, repair_arg = print("oof")))
})


# vec_repair_names() -------------------------------------------------------

test_that("vec_repair_names() repairs names", {
  expect_identical(vec_repair_names(1:2), set_names(1:2, c("", "")))
  expect_identical(
    vec_repair_names(1:2, "unique"),
    set_names(1:2, c("...1", "...2"))
  )
  expect_identical(
    vec_repair_names(set_names(1:2, c("_foo", "_bar")), "universal"),
    set_names(1:2, c("._foo", "._bar"))
  )
})

test_that("vec_repair_names() handles data frames and arrays", {
  df <- data.frame(x = 1:2)
  expect_identical(vec_repair_names(df), df)
  expect_identical(row.names(vec_repair_names(as.matrix(df))), c("", ""))
  expect_identical(
    row.names(vec_repair_names(as.matrix(df), "unique")),
    c("...1", "...2")
  )
})

# vec_set_names() -----------------------------------------------------------

test_that("vec_set_names() sets atomic names", {
  x <- 1:2
  names <- c("x1", "x2")
  exp <- set_names(x, names)
  expect_equal(vec_set_names(x, names), exp)
})

test_that("vec_set_names() sets matrix/array names", {
  x <- matrix(1:2)
  names <- c("x1", "x2")
  exp <- x
  rownames(exp) <- names
  expect_equal(vec_set_names(x, names), exp)

  y <- array(1:4, dim = c(2, 1, 2))
  exp <- y
  rownames(exp) <- names
  expect_equal(vec_set_names(y, names), exp)
})

test_that("vec_set_names() doesn't alter names", {
  x <- matrix(1, dimnames = list(rows = "a", cols = "x"))
  vec_set_names(x, "y")
  expect_equal(vec_names2(x), "a")
  expect_equal(colnames(x), "x")
  vec_set_names(x, NULL)
  expect_equal(vec_names2(x), "a")
  expect_equal(colnames(x), "x")

  y <- array(
    1:4,
    dim = c(1, 2, 2),
    dimnames = list(rows = "a", one = 1:2, two = 1:2)
  )
  vec_set_names(y, "y")
  expect_equal(vec_names2(y), "a")
  vec_set_names(y, NULL)
  expect_equal(vec_names2(y), "a")
})

test_that("vec_set_names() sets row names on data frames", {
  expect_identical(
    vec_set_names(data_frame(x = 1), "foo"),
    new_data_frame(list(x = 1), row.names = "foo")
  )

  expect_identical(
    vec_set_names(data_frame(x = 1:2), c("foo", "foo")),
    new_data_frame(list(x = 1:2), row.names = c("foo...1", "foo...2"))
  )
})

test_that("vec_set_names() correctly sets names on POSIXlt objects", {
  x <- as.POSIXlt(new_datetime(0))
  exp <- set_names(x, "a")
  expect_equal(vec_set_names(x, "a"), exp)
})

test_that("vec_set_names() falls back to `names<-` with proxied objects", {
  x <- structure(1, class = "foobar")
  exp <- set_names(x, "a")

  expect_equal(vec_set_names(x, "a"), exp)

  local_methods(`names<-.foobar` = function(x, value) "fallback!")
  expect_equal(vec_set_names(x, "a"), "fallback!")
})

test_that("vec_set_names() falls back to `rownames<-` with shaped proxied objects", {
  x <- structure(1:2, dim = c(2L, 1L), class = "foobar")
  names <- c("r1", "r2")
  exp <- x
  rownames(exp) <- names

  expect_equal(vec_set_names(x, names), exp)

  # `rownames<-` is not generic, but eventually calls `dimnames<-` which is
  local_methods(`dimnames<-.foobar` = function(x, value) "fallback!")
  expect_equal(vec_set_names(x, names), "fallback!")
})

test_that("vec_set_names() can set NULL names", {
  x <- 1:2
  expect_equal(vec_set_names(x, NULL), x)

  x_named <- set_names(x)
  expect_equal(vec_set_names(x_named, NULL), x)

  x_mat <- as.matrix(x)
  expect_equal(vec_set_names(x_mat, NULL), x_mat)

  x_mat_named <- x_mat
  rownames(x_mat_named) <- c("1", "2")
  exp <- matrix(x_mat, dimnames = list(NULL, NULL))
  expect_equal(vec_set_names(x_mat_named, NULL), exp)
})

test_that("vec_set_names() errors with bad `names`", {
  expect_snapshot({
    (expect_error(vec_set_names(1, 1), "character vector, not a double"))
    (expect_error(vec_set_names(1, c("x", "y")), "The size of `names`, 2"))
  })
})

test_that("vec_names() and vec_set_names() work with 1-dimensional arrays", {
  x <- array(1:2, dimnames = list(c("a", "b")))
  expect_identical(vec_names(x), c("a", "b"))
  expect_identical(vec_names(vec_set_names(x, c("A", "B"))), c("A", "B"))
})

test_that("vec_set_names() is consistent with `names<-` regarding `NULL` inputs", {
  # See also https://github.com/tidyverse/purrr/pull/1224

  # Can "clear" names on `NULL`
  expect_identical(`names<-`(NULL, NULL), NULL)
  expect_identical(vec_set_names(NULL, NULL), NULL)

  # But trying to make a "named `NULL`" is an error
  # (Don't capture the message, these are base R errors)
  expect_error(`names<-`(NULL, character()))
  expect_error(vec_set_names(NULL, character()))

  # This is more obviously an error, because the size of the names doesn't match
  expect_error(`names<-`(NULL, "x"))
  expect_error(vec_set_names(NULL, "x"))
})

# minimal names -------------------------------------------------------------

test_that("minimal names are made from `n` when `name = NULL`", {
  expect_identical(minimal_names(1:2), c("", ""))
})

test_that("as_minimal_names() checks input", {
  expect_error(as_minimal_names(1:3), "must be a character vector")
})

test_that("minimal names have '' instead of NAs", {
  expect_identical(as_minimal_names(c("", NA, "", NA)), c("", "", "", ""))
})

test_that("repairing minimal names copes with NULL input names", {
  x <- 1:3
  x_named <- vec_repair_names(x)
  expect_equal(names(x_named), rep("", 3))
})

test_that("as_minimal_names() is idempotent", {
  x <- c("", "", NA)
  expect_identical(as_minimal_names(x), as_minimal_names(as_minimal_names(x)))
})

test_that("minimal_names() treats data frames and arrays as vectors", {
  expect_identical(minimal_names(mtcars), row.names(mtcars))
  expect_identical(minimal_names(as.matrix(mtcars)), row.names(mtcars))

  df <- unrownames(mtcars)
  exp <- rep_len("", nrow(mtcars))
  expect_identical(minimal_names(df), exp)
  expect_identical(minimal_names(as.matrix(df)), exp)
})

test_that("as_minimal_names() copies on write", {
  nms <- chr(NA, NA)
  as_minimal_names(nms)
  expect_identical(nms, chr(NA, NA))

  nms <- c("a", "b")
  out <- as_minimal_names(nms)
  expect_true(is_reference(nms, out))
})


# unique names -------------------------------------------------------------

test_that("unique_names() handles unnamed vectors", {
  expect_identical(unique_names(1:3), c("...1", "...2", "...3"))
})

test_that("as_unique_names() is a no-op when no repairs are needed", {
  x <- c("x", "y")
  out <- as_unique_names(x)
  expect_true(is_reference(out, x))
  expect_identical(out, c("x", "y"))
})

test_that("as_unique_names() eliminates emptiness and duplication", {
  x <- c("", "x", "y", "x")
  expect_identical(as_unique_names(x), c("...1", "x...2", "y", "x...4"))
})

test_that("as_unique_names(): solo empty or NA gets suffix", {
  expect_identical(as_unique_names(""), "...1")
  expect_identical(as_unique_names(NA_character_), "...1")
})

test_that("as_unique_names() treats ellipsis like empty string", {
  expect_identical(as_unique_names("..."), as_unique_names(""))
})

test_that("two_three_dots() does its job and no more", {
  x <- c(".", ".1", "...1", "..1a")
  expect_identical(two_to_three_dots(x), x)

  expect_identical(two_to_three_dots(c("..1", "..22")), c("...1", "...22"))
})

test_that("two dots then number treated like three dots then number", {
  expect_identical(as_unique_names("..2"), as_unique_names("...5"))
})

test_that("as_unique_names() strips positional suffixes, re-applies as needed", {
  x <- c("...20", "a...1", "b", "", "a...2...34")
  expect_identical(as_unique_names(x), c("...1", "a...2", "b", "...4", "a...5"))

  expect_identical(as_unique_names("a...1"), "a")
  expect_identical(as_unique_names(c("a...2", "a")), c("a...1", "a...2"))
  expect_identical(
    as_unique_names(c("a...3", "a", "a")),
    c("a...1", "a...2", "a...3")
  )
  expect_identical(
    as_unique_names(c("a...2", "a", "a")),
    c("a...1", "a...2", "a...3")
  )
  expect_identical(
    as_unique_names(c("a...2", "a...2", "a...2")),
    c("a...1", "a...2", "a...3")
  )
})

test_that("as_unique_names() is idempotent", {
  x <- c("...20", "a...1", "b", "", "a...2")
  expect_identical(as_unique_names(!!x), as_unique_names(as_unique_names(!!x)))
})

test_that("unique-ification has an 'algebraic'-y property", {
  ## inspired by, but different from, this guarantee about base::make.unique()
  ## make.unique(c(A, B)) == make.unique(c(make.unique(A), B))
  ## If A is already unique, then make.unique(c(A, B)) preserves A.

  ## I haven't formulated what we guarantee very well yet, but it's probably
  ## implicit in this test (?)

  x <- c("...20", "a...1", "b", "", "a...2", "d")
  y <- c("", "a...3", "b", "...3", "e")

  ## fix names on each, catenate, fix the whole
  z1 <- as_unique_names(
    c(
      as_unique_names(x),
      as_unique_names(y)
    )
  )

  ## fix names on x, catenate, fix the whole
  z2 <- as_unique_names(
    c(
      as_unique_names(x),
      y
    )
  )

  ## fix names on y, catenate, fix the whole
  z3 <- as_unique_names(
    c(
      x,
      as_unique_names(y)
    )
  )

  ## catenate, fix the whole
  z4 <- as_unique_names(
    c(
      x,
      y
    )
  )

  expect_identical(z1, z2)
  expect_identical(z1, z3)
  expect_identical(z1, z4)
})

test_that("unique_names() and as_unique_names() are verbose or silent", {
  local_name_repair_verbose()

  expect_snapshot(unique_names(1:2))
  expect_snapshot(as_unique_names(c("", "")))

  expect_message(regexp = NA, unique_names(1:2, quiet = TRUE))
  expect_message(regexp = NA, as_unique_names(c("", ""), quiet = TRUE))
})

test_that("names with only duplicates are repaired", {
  expect_identical(unique_names(list(x = NA, x = NA)), c("x...1", "x...2"))
})


# Universal names ----------------------------------------------------------

test_that("zero-length input", {
  expect_equal(as_universal_names(character()), character())
})

test_that("universal names are not changed", {
  expect_equal(as_universal_names(letters), letters)
})

test_that("as_universal_names() is idempotent", {
  x <- c(NA, "", "x", "x", "a1:", "_x_y}")
  expect_identical(
    as_universal_names(x),
    as_universal_names(as_universal_names(x))
  )
})

test_that("dupes get a suffix", {
  expect_equal(
    as_universal_names(c("a", "b", "a", "c", "b")),
    c("a...1", "b...2", "a...3", "c", "b...5")
  )
})

test_that("as_universal_names(): solo empty or NA gets suffix", {
  expect_identical(as_universal_names(""), "...1")
  expect_identical(as_universal_names(NA_character_), "...1")
})

test_that("as_universal_names() treats ellipsis like empty string", {
  expect_identical(as_universal_names("..."), as_universal_names(""))
})

test_that("solo dot is unchanged", {
  expect_equal(as_universal_names("."), ".")
})

test_that("dot, dot gets suffix", {
  expect_equal(as_universal_names(c(".", ".")), c("....1", "....2"))
})

test_that("dot-dot, dot-dot gets suffix", {
  expect_equal(as_universal_names(c("..", "..")), c(".....1", ".....2"))
})

test_that("empty, dot becomes suffix, dot", {
  expect_equal(as_universal_names(c("", ".")), c("...1", "."))
})

test_that("empty, empty, dot becomes suffix, suffix, dot", {
  expect_equal(as_universal_names(c("", "", ".")), c("...1", "...2", "."))
})

test_that("dot, dot, empty becomes suffix, suffix, suffix", {
  expect_equal(as_universal_names(c(".", ".", "")), c("....1", "....2", "...3"))
})

test_that("dot, empty, dot becomes suffix, suffix, suffix", {
  expect_equal(as_universal_names(c(".", "", ".")), c("....1", "...2", "....3"))
})

test_that("empty, dot, empty becomes suffix, dot, suffix", {
  expect_equal(as_universal_names(c("", ".", "")), c("...1", ".", "...3"))
})

test_that("'...j' gets stripped then names are modified", {
  expect_equal(as_universal_names(c("...6", "...1...2")), c("...1", "...2"))
  expect_equal(as_universal_names("if...2"), ".if")
})

test_that("complicated inputs", {
  expect_equal(
    as_universal_names(c(
      "",
      ".",
      NA,
      "if...4",
      "if",
      "if...8",
      "for",
      "if){]1"
    )),
    c("...1", ".", "...3", ".if...4", ".if...5", ".if...6", ".for", "if...1")
  )
})

test_that("message", {
  local_name_repair_verbose()
  expect_snapshot(as_universal_names(c("a b", "b c")))
})

test_that("quiet", {
  expect_message(
    as_universal_names("", quiet = TRUE),
    NA
  )
})

test_that("unique then universal is universal, with shuffling", {
  x <- c("", ".2", "..3", "...4", "....5", ".....6", "......7", "...")
  expect_identical(
    as_universal_names(as_unique_names(x)),
    as_universal_names(x)
  )

  x2 <- x[c(7L, 4L, 3L, 6L, 5L, 1L, 2L, 8L)]
  expect_identical(
    as_universal_names(as_unique_names(x2)),
    as_universal_names(x2)
  )

  x3 <- x[c(3L, 2L, 4L, 6L, 8L, 1L, 5L, 7L)]
  expect_identical(
    as_universal_names(as_unique_names(x3)),
    as_universal_names(x3)
  )
})

test_that("zero-length inputs given character names", {
  out <- vec_repair_names(character(), "universal")
  expect_equal(names(out), character())
})

test_that("unnamed input gives uniquely named output", {
  out <- vec_repair_names(1:3, "universal")
  expect_equal(names(out), c("...1", "...2", "...3"))
})

test_that("messages by default", {
  local_name_repair_verbose()
  expect_snapshot(vec_repair_names(set_names(1, "a:b"), "universal"))
  expect_snapshot(vec_repair_names(set_names(1, "a:b"), ~ make.names(.)))
})

test_that("quiet = TRUE", {
  expect_message(
    vec_repair_names(set_names(1, ""), "universal", quiet = TRUE),
    NA
  )
})

test_that("non-universal names", {
  out <- vec_repair_names(set_names(1, "a b"), "universal")
  expect_equal(names(out), "a.b")

  expect_equal(as_universal_names("a b"), "a.b")
})


# make_syntactic() ---------------------------------------------------------

test_that("make_syntactic(): empty or NA", {
  expect_syntactic(
    c("", NA_character_),
    c(".", ".")
  )
})

test_that("make_syntactic(): reserved words", {
  expect_syntactic(
    c("if", "TRUE", "Inf", "NA_real_", "normal"),
    c(".if", ".TRUE", ".Inf", ".NA_real_", "normal")
  )
})

test_that("make_syntactic(): underscore", {
  expect_syntactic(
    c("_", "_1", "_a}"),
    c("._", "._1", "._a.")
  )
})

test_that("make_syntactic(): dots", {
  expect_syntactic(
    c(".", "..", "...", "...."),
    c(".", "..", "....", "....")
  )
})

test_that("make_syntactic(): number", {
  expect_syntactic(
    c("0", "1", "22", "333"),
    c("...0", "...1", "...22", "...333")
  )
})

test_that("make_syntactic(): number then character", {
  expect_syntactic(
    c("0a", "1b", "22c", "333d"),
    c("..0a", "..1b", "..22c", "..333d")
  )
})

test_that("make_syntactic(): number then non-character", {
  expect_syntactic(
    c("0)", "1&", "22*", "333@"),
    c("..0.", "..1.", "..22.", "..333.")
  )
})

test_that("make_syntactic(): dot then number", {
  expect_syntactic(
    c(".0", ".1", ".22", ".333"),
    c("...0", "...1", "...22", "...333")
  )
})

test_that("make_syntactic(): dot then number then character", {
  expect_syntactic(
    c(".0a", ".1b", ".22c", ".333d"),
    c("..0a", "..1b", "..22c", "..333d")
  )
})

test_that("make_syntactic(): dot then number then non-character", {
  expect_syntactic(
    c(".0)", ".1&", ".22*", ".333@"),
    c("..0.", "..1.", "..22.", "..333.")
  )
})

test_that("make_syntactic(): dot dot then number", {
  expect_syntactic(
    c("..0", "..1", "..22", "..333"),
    c("...0", "...1", "...22", "...333")
  )
})

test_that("make_syntactic(): dot dot dot then number", {
  expect_syntactic(
    c("...0", "...1", "...22", "...333"),
    c("...0", "...1", "...22", "...333")
  )
})

test_that("make_syntactic(): dot dot dot dot then number", {
  expect_syntactic(
    c("....0", "....1", "....22", "....333"),
    c("....0", "....1", "....22", "....333")
  )
})

test_that("make_syntactic(): dot dot dot dot dot then number", {
  expect_syntactic(
    c(".....0", ".....1", ".....22", ".....333"),
    c(".....0", ".....1", ".....22", ".....333")
  )
})

test_that("make_syntactic(): dot dot then number then character", {
  expect_syntactic(
    c("..0a", "..1b", "..22c", "..333d"),
    c("..0a", "..1b", "..22c", "..333d")
  )
})

test_that("make_syntactic(): dot dot then number then non-character", {
  expect_syntactic(
    c("..0)", "..1&", "..22*", "..333@"),
    c("..0.", "..1.", "..22.", "..333.")
  )
})

# Duplication --------------------------------------------------------------

test_that("Minimal name repair duplicates if needed", {
  x1 <- NA_character_

  x3 <- c(x1, x1)

  # Called to check absence of side effect
  vec_as_names(x3, repair = "minimal")

  expect_identical(x3, c(NA_character_, NA_character_))
})

test_that("Unique name repair duplicates if needed", {
  x1 <- "fa\u00e7ile"

  x3 <- c(x1, x1)

  # Called to check absence of side effect
  vec_as_names(x3, repair = "unique")

  expect_identical(x3, c("fa\u00e7ile", "fa\u00e7ile"))
})

# Encoding -------------------------------------------------------------

test_that("Name repair works with non-UTF-8 names", {
  x1 <- "fa\u00e7ile"
  skip_if_not(Encoding(x1) == "UTF-8")

  x2 <- iconv(x1, from = "UTF-8", to = "latin1")
  skip_if_not(Encoding(x2) == "latin1")

  x3 <- c(x2, x2)
  expect_equal(vec_as_names(x3, repair = "unique"), paste0(x3, "...", 1:2))
})


# Conditions -----------------------------------------------------------

test_that("names cannot be empty", {
  expect_error_cnd(
    stop_names_cannot_be_empty(c("", "")),
    class = c(
      "vctrs_error_names_cannot_be_empty",
      "vctrs_error_names",
      "vctrs_error"
    ),
    message = "Names can't be empty.",
    names = c("", "")
  )
})

test_that("names cannot be dot dot", {
  expect_error_cnd(
    stop_names_cannot_be_dot_dot(c("..1", "..2")),
    class = c(
      "vctrs_error_names_cannot_be_dot_dot",
      "vctrs_error_names",
      "vctrs_error"
    ),
    message = "Names can't be of the form `...` or `..j`.",
    names = c("..1", "..2")
  )
})

test_that("names must be unique", {
  expect_error_cnd(
    stop_names_must_be_unique(c("x", "y", "y", "x")),
    class = c(
      "vctrs_error_names_must_be_unique",
      "vctrs_error_names",
      "vctrs_error"
    ),
    message = "Names must be unique.",
    names = c("x", "y", "y", "x")
  )
})


# Legacy repair --------------------------------------------------------

test_that("vec_as_names_legacy() works", {
  expect_identical(vec_as_names_legacy(chr()), chr())
  expect_identical(
    vec_as_names_legacy(c("a", "a", "", "")),
    c("a", "a1", "V1", "V2")
  )
  expect_identical(
    vec_as_names_legacy(c("a", "a", "", ""), sep = "_"),
    c("a", "a_1", "V_1", "V_2")
  )
  expect_identical(
    vec_as_names_legacy(c("a", "a", "", ""), prefix = "foo"),
    c("a", "a1", "foo1", "foo2")
  )
  expect_identical(
    vec_as_names_legacy(c("a", "a", "", ""), prefix = "foo", sep = "_"),
    c("a", "a_1", "foo_1", "foo_2")
  )

  # From tibble
  expect_identical(vec_as_names_legacy(c("x", "x")), c("x", "x1"))
  expect_identical(vec_as_names_legacy(c("", "")), c("V1", "V2"))
  expect_identical(vec_as_names_legacy(c("", "V1")), c("V2", "V1"))
  expect_identical(vec_as_names_legacy(c("", "V", "V")), c("V2", "V", "V1"))
})


# Name specification ---------------------------------------------------

test_that("NULL name specs works with scalars", {
  expect_identical(apply_name_spec(NULL, "foo", NULL, 1L), "foo")
  expect_named(vec_c(foo = 1), "foo")

  expect_identical(apply_name_spec(NULL, "foo", chr(), 0L), chr())
  expect_equal(vec_c(foo = dbl()), set_names(dbl(), ""))
  expect_named(vec_c(foo = set_names(dbl())), chr())
  expect_named(vec_c(foo = set_names(dbl()), bar = set_names(dbl())), chr())

  expect_error(
    apply_name_spec(NULL, "foo", c("a", "b")),
    "vector of length > 1"
  )
  expect_error(apply_name_spec(NULL, "foo", NULL, 2L), "vector of length > 1")

  expect_snapshot({
    (expect_error(vec_c(foo = c(a = 1, b = 2)), "vector of length > 1"))
    (expect_error(vec_c(foo = 1:2), "vector of length > 1"))
    (expect_error(vec_c(x = c(xx = 1)), "named vector"))
  })
})

test_that("function name spec is applied", {
  spec <- function(outer, inner) {
    sep <- if (is_character(inner)) "_" else ":"
    paste0(outer, sep, inner)
  }

  expect_identical(apply_name_spec(spec, "foo", NULL, 1L), "foo")
  expect_named(vec_c(foo = 1, .name_spec = spec), "foo")

  expect_identical(
    apply_name_spec(spec, "foo", c("a", "b")),
    c("foo_a", "foo_b")
  )
  expect_named(
    vec_c(foo = c(a = 1, b = 2), .name_spec = spec),
    c("foo_a", "foo_b")
  )

  expect_identical(apply_name_spec(spec, "foo", NULL, 2L), c("foo:1", "foo:2"))
  expect_named(vec_c(foo = 1:2, .name_spec = spec), c("foo:1", "foo:2"))
})

test_that("can pass lambda formula as name spec", {
  expect_named(
    vec_c(foo = c(a = 1, b = 2), .name_spec = ~ paste(.x, .y, sep = "_")),
    c("foo_a", "foo_b")
  )
  expect_error(
    vec_c(foo = c(a = 1, b = 2), .name_spec = env()),
    "Can't convert `.name_spec`",
    fixed = TRUE
  )
})

test_that("can pass glue string as name spec", {
  expect_named(
    vec_c(foo = c(a = 1, b = 2), .name_spec = "{outer}_{inner}"),
    c("foo_a", "foo_b")
  )
  expect_named(
    vec_c(foo = 1:2, .name_spec = "{outer}_{inner}"),
    c("foo_1", "foo_2")
  )
  expect_error(
    vec_c(foo = c(a = 1, b = 2), .name_spec = c("a", "b")),
    "single string"
  )
})

test_that("can pass 'inner' string as name spec", {
  expect_named(vec_c(foo = c(a = 1, b = 2), .name_spec = "inner"), c("a", "b"))
  expect_named(vec_c(foo = 1:2, .name_spec = "inner"), NULL)
  expect_named(
    list_unchop(
      list(outer = c(a = 1)),
      indices = list(1:2),
      name_spec = "inner"
    ),
    c("a", "a")
  )
})

test_that("`outer` is recycled after name spec is invoked in functions that use `apply_name_spec()`", {
  expect_identical(
    vec_c(outer = 1:2, .name_spec = "{outer}"),
    c(outer = 1L, outer = 2L)
  )
  expect_identical(
    vec_rbind(
      outer = data_frame(x = 1:2),
      .names_to = NULL,
      .name_spec = "{outer}"
    ),
    new_data_frame(list(x = 1:2), row.names = c("outer...1", "outer...2"))
  )
  expect_identical(
    list_unchop(
      list(outer = c(a = 1)),
      indices = list(1:2),
      name_spec = "{outer}"
    ),
    c(outer = 1, outer = 1)
  )
  expect_identical(
    list_unchop(
      list(outer = c(a = 1)),
      indices = list(1:2),
      name_spec = "{outer}_{inner}"
    ),
    c(outer_a = 1, outer_a = 1)
  )

  # Note that `apply_name_spec()` itself doesn't recycle, it expects the caller
  # to do so
  expect_identical(
    unstructure(apply_name_spec("{outer}", "outer", NULL, n = 2L)),
    "outer"
  )
})

test_that("apply_name_spec() doesn't recycle inputs (#1099)", {
  # We used to recycle the output for the caller, but now we check that the
  # output is recyclable and just return it even if it is size 1, expecting the
  # caller to be able to handle it, possibly efficiently with `chr_assign()`.

  out <- unstructure(apply_name_spec("foo", "outer", c("a", "b", "c")))
  expect_identical(out, "foo")

  inner <- NULL
  outer <- NULL
  spec <- function(outer, inner) {
    inner <<- inner
    outer <<- outer
  }

  apply_name_spec(spec, "outer", c("a", "b", "c"))
  expect_identical(inner, c("a", "b", "c"))
  expect_identical(outer, "outer")

  apply_name_spec(spec, "outer", "a", n = 3L)
  expect_identical(inner, "a")
  expect_identical(outer, "outer")
})

test_that("apply_name_spec() checks recyclability of output", {
  # These are recyclable
  expect_identical(
    apply_name_spec(function(...) c("a", "b"), "outer", "inner", n = 2L),
    c("a", "b")
  )
  expect_identical(
    apply_name_spec(function(...) "a", "outer", "inner", n = 2L),
    "a"
  )

  # This is not
  expect_snapshot(error = TRUE, {
    apply_name_spec(function(...) c("a", "b", "c"), "outer", "inner", n = 2L)
  })
})

test_that("r_chr_paste_prefix() works", {
  nms <- c("foo", "bar")

  expect_equal(
    .Call(ffi_chr_paste_prefix, nms, "baz", "."),
    c("baz.foo", "baz.bar")
  )

  # Greater than `VCTRS_PASTE_BUFFER_MAX_SIZE`
  long_prefix <- strrep("a", 5000)

  expect_equal(
    .Call(ffi_chr_paste_prefix, nms, long_prefix, "."),
    paste0(long_prefix, ".", nms)
  )
})

test_that("vec_as_names() uses internal error if `repair_arg` is not supplied", {
  expect_snapshot({
    (expect_error(vec_as_names("", repair = "foobar", call = quote(tilt()))))
    (expect_error(vec_as_names("", repair = env(), call = quote(tilt()))))
  })
})
