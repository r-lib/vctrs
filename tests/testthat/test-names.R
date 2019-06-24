context("test-names")


# API -----------------------------------------------------------------

test_that("Consistent values for repair argument", {
  expect_identical(formals(validate_repair)$repair, formals(vec_as_names)$repair)
  expect_identical(formals(validate_repair)$repair, formals(vec_names2)$repair)
})

# vec_names() ---------------------------------------------------------

test_that("vec_names() retrieves names", {
  expect_null(vec_names(letters))
  expect_identical(vec_names(set_names(letters)), letters)
  expect_null(vec_names(mtcars))
  expect_identical(vec_names(Titanic), dimnames(Titanic)[[1]])
  x <- matrix(1L, dimnames = list("row", "col"))
  expect_identical(vec_names(x), dimnames(x)[[1]])
})

test_that("vec_names() dispatches", {
  scoped_global_bindings(
    names.vctrs_foobar = function(x) "dispatched!"
  )
  expect_identical(vec_names(foobar()), "dispatched!")
})

test_that("vec_names<- sets names", {
  x <- letters
  vec_names(x) <- letters
  expect_identical(vec_names(x), letters)
  vec_names(x) <- NULL
  expect_null(vec_names(x))

  y <- iris
  vec_names(y) <- as.character(-seq_len(vec_size(y)))
  expect_identical(row.names(y), row.names(iris))
  expect_null(vec_names(y))

  z <- ones(3, 2, 1)
  vec_names(z) <- as.character(1:3)
  expect_identical(vec_names(z), as.character(1:3))
})


# vec_names2() -------------------------------------------------------------

test_that("vec_names2() repairs names", {
  expect_identical(vec_names2(1:2), c("", ""))
  expect_identical(vec_names2(1:2, repair = "unique"), c("...1", "...2"))
  expect_identical(vec_names2(set_names(1:2, c("_foo", "_bar")), repair = "universal"), c("._foo", "._bar"))
})

test_that("vec_names2() treats data frames and arrays as vectors", {
  expect_identical(vec_names2(mtcars), rep_len("", nrow(mtcars)))
  expect_identical(vec_names2(as.matrix(mtcars)), row.names(mtcars))
})

test_that("vec_names2() accepts and checks repair function", {
  expect_identical(vec_names2(1:2, repair = function(nms) rep_along(nms, "foo")), c("foo", "foo"))
  expect_error(vec_names2(1:2, repair = function(nms) "foo"), "length 1 instead of length 2")
})

test_that("vec_names2() repairs names before invoking repair function", {
  x <- set_names(1:2, c(NA, NA))
  expect_identical(vec_names2(x, repair = identity), c("", ""))
})


# vec_as_names() -----------------------------------------------------------

test_that("vec_as_names() requires character vector", {
  expect_error(vec_as_names(NULL), "`names` must be a character vector")
})

test_that("vec_as_names() repairs names", {
  expect_identical(vec_as_names(chr(NA, NA)), c("", ""))
  expect_identical(vec_as_names(chr(NA, NA), repair = "unique"), c("...1", "...2"))
  expect_identical(vec_as_names(chr("_foo", "_bar"), repair = "universal"), c("._foo", "._bar"))
  expect_identical(vec_as_names(chr("a", "b"), repair = "check_unique"), c("a", "b"))
})

test_that("vec_as_names() checks unique names", {
  expect_error(vec_as_names(chr(NA), repair = "check_unique"))
  expect_error(vec_as_names(chr(""), repair = "check_unique"))
  expect_error(vec_as_names(chr("a", "a"), repair = "check_unique"))
  expect_error(vec_as_names(chr("..1"), repair = "check_unique"))
  expect_error(vec_as_names(chr("..."), repair = "check_unique"))
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
  expect_identical(vec_as_names(c("", ""), repair = ~ rep_along(.x, "foo")), c("foo", "foo"))
  expect_error(vec_as_names(c("", ""), repair = function(nms) "foo"), "length 1 instead of length 2")
})

test_that("vec_as_names() repairs names before invoking repair function", {
  expect_identical(vec_as_names(chr(NA, NA), repair = identity), c("", ""))
})

test_that("validate_minimal() checks names", {
  expect_error(validate_minimal(1), "must return a character vector")
  expect_error(validate_minimal(NULL), "can't return `NULL`")
  expect_error(validate_minimal(chr(NA)), "can't return `NA` values")
})

test_that("validate_unique() checks unique names", {
  expect_error(validate_unique(chr(NA)), "`NA`")
  expect_error(validate_unique(chr("")), class = "vctrs_error_names_cannot_be_empty")
  expect_error(validate_unique(chr("a", "a")), class = "vctrs_error_names_must_be_unique")
  expect_error(validate_unique(chr("..1")), class = "vctrs_error_names_cannot_be_dot_dot")
  expect_error(validate_unique(chr("...")), class = "vctrs_error_names_cannot_be_dot_dot")
})


# vec_repair_names() -------------------------------------------------------

test_that("vec_repair_names() repairs names", {
  expect_identical(vec_repair_names(1:2), set_names(1:2, c("", "")))
  expect_identical(vec_repair_names(1:2, "unique"), set_names(1:2, c("...1", "...2")))
  expect_identical(vec_repair_names(set_names(1:2, c("_foo", "_bar")), "universal"), set_names(1:2, c("._foo", "._bar")))
})

test_that("vec_repair_names() handles data frames and arrays", {
  df <- data.frame(x = 1:2)
  expect_identical(vec_repair_names(df), df)
  expect_identical(row.names(vec_repair_names(as.matrix(df))), c("", ""))
  expect_identical(row.names(vec_repair_names(as.matrix(df), "unique")), c("...1", "...2"))
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
  expect_identical(minimal_names(mtcars), rep_len("", nrow(mtcars)))
  expect_identical(minimal_names(as.matrix(mtcars)), row.names(mtcars))
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

test_that("solo empty or NA gets suffix", {
  expect_identical(as_unique_names(""), "...1")
  expect_identical(as_unique_names(NA_character_), "...1")
})

test_that("ellipsis treated like empty string", {
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
  expect_identical(as_unique_names(c("a...3", "a", "a")), c("a...1", "a...2", "a...3"))
  expect_identical(as_unique_names(c("a...2", "a", "a")), c("a...1", "a...2", "a...3"))
  expect_identical(as_unique_names(c("a...2", "a...2", "a...2")), c("a...1", "a...2", "a...3"))
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
      as_unique_names(x), as_unique_names(y)
    )
  )

  ## fix names on x, catenate, fix the whole
  z2 <- as_unique_names(
    c(
      as_unique_names(x), y
    )
  )

  ## fix names on y, catenate, fix the whole
  z3 <- as_unique_names(
    c(
      x, as_unique_names(y)
    )
  )

  ## catenate, fix the whole
  z4 <- as_unique_names(
    c(
      x, y
    )
  )

  expect_identical(z1, z2)
  expect_identical(z1, z3)
  expect_identical(z1, z4)
})

test_that("unique_names() and as_unique_names() are verbose or silent", {
  expect_message(unique_names(1:2), "-> ...1", fixed = TRUE)
  expect_message(as_unique_names(c("", "")), "-> ...1", fixed = TRUE)

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
  expect_identical(as_universal_names(x), as_universal_names(as_universal_names(x)))
})

test_that("dupes get a suffix", {
  expect_equal(
    as_universal_names(c("a", "b", "a", "c", "b")),
    c("a...1", "b...2", "a...3", "c", "b...5")
  )
})

test_that("solo empty or NA gets suffix", {
  expect_identical(as_universal_names(""), "...1")
  expect_identical(as_universal_names(NA_character_), "...1")
})

test_that("ellipsis treated like empty string", {
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
    as_universal_names(c("", ".", NA, "if...4", "if", "if...8", "for", "if){]1")),
    c("...1", ".", "...3", ".if...4", ".if...5", ".if...6", ".for", "if...1")
  )
})

test_that("message", {
  expect_message(
    as_universal_names(c("a b", "b c")),
    "New names:\n* `a b` -> a.b\n* `b c` -> b.c\n",
    fixed = TRUE
  )
})

test_that("quiet", {
  expect_message(
    as_universal_names("", quiet = TRUE),
    NA
  )
})

test_that("unique then universal is universal, with shuffling", {
  x <- c("", ".2", "..3", "...4", "....5", ".....6", "......7", "...")
  expect_identical(as_universal_names(as_unique_names(x)), as_universal_names(x))

  x2 <- x[c(7L, 4L, 3L, 6L, 5L, 1L, 2L, 8L)]
  expect_identical(as_universal_names(as_unique_names(x2)), as_universal_names(x2))

  x3 <- x[c(3L, 2L, 4L, 6L, 8L, 1L, 5L, 7L)]
  expect_identical(as_universal_names(as_unique_names(x3)), as_universal_names(x3))
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
  expect_message(
    vec_repair_names(set_names(1, "a:b"), "universal"),
    "New names:\n* `a:b` -> a.b\n",
    fixed = TRUE
  )

  expect_message(
    vec_repair_names(set_names(1, "a:b"), ~ make.names(.)),
    "New names:\n* `a:b` -> a.b\n",
    fixed = TRUE
  )
})

test_that("quiet = TRUE", {
  expect_message(vec_repair_names(set_names(1, ""), "universal", quiet = TRUE), NA)
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
    c( "_",  "_1",  "_a}"),
    c("._", "._1", "._a.")
  )
})

test_that("make_syntactic(): dots", {
  expect_syntactic(
    c(".", "..",  "...", "...."),
    c(".", "..", "....", "....")
  )
})

test_that("make_syntactic(): number", {
  expect_syntactic(
      c(   "0",    "1",    "22",    "333"),
      c("...0", "...1", "...22", "...333")
  )
})

test_that("make_syntactic(): number then character", {
  expect_syntactic(
    c(  "0a",   "1b",   "22c",   "333d"),
    c("..0a", "..1b", "..22c", "..333d")
  )
})

test_that("make_syntactic(): number then non-character", {
  expect_syntactic(
    c(  "0)",   "1&",   "22*",   "333@"),
    c("..0.", "..1.", "..22.", "..333.")
  )
})

test_that("make_syntactic(): dot then number", {
  expect_syntactic(
    c(  ".0",   ".1",   ".22",   ".333"),
    c("...0", "...1", "...22", "...333")
  )
})

test_that("make_syntactic(): dot then number then character", {
  expect_syntactic(
    c( ".0a",  ".1b",  ".22c",  ".333d"),
    c("..0a", "..1b", "..22c", "..333d")
  )
})

test_that("make_syntactic(): dot then number then non-character", {
  expect_syntactic(
    c( ".0)",  ".1&",  ".22*",  ".333@"),
    c("..0.", "..1.", "..22.", "..333.")
  )
})

test_that("make_syntactic(): dot dot then number", {
  expect_syntactic(
    c( "..0",  "..1",  "..22",  "..333"),
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
    stop_names_cannot_be_empty(1:3),
    class = c("vctrs_error_names_cannot_be_empty", "vctrs_error_names", "vctrs_error"),
    message = "Names must not be empty.",
    locations = 1:3
  )
})

test_that("names cannot be dot dot", {
  expect_error_cnd(
    stop_names_cannot_be_dot_dot(1:3),
    class = c("vctrs_error_names_cannot_be_dot_dot", "vctrs_error_names", "vctrs_error"),
    message = "Names must not be of the form `...` or `..j`.",
    locations = 1:3
  )
})

test_that("names must be unique", {
  expect_error_cnd(
    stop_names_must_be_unique(1:3),
    class = c("vctrs_error_names_must_be_unique", "vctrs_error_names", "vctrs_error"),
    message = "Names must be unique.",
    locations = 1:3
  )
})


# Legacy repair --------------------------------------------------------

test_that("vec_as_names_legacy() works", {
  expect_identical(vec_as_names_legacy(chr()), chr())
  expect_identical(vec_as_names_legacy(c("a", "a", "", "")), c("a", "a1", "V1", "V2"))
  expect_identical(vec_as_names_legacy(c("a", "a", "", ""), sep = "_"), c("a", "a_1", "V_1", "V_2"))
  expect_identical(vec_as_names_legacy(c("a", "a", "", ""), prefix = "foo"), c("a", "a1", "foo1", "foo2"))
  expect_identical(vec_as_names_legacy(c("a", "a", "", ""), prefix = "foo", sep = "_"), c("a", "a_1", "foo_1", "foo_2"))

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

  expect_error(apply_name_spec(NULL, "foo", c("a", "b")), "vector of length > 1")
  expect_error(vec_c(foo = c(a = 1, b = 2)), "vector of length > 1")

  expect_error(apply_name_spec(NULL, "foo", NULL, 2L), "vector of length > 1")
  expect_error(vec_c(foo = 1:2), "vector of length > 1")
})

test_that("function name spec is applied", {
  spec <- function(outer, inner) {
    sep <- if (is_character(inner)) "_" else ":"
    paste0(outer, sep, inner)
  }

  expect_identical(apply_name_spec(spec, "foo", NULL, 1L), "foo")
  expect_named(vec_c(foo = 1, .name_spec = spec), "foo")

  expect_identical(apply_name_spec(spec, "foo", c("a", "b")), c("foo_a", "foo_b"))
  expect_named(vec_c(foo = c(a = 1, b = 2), .name_spec = spec), c("foo_a", "foo_b"))

  expect_identical(apply_name_spec(spec, "foo", NULL, 2L), c("foo:1", "foo:2"))
  expect_named(vec_c(foo = 1:2, .name_spec = spec), c("foo:1", "foo:2"))
})

test_that("can pass lambda formula as name spec", {
  expect_named(vec_c(foo = c(a = 1, b = 2), .name_spec = ~ paste(.x, .y, sep = "_")), c("foo_a", "foo_b"))
  expect_error(vec_c(foo = c(a = 1, b = 2), .name_spec = env()), "Can't convert `.name_spec`", fixed = TRUE)
})

test_that("can pass glue string as name spec", {
  expect_named(vec_c(foo = c(a = 1, b = 2), .name_spec = "{outer}_{inner}"), c("foo_a", "foo_b"))
  expect_named(vec_c(foo = 1:2, .name_spec = "{outer}_{inner}"), c("foo_1", "foo_2"))
  expect_error(vec_c(foo = c(a = 1, b = 2), .name_spec = c("a", "b")), "single string")
})

test_that("`outer` is recycled before name spec is invoked", {
  expect_identical(vec_c(outer = 1:2, .name_spec = "{outer}"), c(outer = 1L, outer = 2L))
})
