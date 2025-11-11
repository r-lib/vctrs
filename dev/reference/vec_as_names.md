# Retrieve and repair names

`vec_as_names()` takes a character vector of names and repairs it
according to the `repair` argument. It is the r-lib and tidyverse
equivalent of
[`base::make.names()`](https://rdrr.io/r/base/make.names.html).

vctrs deals with a few levels of name repair:

- `minimal` names exist. The `names` attribute is not `NULL`. The name
  of an unnamed element is `""` and never `NA`. For instance,
  `vec_as_names()` always returns minimal names and data frames created
  by the tibble package have names that are, at least, `minimal`.

- `unique` names are `minimal`, have no duplicates, and can be used
  where a variable name is expected. Empty names, `...`, and `..`
  followed by a sequence of digits are banned.

  - All columns can be accessed by name via `df[["name"]]` and
    `` df$`name`  `` and `` with(df, `name`) ``.

- `universal` names are `unique` and syntactic (see Details for more).

  - Names work everywhere, without quoting: `df$name` and
    `with(df, name)` and `lm(name1 ~ name2, data = df)` and
    `dplyr::select(df, name)` all work.

`universal` implies `unique`, `unique` implies `minimal`. These levels
are nested.

## Usage

``` r
vec_as_names(
  names,
  ...,
  repair = c("minimal", "unique", "universal", "check_unique", "unique_quiet",
    "universal_quiet"),
  repair_arg = NULL,
  quiet = FALSE,
  call = caller_env()
)
```

## Arguments

- names:

  A character vector.

- ...:

  These dots are for future extensions and must be empty.

- repair:

  Either a string or a function. If a string, it must be one of
  `"check_unique"`, `"minimal"`, `"unique"`, `"universal"`,
  `"unique_quiet"`, or `"universal_quiet"`. If a function, it is invoked
  with a vector of minimal names and must return minimal names,
  otherwise an error is thrown.

  - Minimal names are never `NULL` or `NA`. When an element doesn't have
    a name, its minimal name is an empty string.

  - Unique names are unique. A suffix is appended to duplicate names to
    make them unique.

  - Universal names are unique and syntactic, meaning that you can
    safely use the names as variables without causing a syntax error.

  The `"check_unique"` option doesn't perform any name repair. Instead,
  an error is raised if the names don't suit the `"unique"` criteria.

  The options `"unique_quiet"` and `"universal_quiet"` are here to help
  the user who calls this function indirectly, via another function
  which exposes `repair` but not `quiet`. Specifying
  `repair = "unique_quiet"` is like specifying
  `repair = "unique", quiet = TRUE`. When the `"*_quiet"` options are
  used, any setting of `quiet` is silently overridden.

- repair_arg:

  If specified and `repair = "check_unique"`, any errors will include a
  hint to set the `repair_arg`.

- quiet:

  By default, the user is informed of any renaming caused by repairing
  the names. This only concerns unique and universal repairing. Set
  `quiet` to `TRUE` to silence the messages.

  Users can silence the name repair messages by setting the
  `"rlib_name_repair_verbosity"` global option to `"quiet"`.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## `minimal` names

`minimal` names exist. The `names` attribute is not `NULL`. The name of
an unnamed element is `""` and never `NA`.

Examples:

    Original names of a vector with length 3: NULL
                               minimal names: "" "" ""

                              Original names: "x" NA
                               minimal names: "x" ""

## `unique` names

`unique` names are `minimal`, have no duplicates, and can be used
(possibly with backticks) in contexts where a variable is expected.
Empty names, `...`, and `..` followed by a sequence of digits are
banned. If a data frame has `unique` names, you can index it by name,
and also access the columns by name. In particular, `df[["name"]]` and
`` df$`name` `` and also `` with(df, `name`) `` always work.

There are many ways to make names `unique`. We append a suffix of the
form `...j` to any name that is `""` or a duplicate, where `j` is the
position. We also change `..#` and `...` to `...#`.

Example:

    Original names:     ""     "x"     "" "y"     "x"  "..2"  "..."
      unique names: "...1" "x...2" "...3" "y" "x...5" "...6" "...7"

Pre-existing suffixes of the form `...j` are always stripped, prior to
making names `unique`, i.e. reconstructing the suffixes. If this
interacts poorly with your names, you should take control of name
repair.

## `universal` names

`universal` names are `unique` and syntactic, meaning they:

- Are never empty (inherited from `unique`).

- Have no duplicates (inherited from `unique`).

- Are not `...`. Do not have the form `..i`, where `i` is a number
  (inherited from `unique`).

- Consist of letters, numbers, and the dot `.` or underscore `_`
  characters.

- Start with a letter or start with the dot `.` not followed by a
  number.

- Are not a [reserved](https://rdrr.io/r/base/Reserved.html) word, e.g.,
  `if` or `function` or `TRUE`.

If a vector has `universal` names, variable names can be used "as is" in
code. They work well with nonstandard evaluation, e.g., `df$name` works.

vctrs has a different method of making names syntactic than
[`base::make.names()`](https://rdrr.io/r/base/make.names.html). In
general, vctrs prepends one or more dots `.` until the name is
syntactic.

Examples:

     Original names:     ""     "x"    NA      "x"
    universal names: "...1" "x...2" "...3" "x...4"

      Original names: "(y)"  "_z"  ".2fa"  "FALSE"
     universal names: ".y." "._z" "..2fa" ".FALSE"

## See also

[`rlang::names2()`](https://rlang.r-lib.org/reference/names2.html)
returns the names of an object, after making them `minimal`.

## Examples

``` r
# By default, `vec_as_names()` returns minimal names:
vec_as_names(c(NA, NA, "foo"))
#> [1] ""    ""    "foo"

# You can make them unique:
vec_as_names(c(NA, NA, "foo"), repair = "unique")
#> New names:
#> • `` -> `...1`
#> • `` -> `...2`
#> [1] "...1" "...2" "foo" 

# Universal repairing fixes any non-syntactic name:
vec_as_names(c("_foo", "+"), repair = "universal")
#> New names:
#> • `_foo` -> `._foo`
#> • `+` -> `.`
#> [1] "._foo" "."    
```
