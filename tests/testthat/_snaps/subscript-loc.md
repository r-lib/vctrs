# vec_as_location2() requires integer or character inputs

    Code
      (expect_error(vec_as_location2(TRUE, 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't extract element with `TRUE`.
      x `TRUE` must be numeric or character, not `TRUE`.
    Code
      (expect_error(vec_as_location2(mtcars, 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't extract element with `mtcars`.
      x `mtcars` must be numeric or character, not a <data.frame> object.
    Code
      (expect_error(vec_as_location2(env(), 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't extract element with `env()`.
      x `env()` must be numeric or character, not an environment.
    Code
      (expect_error(vec_as_location2(foobar(), 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't extract element with `foobar()`.
      x `foobar()` must be numeric or character, not a <vctrs_foobar> object.
    Code
      (expect_error(vec_as_location2(2.5, 10L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't extract element with `2.5`.
      x Can't convert from `2.5` <double> to <integer> due to loss of precision.
    Code
      (expect_error(vec_as_location2(Inf, 10L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't extract element with `Inf`.
      x Can't convert from `Inf` <double> to <integer> due to loss of precision.
    Code
      (expect_error(vec_as_location2(-Inf, 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't extract element with `-Inf`.
      x Can't convert from `-Inf` <double> to <integer> due to loss of precision.
    Code
      # Idem with custom `arg`
      (expect_error(vec_as_location2(foobar(), 10L, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't extract element with `foo`.
      x `foo` must be numeric or character, not a <vctrs_foobar> object.
    Code
      (expect_error(vec_as_location2(2.5, 3L, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't extract element with `foo`.
      x Can't convert from `foo` <double> to <integer> due to loss of precision.
    Code
      (expect_error(with_tibble_rows(vec_as_location2(TRUE)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't remove row with `foo(bar)`.
      x `foo(bar)` must be numeric or character, not `TRUE`.

# vec_as_location() requires integer, character, or logical inputs

    Code
      (expect_error(vec_as_location(mtcars, 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `mtcars`.
      x `mtcars` must be logical, numeric, or character, not a <data.frame> object.
    Code
      (expect_error(vec_as_location(env(), 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `env()`.
      x `env()` must be logical, numeric, or character, not an environment.
    Code
      (expect_error(vec_as_location(foobar(), 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `foobar()`.
      x `foobar()` must be logical, numeric, or character, not a <vctrs_foobar> object.
    Code
      (expect_error(vec_as_location(2.5, 10L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `2.5`.
      x Can't convert from `2.5` <double> to <integer> due to loss of precision.
    Code
      (expect_error(vec_as_location(list(), 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `list()`.
      x `list()` must be logical, numeric, or character, not an empty list.
    Code
      (expect_error(vec_as_location(function() NULL, 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `function() NULL`.
      x `function() NULL` must be logical, numeric, or character, not a function.
    Code
      (expect_error(vec_as_location(Sys.Date(), 3L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `Sys.Date()`.
      x `Sys.Date()` must be logical, numeric, or character, not a <Date> object.
    Code
      # Idem with custom `arg`
      (expect_error(vec_as_location(env(), 10L, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't subset elements with `foo`.
      x `foo` must be logical, numeric, or character, not an environment.
    Code
      (expect_error(vec_as_location(foobar(), 10L, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't subset elements with `foo`.
      x `foo` must be logical, numeric, or character, not a <vctrs_foobar> object.
    Code
      (expect_error(vec_as_location(2.5, 3L, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't subset elements with `foo`.
      x Can't convert from `foo` <double> to <integer> due to loss of precision.

# vec_as_location() and variants check for OOB elements (#1605)

    Code
      # Numeric indexing
      (expect_error(vec_as_location(10L, 2L), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements past the end.
      i Location 10 doesn't exist.
      i There are only 2 elements.
    Code
      (expect_error(vec_as_location(-10L, 2L), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't negate elements past the end.
      i Location 10 doesn't exist.
      i There are only 2 elements.
    Code
      (expect_error(vec_as_location2(10L, 2L), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements past the end.
      i Location 10 doesn't exist.
      i There are only 2 elements.
    Code
      # Character indexing
      (expect_error(vec_as_location("foo", 1L, names = "bar"), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements that don't exist.
      x Element `foo` doesn't exist.
    Code
      (expect_error(vec_as_location2("foo", 1L, names = "bar"), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements that don't exist.
      x Element `foo` doesn't exist.
    Code
      (expect_error(vec_as_location2("foo", 1L, names = "bar", call = call("baz")),
      class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `baz()`:
      ! Can't subset elements that don't exist.
      x Element `foo` doesn't exist.

# vec_as_location2() requires length 1 inputs

    Code
      (expect_error(vec_as_location2(1:2, 2L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't extract element with `1:2`.
      x Subscript `1:2` must be size 1, not 2.
    Code
      (expect_error(vec_as_location2(c("foo", "bar"), 2L, c("foo", "bar")), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't extract element with `c("foo", "bar")`.
      x Subscript `c("foo", "bar")` must be size 1, not 2.
    Code
      # Idem with custom `arg`
      (expect_error(vec_as_location2(1:2, 2L, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't extract element with `foo`.
      x Subscript `foo` must be size 1, not 2.
    Code
      (expect_error(vec_as_location2(mtcars, 10L, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't extract element with `foo`.
      x `foo` must be numeric or character, not a <data.frame> object.
    Code
      (expect_error(vec_as_location2(1:2, 2L, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't extract element with `foo`.
      x Subscript `foo` must be size 1, not 2.

# vec_as_location2() requires positive integers

    Code
      (expect_error(vec_as_location2(0, 2L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't extract element with `0`.
      x Subscript `0` must be a positive location, not 0.
    Code
      (expect_error(vec_as_location2(-1, 2L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't extract element with `-1`.
      x Subscript `-1` must be a positive location, not -1.
    Code
      # Idem with custom `arg`
      (expect_error(vec_as_location2(0, 2L, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't extract element with `foo`.
      x Subscript `foo` must be a positive location, not 0.

# vec_as_location2() fails with NA

    Code
      (expect_error(vec_as_location2(na_int, 2L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't extract element with `na_int`.
      x Subscript `na_int` must be a location, not an integer `NA`.
    Code
      (expect_error(vec_as_location2(na_chr, 1L, names = "foo"), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't extract element with `na_chr`.
      x Subscript `na_chr` must be a location, not a character `NA`.
    Code
      # Idem with custom `arg`
      (expect_error(vec_as_location2(na_int, 2L, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't extract element with `foo`.
      x Subscript `foo` must be a location, not an integer `NA`.

# num_as_location() optionally forbids negative indices

    Code
      (expect_error(num_as_location(dbl(1, -1), 2L, negative = "error"), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `dbl(1, -1)`.
      Caused by error:
      ! `dbl(1, -1)` can't contain negative locations.

# num_as_location() optionally forbids zero indices

    Code
      (expect_error(num_as_location(0L, 1L, zero = "error"), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `0L`.
      x Subscript `0L` can't contain `0` values.
      i It has a `0` value at location 1.
    Code
      (expect_error(num_as_location(c(0, 0, 0, 0, 0, 0), 1, zero = "error"), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `c(0, 0, 0, 0, 0, 0)`.
      x Subscript `c(0, 0, 0, 0, 0, 0)` can't contain `0` values.
      i It has 6 `0` values at locations 1, 2, 3, 4, 5, etc.

# vec_as_location() checks for mix of negative and missing locations

    Code
      (expect_error(vec_as_location(-c(1L, NA), 30), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `-c(1L, NA)`.
      Caused by error:
      ! Negative locations can't have missing values.
      x `-c(1L, NA)` has 1 missing value at location 2.
    Code
      (expect_error(vec_as_location(-c(1L, rep(NA, 10)), 30), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `-c(1L, rep(NA, 10))`.
      Caused by error:
      ! Negative locations can't have missing values.
      x `-c(1L, rep(NA, 10))` has 10 missing values at locations 2, 3, 4, 5, 6, 7, 8, 9, 10, and 11.

# vec_as_location() checks for mix of negative and positive locations

    Code
      (expect_error(vec_as_location(c(-1L, 1L), 30), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `c(-1L, 1L)`.
      x Negative and positive locations can't be mixed.
      i Subscript `c(-1L, 1L)` has a positive value at location 2.
    Code
      (expect_error(vec_as_location(c(-1L, rep(1L, 10)), 30), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `c(-1L, rep(1L, 10))`.
      x Negative and positive locations can't be mixed.
      i Subscript `c(-1L, rep(1L, 10))` has 10 positive values at locations 2, 3, 4, 5, 6, etc.

# logical subscripts must match size of indexed vector

    Code
      (expect_error(vec_as_location(c(TRUE, FALSE), 3), class = "vctrs_error_subscript_size")
      )
    Output
      <error/vctrs_error_subscript_size>
      Error:
      ! Can't subset elements with `c(TRUE, FALSE)`.
      x Logical subscript `c(TRUE, FALSE)` must be size 1 or 3, not 2.

# character subscripts require named vectors

    Code
      (expect_error(vec_as_location(letters[1], 3), "unnamed vector"))
    Output
      <error/rlang_error>
      Error in `vec_as_location()`:
      ! Can't use character names to index an unnamed vector.

# can optionally extend beyond the end

    Code
      (expect_error(num_as_location(3, 1, oob = "extend"), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements beyond the end with non-consecutive locations.
      i Input has size 1.
      x Subscript `3` contains non-consecutive location 3.
    Code
      (expect_error(num_as_location(c(1, 3), 1, oob = "extend"), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements beyond the end with non-consecutive locations.
      i Input has size 1.
      x Subscript `c(1, 3)` contains non-consecutive location 3.
    Code
      (expect_error(num_as_location(c(1:5, 7), 3, oob = "extend"), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements beyond the end with non-consecutive locations.
      i Input has size 3.
      x Subscript `c(1:5, 7)` contains non-consecutive locations 4 and 7.
    Code
      (expect_error(num_as_location(c(1:5, 7, 1), 3, oob = "extend"), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements beyond the end with non-consecutive locations.
      i Input has size 3.
      x Subscript `c(1:5, 7, 1)` contains non-consecutive locations 4 and 7.
    Code
      (expect_error(class = "vctrs_error_subscript_oob", num_as_location(c(1:5, 7, 1,
      10), 3, oob = "extend")))
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements beyond the end with non-consecutive locations.
      i Input has size 3.
      x Subscript `c(1:5, 7, 1, 10)` contains non-consecutive locations 4, 7, and 10.

# num_as_location() errors when inverting oob negatives unless `oob = 'remove'` (#1630)

    Code
      num_as_location(-4, 3, oob = "error", negative = "invert")
    Condition
      Error:
      ! Can't negate elements past the end.
      i Location 4 doesn't exist.
      i There are only 3 elements.

---

    Code
      num_as_location(c(-4, 4, 5), 3, oob = "extend", negative = "invert")
    Condition
      Error:
      ! Can't negate elements past the end.
      i Location 4 doesn't exist.
      i There are only 3 elements.

# num_as_location() errors on disallowed zeros when inverting negatives (#1612)

    Code
      num_as_location(c(0, -1), n = 2L, negative = "invert", zero = "error")
    Condition
      Error:
      ! Can't subset elements with `c(0, -1)`.
      x Subscript `c(0, -1)` can't contain `0` values.
      i It has a `0` value at location 1.

---

    Code
      num_as_location(c(-1, 0), n = 2L, negative = "invert", zero = "error")
    Condition
      Error:
      ! Can't subset elements with `c(-1, 0)`.
      x Subscript `c(-1, 0)` can't contain `0` values.
      i It has a `0` value at location 2.

# num_as_location() with `oob = 'extend'` doesn't allow ignored oob negative values (#1614)

    Code
      num_as_location(-6L, 5L, oob = "extend", negative = "ignore")
    Condition
      Error:
      ! Can't negate elements past the end.
      i Location 6 doesn't exist.
      i There are only 5 elements.

---

    Code
      num_as_location(c(-7L, 6L), 5L, oob = "extend", negative = "ignore")
    Condition
      Error:
      ! Can't negate elements past the end.
      i Location 7 doesn't exist.
      i There are only 5 elements.

---

    Code
      num_as_location(c(-7L, NA), 5L, oob = "extend", negative = "ignore")
    Condition
      Error:
      ! Can't negate elements past the end.
      i Location 7 doesn't exist.
      i There are only 5 elements.

# num_as_location() with `oob = 'error'` reports negative and positive oob values

    Code
      num_as_location(c(-6L, 7L), n = 5L, oob = "error", negative = "ignore")
    Condition
      Error:
      ! Can't subset elements past the end.
      i Locations 6 and 7 don't exist.
      i There are only 5 elements.

# missing values are supported in error formatters

    Code
      (expect_error(num_as_location(c(1, NA, 2, 3), 1), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements past the end.
      i Locations 2 and 3 don't exist.
      i There is only 1 element.
    Code
      (expect_error(num_as_location(c(1, NA, 3), 1, oob = "extend"), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements beyond the end with non-consecutive locations.
      i Input has size 1.
      x Subscript `c(1, NA, 3)` contains non-consecutive location 3.

# can disallow missing values

    Code
      (expect_error(vec_as_location(c(1, NA), 2, missing = "error"), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements.
      x Subscript can't contain missing values.
      x It has a missing value at location 2.
    Code
      (expect_error(vec_as_location(c(1, NA, 2, NA), 2, missing = "error", arg = "foo",
      call = call("my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't subset elements.
      x Subscript can't contain missing values.
      x It has missing values at locations 2 and 4.
    Code
      (expect_error(with_tibble_cols(vec_as_location(c(1, NA, 2, NA), 2, missing = "error")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't rename columns with `foo(bar)`.
      x Subscript `foo(bar)` can't contain missing values.
      x It has missing values at locations 2 and 4.
    Code
      (expect_error(with_tibble_cols(vec_as_location(NA, 1, missing = "error")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't rename columns with `foo(bar)`.
      x Subscript `foo(bar)` can't contain missing values.
      x It has a missing value at location 1.
    Code
      (expect_error(with_tibble_cols(vec_as_location(NA, 3, missing = "error")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't rename columns with `foo(bar)`.
      x Subscript `foo(bar)` can't contain missing values.
      x It has a missing value at location 1.
    Code
      (expect_error(with_tibble_cols(vec_as_location(c(TRUE, NA, FALSE), 3, missing = "error")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't rename columns with `foo(bar)`.
      x Subscript `foo(bar)` can't contain missing values.
      x It has a missing value at location 2.
    Code
      (expect_error(with_tibble_cols(vec_as_location(NA_character_, 2, missing = "error",
        names = c("x", "y"))), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't rename columns with `foo(bar)`.
      x Subscript `foo(bar)` can't contain missing values.
      x It has a missing value at location 1.

# can alter logical missing value handling (#1595)

    Code
      vec_as_location(x, n = 4L, missing = "error")
    Condition
      Error:
      ! Can't subset elements.
      x Subscript can't contain missing values.
      x It has missing values at locations 2 and 4.

---

    Code
      vec_as_location(x, n = 2L, missing = "error")
    Condition
      Error:
      ! Can't subset elements.
      x Subscript can't contain missing values.
      x It has a missing value at location 1.

# can alter character missing value handling (#1595)

    Code
      vec_as_location(x, n = 2L, names = names, missing = "error")
    Condition
      Error:
      ! Can't subset elements.
      x Subscript can't contain missing values.
      x It has missing values at locations 1 and 3.

# can alter integer missing value handling (#1595)

    Code
      vec_as_location(x, n = 4L, missing = "error")
    Condition
      Error:
      ! Can't subset elements.
      x Subscript can't contain missing values.
      x It has missing values at locations 1 and 3.

# can alter negative integer missing value handling (#1595)

    Code
      num_as_location(x, n = 4L, missing = "propagate", negative = "invert")
    Condition
      Error:
      ! Can't subset elements with `x`.
      Caused by error:
      ! Negative locations can't have missing values.
      x `x` has 2 missing values at locations 2 and 3.

---

    Code
      num_as_location(x, n = 4L, missing = "error", negative = "invert")
    Condition
      Error:
      ! Can't subset elements with `x`.
      Caused by error:
      ! Negative locations can't have missing values.
      x `x` has 2 missing values at locations 2 and 3.

# empty string character indices never match empty string names (#1489)

    Code
      vec_as_location("", n = 2L, names = names)
    Condition
      Error:
      ! Can't subset elements.
      x Subscript can't contain the empty string.
      x It has an empty string at location 1.

---

    Code
      vec_as_location(c("", "y", ""), n = 2L, names = names)
    Condition
      Error:
      ! Can't subset elements.
      x Subscript can't contain the empty string.
      x It has an empty string at locations 1 and 3.

# can customise subscript type errors

    Code
      # With custom `arg`
      (expect_error(num_as_location(-1, 2, negative = "error", arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't subset elements with `foo`.
      Caused by error:
      ! `foo` can't contain negative locations.
    Code
      (expect_error(num_as_location2(-1, 2, negative = "error", arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't extract element with `foo`.
      x Subscript `foo` must be a positive location, not -1.
    Code
      (expect_error(vec_as_location2(0, 2, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't extract element with `foo`.
      x Subscript `foo` must be a positive location, not 0.
    Code
      (expect_error(vec_as_location2(na_dbl, 2, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't extract element with `foo`.
      x Subscript `foo` must be a location, not an integer `NA`.
    Code
      (expect_error(vec_as_location2(c(1, 2), 2, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't extract element with `foo`.
      x Subscript `foo` must be size 1, not 2.
    Code
      (expect_error(vec_as_location(c(TRUE, FALSE), 3, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_size"))
    Output
      <error/vctrs_error_subscript_size>
      Error in `my_function()`:
      ! Can't subset elements with `foo`.
      x Logical subscript `foo` must be size 1 or 3, not 2.
    Code
      (expect_error(vec_as_location(c(-1, NA), 3, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't subset elements with `foo`.
      Caused by error:
      ! Negative locations can't have missing values.
      x `foo` has 1 missing value at location 2.
    Code
      (expect_error(vec_as_location(c(-1, 1), 3, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't subset elements with `foo`.
      x Negative and positive locations can't be mixed.
      i Subscript `foo` has a positive value at location 2.
    Code
      (expect_error(num_as_location(c(1, 4), 2, oob = "extend", arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `my_function()`:
      ! Can't subset elements beyond the end with non-consecutive locations.
      i Input has size 2.
      x Subscript `foo` contains non-consecutive location 4.
    Code
      (expect_error(num_as_location(0, 1, zero = "error", arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Can't subset elements with `foo`.
      x Subscript `foo` can't contain `0` values.
      i It has a `0` value at location 1.
    Code
      # With tibble columns
      (expect_error(with_tibble_cols(num_as_location(-1, 2, negative = "error")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't rename columns with `foo(bar)`.
      Caused by error:
      ! `foo(bar)` can't contain negative locations.
    Code
      (expect_error(with_tibble_cols(num_as_location2(-1, 2, negative = "error")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't rename column with `foo(bar)`.
      x Subscript `foo(bar)` must be a positive location, not -1.
    Code
      (expect_error(with_tibble_cols(vec_as_location2(0, 2)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't rename column with `foo(bar)`.
      x Subscript `foo(bar)` must be a positive location, not 0.
    Code
      (expect_error(with_tibble_cols(vec_as_location2(na_dbl, 2)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't rename column with `foo(bar)`.
      x Subscript `foo(bar)` must be a location, not an integer `NA`.
    Code
      (expect_error(with_tibble_cols(vec_as_location2(c(1, 2), 2)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't rename column with `foo(bar)`.
      x Subscript `foo(bar)` must be size 1, not 2.
    Code
      (expect_error(with_tibble_cols(vec_as_location(c(TRUE, FALSE), 3)), class = "vctrs_error_subscript_size")
      )
    Output
      <error/vctrs_error_subscript_size>
      Error:
      ! Can't rename columns with `foo(bar)`.
      x Logical subscript `foo(bar)` must be size 1 or 3, not 2.
    Code
      (expect_error(with_tibble_cols(vec_as_location(c(-1, NA), 3)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't rename columns with `foo(bar)`.
      Caused by error:
      ! Negative locations can't have missing values.
      x `foo(bar)` has 1 missing value at location 2.
    Code
      (expect_error(with_tibble_cols(vec_as_location(c(-1, 1), 3)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't rename columns with `foo(bar)`.
      x Negative and positive locations can't be mixed.
      i Subscript `foo(bar)` has a positive value at location 2.
    Code
      (expect_error(with_tibble_cols(num_as_location(c(1, 4), 2, oob = "extend")),
      class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't rename columns beyond the end with non-consecutive locations.
      i Input has size 2.
      x Subscript `foo(bar)` contains non-consecutive location 4.
    Code
      (expect_error(with_tibble_cols(num_as_location(0, 1, zero = "error")), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't rename columns with `foo(bar)`.
      x Subscript `foo(bar)` can't contain `0` values.
      i It has a `0` value at location 1.

# can customise OOB errors

    Code
      (expect_error(vec_slice(set_names(letters), "foo"), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't subset elements that don't exist.
      x Element `foo` doesn't exist.
    Code
      # With custom `arg`
      (expect_error(vec_as_location(30, length(letters), arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `my_function()`:
      ! Can't subset elements past the end.
      i Location 30 doesn't exist.
      i There are only 26 elements.
    Code
      (expect_error(vec_as_location("foo", NULL, letters, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `my_function()`:
      ! Can't subset elements that don't exist.
      x Element `foo` doesn't exist.
    Code
      # With tibble columns
      (expect_error(with_tibble_cols(vec_slice(set_names(letters), "foo")), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't rename columns that don't exist.
      x Column `foo` doesn't exist.
    Code
      (expect_error(with_tibble_cols(vec_slice(set_names(letters), 30)), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't rename columns that don't exist.
      i Location 30 doesn't exist.
      i There are only 26 columns.
    Code
      (expect_error(with_tibble_cols(vec_slice(set_names(letters), -30)), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't rename columns that don't exist.
      i Location 30 doesn't exist.
      i There are only 26 columns.
    Code
      # With tibble rows
      (expect_error(with_tibble_rows(vec_slice(set_names(letters), c("foo", "bar"))),
      class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't remove rows that don't exist.
      x Rows `foo` and `bar` don't exist.
    Code
      (expect_error(with_tibble_rows(vec_slice(set_names(letters), 1:30)), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't remove rows past the end.
      i Locations 27, 28, 29, and 30 don't exist.
      i There are only 26 rows.
    Code
      (expect_error(with_tibble_rows(vec_slice(set_names(letters), -(1:30))), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't remove rows past the end.
      i Locations 27, 28, 29, and 30 don't exist.
      i There are only 26 rows.
    Code
      # With tidyselect select
      (expect_error(with_tidyselect_select(vec_slice(set_names(letters), c("foo",
        "bar"))), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't select columns that don't exist.
      x Columns `foo` and `bar` don't exist.
    Code
      (expect_error(with_tidyselect_select(vec_slice(set_names(letters), 30)), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't select columns past the end.
      i Location 30 doesn't exist.
      i There are only 26 columns.
    Code
      (expect_error(with_tidyselect_select(vec_slice(set_names(letters), -(1:30))),
      class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't select columns past the end.
      i Locations 27, 28, 29, and 30 don't exist.
      i There are only 26 columns.
    Code
      # With tidyselect relocate
      (expect_error(with_tidyselect_relocate(vec_slice(set_names(letters), c("foo",
        "bar"))), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't relocate columns that don't exist.
      x Columns `foo` and `bar` don't exist.
    Code
      (expect_error(with_tidyselect_relocate(vec_slice(set_names(letters), 30)),
      class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't relocate columns that don't exist.
      i Location 30 doesn't exist.
      i There are only 26 columns.
    Code
      (expect_error(with_tidyselect_relocate(vec_slice(set_names(letters), -(1:30))),
      class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't relocate columns that don't exist.
      i Locations 27, 28, 29, and 30 don't exist.
      i There are only 26 columns.

# vec_as_location() checks dimensionality

    Code
      (expect_error(vec_as_location(matrix(TRUE, nrow = 1), 3L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `matrix(TRUE, nrow = 1)`.
      x Subscript `matrix(TRUE, nrow = 1)` must be a simple vector, not a matrix.
    Code
      (expect_error(vec_as_location(array(TRUE, dim = c(1, 1, 1)), 3L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements with `array(TRUE, dim = c(1, 1, 1))`.
      x Subscript `array(TRUE, dim = c(1, 1, 1))` must be a simple vector, not an array.
    Code
      (expect_error(with_tibble_rows(vec_as_location(matrix(TRUE, nrow = 1), 3L)),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't remove rows with `foo(bar)`.
      x Subscript `foo(bar)` must be a simple vector, not a matrix.

# vec_as_location() UI

    Code
      vec_as_location(1, 1L, missing = "bogus")
    Condition
      Error in `vec_as_location()`:
      ! `missing` must be one of "propagate", "remove", or "error".

# num_as_location() UI

    Code
      num_as_location(1, 1L, missing = "bogus")
    Condition
      Error in `num_as_location()`:
      ! `missing` must be one of "propagate", "remove", or "error".

---

    Code
      num_as_location(1, 1L, negative = "bogus")
    Condition
      Error in `num_as_location()`:
      ! `negative` must be one of "invert", "error", or "ignore".

---

    Code
      num_as_location(1, 1L, oob = "bogus")
    Condition
      Error in `num_as_location()`:
      ! `oob` must be one of "error", "remove", or "extend".

---

    Code
      num_as_location(1, 1L, zero = "bogus")
    Condition
      Error in `num_as_location()`:
      ! `zero` must be one of "remove", "error", or "ignore".

# vec_as_location2() UI

    Code
      vec_as_location2(1, 1L, missing = "bogus")
    Condition
      Error in `vec_as_location2_result()`:
      ! `missing` must be one of "error" or "propagate", not "bogus".

