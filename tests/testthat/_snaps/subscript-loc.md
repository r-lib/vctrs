# vec_as_location2() requires integer or character inputs

    Code
      (expect_error(vec_as_location2(TRUE, 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript `TRUE` has the wrong type `logical`.
      i It must be numeric or character.
    Code
      (expect_error(vec_as_location2(mtcars, 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript `mtcars` has the wrong type `data.frame<
        mpg : double
        cyl : double
        disp: double
        hp  : double
        drat: double
        wt  : double
        qsec: double
        vs  : double
        am  : double
        gear: double
        carb: double
      >`.
      i It must be numeric or character.
    Code
      (expect_error(vec_as_location2(env(), 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript `env()` has the wrong type `environment`.
      i It must be numeric or character.
    Code
      (expect_error(vec_as_location2(foobar(), 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript `foobar()` has the wrong type `vctrs_foobar`.
      i It must be numeric or character.
    Code
      (expect_error(vec_as_location2(2.5, 10L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Can't convert from `2.5` <double> to <integer> due to loss of precision.
    Code
      (expect_error(vec_as_location2(Inf, 10L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Can't convert from `Inf` <double> to <integer> due to loss of precision.
    Code
      (expect_error(vec_as_location2(-Inf, 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Can't convert from `-Inf` <double> to <integer> due to loss of precision.
    Code
      # Idem with custom `arg`
      (expect_error(vec_as_location2(foobar(), 10L, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must extract element with a single valid subscript.
      x Subscript `foo` has the wrong type `vctrs_foobar`.
      i It must be numeric or character.
    Code
      (expect_error(vec_as_location2(2.5, 3L, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must extract element with a single valid subscript.
      x Can't convert from `foo` <double> to <integer> due to loss of precision.
    Code
      (expect_error(with_tibble_rows(vec_as_location2(TRUE)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must remove row with a single valid subscript.
      x Subscript `foo(bar)` has the wrong type `logical`.
      i It must be numeric or character.

# vec_as_location() requires integer, character, or logical inputs

    Code
      (expect_error(vec_as_location(mtcars, 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript `mtcars` has the wrong type `data.frame<
        mpg : double
        cyl : double
        disp: double
        hp  : double
        drat: double
        wt  : double
        qsec: double
        vs  : double
        am  : double
        gear: double
        carb: double
      >`.
      i It must be logical, numeric, or character.
    Code
      (expect_error(vec_as_location(env(), 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript `env()` has the wrong type `environment`.
      i It must be logical, numeric, or character.
    Code
      (expect_error(vec_as_location(foobar(), 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript `foobar()` has the wrong type `vctrs_foobar`.
      i It must be logical, numeric, or character.
    Code
      (expect_error(vec_as_location(2.5, 10L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Can't convert from `2.5` <double> to <integer> due to loss of precision.
    Code
      (expect_error(vec_as_location(list(), 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript `list()` has the wrong type `list`.
      i It must be logical, numeric, or character.
    Code
      (expect_error(vec_as_location(function() NULL, 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript `function() NULL` has the wrong type `function`.
      i It must be logical, numeric, or character.
    Code
      (expect_error(vec_as_location(Sys.Date(), 3L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript `Sys.Date()` has the wrong type `date`.
      i It must be logical, numeric, or character.
    Code
      # Idem with custom `arg`
      (expect_error(vec_as_location(env(), 10L, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must subset elements with a valid subscript vector.
      x Subscript `foo` has the wrong type `environment`.
      i It must be logical, numeric, or character.
    Code
      (expect_error(vec_as_location(foobar(), 10L, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must subset elements with a valid subscript vector.
      x Subscript `foo` has the wrong type `vctrs_foobar`.
      i It must be logical, numeric, or character.
    Code
      (expect_error(vec_as_location(2.5, 3L, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must subset elements with a valid subscript vector.
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
      ! Must extract element with a single valid subscript.
      x Subscript `1:2` has size 2 but must be size 1.
    Code
      (expect_error(vec_as_location2(c("foo", "bar"), 2L, c("foo", "bar")), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript `c("foo", "bar")` has size 2 but must be size 1.
    Code
      # Idem with custom `arg`
      (expect_error(vec_as_location2(1:2, 2L, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must extract element with a single valid subscript.
      x Subscript `foo` has size 2 but must be size 1.
    Code
      (expect_error(vec_as_location2(mtcars, 10L, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must extract element with a single valid subscript.
      x Subscript `foo` has the wrong type `data.frame<
        mpg : double
        cyl : double
        disp: double
        hp  : double
        drat: double
        wt  : double
        qsec: double
        vs  : double
        am  : double
        gear: double
        carb: double
      >`.
      i It must be numeric or character.
    Code
      (expect_error(vec_as_location2(1:2, 2L, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must extract element with a single valid subscript.
      x Subscript `foo` has size 2 but must be size 1.

# vec_as_location2() requires positive integers

    Code
      (expect_error(vec_as_location2(0, 2L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript `0` has value 0 but must be a positive location.
    Code
      (expect_error(vec_as_location2(-1, 2L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript `-1` has value -1 but must be a positive location.
    Code
      # Idem with custom `arg`
      (expect_error(vec_as_location2(0, 2L, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must extract element with a single valid subscript.
      x Subscript `foo` has value 0 but must be a positive location.

# vec_as_location2() fails with NA

    Code
      (expect_error(vec_as_location2(na_int, 2L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript `na_int` can't be `NA`.
    Code
      (expect_error(vec_as_location2(na_chr, 1L, names = "foo"), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript `na_chr` can't be `NA`.
    Code
      # Idem with custom `arg`
      (expect_error(vec_as_location2(na_int, 2L, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must extract element with a single valid subscript.
      x Subscript `foo` can't be `NA`.

# num_as_location() optionally forbids negative indices

    Code
      (expect_error(num_as_location(dbl(1, -1), 2L, negative = "error"), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript `dbl(1, -1)` can't contain negative locations.

# num_as_location() optionally forbids zero indices

    Code
      (expect_error(num_as_location(0L, 1L, zero = "error"), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript `0L` can't contain `0` values.
      i It has a `0` value at location 1.
    Code
      (expect_error(num_as_location(c(0, 0, 0, 0, 0, 0), 1, zero = "error"), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript `c(0, 0, 0, 0, 0, 0)` can't contain `0` values.
      i It has 6 `0` values at locations 1, 2, 3, 4, 5, etc.

# vec_as_location() checks for mix of negative and missing locations

    Code
      (expect_error(vec_as_location(-c(1L, NA), 30), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Negative locations can't have missing values.
      i Subscript `-c(1L, NA)` has a missing value at location 2.
    Code
      (expect_error(vec_as_location(-c(1L, rep(NA, 10)), 30), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Negative locations can't have missing values.
      i Subscript `-c(1L, rep(NA, 10))` has 10 missing values at locations 2, 3, 4, 5, 6, etc.

# vec_as_location() checks for mix of negative and positive locations

    Code
      (expect_error(vec_as_location(c(-1L, 1L), 30), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Negative and positive locations can't be mixed.
      i Subscript `c(-1L, 1L)` has a positive value at location 2.
    Code
      (expect_error(vec_as_location(c(-1L, rep(1L, 10)), 30), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Negative and positive locations can't be mixed.
      i Subscript `c(-1L, rep(1L, 10))` has 10 positive values at locations 2, 3, 4, 5, 6, etc.

# logical subscripts must match size of indexed vector

    Code
      (expect_error(vec_as_location(c(TRUE, FALSE), 3), class = "vctrs_error_subscript_size")
      )
    Output
      <error/vctrs_error_subscript_size>
      Error:
      ! Must subset elements with a valid subscript vector.
      i Logical subscripts must match the size of the indexed input.
      x Input has size 3 but subscript `c(TRUE, FALSE)` has size 2.

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
      ! Must subset elements with a valid subscript vector.
      x Subscript `c(0, -1)` can't contain `0` values.
      i It has a `0` value at location 1.

---

    Code
      num_as_location(c(-1, 0), n = 2L, negative = "invert", zero = "error")
    Condition
      Error:
      ! Must subset elements with a valid subscript vector.
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
      ! Must subset elements with a valid subscript vector.
      x Subscript can't contain missing values.
      x It has a missing value at location 2.
    Code
      (expect_error(vec_as_location(c(1, NA, 2, NA), 2, missing = "error", arg = "foo",
      call = call("my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must subset elements with a valid subscript vector.
      x Subscript can't contain missing values.
      x It has missing values at locations 2 and 4.
    Code
      (expect_error(with_tibble_cols(vec_as_location(c(1, NA, 2, NA), 2, missing = "error")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must rename columns with a valid subscript vector.
      x Subscript `foo(bar)` can't contain missing values.
      x It has missing values at locations 2 and 4.
    Code
      (expect_error(with_tibble_cols(vec_as_location(NA, 1, missing = "error")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must rename columns with a valid subscript vector.
      x Subscript `foo(bar)` can't contain missing values.
      x It has a missing value at location 1.
    Code
      (expect_error(with_tibble_cols(vec_as_location(NA, 3, missing = "error")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must rename columns with a valid subscript vector.
      x Subscript `foo(bar)` can't contain missing values.
      x It has a missing value at location 1.
    Code
      (expect_error(with_tibble_cols(vec_as_location(c(TRUE, NA, FALSE), 3, missing = "error")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must rename columns with a valid subscript vector.
      x Subscript `foo(bar)` can't contain missing values.
      x It has a missing value at location 2.
    Code
      (expect_error(with_tibble_cols(vec_as_location(NA_character_, 2, missing = "error",
        names = c("x", "y"))), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must rename columns with a valid subscript vector.
      x Subscript `foo(bar)` can't contain missing values.
      x It has a missing value at location 1.

# can customise subscript type errors

    Code
      # With custom `arg`
      (expect_error(num_as_location(-1, 2, negative = "error", arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must subset elements with a valid subscript vector.
      x Subscript `foo` can't contain negative locations.
    Code
      (expect_error(num_as_location2(-1, 2, negative = "error", arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must extract element with a single valid subscript.
      x Subscript `foo` has value -1 but must be a positive location.
    Code
      (expect_error(vec_as_location2(0, 2, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must extract element with a single valid subscript.
      x Subscript `foo` has value 0 but must be a positive location.
    Code
      (expect_error(vec_as_location2(na_dbl, 2, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must extract element with a single valid subscript.
      x Subscript `foo` can't be `NA`.
    Code
      (expect_error(vec_as_location2(c(1, 2), 2, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must extract element with a single valid subscript.
      x Subscript `foo` has size 2 but must be size 1.
    Code
      (expect_error(vec_as_location(c(TRUE, FALSE), 3, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_size"))
    Output
      <error/vctrs_error_subscript_size>
      Error in `my_function()`:
      ! Must subset elements with a valid subscript vector.
      i Logical subscripts must match the size of the indexed input.
      x Input has size 3 but subscript `foo` has size 2.
    Code
      (expect_error(vec_as_location(c(-1, NA), 3, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must subset elements with a valid subscript vector.
      x Negative locations can't have missing values.
      i Subscript `foo` has a missing value at location 2.
    Code
      (expect_error(vec_as_location(c(-1, 1), 3, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must subset elements with a valid subscript vector.
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
      ! Must subset elements with a valid subscript vector.
      x Subscript `foo` can't contain `0` values.
      i It has a `0` value at location 1.
    Code
      # With tibble columns
      (expect_error(with_tibble_cols(num_as_location(-1, 2, negative = "error")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must rename columns with a valid subscript vector.
      x Subscript `foo(bar)` can't contain negative locations.
    Code
      (expect_error(with_tibble_cols(num_as_location2(-1, 2, negative = "error")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must rename column with a single valid subscript.
      x Subscript `foo(bar)` has value -1 but must be a positive location.
    Code
      (expect_error(with_tibble_cols(vec_as_location2(0, 2)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must rename column with a single valid subscript.
      x Subscript `foo(bar)` has value 0 but must be a positive location.
    Code
      (expect_error(with_tibble_cols(vec_as_location2(na_dbl, 2)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must rename column with a single valid subscript.
      x Subscript `foo(bar)` can't be `NA`.
    Code
      (expect_error(with_tibble_cols(vec_as_location2(c(1, 2), 2)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must rename column with a single valid subscript.
      x Subscript `foo(bar)` has size 2 but must be size 1.
    Code
      (expect_error(with_tibble_cols(vec_as_location(c(TRUE, FALSE), 3)), class = "vctrs_error_subscript_size")
      )
    Output
      <error/vctrs_error_subscript_size>
      Error:
      ! Must rename columns with a valid subscript vector.
      i Logical subscripts must match the size of the indexed input.
      x Input has size 3 but subscript `foo(bar)` has size 2.
    Code
      (expect_error(with_tibble_cols(vec_as_location(c(-1, NA), 3)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must rename columns with a valid subscript vector.
      x Negative locations can't have missing values.
      i Subscript `foo(bar)` has a missing value at location 2.
    Code
      (expect_error(with_tibble_cols(vec_as_location(c(-1, 1), 3)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must rename columns with a valid subscript vector.
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
      ! Must rename columns with a valid subscript vector.
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
      ! Must subset elements with a valid subscript vector.
      x Subscript `matrix(TRUE, nrow = 1)` must be a simple vector, not a matrix.
    Code
      (expect_error(vec_as_location(array(TRUE, dim = c(1, 1, 1)), 3L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript `array(TRUE, dim = c(1, 1, 1))` must be a simple vector, not an array.
    Code
      (expect_error(with_tibble_rows(vec_as_location(matrix(TRUE, nrow = 1), 3L)),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must remove rows with a valid subscript vector.
      x Subscript `foo(bar)` must be a simple vector, not a matrix.

# vec_as_location() UI

    Code
      vec_as_location(1, 1L, missing = "bogus")
    Condition
      Error in `vec_as_location()`:
      ! `missing` must be one of "propagate" or "error".

# num_as_location() UI

    Code
      num_as_location(1, 1L, missing = "bogus")
    Condition
      Error in `num_as_location()`:
      ! `missing` must be one of "propagate" or "error".

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
      ! `oob` must be one of "error" or "extend".

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

