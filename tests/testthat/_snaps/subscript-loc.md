# vec_as_location2() requires integer or character inputs

    Code
      (expect_error(vec_as_location2(TRUE, 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript has the wrong type `logical`.
      i It must be numeric or character.
      Caused by error:
      ! Must extract element with a single valid subscript.
      x Subscript has the wrong type `logical`.
      i It must be numeric or character.
    Code
      (expect_error(vec_as_location2(mtcars, 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript has the wrong type `data.frame<
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
      Caused by error:
      ! Must extract element with a single valid subscript.
      x Subscript has the wrong type `data.frame<
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
      x Subscript has the wrong type `environment`.
      i It must be numeric or character.
      Caused by error:
      ! Must extract element with a single valid subscript.
      x Subscript has the wrong type `environment`.
      i It must be numeric or character.
    Code
      (expect_error(vec_as_location2(foobar(), 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript has the wrong type `vctrs_foobar`.
      i It must be numeric or character.
      Caused by error:
      ! Must extract element with a single valid subscript.
      x Subscript has the wrong type `vctrs_foobar`.
      i It must be numeric or character.
    Code
      (expect_error(vec_as_location2(2.5, 10L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Can't convert from <double> to <integer> due to loss of precision.
      Caused by error:
      ! Must extract element with a single valid subscript.
      x Can't convert from <double> to <integer> due to loss of precision.
      Caused by error:
      ! Can't convert from <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      (expect_error(vec_as_location2(Inf, 10L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Can't convert from <double> to <integer> due to loss of precision.
      Caused by error:
      ! Must extract element with a single valid subscript.
      x Can't convert from <double> to <integer> due to loss of precision.
      Caused by error:
      ! Can't convert from <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      (expect_error(vec_as_location2(-Inf, 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Can't convert from <double> to <integer> due to loss of precision.
      Caused by error:
      ! Must extract element with a single valid subscript.
      x Can't convert from <double> to <integer> due to loss of precision.
      Caused by error:
      ! Can't convert from <double> to <integer> due to loss of precision.
      * Locations: 1
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
      Caused by error:
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
      Caused by error:
      ! Must extract element with a single valid subscript.
      x Can't convert from `foo` <double> to <integer> due to loss of precision.
      Caused by error:
      ! Can't convert from `foo` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      (expect_error(with_tibble_rows(vec_as_location2(TRUE)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must remove row with a single valid subscript.
      x Subscript `foo(bar)` has the wrong type `logical`.
      i It must be numeric or character.
      Caused by error:
      ! Must extract element with a single valid subscript.
      x Subscript has the wrong type `logical`.
      i It must be numeric or character.

# vec_as_location() requires integer, character, or logical inputs

    Code
      (expect_error(vec_as_location(mtcars, 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `data.frame<
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
      x Subscript has the wrong type `environment`.
      i It must be logical, numeric, or character.
    Code
      (expect_error(vec_as_location(foobar(), 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `vctrs_foobar`.
      i It must be logical, numeric, or character.
    Code
      (expect_error(vec_as_location(2.5, 10L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Can't convert from <double> to <integer> due to loss of precision.
      Caused by error:
      ! Can't convert from <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      (expect_error(vec_as_location(list(), 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `list`.
      i It must be logical, numeric, or character.
    Code
      (expect_error(vec_as_location(function() NULL, 10L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `function`.
      i It must be logical, numeric, or character.
    Code
      (expect_error(vec_as_location(Sys.Date(), 3L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `date`.
      i It must be logical, numeric, or character.
    Code
      # Idem with custom `arg`
      (expect_error(vec_as_location(env(), 10L, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript `foo` has the wrong type `environment`.
      i It must be logical, numeric, or character.
    Code
      (expect_error(vec_as_location(foobar(), 10L, arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript `foo` has the wrong type `vctrs_foobar`.
      i It must be logical, numeric, or character.
    Code
      (expect_error(vec_as_location(2.5, 3L, arg = "foo", call = call("my_function")),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Can't convert from `foo` <double> to <integer> due to loss of precision.
      Caused by error:
      ! Can't convert from `foo` <double> to <integer> due to loss of precision.
      * Locations: 1

# vec_as_location() and variants check for OOB elements

    Code
      # Numeric indexing
      (expect_error(vec_as_location(10L, 2L), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements that don't exist.
      x Location 10 doesn't exist.
      i There are only 2 elements.
    Code
      (expect_error(vec_as_location(-10L, 2L), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't negate elements that don't exist.
      x Location 10 doesn't exist.
      i There are only 2 elements.
    Code
      (expect_error(vec_as_location2(10L, 2L), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_as_location2_result()`:
      ! Can't subset elements that don't exist.
      x Location 10 doesn't exist.
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
      Error in `vec_as_location2_result()`:
      ! Can't subset elements that don't exist.
      x Element `foo` doesn't exist.

# vec_as_location2() requires length 1 inputs

    Code
      (expect_error(vec_as_location2(1:2, 2L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript has size 2 but must be size 1.
    Code
      (expect_error(vec_as_location2(c("foo", "bar"), 2L, c("foo", "bar")), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript has size 2 but must be size 1.
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
      Caused by error:
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
      x Subscript has value 0 but must be a positive location.
    Code
      (expect_error(vec_as_location2(-1, 2L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript has value -1 but must be a positive location.
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
      x Subscript can't be `NA`.
    Code
      (expect_error(vec_as_location2(na_chr, 1L, names = "foo"), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript can't be `NA`.
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
      x Subscript can't contain negative locations.

# num_as_location() optionally forbids zero indices

    Code
      (expect_error(num_as_location(0L, 1L, zero = "error"), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript can't contain `0` values.
      i It has a `0` value at location 1.
    Code
      (expect_error(num_as_location(c(0, 0, 0, 0, 0, 0), 1, zero = "error"), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript can't contain `0` values.
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
      i Subscript has a missing value at location 2.
    Code
      (expect_error(vec_as_location(-c(1L, rep(NA, 10)), 30), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Negative locations can't have missing values.
      i Subscript has 10 missing values at locations 2, 3, 4, 5, 6, etc.

# vec_as_location() checks for mix of negative and positive locations

    Code
      (expect_error(vec_as_location(c(-1L, 1L), 30), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Negative and positive locations can't be mixed.
      i Subscript has a positive value at location 2.
    Code
      (expect_error(vec_as_location(c(-1L, rep(1L, 10)), 30), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Negative and positive locations can't be mixed.
      i Subscript has 10 positive values at locations 2, 3, 4, 5, 6, etc.

# logical subscripts must match size of indexed vector

    Code
      (expect_error(vec_as_location(c(TRUE, FALSE), 3), class = "vctrs_error_subscript_size")
      )
    Output
      <error/vctrs_error_subscript_size>
      Error:
      ! Must subset elements with a valid subscript vector.
      i Logical subscripts must match the size of the indexed input.
      x Input has size 3 but subscript has size 2.

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
      x Subscript contains non-consecutive location 3.
    Code
      (expect_error(num_as_location(c(1, 3), 1, oob = "extend"), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements beyond the end with non-consecutive locations.
      i Input has size 1.
      x Subscript contains non-consecutive location 3.
    Code
      (expect_error(num_as_location(c(1:5, 7), 3, oob = "extend"), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements beyond the end with non-consecutive locations.
      i Input has size 3.
      x Subscript contains non-consecutive locations 4 and 7.
    Code
      (expect_error(num_as_location(c(1:5, 7, 1), 3, oob = "extend"), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements beyond the end with non-consecutive locations.
      i Input has size 3.
      x Subscript contains non-consecutive locations 4 and 7.
    Code
      (expect_error(class = "vctrs_error_subscript_oob", num_as_location(c(1:5, 7, 1,
      10), 3, oob = "extend")))
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements beyond the end with non-consecutive locations.
      i Input has size 3.
      x Subscript contains non-consecutive locations 4, 7, and 10.

# missing values are supported in error formatters

    Code
      (expect_error(num_as_location(c(1, NA, 2, 3), 1), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements that don't exist.
      x Locations 2 and 3 don't exist.
      i There are only 1 element.
    Code
      (expect_error(num_as_location(c(1, NA, 3), 1, oob = "extend"), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements beyond the end with non-consecutive locations.
      i Input has size 1.
      x Subscript contains non-consecutive location 3.

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
      Error:
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
      Error:
      ! Can't subset elements that don't exist.
      x Element `foo` doesn't exist.
    Code
      # With custom `arg`
      (expect_error(vec_as_location(30, length(letters), arg = "foo", call = call(
        "my_function")), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `my_function()`:
      ! Can't subset elements that don't exist.
      x Location 30 doesn't exist.
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
      Error:
      ! Can't rename columns that don't exist.
      x Column `foo` doesn't exist.
    Code
      (expect_error(with_tibble_cols(vec_slice(set_names(letters), 30)), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't rename columns that don't exist.
      x Location 30 doesn't exist.
      i There are only 26 columns.
    Code
      (expect_error(with_tibble_cols(vec_slice(set_names(letters), -30)), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't rename columns that don't exist.
      x Location 30 doesn't exist.
      i There are only 26 columns.
    Code
      # With tibble rows
      (expect_error(with_tibble_rows(vec_slice(set_names(letters), c("foo", "bar"))),
      class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't remove rows that don't exist.
      x Rows `foo` and `bar` don't exist.
    Code
      (expect_error(with_tibble_rows(vec_slice(set_names(letters), 1:30)), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't remove rows that don't exist.
      x Locations 27, 28, 29, and 30 don't exist.
      i There are only 26 rows.
    Code
      (expect_error(with_tibble_rows(vec_slice(set_names(letters), -(1:30))), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't remove rows that don't exist.
      x Locations 27, 28, 29, and 30 don't exist.
      i There are only 26 rows.

# vec_as_location() checks dimensionality

    Code
      (expect_error(vec_as_location(matrix(TRUE, nrow = 1), 3L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript must be a simple vector, not a matrix.
    Code
      (expect_error(vec_as_location(array(TRUE, dim = c(1, 1, 1)), 3L), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript must be a simple vector, not an array.
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
      Error:
      ! `missing` must be one of "propagate" or "error".

# num_as_location() UI

    Code
      num_as_location(1, 1L, missing = "bogus")
    Condition
      Error:
      ! `missing` must be one of "propagate" or "error".

---

    Code
      num_as_location(1, 1L, negative = "bogus")
    Condition
      Error:
      ! `negative` must be one of "invert", "error", or "ignore".

---

    Code
      num_as_location(1, 1L, oob = "bogus")
    Condition
      Error:
      ! `oob` must be one of "error" or "extend".

---

    Code
      num_as_location(1, 1L, zero = "bogus")
    Condition
      Error:
      ! `zero` must be one of "remove", "error", or "ignore".

# vec_as_location2() UI

    Code
      vec_as_location2(1, 1L, missing = "bogus")
    Condition
      Error in `vec_as_location2_result()`:
      ! `missing` must be one of "error" or "propagate", not "bogus".

