# failing common type reports correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `my_function()`:
      ! Can't combine <double> and <character>.

# failing cast reports correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `my_function()`:
      ! Can't convert <double> to <character>.

# lossy cast reports correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `my_function()`:
      ! Can't convert from <double> to <logical> due to loss of precision.
      * Locations: 1

# failing common size reports correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `my_function()`:
      ! Can't recycle input of size 2 to size 10.

---

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_incompatible_size>
      Error:
      ! Can't recycle `..1` (size 2) to match `..2` (size 10).

# unsupported error reports correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_unsupported>
      Error in `dim<-`:
      ! `dim<-.vctrs_vctr()` not supported.

---

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_unimplemented>
      Error in `median()`:
      ! `median.vctrs_vctr()` not implemented.

# scalar error reports correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_scalar_type>
      Error in `my_function()`:
      ! `foobar()` must be a vector, not a <vctrs_foobar> object.

---

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_assert_ptype>
      Error in `my_function()`:
      ! `1:2` must be a vector with type <double>.
      Instead, it has type <integer>.

---

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_assert_size>
      Error in `my_function()`:
      ! `1:2` must have size 1, not size 2.

# bare casts report correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `my_function()`:
      ! Can't convert from <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `my_function()`:
      ! Can't convert from <double> to <logical> due to loss of precision.
      * Locations: 1

---

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `my_function()`:
      ! Can't convert from <integer> to <logical> due to loss of precision.
      * Locations: 1

---

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `my_function()`:
      ! Can't convert <double[,1]> to <double>.
      Cannot decrease dimensions.

# base S3 casts report correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `my_function()`:
      ! Can't convert from <character> to <factor<9b7e3>> due to loss of generality.
      * Locations: 1

# names validation reports correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_names_cannot_be_empty>
      Error in `my_function()`:
      ! Names can't be empty.
      x Empty name found at location 2.

---

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_names_must_be_unique>
      Error in `my_function()`:
      ! Names must be unique.
      x These names are duplicated:
        * "x" at locations 1 and 2.
      i Use argument `repair` to specify repair strategy.

---

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_names_cannot_be_dot_dot>
      Error in `my_function()`:
      ! Names can't be of the form `...` or `..j`.
      x These names are invalid:
        * "..." at location 1.

# subscript validation reports correct error calls

    Code
      (expect_error(my_function()))
    Output
      <error/rlang_error>
      Error in `my_function()`:
      ! `missing` must be one of "propagate" or "error".

---

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `my_function()`:
      ! Can't subset elements that don't exist.
      x Location 10 doesn't exist.
      i There are only 2 elements.

---

    Code
      (expect_error(my_function(1.5)))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must subset elements with a valid subscript vector.
      x Can't convert from <double> to <integer> due to loss of precision.

---

    Code
      (expect_error(my_function(1.5)))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must subset elements with a valid subscript vector.
      x Can't convert from <double> to <integer> due to loss of precision.

---

    Code
      (expect_error(my_function(list())))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `list`.
      i It must be logical, numeric, or character.

