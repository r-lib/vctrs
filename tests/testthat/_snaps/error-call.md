# failing common type reports correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `vec_default_ptype2()`:
      ! Can't combine <double> and <character>.

# failing cast reports correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `vec_default_cast()`:
      ! Can't convert <double> to <character>.

# lossy cast reports correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `vec_cast.logical.double()`:
      ! Can't convert from <double> to <logical> due to loss of precision.
      * Locations: 1

# failing common size reports correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_incompatible_size>
      Error:
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

