# failing common type reports correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `my_function()`:
      ! Can't combine `2` <double> and `chr()` <character>.

# failing cast reports correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `my_function()`:
      ! Can't convert `2` <double> to match type of `chr()` <character>.

# lossy cast reports correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `my_function()`:
      ! Can't convert from `2` <double> to `lgl()` <logical> due to loss of precision.
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
      ! Can't convert from `1.5` <double> to `int()` <integer> due to loss of precision.
      * Locations: 1

---

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `my_function()`:
      ! Can't convert from `1.5` <double> to `lgl()` <logical> due to loss of precision.
      * Locations: 1

---

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `my_function()`:
      ! Can't convert from `2L` <integer> to `lgl()` <logical> due to loss of precision.
      * Locations: 1

---

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `my_function()`:
      ! Can't convert `matrix(TRUE)` <double[,1]> to match type of `dbl()` <double>.
      Cannot decrease dimensions.

# base S3 casts report correct error call

    Code
      (expect_error(my_function()))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `my_function()`:
      ! Can't convert from `"a"` <character> to `factor("b")` <factor<9b7e3>> due to loss of generality.
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
      Error in `vctrs::num_as_location()`:
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
      x Can't convert from `my_arg` <double> to <integer> due to loss of precision.

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
      x Subscript `my_arg` has the wrong type `list`.
      i It must be logical, numeric, or character.

---

    Code
      (expect_error(my_function(1.5)))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `vec_as_location()`:
      ! Can't convert from `n` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      (expect_error(my_function(NA)))
    Output
      <error/vctrs_error_subscript_type>
      Error in `my_function()`:
      ! Must subset elements with a valid subscript vector.
      x Subscript can't contain missing values.
      x It has a missing value at location 1.

# `vec_ptype()` reports correct error call

    Code
      (expect_error(my_function(env())))
    Output
      <error/vctrs_error_scalar_type>
      Error in `my_function()`:
      ! Input must be a vector, not an environment.
    Code
      (expect_error(my_function(foobar(list()))))
    Output
      <error/vctrs_error_scalar_type>
      Error in `my_function()`:
      ! Input must be a vector, not a <vctrs_foobar> object.

# can take ownership of vctrs errors

    Code
      (expect_error(vec_assert(foobar(list()))))
    Output
      <error/vctrs_error_scalar_type>
      Error in `foo()`:
      ! `foobar(list())` must be a vector, not a <vctrs_foobar> object.
    Code
      (expect_error(local(vec_assert(foobar(list())))))
    Output
      <error/vctrs_error_scalar_type>
      Error in `foo()`:
      ! `foobar(list())` must be a vector, not a <vctrs_foobar> object.
    Code
      (expect_error(vec_cast(1, list())))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `foo()`:
      ! Can't convert `1` <double> to match type of `list()` <list>.
    Code
      (expect_error(vec_slice(env(), list())))
    Output
      <error/vctrs_error_scalar_type>
      Error in `foo()`:
      ! `x` must be a vector, not an environment.
    Code
      local({
        vctrs_local_error_call(NULL)
        (expect_error(vec_slice(env(), list())))
      })
    Output
      <error/vctrs_error_scalar_type>
      Error in `vec_slice()`:
      ! `x` must be a vector, not an environment.

# vec_slice() reports error context

    Code
      (expect_error(vec_slice(foobar(list()), 1)))
    Output
      <error/vctrs_error_scalar_type>
      Error in `vec_slice()`:
      ! `x` must be a vector, not a <vctrs_foobar> object.
    Code
      (expect_error(vec_slice(list(), env())))
    Output
      <error/vctrs_error_subscript_type>
      Error in `vec_slice()`:
      ! Must subset elements with a valid subscript vector.
      x Subscript `i` has the wrong type `environment`.
      i It must be logical, numeric, or character.

# list_sizes() reports error context

    Code
      (expect_error(list_sizes(foobar(list()))))
    Output
      <error/rlang_error>
      Error in `list_sizes()`:
      ! `x` must be a list.
    Code
      (expect_error(list_sizes(list(env()))))
    Output
      <error/vctrs_error_scalar_type>
      Error in `list_sizes()`:
      ! `x[[1]]` must be a vector, not an environment.
    Code
      (expect_error(list_sizes(list(1, 2, env()))))
    Output
      <error/vctrs_error_scalar_type>
      Error in `list_sizes()`:
      ! `x[[3]]` must be a vector, not an environment.
    Code
      (expect_error(list_sizes(list(1, 2, foo = env()))))
    Output
      <error/vctrs_error_scalar_type>
      Error in `list_sizes()`:
      ! `x$foo` must be a vector, not an environment.

# vec_size() reports error context

    Code
      (expect_error(vec_size(env())))
    Output
      <error/vctrs_error_scalar_type>
      Error in `vec_size()`:
      ! `x` must be a vector, not an environment.

# vec_cast_common() reports error context

    Code
      (expect_error(my_function(my_arg = 1.5, .to = int())))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `my_function()`:
      ! Can't convert from `my_arg` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      (expect_error(my_function(my_arg = 1.5, .to = int(), .arg = "my_arg")))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `my_function()`:
      ! Can't convert from `my_arg$my_arg` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      (expect_error(my_function(this_arg = 1, that_arg = "foo", .arg = "my_arg")))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `my_function()`:
      ! Can't combine `my_arg$this_arg` <double> and `my_arg$that_arg` <character>.

---

    Code
      (expect_error(my_function(1, "foo", .arg = "my_arg")))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `my_function()`:
      ! Can't combine `my_arg[[1]]` <double> and `my_arg[[2]]` <character>.

---

    Code
      (expect_error(my_function(this_arg = x, that_arg = y)))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `my_function()`:
      ! Can't combine `this_arg$x` <character> and `that_arg$x` <double>.

# vec_ptype_common() reports error context

    Code
      (expect_error(my_function(this_arg = 1, that_arg = "foo")))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `my_function()`:
      ! Can't combine `this_arg` <double> and `that_arg` <character>.

---

    Code
      (expect_error(my_function(this_arg = 1, that_arg = "foo", .arg = "my_arg")))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `my_function()`:
      ! Can't combine `my_arg$this_arg` <double> and `my_arg$that_arg` <character>.

---

    Code
      (expect_error(my_function(1, "foo", .arg = "my_arg")))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `my_function()`:
      ! Can't combine `my_arg[[1]]` <double> and `my_arg[[2]]` <character>.

