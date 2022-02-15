# vec_slice throws error with non-vector subscripts

    Code
      (expect_error(vec_slice(1:3, Sys.Date()), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `vec_slice()`:
      ! Must subset elements with a valid subscript vector.
      x Subscript `i` has the wrong type `date`.
      i It must be logical, numeric, or character.
    Code
      (expect_error(vec_slice(1:3, matrix(TRUE, nrow = 1)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error in `vec_slice()`:
      ! Must subset elements with a valid subscript vector.
      x Subscript `i` must be a simple vector, not a matrix.

# can't index beyond the end of a vector

    Code
      (expect_error(vec_slice(1:2, 3L), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't subset elements past the end.
      i Location 3 doesn't exist.
      i There are only 2 elements.
    Code
      (expect_error(vec_slice(1:2, -3L), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't negate elements past the end.
      i Location 3 doesn't exist.
      i There are only 2 elements.

# can slice with double indices

    Code
      (expect_error(vec_as_location(2^31, 3L), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Can't convert from `2^31` <double> to <integer> due to loss of precision.

# Unnamed vector with character subscript is caught

    Code
      vec_slice(1:3, letters[1])
    Condition
      Error in `vec_slice()`:
      ! Can't use character names to index an unnamed vector.

# Negative subscripts are checked

    Code
      vec_slice(1:3, -c(1L, NA))
    Condition
      Error in `vec_slice()`:
      ! Must subset elements with a valid subscript vector.
      x Negative locations can't have missing values.
      i Subscript `i` has a missing value at location 2.

---

    Code
      vec_slice(1:3, c(-1L, 1L))
    Condition
      Error in `vec_slice()`:
      ! Must subset elements with a valid subscript vector.
      x Negative and positive locations can't be mixed.
      i Subscript `i` has a positive value at location 2.

# oob error messages are properly constructed

    Code
      vec_slice(c(bar = 1), "foo")
    Condition
      Error in `vec_slice()`:
      ! Can't subset elements past the end.
      x Element `foo` doesn't exist.

---

    Code
      vec_slice(letters, c(100, 1000))
    Condition
      Error in `vec_slice()`:
      ! Can't subset elements past the end.
      i Locations 100 and 1000 don't exist.
      i There are only 26 elements.

---

    Code
      vec_slice(letters, c(1, 100:103, 2, 104:110))
    Condition
      Error in `vec_slice()`:
      ! Can't subset elements past the end.
      i Locations 100, 101, 102, 103, 104, ... don't exist.
      i There are only 26 elements.

---

    Code
      vec_slice(set_names(letters), c("foo", "bar"))
    Condition
      Error in `vec_slice()`:
      ! Can't subset elements past the end.
      x Elements `foo` and `bar` don't exist.

---

    Code
      vec_slice(set_names(letters), toupper(letters))
    Condition
      Error in `vec_slice()`:
      ! Can't subset elements past the end.
      x Elements `A`, `B`, `C`, `D`, `E`, etc. don't exist.

# vec_init() asserts vectorness (#301)

    Code
      (expect_error(vec_init(NULL, 1L), class = "vctrs_error_scalar_type"))
    Output
      <error/vctrs_error_scalar_type>
      Error in `vec_init()`:
      ! Input must be a vector, not NULL.

# vec_init() validates `n`

    Code
      (expect_error(vec_init(1L, 1.5), class = "vctrs_error_cast_lossy"))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `vec_init()`:
      ! Can't convert from `n` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      (expect_error(vec_init(1L, c(1, 2))))
    Output
      <error/vctrs_error_assert_size>
      Error:
      ! `n` must have size 1, not size 2.
    Code
      (expect_error(vec_init(1L, -1L)))
    Output
      <error/rlang_error>
      Error in `vec_init()`:
      ! `n` must be a positive integer.
    Code
      (expect_error(vec_init(1L, NA_integer_)))
    Output
      <error/rlang_error>
      Error in `vec_init()`:
      ! `n` must be a positive integer.

