# vec_slice throws error with non-vector subscripts

    Code
      (expect_error(vec_slice(1:3, Sys.Date()), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error in `vec_slice()`:
      ! Can't subset elements with `i`.
      x `i` must be logical, numeric, or character, not a <Date> object.
    Code
      (expect_error(vec_slice(1:3, matrix(TRUE, nrow = 1)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error in `vec_slice()`:
      ! Can't subset elements with `i`.
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
      ! Can't subset elements with `2^31`.
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
      ! Can't subset elements with `i`.
      x Negative locations can't have missing values.
      i Subscript `i` has a missing value at location 2.

---

    Code
      vec_slice(1:3, c(-1L, 1L))
    Condition
      Error in `vec_slice()`:
      ! Can't subset elements with `i`.
      x Negative and positive locations can't be mixed.
      i Subscript `i` has a positive value at location 2.

# oob error messages are properly constructed

    Code
      vec_slice(c(bar = 1), "foo")
    Condition
      Error in `vec_slice()`:
      ! Can't subset elements that don't exist.
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
      i Locations 100, 101, 102, ..., 109, and 110 don't exist.
      i There are only 26 elements.

---

    Code
      vec_slice(set_names(letters), c("foo", "bar"))
    Condition
      Error in `vec_slice()`:
      ! Can't subset elements that don't exist.
      x Elements `foo` and `bar` don't exist.

---

    Code
      vec_slice(set_names(letters), toupper(letters))
    Condition
      Error in `vec_slice()`:
      ! Can't subset elements that don't exist.
      x Elements `A`, `B`, `C`, `D`, `E`, etc. don't exist.

# vec_init() validates `n`

    Code
      (expect_error(vec_init(1L, 1.5)))
    Output
      <error/rlang_error>
      Error in `vec_init()`:
      ! `n` must be a whole number, not a fractional number.
    Code
      (expect_error(vec_init(1L, c(1, 2))))
    Output
      <error/rlang_error>
      Error in `vec_init()`:
      ! `n` must be a single number, not a double vector of length 2.
    Code
      (expect_error(vec_init(1L, -1L)))
    Output
      <error/rlang_error>
      Error in `vec_init()`:
      ! `n` must be a positive number or zero.
    Code
      (expect_error(vec_init(1L, NA)))
    Output
      <error/rlang_error>
      Error in `vec_init()`:
      ! `n` must be a single number, not `NA`.
    Code
      (expect_error(vec_init(1L, NA_integer_)))
    Output
      <error/rlang_error>
      Error in `vec_init()`:
      ! `n` must be a single number, not an integer `NA`.

