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
      ! Can't subset elements with `i`.
      x Location must be less than or equal to 2, not 3.
      i There are only 2 elements.
    Code
      (expect_error(vec_slice(1:2, -3L), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `vec_slice()`:
      ! Can't negate elements with `i`.
      x Location must be less than or equal to 2, not 3.
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
      ! Can't subset elements with `i`.
      x Can't find element `foo`.

---

    Code
      vec_slice(letters, c(100, 1000))
    Condition
      Error in `vec_slice()`:
      ! Can't subset elements with `i`.
      x Locations must be less than or equal to 26.
      x Larger locations: 100 and 1000
      i There are only 26 elements.

---

    Code
      vec_slice(letters, c(1, 100:103, 2, 104:110))
    Condition
      Error in `vec_slice()`:
      ! Can't subset elements with `i`.
      x Locations must be less than or equal to 26.
      x Larger locations: 100, 101, 102, ..., 109, and 110
      i There are only 26 elements.

---

    Code
      vec_slice(set_names(letters), c("foo", "bar"))
    Condition
      Error in `vec_slice()`:
      ! Can't subset elements with `i`.
      x Can't find elements `foo` and `bar`.

---

    Code
      vec_slice(set_names(letters), toupper(letters))
    Condition
      Error in `vec_slice()`:
      ! Can't subset elements with `i`.
      x Can't find elements `A`, `B`, `C`, `D`, `E`, etc..

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

