# vec_slice throws error with non-vector subscripts

    Code
      (expect_error(vec_slice(1:3, Sys.Date()), class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `date`.
      i It must be logical, numeric, or character.
    Code
      (expect_error(vec_slice(1:3, matrix(TRUE, nrow = 1)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Must subset elements with a valid subscript vector.
      x Subscript must be a simple vector, not a matrix.

# can't index beyond the end of a vector

    Code
      (expect_error(vec_slice(1:2, 3L), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Can't subset elements that don't exist.
      x Location 3 doesn't exist.
      i There are only 2 elements.
    Code
      (expect_error(vec_slice(1:2, -3L), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Can't negate elements that don't exist.
      x Location 3 doesn't exist.
      i There are only 2 elements.

# Unnamed vector with character subscript is caught

    Code
      vec_slice(1:3, letters[1])
    Error <simpleError>
      Can't use character names to index an unnamed vector.

# Negative subscripts are checked

    Code
      vec_slice(1:3, -c(1L, NA))
    Error <vctrs_error_subscript_type>
      Must subset elements with a valid subscript vector.
      x Negative locations can't have missing values.
      i Subscript has a missing value at location 2.

---

    Code
      vec_slice(1:3, c(-1L, 1L))
    Error <vctrs_error_subscript_type>
      Must subset elements with a valid subscript vector.
      x Negative and positive locations can't be mixed.
      i Subscript has a positive value at location 2.

# oob error messages are properly constructed

    Code
      vec_slice(c(bar = 1), "foo")
    Error <vctrs_error_subscript_oob>
      Can't subset elements that don't exist.
      x Element `foo` doesn't exist.

---

    Code
      vec_slice(letters, c(100, 1000))
    Error <vctrs_error_subscript_oob>
      Can't subset elements that don't exist.
      x Locations 100 and 1000 don't exist.
      i There are only 26 elements.

---

    Code
      vec_slice(letters, c(1, 100:103, 2, 104:110))
    Error <vctrs_error_subscript_oob>
      Can't subset elements that don't exist.
      x Locations 100, 101, 102, 103, 104, etc. don't exist.
      i There are only 26 elements.

---

    Code
      vec_slice(set_names(letters), c("foo", "bar"))
    Error <vctrs_error_subscript_oob>
      Can't subset elements that don't exist.
      x Elements `foo` and `bar` don't exist.

---

    Code
      vec_slice(set_names(letters), toupper(letters))
    Error <vctrs_error_subscript_oob>
      Can't subset elements that don't exist.
      x Elements `A`, `B`, `C`, `D`, `E`, etc. don't exist.

