# no casting is done

    Code
      vec_pall(1)
    Condition
      Error in `vec_pall()`:
      ! `..1` must be a logical vector, not the number 1.

---

    Code
      vec_pany(1)
    Condition
      Error in `vec_pany()`:
      ! `..1` must be a logical vector, not the number 1.

---

    Code
      vec_pall(array(TRUE))
    Condition
      Error in `vec_pall()`:
      ! `..1` must be a logical vector, not a logical 1D array.

---

    Code
      vec_pany(array(TRUE))
    Condition
      Error in `vec_pany()`:
      ! `..1` must be a logical vector, not a logical 1D array.

---

    Code
      vec_pall(structure(TRUE, class = "foo"))
    Condition
      Error in `vec_pall()`:
      ! `..1` must be a logical vector, not a <foo> object.

---

    Code
      vec_pany(structure(TRUE, class = "foo"))
    Condition
      Error in `vec_pany()`:
      ! `..1` must be a logical vector, not a <foo> object.

# no recycling is done

    Code
      vec_pall(TRUE, c(TRUE, TRUE, TRUE))
    Condition
      Error in `vec_pall()`:
      ! `..2` must have size 1, not size 3.

---

    Code
      vec_pany(TRUE, c(TRUE, TRUE, TRUE))
    Condition
      Error in `vec_pany()`:
      ! `..2` must have size 1, not size 3.

---

    Code
      vec_pall(TRUE, c(TRUE, TRUE, TRUE), .size = 3L)
    Condition
      Error in `vec_pall()`:
      ! `..1` must have size 3, not size 1.

---

    Code
      vec_pany(TRUE, c(TRUE, TRUE, TRUE), .size = 3L)
    Condition
      Error in `vec_pany()`:
      ! `..1` must have size 3, not size 1.

# validates `.missing`

    Code
      vec_pall(.missing = c(TRUE, FALSE))
    Condition
      Error in `vec_pall()`:
      ! `.missing` must be `NA`, `FALSE`, or `TRUE`.

---

    Code
      vec_pany(.missing = c(TRUE, FALSE))
    Condition
      Error in `vec_pany()`:
      ! `.missing` must be `NA`, `FALSE`, or `TRUE`.

---

    Code
      vec_pall(.missing = 1)
    Condition
      Error in `vec_pall()`:
      ! `.missing` must be `NA`, `FALSE`, or `TRUE`.

---

    Code
      vec_pany(.missing = 1)
    Condition
      Error in `vec_pany()`:
      ! `.missing` must be `NA`, `FALSE`, or `TRUE`.

---

    Code
      vec_pall(.missing = NULL)
    Condition
      Error in `vec_pall()`:
      ! `.missing` must be `NA`, `FALSE`, or `TRUE`.

---

    Code
      vec_pany(.missing = NULL)
    Condition
      Error in `vec_pany()`:
      ! `.missing` must be `NA`, `FALSE`, or `TRUE`.

# validates `.size`

    Code
      vec_pall(.size = c(1, 2))
    Condition
      Error in `vec_pall()`:
      ! `.size` must be a scalar integer or double.

---

    Code
      vec_pany(.size = c(1, 2))
    Condition
      Error in `vec_pany()`:
      ! `.size` must be a scalar integer or double.

---

    Code
      vec_pall(.size = 1.5)
    Condition
      Error in `vec_pall()`:
      ! `.size` must be a whole number, not a decimal number.

---

    Code
      vec_pany(.size = 1.5)
    Condition
      Error in `vec_pany()`:
      ! `.size` must be a whole number, not a decimal number.

---

    Code
      vec_pall(.size = NA_integer_)
    Condition
      Error in `vec_pall()`:
      ! negative length vectors are not allowed

---

    Code
      vec_pany(.size = NA_integer_)
    Condition
      Error in `vec_pany()`:
      ! negative length vectors are not allowed

# names are used in errors

    Code
      vec_pall(1.5, .arg = "x")
    Condition
      Error in `vec_pall()`:
      ! `x[[1]]` must be a logical vector, not the number 1.5.

---

    Code
      vec_pall(a = 1.5, .arg = "x")
    Condition
      Error in `vec_pall()`:
      ! `x$a` must be a logical vector, not the number 1.5.

---

    Code
      x <- c(TRUE, FALSE)
      y <- logical()
      vec_pany(x, y)
    Condition
      Error in `vec_pany()`:
      ! `..2` must have size 2, not size 0.

---

    Code
      x <- c(TRUE, FALSE)
      y <- logical()
      vec_pany(a = x, b = y, .arg = "x", .error_call = quote(foo()))
    Condition
      Error in `foo()`:
      ! `x$b` must have size 2, not size 0.

