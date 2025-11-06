# no casting is done

    Code
      list_pall(list(1))
    Condition
      Error in `list_pall()`:
      ! `list(1)[[1]]` must be a logical vector, not the number 1.

---

    Code
      list_pany(list(1))
    Condition
      Error in `list_pany()`:
      ! `list(1)[[1]]` must be a logical vector, not the number 1.

---

    Code
      list_pall(list(array(TRUE)))
    Condition
      Error in `list_pall()`:
      ! `list(array(TRUE))[[1]]` must be a logical vector, not a logical 1D array.

---

    Code
      list_pany(list(array(TRUE)))
    Condition
      Error in `list_pany()`:
      ! `list(array(TRUE))[[1]]` must be a logical vector, not a logical 1D array.

---

    Code
      list_pall(list(structure(TRUE, class = "foo")))
    Condition
      Error in `list_pall()`:
      ! `list(structure(TRUE, class = "foo"))[[1]]` must be a logical vector, not a <foo> object.

---

    Code
      list_pany(list(structure(TRUE, class = "foo")))
    Condition
      Error in `list_pany()`:
      ! `list(structure(TRUE, class = "foo"))[[1]]` must be a logical vector, not a <foo> object.

# no recycling is done

    Code
      list_pall(list(TRUE, c(TRUE, TRUE, TRUE)))
    Condition
      Error in `list_pall()`:
      ! `list(TRUE, c(TRUE, TRUE, TRUE))[[2]]` must have size 1, not size 3.

---

    Code
      list_pany(list(TRUE, c(TRUE, TRUE, TRUE)))
    Condition
      Error in `list_pany()`:
      ! `list(TRUE, c(TRUE, TRUE, TRUE))[[2]]` must have size 1, not size 3.

---

    Code
      list_pall(list(TRUE, c(TRUE, TRUE, TRUE)), size = 3L)
    Condition
      Error in `list_pall()`:
      ! `list(TRUE, c(TRUE, TRUE, TRUE))[[1]]` must have size 3, not size 1.

---

    Code
      list_pany(list(TRUE, c(TRUE, TRUE, TRUE)), size = 3L)
    Condition
      Error in `list_pany()`:
      ! `list(TRUE, c(TRUE, TRUE, TRUE))[[1]]` must have size 3, not size 1.

# validates `x`

    Code
      list_pall(1)
    Condition
      Error in `list_pall()`:
      ! `1` must be a list, not the number 1.

---

    Code
      list_pany(1)
    Condition
      Error in `list_pany()`:
      ! `1` must be a list, not the number 1.

---

    Code
      list_pall(1, x_arg = "")
    Condition
      Error in `list_pall()`:
      ! Input must be a list, not the number 1.

---

    Code
      list_pany(1, x_arg = "")
    Condition
      Error in `list_pany()`:
      ! Input must be a list, not the number 1.

---

    Code
      list_pall(1, x_arg = "foo")
    Condition
      Error in `list_pall()`:
      ! `foo` must be a list, not the number 1.

---

    Code
      list_pany(1, x_arg = "foo")
    Condition
      Error in `list_pany()`:
      ! `foo` must be a list, not the number 1.

---

    Code
      list_pall(data_frame(x = 1))
    Condition
      Error in `list_pall()`:
      ! `data_frame(x = 1)` must be a list, not a <data.frame> object.

---

    Code
      list_pany(data_frame(x = 1))
    Condition
      Error in `list_pany()`:
      ! `data_frame(x = 1)` must be a list, not a <data.frame> object.

# validates `missing`

    Code
      list_pall(list(), missing = c(TRUE, FALSE))
    Condition
      Error in `list_pall()`:
      ! `missing` must be `NULL`, `TRUE`, or `FALSE`.

---

    Code
      list_pany(list(), missing = c(TRUE, FALSE))
    Condition
      Error in `list_pany()`:
      ! `missing` must be `NULL`, `TRUE`, or `FALSE`.

---

    Code
      list_pall(list(), missing = 1)
    Condition
      Error in `list_pall()`:
      ! `missing` must be `NULL`, `TRUE`, or `FALSE`.

---

    Code
      list_pany(list(), missing = 1)
    Condition
      Error in `list_pany()`:
      ! `missing` must be `NULL`, `TRUE`, or `FALSE`.

---

    Code
      list_pall(list(), missing = NA)
    Condition
      Error in `list_pall()`:
      ! `missing` must be `NULL`, `TRUE`, or `FALSE`.

---

    Code
      list_pany(list(), missing = NA)
    Condition
      Error in `list_pany()`:
      ! `missing` must be `NULL`, `TRUE`, or `FALSE`.

# validates `size`

    Code
      list_pall(list(), size = c(1, 2))
    Condition
      Error in `list_pall()`:
      ! `size` must be a scalar integer or double.

---

    Code
      list_pany(list(), size = c(1, 2))
    Condition
      Error in `list_pany()`:
      ! `size` must be a scalar integer or double.

---

    Code
      list_pall(list(), size = 1.5)
    Condition
      Error in `list_pall()`:
      ! `size` must be a whole number, not a decimal number.

---

    Code
      list_pany(list(), size = 1.5)
    Condition
      Error in `list_pany()`:
      ! `size` must be a whole number, not a decimal number.

---

    Code
      list_pall(list(), size = NA_integer_)
    Condition
      Error in `list_pall()`:
      ! negative length vectors are not allowed

---

    Code
      list_pany(list(), size = NA_integer_)
    Condition
      Error in `list_pany()`:
      ! negative length vectors are not allowed

# names are used in errors

    Code
      foo <- list(x = 1.5)
      list_pall(foo)
    Condition
      Error in `list_pall()`:
      ! `foo$x` must be a logical vector, not the number 1.5.

---

    Code
      foo <- list(x = c(TRUE, FALSE), y = logical())
      list_pany(foo)
    Condition
      Error in `list_pany()`:
      ! `foo$y` must have size 2, not size 0.

