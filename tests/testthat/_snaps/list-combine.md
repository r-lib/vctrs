# `x_arg` works

    Code
      list_combine(list(1, "2"), list(1, 2), x_arg = "xs")
    Condition
      Error in `list_combine()`:
      ! Can't combine `xs[[1]]` <double> and `xs[[2]]` <character>.

---

    Code
      list_combine(list(1, 2), list(1, 2, 3), x_arg = "xs")
    Condition
      Error in `list_combine()`:
      ! `xs` (size 2) and `indices` (size 3) must be lists of the same size.

# `indices_arg` works

    Code
      list_combine(list(1, 2), 1, indices_arg = "i")
    Condition
      Error in `list_combine()`:
      ! `i` must be a list, not the number 1.

---

    Code
      list_combine(list(1, 2), list(1, 2, 3), indices_arg = "i")
    Condition
      Error in `list_combine()`:
      ! `x` (size 2) and `i` (size 3) must be lists of the same size.

# `...` must be empty

    Code
      list_combine(list(1, 2), list(1, 2), "foo")
    Condition
      Error in `list_combine()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = "foo"
      i Did you forget to name an argument?

