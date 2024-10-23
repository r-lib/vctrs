# `direction` is recycled right with array columns (#1753)

    Code
      vec_order_radix(df, direction = c("asc", "desc", "desc"))
    Condition
      Error:
      ! `direction` should have length 1 or length equal to the number of columns of `x` when `x` is a data frame.

# `na_value` is recycled right with array columns (#1753)

    Code
      vec_order_radix(df, direction = c("smallest", "largest", "largest"))
    Condition
      Error:
      ! `direction` must contain only "asc" or "desc".

# dots must be empty (#1647)

    Code
      vec_order(1, 2)
    Condition
      Error in `vec_order()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 2
      i Did you forget to name an argument?

---

    Code
      vec_sort(1, 2)
    Condition
      Error in `vec_sort()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 2
      i Did you forget to name an argument?

