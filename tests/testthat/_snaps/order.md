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

