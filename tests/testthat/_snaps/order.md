# `appearance` is validated

    Code
      vec_locate_sorted_groups(1, appearance = NA)
    Condition
      Error in `vec_locate_sorted_groups()`:
      ! `appearance` must be `TRUE` or `FALSE`.

---

    Code
      vec_locate_sorted_groups(1, appearance = 1)
    Condition
      Error in `vec_locate_sorted_groups()`:
      ! `appearance` must be `TRUE` or `FALSE`.

---

    Code
      vec_locate_sorted_groups(1, appearance = c(TRUE, FALSE))
    Condition
      Error in `vec_locate_sorted_groups()`:
      ! `appearance` must be `TRUE` or `FALSE`.

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

