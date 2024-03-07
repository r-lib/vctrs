# `x` must not be `NULL` (#1823)

    Code
      vec_rank(NULL)
    Condition
      Error:
      ! This type is not supported by `vec_order()`.

---

    Code
      vec_rank(NULL, incomplete = "na")
    Condition
      Error:
      ! This type is not supported by `vec_order()`.

# `ties` is validated

    Code
      vec_rank(1, ties = "foo")
    Condition
      Error in `vec_rank()`:
      ! `ties` must be one of "min", "max", "sequential", or "dense", not "foo".

---

    Code
      vec_rank(1, ties = 1)
    Condition
      Error in `vec_rank()`:
      ! `ties` must be a string or character vector.

# `incomplete` is validated

    Code
      vec_rank(1, incomplete = NA)
    Condition
      Error in `vec_rank()`:
      ! `incomplete` must be a string or character vector.

---

    Code
      vec_rank(1, incomplete = c(TRUE, FALSE))
    Condition
      Error in `vec_rank()`:
      ! `incomplete` must be a string or character vector.

---

    Code
      vec_rank(1, incomplete = "foo")
    Condition
      Error in `vec_rank()`:
      ! `incomplete` must be one of "rank" or "na", not "foo".

