# `ties` is validated

    Code
      vec_rank(1, ties = "foo")
    Error <rlang_error>
      `ties` must be one of "min", "max", "sequential", or "dense", not "foo".

---

    Code
      vec_rank(1, ties = 1)
    Error <rlang_error>
      `ties` must be a string or character vector.

# `incomplete` is validated

    Code
      vec_rank(1, incomplete = NA)
    Error <rlang_error>
      `incomplete` must be a string or character vector.

---

    Code
      vec_rank(1, incomplete = c(TRUE, FALSE))
    Error <rlang_error>
      `incomplete` must be a string or character vector.

---

    Code
      vec_rank(1, incomplete = "foo")
    Error <rlang_error>
      `incomplete` must be one of "rank" or "na", not "foo".

