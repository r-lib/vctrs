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

