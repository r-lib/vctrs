# recycling to size 1 has informative error

    Code
      (expect_error(vec_recycle(1:2, 1), class = "vctrs_error_recycle_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Can't recycle input of size 2 to size 1.

# incompatible recycling size has informative error

    Code
      vec_recycle(1:2, 4)
    Error <vctrs_error_incompatible_size>
      Can't recycle input of size 2 to size 4.

---

    Code
      vec_recycle(1:2, 4, x_arg = "foo")
    Error <vctrs_error_incompatible_size>
      Can't recycle `foo` (size 2) to size 4.

