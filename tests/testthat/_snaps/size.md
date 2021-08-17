# argument tags are forwarded

    Code
      vec_size_common(1:2, 1, 1:4)
    Error <vctrs_error_incompatible_size>
      Can't recycle `..1` (size 2) to match `..3` (size 4).

---

    Code
      vec_size_common(foo = 1:2, 1, bar = 1:4)
    Error <vctrs_error_incompatible_size>
      Can't recycle `foo` (size 2) to match `bar` (size 4).

