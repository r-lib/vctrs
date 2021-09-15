# allows for name repair

    Code
      vec_interleave(x, x, .name_repair = "unique")
    Message <rlib_message_name_repair>
      New names:
      * `x` -> `x...1`
      * `x` -> `x...2`
    Output
      x...1 x...2 
          1     1 

# uses recycling errors

    Code
      vec_interleave(1:2, 1:3)
    Error <vctrs_error_incompatible_size>
      Can't recycle `..1` (size 2) to match `..2` (size 3).

# errors if the result would be a long vector

    Code
      vec_interleave_indices(3L, 1000000000L)
    Error <rlang_error>
      Long vectors are not yet supported in `vec_interleave()`. Result from interleaving would have size 3000000000, which is larger than the maximum supported size of 2^31 - 1.

