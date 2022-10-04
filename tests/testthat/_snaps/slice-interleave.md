# allows for name repair

    Code
      vec_interleave(x, x, .name_repair = "unique")
    Message
      New names:
      * `x` -> `x...1`
      * `x` -> `x...2`
    Output
      x...1 x...2 
          1     1 

# can repair names quietly

    Code
      res_unique <- vec_interleave(c(x = 1), c(x = 2), .name_repair = "unique_quiet")
      res_universal <- vec_interleave(c(`if` = 1), c(`in` = 2), .name_repair = "universal_quiet")

# uses recycling errors

    Code
      vec_interleave(1:2, 1:3)
    Condition
      Error in `vec_interleave()`:
      ! Can't recycle `..1` (size 2) to match `..2` (size 3).

# errors if the result would be a long vector

    Code
      vec_interleave_indices(3L, 1000000000L)
    Condition
      Error in `vec_interleave_indices()`:
      ! Long vectors are not yet supported in `vec_interleave()`. Result from interleaving would have size 3000000000, which is larger than the maximum supported size of 2^31 - 1.

