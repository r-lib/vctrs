# common type is taken

    Code
      (expect_error(vec_locate_interval_merge_groups(1, "x")))
    Output
      <error/vctrs_error_incompatible_type>
      Error:
      ! Can't combine `start` <double> and `end` <character>.

