# vec_as_names() is noisy by default

    Code
      vec_as_names(c("x", "x"), repair = "unique")
    Message <simpleMessage>
      New names:
      * x -> x...1
      * x -> x...2
    Output
      [1] "x...1" "x...2"
    Code
      vec_as_names(c("x", "x"), repair = "unique", quiet = TRUE)
    Output
      [1] "x...1" "x...2"
    Code
      (expect_error(vec_as_names(c("x", "x"), repair = "check_unique", repair_arg = "repair"))
      )
    Output
      <error/vctrs_error_names_must_be_unique>
      Names must be unique.
      x These names are duplicated:
        * "x" at locations 1 and 2.
      i Use argument `repair` to specify repair strategy.

