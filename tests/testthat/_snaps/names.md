# vec_as_names() is noisy by default

    Code
      vec_as_names(c("x", "x"), repair = "unique")
    Message <rlib_message_name_repair>
      New names:
      * `x` -> `x...1`
      * `x` -> `x...2`
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
      Error in `stop_vctrs()`: Names must be unique.
      x These names are duplicated:
        * "x" at locations 1 and 2.
      i Use argument `repair` to specify repair strategy.

# unique_names() and as_unique_names() are verbose or silent

    Code
      unique_names(1:2)
    Message <rlib_message_name_repair>
      New names:
      * `` -> `...1`
      * `` -> `...2`
    Output
      [1] "...1" "...2"

---

    Code
      as_unique_names(c("", ""))
    Message <rlib_message_name_repair>
      New names:
      * `` -> `...1`
      * `` -> `...2`
    Output
      [1] "...1" "...2"

# message

    Code
      as_universal_names(c("a b", "b c"))
    Message <rlib_message_name_repair>
      New names:
      * `a b` -> `a.b`
      * `b c` -> `b.c`
    Output
      [1] "a.b" "b.c"

# messages by default

    Code
      vec_repair_names(set_names(1, "a:b"), "universal")
    Message <rlib_message_name_repair>
      New names:
      * `a:b` -> `a.b`
    Output
      a.b 
        1 

---

    Code
      vec_repair_names(set_names(1, "a:b"), ~ make.names(.))
    Message <rlib_message_name_repair>
      New names:
      * `a:b` -> `a.b`
    Output
      a.b 
        1 

