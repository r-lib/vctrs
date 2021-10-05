# names are supplied if needed

    Code
      out <- vec_rbind(data_frame(...1 = 1), 1)
    Message <rlib_message_name_repair>
      New names:
      * `` -> `...1`

# duplicate names are de-deduplicated

    Code
      (expect_named(vec_cbind(x = 1, x = 1), c("x...1", "x...2")))
    Message <rlib_message_name_repair>
      New names:
      * `x` -> `x...1`
      * `x` -> `x...2`
    Output
        x...1 x...2
      1     1     1
    Code
      (expect_named(vec_cbind(data.frame(x = 1), data.frame(x = 1)), c("x...1",
        "x...2")))
    Message <rlib_message_name_repair>
      New names:
      * `x` -> `x...1`
      * `x` -> `x...2`
    Output
        x...1 x...2
      1     1     1

# vec_rbind() name repair messages are useful

    Code
      vec_rbind(1, 2)
    Message <rlib_message_name_repair>
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
    Output
        ...1
      1    1
      2    2
    Code
      vec_rbind(1, 2, .names_to = NULL)
    Message <rlib_message_name_repair>
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
    Output
        ...1
      1    1
      2    2
    Code
      vec_rbind(1, 2, ...10 = 3)
    Message <rlib_message_name_repair>
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
    Output
        ...1
      1    1
      2    2
      3    3
    Code
      vec_rbind(1, 2, ...10 = 3, .names_to = NULL)
    Message <rlib_message_name_repair>
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
    Output
           ...1
      ...1    1
      ...2    2
      ...3    3
    Code
      vec_rbind(a = 1, b = 2)
    Message <rlib_message_name_repair>
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
    Output
        ...1
      1    1
      2    2
    Code
      vec_rbind(a = 1, b = 2, .names_to = NULL)
    Message <rlib_message_name_repair>
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
    Output
        ...1
      a    1
      b    2
    Code
      vec_rbind(c(a = 1), c(b = 2))
    Output
         a  b
      1  1 NA
      2 NA  2
    Code
      vec_rbind(c(a = 1), c(b = 2), .names_to = NULL)
    Output
         a  b
      1  1 NA
      2 NA  2

# vec_rbind() is silent when assigning duplicate row names of df-cols

    Code
      vec_rbind(df, df)
    Output
         mpg
      1 21.0
      2 21.0
      3 22.8
      4 21.0
      5 21.0
      6 22.8

---

    Code
      vec_rbind(mtcars[1:4, ], mtcars[1:3, ])
    Output
                         mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4...1     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
      Mazda RX4 Wag...2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
      Datsun 710...3    22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
      Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
      Mazda RX4...5     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
      Mazda RX4 Wag...6 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
      Datsun 710...7    22.8   4  108  93 3.85 2.320 18.61  1  1    4    1

# vec_cbind() name repair messages are useful

    Code
      vec_cbind(1, 2)
    Message <rlib_message_name_repair>
      New names:
      * `` -> `...1`
      * `` -> `...2`
    Output
        ...1 ...2
      1    1    2
    Code
      vec_cbind(1, 2, ...10 = 3)
    Message <rlib_message_name_repair>
      New names:
      * `` -> `...1`
      * `` -> `...2`
      * `...10` -> `...3`
    Output
        ...1 ...2 ...3
      1    1    2    3
    Code
      vec_cbind(a = 1, b = 2)
    Output
        a b
      1 1 2
    Code
      vec_cbind(c(a = 1), c(b = 2))
    Message <rlib_message_name_repair>
      New names:
      * `` -> `...1`
      * `` -> `...2`
    Output
        ...1 ...2
      1    1    2

# vec_rbind() fails with complex foreign S3 classes

    Code
      x <- structure(foobar(1), attr_foo = "foo")
      y <- structure(foobar(2), attr_bar = "bar")
      (expect_error(vec_rbind(set_names(x, "x"), set_names(y, "x")), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`: Can't combine `..1` <vctrs_foobar> and `..2` <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# vec_rbind() fails with complex foreign S4 classes

    Code
      joe <- .Counts(1L, name = "Joe")
      jane <- .Counts(2L, name = "Jane")
      (expect_error(vec_rbind(set_names(joe, "x"), set_names(jane, "y")), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`: Can't combine `..1` <vctrs_Counts> and `..2` <vctrs:::common_class_fallback>.

# row-binding performs expected allocations

    Code
      ints <- rep(list(1L), 100)
      named_ints <- rep(list(set_names(1:3, letters[1:3])), 100)
      # Integers as rows
      suppressMessages(with_memory_prof(vec_rbind_list(ints)))
    Output
      [1] 2.53KB
    Code
      suppressMessages(with_memory_prof(vec_rbind_list(named_ints)))
    Output
      [1] 3.41KB
    Code
      # Data frame with named columns
      df <- data_frame(x = set_names(as.list(1:2), c("a", "b")), y = set_names(1:2, c(
        "A", "B")), z = data_frame(Z = set_names(1:2, c("Za", "Zb"))))
      dfs <- rep(list(df), 100)
      with_memory_prof(vec_rbind_list(dfs))
    Output
      [1] 10.2KB
    Code
      # Data frame with rownames (non-repaired, non-recursive case)
      df <- data_frame(x = 1:2)
      dfs <- rep(list(df), 100)
      dfs <- map2(dfs, seq_along(dfs), set_rownames_recursively)
      with_memory_prof(vec_rbind_list(dfs))
    Output
      [1] 7.42KB
    Code
      # Data frame with rownames (repaired, non-recursive case)
      dfs <- map(dfs, set_rownames_recursively)
      with_memory_prof(vec_rbind_list(dfs))
    Output
      [1] 13.6KB
    Code
      # FIXME (#1217): Data frame with rownames (non-repaired, recursive case)
      df <- data_frame(x = 1:2, y = data_frame(x = 1:2))
      dfs <- rep(list(df), 100)
      dfs <- map2(dfs, seq_along(dfs), set_rownames_recursively)
      with_memory_prof(vec_rbind_list(dfs))
    Output
      [1] 909KB
    Code
      # FIXME (#1217): Data frame with rownames (repaired, recursive case)
      dfs <- map(dfs, set_rownames_recursively)
      with_memory_prof(vec_rbind_list(dfs))
    Output
      [1] 921KB

