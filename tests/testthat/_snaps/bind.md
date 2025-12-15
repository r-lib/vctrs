# incompatible columns throws common type error

    Code
      (expect_error(vec_rbind(x_int, x_chr), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `vec_rbind()`:
      ! Can't combine `..1$x` <integer> and `..2$x` <character>.
    Code
      (expect_error(vec_rbind(x_int, x_chr, .error_call = call("foo")), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_ptype2>
      Error in `foo()`:
      ! Can't combine `..1$x` <integer> and `..2$x` <character>.
    Code
      (expect_error(vec_rbind(x_int, x_chr, .ptype = x_chr, .error_call = call("foo")),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_cast>
      Error in `foo()`:
      ! Can't convert `..1$x` <integer> to match type of `x` <character>.

# names are supplied if needed

    Code
      out <- vec_rbind(data_frame(...1 = 1), 1)
    Message
      New names:
      * `` -> `...1`

# can repair names in `vec_rbind()` (#229)

    Code
      (expect_error(vec_rbind(.name_repair = "none"), "can't be `\"none\"`"))
    Output
      <error/rlang_error>
      Error:
      ! `.name_repair` can't be `"none"`.
      It must be one of `"unique"`, `"universal"`, or `"check_unique"`.
    Code
      (expect_error(vec_rbind(.name_repair = "minimal"), "can't be `\"minimal\"`"))
    Output
      <error/rlang_error>
      Error:
      ! `.name_repair` can't be `"minimal"`.
      It must be one of `"unique"`, `"universal"`, or `"check_unique"`.
    Code
      (expect_error(vec_rbind(list(a = 1, a = 2), .name_repair = "check_unique"),
      class = "vctrs_error_names_must_be_unique"))
    Output
      <error/vctrs_error_names_must_be_unique>
      Error in `vec_rbind()`:
      ! Names must be unique.
      x These names are duplicated:
        * "a" at locations 1 and 2.

# can repair names quietly

    Code
      res_unique <- vec_rbind(c(x = 1, x = 2), c(x = 3, x = 4), .name_repair = "unique_quiet")
      res_universal <- vec_rbind(c(`if` = 1, `in` = 2), c(`if` = 3, `for` = 4),
      .name_repair = "universal_quiet")

---

    Code
      res_unique <- vec_cbind(x = 1, x = 2, .name_repair = "unique_quiet")
      res_universal <- vec_cbind(`if` = 1, `in` = 2, .name_repair = "universal_quiet")

# vec_rbind() fails with arrays of dimensionality > 3

    Code
      (expect_error(vec_rbind(array(NA, c(1, 1, 1)))))
    Output
      <error/rlang_error>
      Error in `vec_rbind()`:
      ! Can't bind arrays.
    Code
      (expect_error(vec_rbind(array(NA, c(1, 1, 1)), .error_call = call("foo"))))
    Output
      <error/rlang_error>
      Error in `foo()`:
      ! Can't bind arrays.

# can assign row names in vec_rbind()

    Code
      (expect_error(vec_rbind(foo = df1, df2, .names_to = NULL), "specification"))
    Output
      <error/rlang_error>
      Error in `vec_rbind()`:
      ! Can't merge the outer name `foo` with a vector of length > 1.
      Please supply a `.name_spec` specification.

# vec_rbind() requires a data frame proxy for data frame ptypes

    Code
      vec_rbind(df, df)
    Condition
      Error in `vec_rbind()`:
      ! Attempt to restore data frame from a double.
      i In file 'proxy-restore.c' at line <scrubbed>.
      i This is an internal error that was detected in the vctrs package.
        Please report it at <https://github.com/r-lib/vctrs/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

# names of `...` are used for type and cast errors even when zapped

    Code
      vec_rbind(!!!xs)
    Condition
      Error in `vec_rbind()`:
      ! Can't combine `a$x` <double> and `b$x` <character>.

---

    Code
      vec_rbind(!!!xs, .ptype = data_frame(x = double()))
    Condition
      Error in `vec_rbind()`:
      ! Can't convert `b$x` <character> to match type of `x` <double>.

# vec_cbind() reports error context

    Code
      (expect_error(vec_cbind(foobar(list()))))
    Output
      <error/vctrs_error_scalar_type>
      Error in `vec_cbind()`:
      ! `..1` must be a vector, not a <vctrs_foobar> object.
      x Detected incompatible scalar S3 list. To be treated as a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <vctrs_foobar>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?vctrs::howto_faq_fix_scalar_type_error`) to learn more.
    Code
      (expect_error(vec_cbind(foobar(list()), .error_call = call("foo"))))
    Output
      <error/vctrs_error_scalar_type>
      Error in `foo()`:
      ! `..1` must be a vector, not a <vctrs_foobar> object.
      x Detected incompatible scalar S3 list. To be treated as a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <vctrs_foobar>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?vctrs::howto_faq_fix_scalar_type_error`) to learn more.
    Code
      (expect_error(vec_cbind(a = 1:2, b = int())))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `vec_cbind()`:
      ! Can't recycle `a` (size 2) to match `b` (size 0).
    Code
      (expect_error(vec_cbind(a = 1:2, b = int(), .error_call = call("foo"))))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `foo()`:
      ! Can't recycle `a` (size 2) to match `b` (size 0).

# duplicate names are de-deduplicated

    Code
      (expect_named(vec_cbind(x = 1, x = 1), c("x...1", "x...2")))
    Message
      New names:
      * `x` -> `x...1`
      * `x` -> `x...2`
    Output
        x...1 x...2
      1     1     1
    Code
      (expect_named(vec_cbind(data.frame(x = 1), data.frame(x = 1)), c("x...1",
        "x...2")))
    Message
      New names:
      * `x` -> `x...1`
      * `x` -> `x...2`
    Output
        x...1 x...2
      1     1     1

# can repair names in `vec_cbind()` (#227)

    Code
      (expect_error(vec_cbind(a = 1, a = 2, .name_repair = "none"),
      "can't be `\"none\"`"))
    Output
      <error/rlang_error>
      Error:
      ! `.name_repair` can't be `"none"`.
      It must be one of `"unique"`, `"universal"`, `"check_unique"`, or `"minimal"`.
    Code
      (expect_error(vec_cbind(a = 1, a = 2, .name_repair = "check_unique"), class = "vctrs_error_names_must_be_unique")
      )
    Output
      <error/vctrs_error_names_must_be_unique>
      Error in `vec_cbind()`:
      ! Names must be unique.
      x These names are duplicated:
        * "a" at locations 1 and 2.

# can supply `.names_to` to `vec_rbind()` (#229)

    Code
      (expect_error(vec_rbind(data_frame(), .names_to = letters)))
    Output
      <error/rlang_error>
      Error in `vec_rbind()`:
      ! `.names_to` must be `NULL`, a string, or an `rlang::zap()` object.
    Code
      (expect_error(vec_rbind(data_frame(), .names_to = 10)))
    Output
      <error/rlang_error>
      Error in `vec_rbind()`:
      ! `.names_to` must be `NULL`, a string, or an `rlang::zap()` object.
    Code
      (expect_error(vec_rbind(data_frame(), .names_to = letters, .error_call = call(
        "foo"))))
    Output
      <error/rlang_error>
      Error in `foo()`:
      ! `.names_to` must be `NULL`, a string, or an `rlang::zap()` object.

# vec_cbind() fails with arrays of dimensionality > 3

    Code
      (expect_error(vec_cbind(a)))
    Output
      <error/rlang_error>
      Error in `vec_cbind()`:
      ! Can't bind arrays.
    Code
      (expect_error(vec_cbind(a, .error_call = call("foo"))))
    Output
      <error/rlang_error>
      Error in `foo()`:
      ! Can't bind arrays.
    Code
      (expect_error(vec_cbind(x = a)))
    Output
      <error/rlang_error>
      Error in `vec_cbind()`:
      ! Can't bind arrays.

# vec_rbind() name repair messages are useful

    Code
      vec_rbind(1, 2)
    Message
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
    Message
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
    Message
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
    Message
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
    Message
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
    Message
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
    Message
      New names:
      * `` -> `...1`
      * `` -> `...2`
    Output
        ...1 ...2
      1    1    2
    Code
      vec_cbind(1, 2, ...10 = 3)
    Message
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
    Message
      New names:
      * `` -> `...1`
      * `` -> `...2`
    Output
        ...1 ...2
      1    1    2

# rbind repairs names of data frames (#704)

    Code
      (expect_error(vec_rbind(df, df, .name_repair = "check_unique"), class = "vctrs_error_names_must_be_unique")
      )
    Output
      <error/vctrs_error_names_must_be_unique>
      Error in `vec_rbind()`:
      ! Names must be unique.
      x These names are duplicated:
        * "x" at locations 1 and 2.
    Code
      (expect_error(vec_rbind(df, df, .name_repair = "check_unique", .error_call = call(
        "foo")), class = "vctrs_error_names_must_be_unique"))
    Output
      <error/vctrs_error_names_must_be_unique>
      Error in `foo()`:
      ! Names must be unique.
      x These names are duplicated:
        * "x" at locations 1 and 2.

# vec_rbind() fails with complex foreign S3 classes

    Code
      x <- structure(foobar(1), attr_foo = "foo")
      y <- structure(foobar(2), attr_bar = "bar")
      (expect_error(vec_rbind(set_names(x, "x"), set_names(y, "x")), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_ptype2>
      Error in `vec_rbind()`:
      ! Can't combine `..1` <vctrs_foobar> and `..2` <vctrs_foobar>.
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
      <error/vctrs_error_ptype2>
      Error in `vec_rbind()`:
      ! Can't combine `..1` <vctrs_Counts> and `..2` <vctrs:::common_class_fallback>.

# row-binding performs expected allocations

    Code
      ints <- rep(list(1L), 100)
      named_ints <- rep(list(set_names(1:3, letters[1:3])), 100)
      # Integers as rows
      suppressMessages(with_memory_prof(vec_rbind_list(ints)))
    Output
      [1] 2.74KB
    Code
      suppressMessages(with_memory_prof(vec_rbind_list(named_ints)))
    Output
      [1] 3.62KB
    Code
      # Data frame with named columns
      df <- data_frame(x = set_names(as.list(1:2), c("a", "b")), y = set_names(1:2, c(
        "A", "B")), z = data_frame(Z = set_names(1:2, c("Za", "Zb"))))
      dfs <- rep(list(df), 100)
      with_memory_prof(vec_rbind_list(dfs))
    Output
      [1] 10.4KB
    Code
      # Data frame with rownames (non-repaired, non-recursive case)
      df <- data_frame(x = 1:2)
      dfs <- rep(list(df), 100)
      dfs <- map2(dfs, seq_along(dfs), set_rownames_recursively)
      with_memory_prof(vec_rbind_list(dfs))
    Output
      [1] 7.63KB
    Code
      # Data frame with rownames (repaired, non-recursive case)
      dfs <- map(dfs, set_rownames_recursively)
      with_memory_prof(vec_rbind_list(dfs))
    Output
      [1] 13.8KB
    Code
      # Data frame with rownames (non-repaired, recursive case) (#1217)
      df <- data_frame(x = 1:2, y = data_frame(x = 1:2))
      dfs <- rep(list(df), 100)
      dfs <- map2(dfs, seq_along(dfs), set_rownames_recursively)
      with_memory_prof(vec_rbind_list(dfs))
    Output
      [1] 13KB
    Code
      # Data frame with rownames (repaired, recursive case) (#1217)
      dfs <- map(dfs, set_rownames_recursively)
      with_memory_prof(vec_rbind_list(dfs))
    Output
      [1] 25.3KB

