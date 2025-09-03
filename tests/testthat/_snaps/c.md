# common type failure uses error call and error arg (#1641, #1692)

    Code
      vec_c("x", 1, .error_call = call("foo"), .error_arg = "arg")
    Condition
      Error in `foo()`:
      ! Can't combine `arg[[1]]` <character> and `arg[[2]]` <double>.

---

    Code
      vec_c("x", .ptype = integer(), .error_call = call("foo"), .error_arg = "arg")
    Condition
      Error in `foo()`:
      ! Can't convert `arg[[1]]` <character> to <integer>.

# common type failure uses positional errors

    Code
      (expect_error(vec_c(1, a = "x", 2)))
    Output
      <error/vctrs_error_ptype2>
      Error in `vec_c()`:
      ! Can't combine `..1` <double> and `a` <character>.
    Code
      (expect_error(vec_c(1, a = "x", 2, .ptype = double(), .error_arg = "arg")))
    Output
      <error/vctrs_error_cast>
      Error in `vec_c()`:
      ! Can't convert `arg$a` <character> to <double>.
    Code
      (expect_error(vec_c(1, a = 2.5, .ptype = integer())))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `vec_c()`:
      ! Can't convert from `a` <double> to <integer> due to loss of precision.
      * Locations: 1

# vec_c() includes index in argument tag

    Code
      vec_c(df1, df2)
    Condition
      Error in `vec_c()`:
      ! Can't combine `..1$x$y$z` <double> and `..2$x$y$z` <character>.

---

    Code
      vec_c(df1, df1, df2)
    Condition
      Error in `vec_c()`:
      ! Can't combine `..1$x$y$z` <double> and `..3$x$y$z` <character>.

---

    Code
      vec_c(foo = df1, bar = df2)
    Condition
      Error in `vec_c()`:
      ! Can't combine `foo$x$y$z` <double> and `bar$x$y$z` <character>.

# vec_c() can repair names quietly

    Code
      res_unique <- vec_c(x = TRUE, x = 0, .name_repair = "unique_quiet")
      res_universal <- vec_c(`if` = TRUE, `in` = 0, .name_repair = "universal_quiet")

# vec_c() fails with complex foreign S3 classes

    Code
      x <- structure(foobar(1), attr_foo = "foo")
      y <- structure(foobar(2), attr_bar = "bar")
      (expect_error(vec_c(x, y), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `vec_c()`:
      ! Can't combine `..1` <vctrs_foobar> and `..2` <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.
    Code
      (expect_error(vec_c(x, y, .error_call = call("foo"), .error_arg = "arg"),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `foo()`:
      ! Can't combine `arg[[1]]` <vctrs_foobar> and `arg[[2]]` <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# vec_c() fails with complex foreign S4 classes

    Code
      joe <- .Counts(c(1L, 2L), name = "Joe")
      jane <- .Counts(3L, name = "Jane")
      (expect_error(vec_c(joe, jane), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `vec_c()`:
      ! Can't combine `..1` <vctrs_Counts> and `..2` <vctrs_Counts>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.
    Code
      (expect_error(vec_c(joe, jane, .error_call = call("foo"), .error_arg = "arg"),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `foo()`:
      ! Can't combine `arg[[1]]` <vctrs_Counts> and `arg[[2]]` <vctrs_Counts>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# vec_c() fallback doesn't support (most) `name_spec` or `ptype`

    Code
      (expect_error(with_c_foobar(vec_c(foobar(1), foobar(2), .name_spec = "{outer}_{inner}")),
      "name specification"))
    Output
      <error/rlang_error>
      Error in `vec_c()`:
      ! Can't use a name specification with non-vctrs types.
      vctrs methods must be implemented for class `vctrs_foobar`.
      See <https://vctrs.r-lib.org/articles/s3-vector.html>.
    Code
      (expect_error(with_c_foobar(vec_c(foobar(1), foobar(2), .ptype = "")), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error in `vec_c()`:
      ! Can't convert `..1` <vctrs_foobar> to <character>.
    Code
      (expect_error(with_c_foobar(vec_c(foobar(1), foobar(2), .error_call = call(
        "foo"), .name_spec = "{outer}_{inner}"))))
    Output
      <error/rlang_error>
      Error in `foo()`:
      ! Can't use a name specification with non-vctrs types.
      vctrs methods must be implemented for class `vctrs_foobar`.
      See <https://vctrs.r-lib.org/articles/s3-vector.html>.

# can ignore names in `vec_c()` by providing a `zap()` name-spec (#232)

    Code
      (expect_error(vec_c(a = c(b = letters), b = 1, .name_spec = zap()), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_ptype2>
      Error in `vec_c()`:
      ! Can't combine `a` <character> and `b` <double>.

# can ignore outer names in `vec_c()` by providing an 'inner' name-spec (#1988)

    Code
      vec_c(x = c(a = 1), y = c(b = "2"), .name_spec = "inner")
    Condition
      Error in `vec_c()`:
      ! Can't combine `x` <double> and `y` <character>.

# calls cast method even with empty objects

    Code
      vec_c(foobar(integer()), foobar(integer(), foo = "bar"))
    Condition
      Error in `vec_c()`:
      ! Can't convert `..2` <vctrs_foobar> to <vctrs_foobar>.

# concatenation performs expected allocations

    Code
      ints <- rep(list(1L), 100)
      dbls <- rep(list(1), 100)
      # # `vec_c()` 
      # Integers
      with_memory_prof(vec_c_list(ints))
    Output
      [1] 2.35KB
    Code
      # Doubles
      with_memory_prof(vec_c_list(dbls))
    Output
      [1] 2.74KB
    Code
      # Integers to integer
      with_memory_prof(vec_c_list(ints, ptype = int()))
    Output
      [1] 2.09KB
    Code
      # Doubles to integer
      with_memory_prof(vec_c_list(dbls, ptype = int()))
    Output
      [1] 2.09KB
    Code
      # # `list_unchop()` 
      # Integers
      with_memory_prof(list_unchop(ints))
    Output
      [1] 1.52KB
    Code
      # Doubles
      with_memory_prof(list_unchop(dbls))
    Output
      [1] 1.91KB
    Code
      # Integers to integer
      with_memory_prof(list_unchop(ints, ptype = int()))
    Output
      [1] 1.27KB
    Code
      # Doubles to integer
      with_memory_prof(list_unchop(dbls, ptype = int()))
    Output
      [1] 1.27KB
    Code
      # # Concatenation with names
      # Named integers
      ints <- rep(list(set_names(1:3, letters[1:3])), 100)
      with_memory_prof(list_unchop(ints))
    Output
      [1] 4.7KB
    Code
      # Named matrices
      mat <- matrix(1:4, 2, dimnames = list(c("foo", "bar")))
      mats <- rep(list(mat), 100)
      with_memory_prof(list_unchop(mats))
    Output
      [1] 5.91KB
    Code
      # Data frame with named columns
      df <- data_frame(x = set_names(as.list(1:2), c("a", "b")), y = set_names(1:2, c(
        "A", "B")), z = data_frame(Z = set_names(1:2, c("Za", "Zb"))))
      dfs <- rep(list(df), 100)
      with_memory_prof(list_unchop(dfs))
    Output
      [1] 9.18KB
    Code
      # Data frame with rownames (non-repaired, non-recursive case)
      df <- data_frame(x = 1:2)
      dfs <- rep(list(df), 100)
      dfs <- map2(dfs, seq_along(dfs), set_rownames_recursively)
      with_memory_prof(list_unchop(dfs))
    Output
      [1] 6.41KB
    Code
      # Data frame with rownames (repaired, non-recursive case)
      dfs <- map(dfs, set_rownames_recursively)
      with_memory_prof(list_unchop(dfs))
    Output
      [1] 12.6KB
    Code
      # Data frame with rownames (non-repaired, recursive case) (#1217)
      df <- data_frame(x = 1:2, y = data_frame(x = 1:2))
      dfs <- rep(list(df), 100)
      dfs <- map2(dfs, seq_along(dfs), set_rownames_recursively)
      with_memory_prof(list_unchop(dfs))
    Output
      [1] 11.7KB
    Code
      # Data frame with rownames (repaired, recursive case) (#1217)
      dfs <- map(dfs, set_rownames_recursively)
      with_memory_prof(list_unchop(dfs))
    Output
      [1] 24.1KB
    Code
      # list-ofs (#1496)
      make_list_of <- (function(n) {
        df <- tibble::tibble(x = new_list_of(vec_chop(1:n), ptype = integer()))
        vec_chop(df)
      })
      with_memory_prof(list_unchop(make_list_of(1000)))
    Output
      [1] 59.8KB
    Code
      with_memory_prof(list_unchop(make_list_of(2000)))
    Output
      [1] 118KB
    Code
      with_memory_prof(list_unchop(make_list_of(4000)))
    Output
      [1] 236KB

