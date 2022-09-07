# vec_c() includes index in argument tag

    Code
      vec_c(df1, df2)
    Condition
      Error:
      ! Can't combine `..1$x$y$z` <double> and `..2$x$y$z` <character>.

---

    Code
      vec_c(df1, df1, df2)
    Condition
      Error:
      ! Can't combine `..1$x$y$z` <double> and `..3$x$y$z` <character>.

---

    Code
      vec_c(foo = df1, bar = df2)
    Condition
      Error:
      ! Can't combine `foo$x$y$z` <double> and `bar$x$y$z` <character>.

# vec_c() fails with complex foreign S3 classes

    Code
      x <- structure(foobar(1), attr_foo = "foo")
      y <- structure(foobar(2), attr_bar = "bar")
      (expect_error(vec_c(x, y), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Error:
      ! Can't combine `..1` <vctrs_foobar> and `..2` <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# vec_c() fails with complex foreign S4 classes

    Code
      joe <- .Counts(c(1L, 2L), name = "Joe")
      jane <- .Counts(3L, name = "Jane")
      (expect_error(vec_c(joe, jane), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Error:
      ! Can't combine `..1` <vctrs_Counts> and `..2` <vctrs_Counts>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# vec_c() fallback doesn't support `name_spec` or `ptype`

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
      <error/vctrs_error_incompatible_type>
      Error:
      ! Can't convert <vctrs_foobar> to <character>.

# can ignore names in `vec_c()` by providing a `zap()` name-spec (#232)

    Code
      (expect_error(vec_c(a = c(b = letters), b = 1, .name_spec = zap()), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Error:
      ! Can't combine `a` <character> and `b` <double>.

# concatenation performs expected allocations

    Code
      ints <- rep(list(1L), 100)
      dbls <- rep(list(1), 100)
      # # `vec_c()` 
      # Integers
      with_memory_prof(vec_c_list(ints))
    Output
      [1] 1.96KB
    Code
      # Doubles
      with_memory_prof(vec_c_list(dbls))
    Output
      [1] 2.35KB
    Code
      # Integers to integer
      with_memory_prof(vec_c_list(ints, ptype = int()))
    Output
      [1] 1.7KB
    Code
      # Doubles to integer
      with_memory_prof(vec_c_list(dbls, ptype = int()))
    Output
      [1] 1.7KB
    Code
      # # `list_unchop()` 
      # Integers
      with_memory_prof(list_unchop(ints))
    Output
      [1] 1.13KB
    Code
      # Doubles
      with_memory_prof(list_unchop(dbls))
    Output
      [1] 1.52KB
    Code
      # Integers to integer
      with_memory_prof(list_unchop(ints, ptype = int()))
    Output
      [1] 896B
    Code
      # Doubles to integer
      with_memory_prof(list_unchop(dbls, ptype = int()))
    Output
      [1] 896B
    Code
      # # Concatenation with names
      # Named integers
      ints <- rep(list(set_names(1:3, letters[1:3])), 100)
      with_memory_prof(list_unchop(ints))
    Output
      [1] 4.3KB
    Code
      # Named matrices
      mat <- matrix(1:4, 2, dimnames = list(c("foo", "bar")))
      mats <- rep(list(mat), 100)
      with_memory_prof(list_unchop(mats))
    Output
      [1] 5.52KB
    Code
      # Data frame with named columns
      df <- data_frame(x = set_names(as.list(1:2), c("a", "b")), y = set_names(1:2, c(
        "A", "B")), z = data_frame(Z = set_names(1:2, c("Za", "Zb"))))
      dfs <- rep(list(df), 100)
      with_memory_prof(list_unchop(dfs))
    Output
      [1] 9.05KB
    Code
      # Data frame with rownames (non-repaired, non-recursive case)
      df <- data_frame(x = 1:2)
      dfs <- rep(list(df), 100)
      dfs <- map2(dfs, seq_along(dfs), set_rownames_recursively)
      with_memory_prof(list_unchop(dfs))
    Output
      [1] 6.28KB
    Code
      # Data frame with rownames (repaired, non-recursive case)
      dfs <- map(dfs, set_rownames_recursively)
      with_memory_prof(list_unchop(dfs))
    Output
      [1] 12.4KB
    Code
      # FIXME (#1217): Data frame with rownames (non-repaired, recursive case)
      df <- data_frame(x = 1:2, y = data_frame(x = 1:2))
      dfs <- rep(list(df), 100)
      dfs <- map2(dfs, seq_along(dfs), set_rownames_recursively)
      with_memory_prof(list_unchop(dfs))
    Output
      [1] 11.6KB
    Code
      # FIXME (#1217): Data frame with rownames (repaired, recursive case)
      dfs <- map(dfs, set_rownames_recursively)
      with_memory_prof(list_unchop(dfs))
    Output
      [1] 23.9KB

