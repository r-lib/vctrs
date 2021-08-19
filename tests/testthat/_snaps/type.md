# output tests

    Code
      vec_ptype_show()
    Output
      Prototype: NULL

---

    Code
      vec_ptype_show(integer())
    Output
      Prototype: integer

---

    Code
      vec_ptype_show(integer(), double())
    Output
      Prototype: <double>
      0. (           , <integer> ) = <integer>
      1. ( <integer> , <double>  ) = <double> 

---

    Code
      vec_ptype_show(logical(), integer(), double())
    Output
      Prototype: <double>
      0. (           , <logical> ) = <logical>
      1. ( <logical> , <integer> ) = <integer>
      2. ( <integer> , <double>  ) = <double> 

# vec_ptype_common() includes index in argument tag

    Code
      vec_ptype_common(df1, df2)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1$x$y$z` <double> and `..2$x$y$z` <character>.

---

    Code
      vec_ptype_common(df1, df1, df2)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1$x$y$z` <double> and `..3$x$y$z` <character>.

---

    Code
      vec_ptype_common(large_df1, large_df2)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1$foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar$y$z` <double> and `..2$foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar$y$z` <character>.

---

    Code
      vec_ptype_common(foo = TRUE, bar = "foo")
    Error <vctrs_error_incompatible_type>
      Can't combine `foo` <logical> and `bar` <character>.

---

    Code
      vec_ptype_common(foo = TRUE, baz = FALSE, bar = "foo")
    Error <vctrs_error_incompatible_type>
      Can't combine `foo` <logical> and `bar` <character>.

---

    Code
      vec_ptype_common(foo = df1, bar = df2)
    Error <vctrs_error_incompatible_type>
      Can't combine `foo$x$y$z` <double> and `bar$x$y$z` <character>.

---

    Code
      vec_ptype_common(df1, df1, bar = df2)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1$x$y$z` <double> and `bar$x$y$z` <character>.

---

    Code
      vec_ptype_common(TRUE, 1, "foo")
    Error <vctrs_error_incompatible_type>
      Can't combine `..2` <double> and `..3` <character>.

---

    Code
      vec_ptype_common(TRUE, 1, 2, "foo")
    Error <vctrs_error_incompatible_type>
      Can't combine `..2` <double> and `..4` <character>.

---

    Code
      vec_ptype_common(1, TRUE, FALSE, "foo")
    Error <vctrs_error_incompatible_type>
      Can't combine `..1` <double> and `..4` <character>.

---

    Code
      vec_ptype_common(foo = TRUE, FALSE, FALSE, bar = "foo")
    Error <vctrs_error_incompatible_type>
      Can't combine `foo` <logical> and `bar` <character>.

---

    Code
      vec_ptype_common(foo = TRUE, bar = 1, "foo")
    Error <vctrs_error_incompatible_type>
      Can't combine `foo` <double> and `..3` <character>.

---

    Code
      vec_ptype_common(foo = TRUE, bar = "foo")
    Error <vctrs_error_incompatible_type>
      Can't combine `foo` <logical> and `bar` <character>.

---

    Code
      vec_ptype_common(foo = TRUE, bar = FALSE, baz = "chr")
    Error <vctrs_error_incompatible_type>
      Can't combine `foo` <logical> and `baz` <character>.

---

    Code
      vec_ptype_common(foo = TRUE, bar = FALSE, baz = "chr")
    Error <vctrs_error_incompatible_type>
      Can't combine `foo` <logical> and `baz` <character>.

