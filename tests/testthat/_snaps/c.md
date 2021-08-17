# vec_c() includes index in argument tag

    Code
      vec_c(df1, df2)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1$x$y$z` <double> and `..2$x$y$z` <character>.

---

    Code
      vec_c(df1, df1, df2)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1$x$y$z` <double> and `..3$x$y$z` <character>.

---

    Code
      vec_c(foo = df1, bar = df2)
    Error <vctrs_error_incompatible_type>
      Can't combine `foo$x$y$z` <double> and `bar$x$y$z` <character>.

