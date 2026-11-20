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
    Condition
      Error:
      ! Can't combine `..1$x$y$z` <double> and `..2$x$y$z` <character>.

---

    Code
      vec_ptype_common(df1, df1, df2)
    Condition
      Error:
      ! Can't combine `..1$x$y$z` <double> and `..3$x$y$z` <character>.

---

    Code
      vec_ptype_common(large_df1, large_df2)
    Condition
      Error:
      ! Can't combine `..1$foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar$y$z` <double> and `..2$foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar$y$z` <character>.

---

    Code
      vec_ptype_common(foo = TRUE, bar = "foo")
    Condition
      Error:
      ! Can't combine `foo` <logical> and `bar` <character>.

---

    Code
      vec_ptype_common(foo = TRUE, baz = FALSE, bar = "foo")
    Condition
      Error:
      ! Can't combine `foo` <logical> and `bar` <character>.

---

    Code
      vec_ptype_common(foo = df1, bar = df2)
    Condition
      Error:
      ! Can't combine `foo$x$y$z` <double> and `bar$x$y$z` <character>.

---

    Code
      vec_ptype_common(df1, df1, bar = df2)
    Condition
      Error:
      ! Can't combine `..1$x$y$z` <double> and `bar$x$y$z` <character>.

---

    Code
      vec_ptype_common(TRUE, !!!list(1, "foo"))
    Condition
      Error:
      ! Can't combine `..2` <double> and `..3` <character>.

---

    Code
      vec_ptype_common(TRUE, !!!list(1, 2), "foo")
    Condition
      Error:
      ! Can't combine `..2` <double> and `..4` <character>.

---

    Code
      vec_ptype_common(1, !!!list(TRUE, FALSE), "foo")
    Condition
      Error:
      ! Can't combine `..1` <double> and `..4` <character>.

---

    Code
      vec_ptype_common(foo = TRUE, !!!list(FALSE, FALSE), bar = "foo")
    Condition
      Error:
      ! Can't combine `foo` <logical> and `bar` <character>.

---

    Code
      vec_ptype_common(foo = TRUE, !!!list(bar = 1, "foo"))
    Condition
      Error:
      ! Can't combine `bar` <double> and `..3` <character>.

---

    Code
      vec_ptype_common(foo = TRUE, !!!list(bar = "foo"))
    Condition
      Error:
      ! Can't combine `foo` <logical> and `bar` <character>.

---

    Code
      vec_ptype_common(foo = TRUE, !!!list(bar = FALSE), baz = "chr")
    Condition
      Error:
      ! Can't combine `foo` <logical> and `baz` <character>.

---

    Code
      vec_ptype_common(foo = TRUE, !!!list(bar = FALSE), !!!list(baz = "chr"))
    Condition
      Error:
      ! Can't combine `foo` <logical> and `baz` <character>.

# vec_ptype_common() of many data frames reports the same incompatible column

    Code
      vec_ptype_common(data_frame(a = 1), data_frame(b = 1), data_frame(c = 1),
      data_frame(b = "x"))
    Condition
      Error:
      ! Can't combine `..1$b` <double> and `..4$b` <character>.

---

    Code
      vec_ptype_common(data_frame(a = 1, b = 1), data_frame(c = 1), data_frame(d = 1),
      data_frame(b = "x", a = "x"))
    Condition
      Error:
      ! Can't combine `..1$a` <double> and `..4$a` <character>.

---

    Code
      vec_ptype_common(x = data_frame(a = 1), y = data_frame(b = 1), z = tibble(c = 1),
      w = data_frame(d = 1), v = data_frame(a = "x"))
    Condition
      Error:
      ! Can't combine `z$a` <double> and `v$a` <character>.

# vec_ptype_common() handles spliced names consistently (#1570)

    Code
      vec_ptype_common(a = "foo", b = "bar", y = NULL, z = 1)
    Condition
      Error:
      ! Can't combine `a` <character> and `z` <double>.
    Code
      vec_ptype_common(!!!args1, !!!args2)
    Condition
      Error:
      ! Can't combine `a` <character> and `z` <double>.
    Code
      vec_ptype_common(!!!args1, "{y_name}" := NULL, "{z_name}" := 1)
    Condition
      Error:
      ! Can't combine `a` <character> and `z` <double>.

# `.finalise` is validated

    Code
      vec_ptype_common(.finalise = 1)
    Condition
      Error in `vec_ptype_common()`:
      ! `.finalise` must be `TRUE` or `FALSE`.

---

    Code
      vec_ptype_common_params(.finalise = 1)
    Condition
      Error in `vec_ptype_common_params()`:
      ! `.finalise` must be `TRUE` or `FALSE`.

