# data frames print nicely

    Code
      vec_ptype_show(mtcars)
    Output
      Prototype: data.frame<
        mpg : double
        cyl : double
        disp: double
        hp  : double
        drat: double
        wt  : double
        qsec: double
        vs  : double
        am  : double
        gear: double
        carb: double
      >

---

    Code
      vec_ptype_show(iris)
    Output
      Prototype: data.frame<
        Sepal.Length: double
        Sepal.Width : double
        Petal.Length: double
        Petal.Width : double
        Species     : factor<fb977>
      >

# embedded data frames print nicely

    Code
      vec_ptype_show(df)
    Output
      Prototype: data.frame<
        x: integer
        a: 
          data.frame<
            a: integer
            b: character
          >
        b: list_of<double>
        c: 
          list_of<
            data.frame<
              x: integer
              y: character
            >
          >
      >

# combining data frames with foreign classes uses fallback

    Code
      vec_ptype_common_df_fallback(foo, bar, baz)
    Warning <rlang_warning>
      Can't combine <foo> and <bar>; falling back to <data.frame>.
      Can't combine <data.frame> and <baz>; falling back to <data.frame>.
    Output
      [1] mpg  cyl  disp hp   drat wt   qsec vs   am  
      <0 rows> (or 0-length row.names)
    Code
      vec_ptype_common_df_fallback(foo, baz, bar, baz, foo, bar)
    Warning <rlang_warning>
      Can't combine <foo> and <baz>; falling back to <data.frame>.
      Can't combine <data.frame> and <bar>; falling back to <data.frame>.
    Output
      [1] mpg  cyl  disp qsec vs   am   hp   drat wt  
      <0 rows> (or 0-length row.names)
    Code
      with_fallback_warning(invisible(vec_rbind(foo, data.frame(), foo)))
    Warning <rlang_warning>
      Can't combine <foo> and <data.frame>; falling back to <data.frame>.
    Code
      with_fallback_warning(invisible(vec_cbind(foo, data.frame(x = 1))))
    Warning <rlang_warning>
      Can't combine <foo> and <data.frame>; falling back to <data.frame>.
    Code
      with_fallback_warning(invisible(vec_cbind(foo, data.frame(x = 1), bar)))
    Warning <rlang_warning>
      Can't combine <foo> and <data.frame>; falling back to <data.frame>.
      Can't combine <data.frame> and <bar>; falling back to <data.frame>.
    Code
      with_fallback_warning(invisible(vec_rbind(foo, baz, bar, baz, foo, bar)))
    Warning <rlang_warning>
      Can't combine <foo> and <baz>; falling back to <data.frame>.
      Can't combine <data.frame> and <bar>; falling back to <data.frame>.
    Code
      with_fallback_quiet(invisible(vec_rbind(foo, data.frame(), foo)))
      with_fallback_quiet(invisible(vec_cbind(foo, data.frame(x = 1))))
      with_fallback_quiet(invisible(vec_cbind(foo, data.frame(x = 1), bar)))
      with_fallback_quiet(invisible(vec_rbind(foo, baz, bar, baz, foo, bar)))

# falls back to tibble for tibble subclasses (#1025)

    Code
      with_fallback_warning(invisible(vec_rbind(foobar(tibble::as_tibble(mtcars)),
      mtcars, foobaz(mtcars))))
    Warning <rlang_warning>
      Can't combine <vctrs_foobar> and <data.frame>; falling back to <tibble>.
      Can't combine <tibble> and <vctrs_foobaz>; falling back to <tibble>.
    Code
      with_fallback_warning(invisible(vec_rbind(tibble::as_tibble(mtcars), foobar(
        tibble::as_tibble(mtcars)))))
    Warning <rlang_warning>
      Can't combine <tibble> and <vctrs_foobar>; falling back to <tibble>.
    Code
      with_fallback_warning(invisible(vec_rbind(foobar(tibble::as_tibble(mtcars)),
      mtcars, foobar(tibble::as_tibble(mtcars)))))
    Warning <rlang_warning>
      Can't combine <vctrs_foobar> and <data.frame>; falling back to <tibble>.
    Code
      with_fallback_quiet(invisible(vec_rbind(foobar(tibble::as_tibble(mtcars)),
      mtcars, foobaz(mtcars))))
      with_fallback_quiet(invisible(vec_rbind(tibble::as_tibble(mtcars), foobar(
        tibble::as_tibble(mtcars)))))
      with_fallback_quiet(invisible(vec_rbind(foobar(tibble::as_tibble(mtcars)),
      mtcars, foobar(tibble::as_tibble(mtcars)))))

