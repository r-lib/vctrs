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
    Condition
      Warning:
      Can't combine `..1` <foo> and `..2` <bar>; falling back to <data.frame>.
      Warning:
      Can't combine `..1` <data.frame> and `..3` <baz>; falling back to <data.frame>.
    Output
      [1] mpg  cyl  disp hp   drat wt   qsec vs   am  
      <0 rows> (or 0-length row.names)
    Code
      vec_ptype_common_df_fallback(foo, baz, bar, baz, foo, bar)
    Condition
      Warning:
      Can't combine `..1` <foo> and `..2` <baz>; falling back to <data.frame>.
      Warning:
      Can't combine `..1` <data.frame> and `..3` <bar>; falling back to <data.frame>.
    Output
      [1] mpg  cyl  disp qsec vs   am   hp   drat wt  
      <0 rows> (or 0-length row.names)
    Code
      with_fallback_warning(invisible(vec_rbind(foo, data.frame(), foo)))
    Condition
      Warning:
      Can't combine `..1` <foo> and `..2` <data.frame>; falling back to <data.frame>.
    Code
      with_fallback_warning(invisible(vec_cbind(foo, data.frame(x = 1))))
    Condition
      Warning:
      Can't combine `..1` <foo> and `..2` <data.frame>; falling back to <data.frame>.
    Code
      with_fallback_warning(invisible(vec_cbind(foo, data.frame(x = 1), bar)))
    Condition
      Warning:
      Can't combine `..1` <foo> and `..2` <data.frame>; falling back to <data.frame>.
      Warning:
      Can't combine `..1` <data.frame> and `..3` <bar>; falling back to <data.frame>.
    Code
      with_fallback_warning(invisible(vec_rbind(foo, baz, bar, baz, foo, bar)))
    Condition
      Warning:
      Can't combine `..1` <foo> and `..2` <baz>; falling back to <data.frame>.
      Warning:
      Can't combine `..1` <data.frame> and `..3` <bar>; falling back to <data.frame>.
    Code
      with_fallback_quiet(invisible(vec_rbind(foo, data.frame(), foo)))
      with_fallback_quiet(invisible(vec_cbind(foo, data.frame(x = 1))))
      with_fallback_quiet(invisible(vec_cbind(foo, data.frame(x = 1), bar)))
      with_fallback_quiet(invisible(vec_rbind(foo, baz, bar, baz, foo, bar)))

# `x` must be a list

    Code
      (expect_error(new_data_frame(1), "`x` must be a list"))
    Output
      <error/rlang_error>
      Error:
      ! `x` must be a list

# if supplied, `n` must be an integer of size 1

    Code
      (expect_error(new_data_frame(n = c(1L, 2L)), "must be an integer of size 1"))
    Output
      <error/rlang_error>
      Error in `new_data_frame()`:
      ! `n` must be an integer of size 1.
    Code
      (expect_error(new_data_frame(n = "x"), "must be an integer of size 1"))
    Output
      <error/rlang_error>
      Error in `new_data_frame()`:
      ! `n` must be an integer of size 1.

# if supplied, `n` can't be negative or missing (#1477)

    Code
      (expect_error(new_data_frame(n = -1L)))
    Output
      <error/rlang_error>
      Error in `new_data_frame()`:
      ! `n` can't be negative.
    Code
      (expect_error(new_data_frame(n = NA_integer_)))
    Output
      <error/rlang_error>
      Error in `new_data_frame()`:
      ! `n` can't be missing.

# `class` must be a character vector

    Code
      (expect_error(new_data_frame(class = 1), "must be NULL or a character vector"))
    Output
      <error/rlang_error>
      Error:
      ! `class` must be NULL or a character vector

# data_frame() and df_list() report error context

    Code
      (expect_error(data_frame(a = 1, a = 1)))
    Output
      <error/vctrs_error_names_must_be_unique>
      Error in `data_frame()`:
      ! Names must be unique.
      x These names are duplicated:
        * "a" at locations 1 and 2.
      i Use argument `.name_repair` to specify repair strategy.
    Code
      (expect_error(data_frame(a = 1, a = 1, .error_call = call("foo"))))
    Output
      <error/vctrs_error_names_must_be_unique>
      Error in `foo()`:
      ! Names must be unique.
      x These names are duplicated:
        * "a" at locations 1 and 2.
      i Use argument `.name_repair` to specify repair strategy.
    Code
      (expect_error(data_frame(a = 1:2, b = int())))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `data_frame()`:
      ! Can't recycle `a` (size 2) to match `b` (size 0).
    Code
      (expect_error(data_frame(a = 1:2, b = int(), .error_call = call("foo"))))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `foo()`:
      ! Can't recycle `a` (size 2) to match `b` (size 0).
    Code
      (expect_error(df_list(a = 1, a = 1)))
    Output
      <error/vctrs_error_names_must_be_unique>
      Error in `df_list()`:
      ! Names must be unique.
      x These names are duplicated:
        * "a" at locations 1 and 2.
      i Use argument `.name_repair` to specify repair strategy.
    Code
      (expect_error(df_list(a = 1, a = 1, .error_call = call("foo"))))
    Output
      <error/vctrs_error_names_must_be_unique>
      Error in `foo()`:
      ! Names must be unique.
      x These names are duplicated:
        * "a" at locations 1 and 2.
      i Use argument `.name_repair` to specify repair strategy.
    Code
      (expect_error(df_list(a = 1:2, b = int())))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `df_list()`:
      ! Can't recycle `a` (size 2) to match `b` (size 0).
    Code
      (expect_error(df_list(a = 1:2, b = int(), .error_call = call("foo"))))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `foo()`:
      ! Can't recycle `a` (size 2) to match `b` (size 0).

# input is tidy recycled

    Code
      expect_error(data_frame(1:2, 1:3), class = "vctrs_error_incompatible_size")

# `.unpack` is validated

    Code
      df_list(.unpack = 1)
    Condition
      Error in `df_list()`:
      ! `.unpack` must be `TRUE` or `FALSE`.

---

    Code
      df_list(.unpack = c(TRUE, FALSE))
    Condition
      Error in `df_list()`:
      ! `.unpack` must be `TRUE` or `FALSE`.

# `.name_repair` can be quiet

    Code
      dfl_unique <- df_list(1, 2, .name_repair = "unique_quiet")
      dfl_universal <- df_list(`if` = 1, `in` = 2, .name_repair = "universal_quiet")
      df_unique <- data_frame(1, 2, .name_repair = "unique_quiet")
      df_universal <- data_frame(`if` = 1, `in` = 2, .name_repair = "universal_quiet")

# data frame fallback handles column types (#999)

    Code
      local_error_call(call("my_function"))
      (expect_error(vec_ptype2(df1, df3), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `my_function()`:
      ! Can't combine `x` <double> and `x` <character>.
    Code
      (expect_error(vec_ptype2(df3, df1), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `my_function()`:
      ! Can't combine `x` <character> and `x` <double>.

# falls back to tibble for tibble subclasses (#1025)

    Code
      with_fallback_warning(invisible(vec_rbind(foobar(tibble::as_tibble(mtcars)),
      mtcars, foobaz(mtcars))))
    Condition
      Warning:
      Can't combine `..1` <vctrs_foobar> and `..2` <data.frame>; falling back to <tibble>.
      Warning:
      Can't combine `..1` <tibble> and `..3` <vctrs_foobaz>; falling back to <tibble>.
    Code
      with_fallback_warning(invisible(vec_rbind(tibble::as_tibble(mtcars), foobar(
        tibble::as_tibble(mtcars)))))
    Condition
      Warning:
      Can't combine `..1` <tibble> and `..2` <vctrs_foobar>; falling back to <tibble>.
    Code
      with_fallback_warning(invisible(vec_rbind(foobar(tibble::as_tibble(mtcars)),
      mtcars, foobar(tibble::as_tibble(mtcars)))))
    Condition
      Warning:
      Can't combine `..1` <vctrs_foobar> and `..2` <data.frame>; falling back to <tibble>.
    Code
      with_fallback_quiet(invisible(vec_rbind(foobar(tibble::as_tibble(mtcars)),
      mtcars, foobaz(mtcars))))
      with_fallback_quiet(invisible(vec_rbind(tibble::as_tibble(mtcars), foobar(
        tibble::as_tibble(mtcars)))))
      with_fallback_quiet(invisible(vec_rbind(foobar(tibble::as_tibble(mtcars)),
      mtcars, foobar(tibble::as_tibble(mtcars)))))

