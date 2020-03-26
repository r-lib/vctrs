# amt

<details>

* Version: 0.0.8
* Source code: https://github.com/cran/amt
* URL: https://github.com/jmsigner/amt
* Date/Publication: 2020-03-23 14:20:02 UTC
* Number of recursive dependencies: 168

Run `revdep_details(,"amt")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   mutate(hr = map(data, hr_mcp), n = map_int(data, nrow)) %>%
    +   hr_to_sf(hr, id, n)
    Error: No common type for `..1` <track_xyt<>> and `..2` <data.frame<>>.
    Backtrace:
         █
      1. ├─`%>%`(...)
      2. │ ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3. │ └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─`_fseq`(`_lhs`)
      6. │       └─magrittr::freduce(value, `_function_list`)
      7. │         └─function_list[[i]](value)
      8. │           ├─tidyr::nest(., data = -id)
      9. │           ├─amt:::nest.track_xy(., data = -id)
     10. │           ├─base::NextMethod() 00_pkg_src/amt/R/track.R:209:1
     11. │           └─tidyr:::nest.tbl_df(., data = -id)
     12. │             └─vctrs::vec_cbind(u_keys, new_data_frame(out, n = nrow(u_keys)))
     13. └─vctrs::vec_default_ptype2(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
     14.   └─vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
     15.     └─vctrs:::stop_incompatible(...
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘Rcpp’
      All declared Imports should be used.
    ```

# anomalize

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/anomalize
* URL: https://github.com/business-science/anomalize
* BugReports: https://github.com/business-science/anomalize/issues
* Date/Publication: 2019-09-21 04:10:03 UTC
* Number of recursive dependencies: 150

Run `revdep_details(,"anomalize")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    >> and `..2$nested.col` <tbl_time<
      date     : date
      observed : double
      season   : double
      trend    : double
      remainder: double
    >>.
    Backtrace:
         █
      1. ├─`%>%`(...)
      2. │ ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3. │ └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─`_fseq`(`_lhs`)
      6. │       └─magrittr::freduce(value, `_function_list`)
      7. │         └─function_list[[i]](value)
      8. │           ├─anomalize::time_decompose(., count, method = "stl")
      9. │           └─anomalize:::time_decompose.grouped_tbl_time(., count, method = "stl") 00_pkg_src/anomalize/R/time_decompose.R:102:4
     10. │             └─`%>%`(...) 00_pkg_src/anomalize/R/time_decompose.R:183:4
     11. │               
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5.         ├─testthat::with_reporter(...)
        6.         │ ├─base::withRestarts(...)
        7.         │ │ └─base:::withOneRestart(expr, restarts[[1L]])
        8.         │ │   └─base:::doWithOneRestart(return(expr), restart)
        9.         │ └─base::force(code)
       10.         └─base::lapply(...)
       11.           └─testthat:::FUN(X[[i]], ...)
       12.             ├─testthat::with_reporter(...)
       13.             │ ├─base::withRestarts(...)
       14.             │ │ └─base:::withOneRestart(expr, restarts[[1L]])
       15.             │ │   └─base:::doWithOneRestart(return(expr), restart)
       16.             │ └─base::force(code)
       17.             └─testthat::source_file(...)
       18.               └─testthat:
      Execution halted
    ```

# comperes

<details>

* Version: 0.2.3
* Source code: https://github.com/cran/comperes
* URL: https://github.com/echasnovski/comperes
* BugReports: https://github.com/echasnovski/comperes/issues
* Date/Publication: 2019-12-14 21:40:03 UTC
* Number of recursive dependencies: 58

Run `revdep_details(,"comperes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > to_pairgames(cr_data)
    Error: No common type for `..1` <longcr<>> and `..2` <data.frame<>>.
    Backtrace:
         █
      1. ├─comperes::to_pairgames(cr_data)
      2. │ └─`%>%`(...) 00_pkg_src/comperes/R/pairgames.R:73:2
      3. │   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6. │       └─comperes:::`_fseq`(`_lhs`)
      7. │         └─magrittr::freduce(value, `_function_list`)
      8. │           └─function_list[[i]](value)
      9. │             ├─tidyr::nest(., data = -.data$game)
     10. │             └─tidyr:::nest.tbl_df(., data = -.data$game)
     11. │               └─vctrs::vec_cbind(u_keys, new_data_frame(out, n = nrow(u_keys)))
     12. └─vctrs::vec_default_ptype2(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
     13.   └─vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
     14.     └─vctrs:::stop_incompatible(...)
     15. 
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. testthat::expect_identical(as_widecr(ncaa2005), to_pairgames(ncaa2005))
       15. vctrs::vec_default_ptype2(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
       16. vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
       17. vctrs:::stop_incompatible(...)
       18. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 259 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Error: to_pairgames works (@test-pairgames.R#28) 
      2. Error: to_pairgames handles NA and NaN (@test-pairgames.R#46) 
      3. Error: to_pairgames doesn't change pairgames (@test-pairgames.R#50) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# cutpointr

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/cutpointr
* URL: https://github.com/thie1e/cutpointr
* BugReports: https://github.com/thie1e/cutpointr/issues
* Date/Publication: 2019-12-18 15:00:08 UTC
* Number of recursive dependencies: 74

Run `revdep_details(,"cutpointr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > cutpointr(suicide, dsi, suicide, method = maximize_boot_metric,
    +           metric = accuracy, boot_cut = 30)
    Assuming the positive class is yes
    Assuming the positive class has higher x values
    Error: No common type for `..1` <roc_cutpointr<>> and `..2` <data.frame<>>.
    Backtrace:
         █
      1. ├─cutpointr::cutpointr(...)
      2. ├─cutpointr:::cutpointr.default(...) 00_pkg_src/cutpointr/R/cutpointr.R:264:4
      3. │ └─cutpointr:::cutpointr_internal(...) 00_pkg_src/cutpointr/R/cutpointr.R:321:8
      4. │   ├─`%>%`(...) 00_pkg_src/cutpointr/R/cutpointr.R:626:12
      5. │   │ └─base::eval(lhs, parent, parent)
      6. │   │   └─base::eval(lhs, parent, parent)
      7. │   ├─tidyr::nest(.data = roc_curve, roc_curve = dplyr::everything())
      8. │   └─tidyr:::nest.tbl_df(.data = roc_curve, roc_curve = dplyr::everything())
      9. │     └─vctrs::vec_cbind(u_keys, new_data_frame(out, n = nrow(u_keys)))
     10. └─vctrs::vec_default_ptype2(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
     11.   └─vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
     12.     └─vctrs:::stop_incompatible(...)
     13.       └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 250 | SKIPPED: 0 | WARNINGS: 43 | FAILED: 27 ]
      1. Error: Plotting with bootstrapping is silent (@test-cutpointr.R#100) 
      2. Error: no duplicate column names are returned (@test-cutpointr.R#164) 
      3. Error: Metric colnames that are already in cutpointr are modified (@test-cutpointr.R#253) 
      4. Failure: Bootstrap returns plausible results (@test-cutpointr.R#335) 
      5. Failure: Bootstrap returns plausible results (@test-cutpointr.R#337) 
      6. Failure: Bootstrap returns plausible results (@test-cutpointr.R#339) 
      7. Failure: Bootstrap returns plausible results (@test-cutpointr.R#341) 
      8. Failure: Bootstrap returns plausible results (@test-cutpointr.R#347) 
      9. Failure: Bootstrap returns plausible results (@test-cutpointr.R#349) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# fable

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/fable
* URL: https://fable.tidyverts.org
* BugReports: https://github.com/tidyverts/fable/issues
* Date/Publication: 2020-01-29 11:00:03 UTC
* Number of recursive dependencies: 99

Run `revdep_details(,"fable")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   autoplot(sim_poisson)
    Error: No common type for `..1` <tbl_ts<>> and `..2` <tbl_df<>>.
    Backtrace:
         █
      1. ├─`%>%`(...)
      2. │ ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3. │ └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─`_fseq`(`_lhs`)
      6. │       └─magrittr::freduce(value, `_function_list`)
      7. │         ├─base::withVisible(function_list[[k]](value))
      8. │         └─function_list[[k]](value)
      9. │           ├─ggplot2::autoplot(., sim_poisson)
     10. │           └─fabletools:::autoplot.fbl_ts(., sim_poisson)
     11. │             ├─ggplot2::autolayer(...)
     12. │             └─fabletools:::autolayer.fbl_ts(...)
     13. │               ├─`%>%`(...)
     14. │               │ └─base::eval(lhs, parent, parent)
     15. │               │   └─base::eval(lhs, parent, parent)
     16. │               ├─ggplot2::fortify(object, level = level
    Execution halted
    ```

# predictrace

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/predictrace
* Date/Publication: 2019-05-22 07:50:03 UTC
* Number of recursive dependencies: 63

Run `revdep_details(,"predictrace")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
        R      2.1Mb
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘here’ ‘tidyr’
      All declared Imports should be used.
    ```

# tidyfast

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/tidyfast
* Date/Publication: 2020-03-20 10:40:02 UTC
* Number of recursive dependencies: 51

Run `revdep_details(,"tidyfast")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12. vctrs:::stop_incompatible(...)
       13. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 80 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 7 ]
      1. Error: can pivot all cols (unspecified) to long (@test-dt_pivot_longer.R#7) 
      2. Error: can pivot all cols (specified) to long (@test-dt_pivot_longer.R#18) 
      3. Error: can select a single column (@test-dt_pivot_longer.R#28) 
      4. Error: preserves original keys (@test-dt_pivot_longer.R#41) 
      5. Error: can drop missing values (@test-dt_pivot_longer.R#50) 
      6. Error: works with select helpers (@test-dt_pivot_longer.R#77) 
      7. Error: a single helper works outside of c() call (@test-dt_pivot_longer.R#86) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidygapminder

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/tidygapminder
* URL: https://ebedthan.github.io/tidygapminder
* BugReports: https://github.com/ebedthan/tidygapminder/issues
* Date/Publication: 2020-02-04 07:30:02 UTC
* Number of recursive dependencies: 64

Run `revdep_details(,"tidygapminder")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > folder_path <- system.file("extdata", package = "tidygapminder")
    > 
    > tidy_bunch(folder_path)
    Error: No common type for `..1` <data.table<>> and `..2` <tbl_df<>>.
    Backtrace:
         █
      1. ├─tidygapminder::tidy_bunch(folder_path)
      2. │ └─base::lapply(file_list, tidy_indice) 00_pkg_src/tidygapminder/R/tidy_bunch.R:28:2
      3. │   └─tidygapminder:::FUN(X[[i]], ...)
      4. │     └─tidyr::pivot_longer(...) 00_pkg_src/tidygapminder/R/tidy_indice.R:41:2
      5. │       └─tidyr::pivot_longer_spec(...)
      6. │         ├─tidyr:::wrap_error_names(...)
      7. │         │ └─base::tryCatch(...)
      8. │         │   └─base:::tryCatchList(expr, classes, parentenv, handlers)
      9. │         │     └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
     10. │         │       └─base:::doTryCatch(return(expr), name, parentenv, handler)
     11. │         └─vctrs::vec_cbind(...)
     12. └─vctrs::vec_default_ptype2(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
     13.   └─vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
     
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. testthat::expect_equal(...)
       15. vctrs::vec_default_ptype2(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
       16. vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
       17. vctrs:::stop_incompatible(...)
       18. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 0 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: Output a list of data frame (@test-tidy_bunch.R#4) 
      2. Error: Output a big data frame (@test-tidy_bunch.R#8) 
      3. Error: Output a data frame (@test-tidy_indice.R#5) 
      4. Error: Colnames are right (@test-tidy_indice.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# tidyjson

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/tidyjson
* URL: https://github.com/colearendt/tidyjson
* BugReports: https://github.com/colearendt/tidyjson/issues
* Date/Publication: 2019-12-02 21:39:30
* Number of recursive dependencies: 88

Run `revdep_details(,"tidyjson")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 290 | SKIPPED: 7 | WARNINGS: 0 | FAILED: 10 ]
      1.  Failure: has correct complete structure with simple input (@test-append_values.R#7) 
      2.  Failure: recursive works as expected (@test-append_values.R#191) 
      3.  Failure: recursive works as expected (@test-append_values.R#206) 
      4.  Failure: works in a simple case (@test-gather_object.R#7) 
      5.  Failure: works with compound values (@test-gather_object.R#31) 
      6.  Failure: column.name works and doesn't clobber existing name (@test-gather_object.R#80) 
      7.  Error: preserves a NULL column (@test-gather_object.R#100) 
      8.  Failure: can call repeatedly without having to change column.name (@test-gather_object.R#150) 
      9.  Error: simple object works (@test-json_structure.R#27) 
      10. Error: nested object works (@test-json_structure.R#71) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

