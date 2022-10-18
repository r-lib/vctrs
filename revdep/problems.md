# cubble

<details>

* Version: 0.1.1
* GitHub: https://github.com/huizezhang-sherry/cubble
* Source code: https://github.com/cran/cubble
* Date/Publication: 2022-06-02 12:30:06 UTC
* Number of recursive dependencies: 137

Run `cloud_details(, "cubble")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. │ └─vctrs::vec_rbind(!!!dots, .names_to = .id)
        5. └─vctrs (local) `<fn>`()
        6.   └─vctrs::vec_default_cast(...)
        7.     ├─base::withRestarts(...)
        8.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
        9.     │   └─base (local) doWithOneRestart(return(expr), restart)
       10.     └─vctrs::stop_incompatible_cast(...)
       11.       └─vctrs::stop_incompatible_type(...)
       12.         └─vctrs:::stop_incompatible(...)
       13.           └─vctrs:::stop_vctrs(...)
       14.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 2 | WARN 0 | SKIP 6 | PASS 31 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘aggregation.Rmd’ using rmarkdown
    `summarise()` has grouped output by 'id'. You can override using the `.groups`
    argument.
    `summarise()` has grouped output by 'cluster', 'id'. You can override using the
    `.groups` argument.
    `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
    Adding missing grouping variables: `id`
    Quitting from lines 123-131 (aggregation.Rmd) 
    Error: processing vignette 'aggregation.Rmd' failed with diagnostics:
    ...
    Quitting from lines 75-79 (matching.Rmd) 
    Error: processing vignette 'matching.Rmd' failed with diagnostics:
    Can't convert `..1` <cubble_df> to <rowwise_df>.
    --- failed re-building ‘matching.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘aggregation.Rmd’ ‘matching.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# dm

<details>

* Version: 1.0.3
* GitHub: https://github.com/cynkra/dm
* Source code: https://github.com/cran/dm
* Date/Publication: 2022-10-12 15:42:33 UTC
* Number of recursive dependencies: 152

Run `cloud_details(, "dm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      `actual$parent_key_cols[[4]]` is absent
      `expected$parent_key_cols[[4]]` is a character vector ('h')
      
      `actual$parent_key_cols[[5]]` is absent
      `expected$parent_key_cols[[5]]` is a character vector ('n')
      Backtrace:
          ▆
       1. └─dm:::expect_equivalent_dm(...) at test-dm_wrap.R:73:2
       2.   └─dm:::expect_equivalent_tbl(...) at tests/testthat/helper-expectations.R:13:4
       3.     └─testthat::expect_identical(...) at tests/testthat/helper-expectations.R:76:2
      
      [ FAIL 18 | WARN 0 | SKIP 191 | PASS 1275 ]
      Error: Test failures
      Execution halted
    ```

# dplyr

<details>

* Version: 1.0.10
* GitHub: https://github.com/tidyverse/dplyr
* Source code: https://github.com/cran/dplyr
* Date/Publication: 2022-09-01 09:20:06 UTC
* Number of recursive dependencies: 97

Run `cloud_details(, "dplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Failure ('test-group_split.R:115'): group_split() works with subclasses implementing group_by() / ungroup() ──
      group_split(df, x) (`actual`) not identical to `expect` (`expected`).
      
      `class(attr(actual, 'ptype'))`:   "foo_df" "tbl_df" "tbl" "data.frame"
      `class(attr(expected, 'ptype'))`:          "tbl_df" "tbl" "data.frame"
      
      `class(actual[[1]])`:   "foo_df" "tbl_df" "tbl" "data.frame"
      `class(expected[[1]])`:          "tbl_df" "tbl" "data.frame"
      
      `class(actual[[2]])`:   "foo_df" "tbl_df" "tbl" "data.frame"
      `class(expected[[2]])`:          "tbl_df" "tbl" "data.frame"
      
      [ FAIL 1 | WARN 109 | SKIP 107 | PASS 2364 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# groupr

<details>

* Version: 0.1.0
* GitHub: https://github.com/ngriffiths21/groupr
* Source code: https://github.com/cran/groupr
* Date/Publication: 2020-10-14 12:30:06 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "groupr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        Please report it at <https://github.com/r-lib/vctrs/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.
      Backtrace:
          ▆
       1. ├─... %>% group_by2(is_ok, grp) at test_pivots.R:3:0
       2. ├─groupr::group_by2(., is_ok, grp)
       3. ├─groupr:::group_by2.data.frame(., is_ok, grp)
       4. │ └─groupr:::group_by2_ok(data, dots)
       5. │   └─groupr:::igrouped_df(grouped, groups_out)
       6. │     └─vctrs::vec_rbind(groups, data.frame())
       7. └─rlang:::stop_internal_c_lib(...)
       8.   └─rlang::abort(message, call = call, .internal = TRUE, .frame = frame)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    Quitting from lines 33-35 (introduction.Rmd) 
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    Column `.rows` (size 0) must match the data frame (size 2).
    ℹ In file 'slice.c' at line 188.
    ℹ This is an internal error that was detected in the vctrs package.
      Please report it at <https://github.com/r-lib/vctrs/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ricu

<details>

* Version: 0.5.3
* GitHub: https://github.com/eth-mds/ricu
* Source code: https://github.com/cran/ricu
* Date/Publication: 2022-07-12 10:50:14 UTC
* Number of recursive dependencies: 114

Run `cloud_details(, "ricu")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      See <https://vctrs.r-lib.org/articles/s3-vector.html>.
      Backtrace:
          ▆
       1. ├─testthat::expect_identical(as_src_cfg(mimic_demo), mi) at test-config.R:19:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─ricu::as_src_cfg(mimic_demo)
       5. ├─ricu:::as_src_cfg.src_env(mimic_demo)
       6. │ └─vctrs::vec_unchop(lapply(x, as_col_cfg), name_spec = "{inner}")
       7. │   └─vctrs::list_unchop(...)
       8. └─rlang::abort(message = message, call = call)
      
      [ FAIL 1 | WARN 1 | SKIP 7 | PASS 592 ]
      Error: Test failures
      Execution halted
    ```

# srvyr

<details>

* Version: 1.1.2
* GitHub: https://github.com/gergness/srvyr
* Source code: https://github.com/cran/srvyr
* Date/Publication: 2022-10-05 23:00:06 UTC
* Number of recursive dependencies: 87

Run `cloud_details(, "srvyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘srvyr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cascade
    > ### Title: Summarise multiple values into cascading groups
    > ### Aliases: cascade
    > 
    > ### ** Examples
    > 
    > library(survey)
    ...
      8. └─vctrs (local) `<fn>`()
      9.   └─vctrs::vec_default_ptype2(...)
     10.     ├─base::withRestarts(...)
     11.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     12.     │   └─base (local) doWithOneRestart(return(expr), restart)
     13.     └─vctrs::stop_incompatible_type(...)
     14.       └─vctrs:::stop_incompatible(...)
     15.         └─vctrs:::stop_vctrs(...)
     16.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Failure (???): deff and df work for grouped survey total ────────────────────
      `x` not equivalent to `y`.
      Length mismatch: comparison on first 2 components
      Component "survey_total": Length mismatch: comparison on first 3 components
      Component "survey_total": Component 1: Numeric: lengths (3, 1) differ
      Component "survey_total": Component 2: Numeric: lengths (3, 1) differ
      Component "survey_total": Component 3: Numeric: lengths (3, 1) differ
      Backtrace:
          ▆
       1. └─global expect_df_equal(out_srvyr, select(out_survey, -survey_total_se))
       2.   └─testthat::expect_equivalent(x, y)
      
      [ FAIL 47 | WARN 12 | SKIP 0 | PASS 216 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘extending-srvyr.Rmd’ using rmarkdown
    --- finished re-building ‘extending-srvyr.Rmd’
    
    --- re-building ‘srvyr-database.Rmd’ using rmarkdown
    --- finished re-building ‘srvyr-database.Rmd’
    
    --- re-building ‘srvyr-vs-survey.Rmd’ using rmarkdown
    Loading required package: grid
    Loading required package: Matrix
    ...
    ! Problem while computing column `n`.
    Caused by error in `initialize()`:
    ! attempt to use zero-length variable name
    --- failed re-building ‘srvyr-vs-survey.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘srvyr-vs-survey.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘convey’
    ```

# yamlet

<details>

* Version: 0.9.6
* GitHub: https://github.com/bergsmat/yamlet
* Source code: https://github.com/cran/yamlet
* Date/Publication: 2022-09-20 20:10:01 UTC
* Number of recursive dependencies: 81

Run `cloud_details(, "yamlet")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12. │ └─vctrs::vec_rbind(!!!dots, .names_to = .id)
       13. └─vctrs (local) `<fn>`()
       14.   └─vctrs::vec_default_cast(...)
       15.     ├─base::withRestarts(...)
       16.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
       17.     │   └─base (local) doWithOneRestart(return(expr), restart)
       18.     └─vctrs::stop_incompatible_cast(...)
       19.       └─vctrs::stop_incompatible_type(...)
       20.         └─vctrs:::stop_incompatible(...)
       21.           └─vctrs:::stop_vctrs(...)
       22.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 402 ]
      Error: Test failures
      Execution halted
    ```

