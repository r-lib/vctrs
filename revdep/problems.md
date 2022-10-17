# codebook

<details>

* Version: 0.9.2
* GitHub: https://github.com/rubenarslan/codebook
* Source code: https://github.com/cran/codebook
* Date/Publication: 2020-06-06 23:40:03 UTC
* Number of recursive dependencies: 154

Run `cloud_details(, "codebook")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘codebook.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    ! Can't convert `haven_labelled` <one_skim_df> to <one_skim_df>.
    
    --- failed re-building ‘codebook_tutorial.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘codebook.Rmd’ ‘codebook_qualtrics.Rmd’ ‘codebook_sav.Rmd’
      ‘codebook_tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘userfriendlyscience’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘jsonlite’ ‘rlang’ ‘tidyselect’ ‘vctrs’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 65 marked UTF-8 strings
    ```

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

# cutpointr

<details>

* Version: 1.1.2
* GitHub: https://github.com/thie1e/cutpointr
* Source code: https://github.com/cran/cutpointr
* Date/Publication: 2022-04-13 18:12:29 UTC
* Number of recursive dependencies: 83

Run `cloud_details(, "cutpointr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cutpointr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cutpointr
    > ### Title: Determine and evaluate optimal cutpoints
    > ### Aliases: cutpointr cutpointr.default cutpointr.numeric
    > 
    > ### ** Examples
    > 
    > library(cutpointr)
    ...
     13.   └─vctrs::vec_default_cast(...)
     14.     ├─base::withRestarts(...)
     15.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     16.     │   └─base (local) doWithOneRestart(return(expr), restart)
     17.     └─vctrs::stop_incompatible_cast(...)
     18.       └─vctrs::stop_incompatible_type(...)
     19.         └─vctrs:::stop_incompatible(...)
     20.           └─vctrs:::stop_vctrs(...)
     21.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12. │       └─vctrs::list_unchop(...)
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
      
      [ FAIL 10 | WARN 8 | SKIP 0 | PASS 273 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘cutpointr.Rmd’ using rmarkdown
    Quitting from lines 51-52 (cutpointr.Rmd) 
    Error: processing vignette 'cutpointr.Rmd' failed with diagnostics:
    Can't convert `x[[1]]` <roc_cutpointr> to <roc_cutpointr>.
    --- failed re-building ‘cutpointr.Rmd’
    
    --- re-building ‘cutpointr_benchmarks.Rmd’ using rmarkdown
    --- finished re-building ‘cutpointr_benchmarks.Rmd’
    
    ...
    
    --- re-building ‘cutpointr_user_functions.Rmd’ using rmarkdown
    --- finished re-building ‘cutpointr_user_functions.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘cutpointr.Rmd’ ‘cutpointr_bootstrapping.Rmd’
      ‘cutpointr_estimation.Rmd’
    
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

# gratia

<details>

* Version: 0.7.3
* GitHub: https://github.com/gavinsimpson/gratia
* Source code: https://github.com/cran/gratia
* Date/Publication: 2022-05-09 11:20:03 UTC
* Number of recursive dependencies: 83

Run `cloud_details(, "gratia")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gratia-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: basis
    > ### Title: Basis expansions for smooths
    > ### Aliases: basis
    > 
    > ### ** Examples
    > 
    > load_mgcv()
    ...
      7.   \-vctrs::vec_default_cast(...)
      8.     +-base::withRestarts(...)
      9.     | \-base (local) withOneRestart(expr, restarts[[1L]])
     10.     |   \-base (local) doWithOneRestart(return(expr), restart)
     11.     \-vctrs::stop_incompatible_cast(...)
     12.       \-vctrs::stop_incompatible_type(...)
     13.         \-vctrs:::stop_incompatible(...)
     14.           \-vctrs:::stop_vctrs(...)
     15.             \-rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      • hgam-paper/hgam-paper-bird-move-model-1.svg
      • hgam-paper/hgam-paper-bird-move-model-2.svg
      • hgam-paper/hgam-paper-bird-move-model-3.svg
      • hgam-paper/hgam-paper-bird-move-model-5.svg
      • hgam-paper/hgam-paper-co2-model-1.svg
      • hgam-paper/hgam-paper-co2-model-2.svg
      • hgam-paper/hgam-paper-co2-model-3.svg
      • hgam-paper/hgam-paper-co2-model-4.svg
      • hgam-paper/hgam-paper-co2-model-5.svg
      • hgam-paper/hgam-paper-zoop-model-4.svg
      • hgam-paper/hgam-paper-zoop-model-5.svg
      • rootograms/draw-gaussian-rootogram.svg
      • rootograms/draw-neg-bin-rootogram.svg
      Error: Test failures
      Execution halted
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

# multidplyr

<details>

* Version: 0.1.2
* GitHub: https://github.com/tidyverse/multidplyr
* Source code: https://github.com/cran/multidplyr
* Date/Publication: 2022-09-26 19:40:02 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "multidplyr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘multidplyr.Rmd’ using rmarkdown
    Quitting from lines 81-85 (multidplyr.Rmd) 
    Error: processing vignette 'multidplyr.Rmd' failed with diagnostics:
    Can't convert `..1` <spec_tbl_df> to <spec_tbl_df>.
    --- failed re-building ‘multidplyr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘multidplyr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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
    
    > ### Name: as_survey_twophase
    > ### Title: Create a tbl_svy survey object using two phase design
    > ### Aliases: as_survey_twophase as_survey_twophase.data.frame
    > ###   as_survey_twophase.twophase2
    > 
    > ### ** Examples
    > 
    ...
     13.   └─vctrs::vec_default_cast(...)
     14.     ├─base::withRestarts(...)
     15.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     16.     │   └─base (local) doWithOneRestart(return(expr), restart)
     17.     └─vctrs::stop_incompatible_cast(...)
     18.       └─vctrs::stop_incompatible_type(...)
     19.         └─vctrs:::stop_incompatible(...)
     20.           └─vctrs:::stop_vctrs(...)
     21.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ! Can't convert `..1` <srvyr_result_df> to <srvyr_result_df>.
      ── Error ('test_survey_statistics.r:17'): (code run outside of `test_that()`) ──
      Error in `dplyr::summarise(.data$variables, ..., .groups = .groups)`: Problem while computing `api_ratio = survey_ratio(api00, api99)`.
      Caused by error in `FUN()`:
      ! Can't convert `..1` <srvyr_result_df> to <srvyr_result_df>.
      ── Error ('test_survey_statistics_basic.r:17'): (code run outside of `test_that()`) ──
      Error in `dplyr::summarise(.data$variables, ..., .groups = .groups)`: Problem while computing `survey_mean = survey_mean(api99)`.
      Caused by error in `FUN()`:
      ! Can't convert `..1` <srvyr_result_df> to <srvyr_result_df>.
      
      [ FAIL 49 | WARN 6 | SKIP 0 | PASS 127 ]
      Error: Test failures
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
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
    "ci")`.
    Caused by error in `FUN()`:
    ! Can't convert `..1` <srvyr_result_df> to <srvyr_result_df>.
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

# tibbletime

<details>

* Version: 0.1.6
* GitHub: https://github.com/business-science/tibbletime
* Source code: https://github.com/cran/tibbletime
* Date/Publication: 2020-07-21 13:50:02 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "tibbletime")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tibbletime-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: collapse_index
    > ### Title: Collapse an index vector so that all observations in an interval
    > ###   share the same date
    > ### Aliases: collapse_index
    > 
    > ### ** Examples
    > 
    ...
     14.   └─vctrs::vec_default_cast(...)
     15.     ├─base::withRestarts(...)
     16.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     17.     │   └─base (local) doWithOneRestart(return(expr), restart)
     18.     └─vctrs::stop_incompatible_cast(...)
     19.       └─vctrs::stop_incompatible_type(...)
     20.         └─vctrs:::stop_incompatible(...)
     21.           └─vctrs:::stop_vctrs(...)
     22.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      data[[1]])`.
      Caused by error in `vctrs::as_list_of()`:
      ! Can't convert `..1` <tbl_time> to <tbl_time>.
      ── Error ('test_compat-tidyr.R:47'): unnest() with index goes back to tbl_time ──
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `dplyr::mutate(.data_nested, `:=`(!!nested_column_sym, purrr::map(!!nested_column_sym, 
          ~reconstruct(.x, ..original_data))), `:=`(!!nested_column_sym, 
          vctrs::as_list_of(!!nested_column_sym, .ptype = (!!nested_column_sym)[[1]])))`: Problem while computing `data = vctrs::as_list_of(data, .ptype =
      data[[1]])`.
      Caused by error in `vctrs::as_list_of()`:
      ! Can't convert `..1` <tbl_time> to <tbl_time>.
      
      [ FAIL 2 | WARN 2 | SKIP 0 | PASS 141 ]
      Error: Test failures
      Execution halted
    ```

# tidyjson

<details>

* Version: 0.3.1
* GitHub: https://github.com/colearendt/tidyjson
* Source code: https://github.com/cran/tidyjson
* Date/Publication: 2020-05-31 21:30:03 UTC
* Number of recursive dependencies: 96

Run `cloud_details(, "tidyjson")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidyjson-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: json_schema
    > ### Title: Create a schema for a JSON document or collection
    > ### Aliases: json_schema
    > 
    > ### ** Examples
    > 
    > 
    ...
     12.   └─vctrs::vec_default_cast(...)
     13.     ├─base::withRestarts(...)
     14.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     15.     │   └─base (local) doWithOneRestart(return(expr), restart)
     16.     └─vctrs::stop_incompatible_cast(...)
     17.       └─vctrs::stop_incompatible_type(...)
     18.         └─vctrs:::stop_incompatible(...)
     19.           └─vctrs:::stop_vctrs(...)
     20.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

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
      
      [ FAIL 8 | WARN 0 | SKIP 10 | PASS 347 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction-to-tidyjson.Rmd’ using rmarkdown
    
    Attaching package: 'tidyjson'
    
    The following object is masked from 'package:stats':
    
        filter
    
    
    ...
    Quitting from lines 327-328 (visualizing-json.Rmd) 
    Error: processing vignette 'visualizing-json.Rmd' failed with diagnostics:
    Can't convert `..1` <tbl_json> to <tbl_json>.
    --- failed re-building ‘visualizing-json.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘visualizing-json.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

