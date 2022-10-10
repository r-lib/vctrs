# errors

<details>

* Version: 0.3.6
* GitHub: https://github.com/r-quantities/errors
* Source code: https://github.com/cran/errors
* Date/Publication: 2020-11-10 16:50:02 UTC
* Number of recursive dependencies: 52

Run `cloud_details(, "errors")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ℹ This is an internal error that was detected in the vctrs package.
        Please report it at <https://github.com/r-lib/vctrs/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.
      ── Error ('test-tidyverse.R:123'): split-apply-combine with dplyr can combine integers and errors ──
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `dplyr::mutate(., out = if (x) 0L else y)`: Problem while computing `out = if (x) 0L else y`.
      ℹ The error occurred in group 3: g = 3.
      Caused by error in `list_unchop()`:
      ! `ptype` and `out` must be lists of the same length.
      ℹ In file 'c.c' at line 375.
      ℹ This is an internal error that was detected in the vctrs package.
        Please report it at <https://github.com/r-lib/vctrs/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.
      
      [ FAIL 4 | WARN 2 | SKIP 0 | PASS 317 ]
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

# lubridate

<details>

* Version: 1.8.0
* GitHub: https://github.com/tidyverse/lubridate
* Source code: https://github.com/cran/lubridate
* Date/Publication: 2021-10-07 15:20:02 UTC
* Number of recursive dependencies: 59

Run `cloud_details(, "lubridate")` for more info

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
       1. ├─testthat::expect_identical(vec_order(vec_c(y, x)), c(2L, 1L)) at test-vctrs.R:520:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─vctrs::vec_order(vec_c(y, x))
       5. │ └─vctrs::vec_proxy_order(x)
       6. ├─vctrs::vec_c(y, x)
       7. └─rlang:::stop_internal_c_lib(...)
       8.   └─rlang::abort(message, call = call, .internal = TRUE, .frame = frame)
      
      [ FAIL 7 | WARN 0 | SKIP 8 | PASS 2737 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'chron', 'timeDate', 'tis', 'zoo'
    ```

# quantities

<details>

* Version: 0.1.6
* GitHub: https://github.com/r-quantities/quantities
* Source code: https://github.com/cran/quantities
* Date/Publication: 2021-02-21 15:50:02 UTC
* Number of recursive dependencies: 57

Run `cloud_details(, "quantities")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       2. │ └─vctrs::list_unchop(...)
       3. └─rlang:::stop_internal_c_lib(...)
       4.   └─rlang::abort(message, call = call, .internal = TRUE, .frame = frame)
      ── Error ('test-tidyverse.R:98'): split-apply-combine with dplyr and base agree ──
      Error in `dplyr::summarise(., dplyr::across(where(is.numeric), mean))`: Problem while computing `..1 = dplyr::across(where(is.numeric), mean)`.
      ℹ The error occurred in group 3: Species = virginica.
      Caused by error in `vec_c()`:
      ! `ptype` and `out` must be lists of the same length.
      ℹ In file 'c.c' at line 375.
      ℹ This is an internal error that was detected in the vctrs package.
        Please report it at <https://github.com/r-lib/vctrs/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.
      
      [ FAIL 2 | WARN 12 | SKIP 0 | PASS 634 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    --- failed re-building ‘introduction.Rmd’
    
    --- re-building ‘parsing.Rmd’ using rmarkdown
    --- finished re-building ‘parsing.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
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

# tibble

<details>

* Version: 3.1.8
* GitHub: https://github.com/tidyverse/tibble
* Source code: https://github.com/cran/tibble
* Date/Publication: 2022-07-22 06:10:02 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "tibble")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::expect_equal(...) at test-tribble.R:170:2
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─tibble::tribble(~x, lubridate::days(1), lubridate::days(2))
        5. │ └─tibble:::turn_frame_data_into_tibble(data$frame_names, data$frame_rest)
        6. │   └─tibble:::turn_matrix_into_column_list(frame_mat)
        7. │     ├─tibble:::subclass_tribble_c_errors(names(frame_col)[[i]], col <- vec_c(!!!unname(col)))
        8. │     │ └─base::withCallingHandlers(...)
        9. │     └─vctrs::vec_c(!!!unname(col))
       10. └─rlang:::stop_internal_c_lib(...)
       11.   └─rlang::abort(message, call = call, .internal = TRUE, .frame = frame)
      
      [ FAIL 1 | WARN 1 | SKIP 144 | PASS 1335 ]
      Error: Test failures
      Execution halted
    ```

