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

