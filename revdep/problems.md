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
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-tidyverse.R:123'): split-apply-combine with dplyr can combine integers and errors ──
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `dplyr::mutate(., out = if (x) 0L else y)`: Problem while computing `out = if (x) 0L else y`.
      ℹ The error occurred in group 3: g = 3.
      Caused by error in `list_unchop()`:
      ! `ptype` and `out` must be lists of the same length.
      ℹ In file 'c.c' at line 386.
      ℹ This is an internal error that was detected in the vctrs package.
        Please report it at <https://github.com/r-lib/vctrs/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 322 ]
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

*   checking examples ... ERROR
    ```
    Running examples in ‘ricu-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: load_dictionary
    > ### Title: Load concept dictionaries
    > ### Aliases: load_dictionary concept_availability explain_dictionary
    > 
    > ### ** Examples
    > 
    > if (require(mimic.demo)) {
    + head(load_dictionary("mimic_demo"))
    + load_dictionary("mimic_demo", c("glu", "lact"))
    + }
    Loading required package: mimic.demo
    Error: C stack usage  9966868 is too close to the limit
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ────────────────────────────────────────────────────────────────────────────────
      
      
      Attaching package: 'ricu'
      
      The following objects are masked from 'package:testthat':
      
          is_false, is_true
      
      > 
      > # for running interactively, do Sys.setenv(TESTTHAT_PKG = "ricu")
      > 
      > test_check("ricu")
      Error: C stack usage  9961876 is too close to the limit
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘jss.Rmd’ using rmarkdown
    Error: C stack usage  9963460 is too close to the limit
    Execution halted
    --- re-building ‘ricu.Rmd’ using rmarkdown
    Error: C stack usage  9967556 is too close to the limit
    Execution halted
    --- re-building ‘uom.Rmd’ using rmarkdown
    Error: C stack usage  9967956 is too close to the limit
    Execution halted
    SUMMARY: processing the following files failed:
      ‘jss.Rmd’ ‘ricu.Rmd’ ‘uom.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# RSDA

<details>

* Version: 3.0.13
* GitHub: NA
* Source code: https://github.com/cran/RSDA
* Date/Publication: 2022-07-16 07:30:37 UTC
* Number of recursive dependencies: 154

Run `cloud_details(, "RSDA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(RSDA)
      
      
      Attaching package: 'RSDA'
      
      The following objects are masked from 'package:stats':
      
          cor, sd, var
      
      > 
      > test_check("RSDA")
      Error: C stack usage  9961492 is too close to the limit
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    Error: C stack usage  9961956 is too close to the limit
    Execution halted
    ```

