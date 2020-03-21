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
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > 
    > # Needed to pass CRAN check / This is loaded by default
    > set_time_scale_template(time_scale_template())
    > 
    > data(tidyverse_cran_downloads)
    > 
    > tidyverse_cran_downloads %>%
    +     time_decompose(count, method = "stl") %>%
    +     anomalize(remainder, method = "iqr")
    Error: Error anomalize(): Object is not of class `tbl_df` or `tbl_time`.
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

# RxODE

<details>

* Version: 0.9.2-0
* Source code: https://github.com/cran/RxODE
* URL: https://nlmixrdevelopment.github.io/RxODE/
* BugReports: https://github.com/nlmixrdevelopment/RxODE/issues
* Date/Publication: 2020-03-13 07:10:14 UTC
* Number of recursive dependencies: 132

Run `revdep_details(,"RxODE")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error building model on another thread.
      Backtrace:
       1. RxODE::RxODE(model = ode, modName = "m1", wd = "/tmp")
       2. .env$compile() revdep-all/tidyr/checks.noindex/RxODE/new/RxODE.Rcheck/00_pkg_src/RxODE/R/RxODE.R:417:4
       4. base::with.default(...)
       5. [ base::eval(...) ] with 1 more call
       8. RxODE:::rxCompile.rxModelVars(...) revdep-all/tidyr/checks.noindex/RxODE/new/RxODE.Rcheck/00_pkg_src/RxODE/R/RxODE.R:1325:4
      
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 890 | SKIPPED: 122 | WARNINGS: 20 | FAILED: 1 ]
      1. Error: Issue #56 (@test-issue-56.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      'SnakeCharmR', 'installr'
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        libs   4.3Mb
        R      1.0Mb
    ```

# whatr

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/whatr
* URL: https://github.com/kiernann/whatr
* BugReports: https://github.com/kiernann/whatr/issues
* Date/Publication: 2020-03-19 13:30:02 UTC
* Number of recursive dependencies: 75

Run `revdep_details(,"whatr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(whatr)
      > 
      > test_check("whatr")
      ── 1. Failure: players return from HTML (@test-players.R#9)  ───────────────────
      nrow(p) not equal to 3.
      1/1 mismatches
      [1] 9 - 3 == 6
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 63 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: players return from HTML (@test-players.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 175 marked UTF-8 strings
    ```

