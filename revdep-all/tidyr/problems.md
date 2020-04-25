# anomalize

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/anomalize
* URL: https://github.com/business-science/anomalize
* BugReports: https://github.com/business-science/anomalize/issues
* Date/Publication: 2019-09-21 04:10:03 UTC
* Number of recursive dependencies: 161

Run `revdep_details(,"anomalize")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > tidyverse_cran_downloads %>%
    +     time_decompose(count, method = "stl") %>%
    +     anomalize(remainder, method = "iqr")
    Error: Can't combine `..1$nested.col` <tbl_time> and `..2$nested.col` <tbl_time>.
    ✖ Some attributes are incompatible.
    ℹ The author of the class should implement vctrs methods.
    ℹ See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.
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
     11. │               ├─base::withVisible(e
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

# BiocPkgTools

<details>

* Version: 1.4.6
* Source code: https://github.com/cran/BiocPkgTools
* URL: https://github.com/seandavi/BiocPkgTools
* BugReports: https://github.com/seandavi/BiocPkgTools/issues/new
* Date/Publication: 2020-03-16
* Number of recursive dependencies: 116

Run `revdep_details(,"BiocPkgTools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BiocPkgTools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pkgCombDependencyGain
    > ### Title: Calculate dependency gain achieved by excluding combinations of
    > ###   packages
    > ### Aliases: pkgCombDependencyGain
    > 
    > ### ** Examples
    > 
    > depdf <- buildPkgDependencyDataFrame(
    +   dependencies=c("Depends", "Imports"), 
    +   repo=c("BioCsoft", "CRAN")
    + )
    Error in readRDS(gzcon(con)) : 
      ReadItem: unknown type 101, perhaps written by later version of R
    Calls: buildPkgDependencyDataFrame ... biocPkgList -> lapply -> FUN -> as.data.frame -> readRDS
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rappdirs’
      All declared Imports should be used.
    Unexported object imported by a ':::' call: ‘BiocManager:::.repositories’
      See the note in ?`:::` about the use of this operator.
    ```

# cutpointr

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/cutpointr
* URL: https://github.com/thie1e/cutpointr
* BugReports: https://github.com/thie1e/cutpointr/issues
* Date/Publication: 2020-04-14 08:50:10 UTC
* Number of recursive dependencies: 78

Run `revdep_details(,"cutpointr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 493 | SKIPPED: 0 | WARNINGS: 1643 | FAILED: 10 ]
      1.  Failure: LOESS smoothing does not return warnings or errors (@test-cutpointr.R#681) 
      2.  Failure: LOESS smoothing does not return warnings or errors (@test-cutpointr.R#696) 
      3.  Failure: LOESS smoothing does not return warnings or errors (@test-cutpointr.R#704) 
      4.  Failure: LOESS smoothing does not return warnings or errors (@test-cutpointr.R#718) 
      5.  Failure: LOESS smoothing does not return warnings or errors (@test-cutpointr.R#729) 
      6.  Failure: cutpointr works if method / metric are called with :: (@test-cutpointr.R#1291) 
      7.  Failure: cutpointr works if method / metric are called with :: (@test-cutpointr.R#1294) 
      8.  Failure: Summary(multi_cutpointr) is silent (@test-cutpointr.R#1324) 
      9.  Failure: Summary(multi_cutpointr) is silent (@test-cutpointr.R#1331) 
      10. Failure: multi_cutpointr fetches numeric columns correctly (@test-cutpointr.R#1356) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# simTool

<details>

* Version: 1.1.5
* Source code: https://github.com/cran/simTool
* URL: https://github.com/MarselScheer/simTool
* BugReports: https://github.com/MarselScheer/simTool/issues
* Date/Publication: 2020-03-15 20:10:02 UTC
* Number of recursive dependencies: 70

Run `revdep_details(,"simTool")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      fs$results not identical to fs$expected.
      names for target but not for current
      
      ── 3. Failure: Create a tibble containing the results sumamrized by one summary 
      fs$results not identical to fs$expected.
      names for target but not for current
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 97 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Failure: Simplify the simulation results (@test_eval_tibbles.R#381) 
      2. Failure: Create a tibble containing the results for every replication (@test_frame_simulation.R#33) 
      3. Failure: Create a tibble containing the results sumamrized by one summary function (@test_frame_simulation.R#73) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidyjson

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/tidyjson
* URL: https://github.com/colearendt/tidyjson
* BugReports: https://github.com/colearendt/tidyjson/issues
* Date/Publication: 2019-12-02 21:39:30
* Number of recursive dependencies: 89

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

