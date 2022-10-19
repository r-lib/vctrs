# globaltrends

<details>

* Version: 0.0.12
* GitHub: https://github.com/ha-pu/globaltrends
* Source code: https://github.com/cran/globaltrends
* Date/Publication: 2022-06-23 07:10:11 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "globaltrends")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test_functions.r’
      Running ‘testthat.r’
    Running the tests in ‘tests/testthat.r’ failed.
    Last 13 lines of output:
      Error in `initialize_db()`: Error: File 'db/globaltrends_db.sqlite' already exists.
      Backtrace:
          ▆
       1. └─globaltrends::initialize_db() at test-plot_voi_doi.R:8:0
      ── Error ('test-synonyms.R:8'): (code run outside of `test_that()`) ────────────
      Error in `initialize_db()`: Error: File 'db/globaltrends_db.sqlite' already exists.
      Backtrace:
          ▆
       1. └─globaltrends::initialize_db() at test-synonyms.R:8:0
      
      [ FAIL 15 | WARN 22 | SKIP 0 | PASS 33 ]
      Error: Test failures
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

# goldilocks

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/goldilocks
* Date/Publication: 2021-05-10 08:20:11 UTC
* Number of recursive dependencies: 68

Run `cloud_details(, "goldilocks")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-survival_adapt.R:54'): survival_adapt-cox ──────────────────────
      Error in `if (success > prob_ha) {
          expected_success_test <- expected_success_test + 1
      }`: missing value where TRUE/FALSE needed
      Backtrace:
          ▆
       1. └─goldilocks::survival_adapt(...) at test-survival_adapt.R:54:2
      
      [ FAIL 1 | WARN 2 | SKIP 1 | PASS 17 ]
      Error: Test failures
      Execution halted
    ```

# psfmi

<details>

* Version: 1.0.0
* GitHub: https://github.com/mwheymans/psfmi
* Source code: https://github.com/cran/psfmi
* Date/Publication: 2021-09-23 10:10:05 UTC
* Number of recursive dependencies: 156

Run `cloud_details(, "psfmi")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘psfmi-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: psfmi_validate
    > ### Title: Internal validation and performance of logistic prediction
    > ###   models across Multiply Imputed datasets
    > ### Aliases: psfmi_validate
    > 
    > ### ** Examples
    > 
    ...
    ! `strata` should be a single name or character value.
    Backtrace:
        ▆
     1. └─psfmi::psfmi_validate(...)
     2.   └─psfmi::cv_MI(...)
     3.     ├─purrr::map(...)
     4.     └─rsample::vfold_cv(data_orig, v = folds, strata = unlist(data_orig[pobj$Outcome]))
     5.       └─rsample:::strata_check(strata, data)
     6.         └─rlang::abort("`strata` should be a single name or character value.")
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘MI_boot.Rmd’ using rmarkdown
    --- finished re-building ‘MI_boot.Rmd’
    
    --- re-building ‘MI_cv_naive.Rmd’ using rmarkdown
    --- finished re-building ‘MI_cv_naive.Rmd’
    
    --- re-building ‘Pool_Model_Performance.Rmd’ using rmarkdown
    --- finished re-building ‘Pool_Model_Performance.Rmd’
    
    ...
    --- finished re-building ‘psfmi_StabilityAnalysis.Rmd’
    
    --- re-building ‘psfmi_mice.Rmd’ using rmarkdown
    --- finished re-building ‘psfmi_mice.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘cv_MI.Rmd’ ‘cv_MI_RR.Rmd’ ‘development_workflow.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘miceadds’
      All declared Imports should be used.
    ```

