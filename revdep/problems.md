# brokenstick

<details>

* Version: 2.3.0
* GitHub: https://github.com/growthcharts/brokenstick
* Source code: https://github.com/cran/brokenstick
* Date/Publication: 2022-09-07 22:23:04 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "brokenstick")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘brokenstick.Rmd’ using rmarkdown
    --- finished re-building ‘brokenstick.Rmd’
    
    --- re-building ‘mainfunctions.Rmd’ using rmarkdown
    Loading required package: brokenstick
    Loading required package: dplyr
    
    Attaching package: 'dplyr'
    
    ...
    --- failed re-building ‘oldfriends.Rmd’
    
    --- re-building ‘perfectmodel.Rmd’ using rmarkdown
    --- finished re-building ‘perfectmodel.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘mainfunctions.Rmd’ ‘oldfriends.Rmd’
    
    Error: Vignette re-building failed.
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

