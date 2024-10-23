# covidcast

<details>

* Version: 0.5.0
* GitHub: https://github.com/cmu-delphi/covidcast
* Source code: https://github.com/cran/covidcast
* Date/Publication: 2023-06-01 20:10:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "covidcast")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘correlation-utils.Rmd’ using rmarkdown
    --- finished re-building ‘correlation-utils.Rmd’
    
    --- re-building ‘covidcast.Rmd’ using rmarkdown
    --- finished re-building ‘covidcast.Rmd’
    
    --- re-building ‘external-data.Rmd’ using rmarkdown
    --- finished re-building ‘external-data.Rmd’
    
    ...
    ℹ In index: 1.
    Caused by error in `match.arg()`:
    ! 'arg' should be one of "day", "week"
    --- failed re-building ‘plotting-signals.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘multi-signals.Rmd’ ‘plotting-signals.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20 marked UTF-8 strings
    ```

# scGOclust

<details>

* Version: 0.1.0
* GitHub: https://github.com/YY-SONG0718/scGOclust
* Source code: https://github.com/cran/scGOclust
* Date/Publication: 2023-06-01 11:50:05 UTC
* Number of recursive dependencies: 211

Run `revdepcheck::cloud_details(, "scGOclust")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘scGOclust_mouse_fly_gut_vignette.Rmd’ using rmarkdown
    
    Quitting from lines 37-46 [load_input] (scGOclust_mouse_fly_gut_vignette.Rmd)
    Error: processing vignette 'scGOclust_mouse_fly_gut_vignette.Rmd' failed with diagnostics:
    Timeout was reached: [www.ensembl.org:443] Operation timed out after 10001 milliseconds with 0 bytes received
    --- failed re-building ‘scGOclust_mouse_fly_gut_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘scGOclust_mouse_fly_gut_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

