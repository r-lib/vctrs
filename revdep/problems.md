# openalexR

<details>

* Version: 1.0.0
* GitHub: https://github.com/massimoaria/openalexR
* Source code: https://github.com/cran/openalexR
* Date/Publication: 2022-10-06 10:40:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "openalexR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘A_Brief_Introduction_to_openalexR.Rmd’ using rmarkdown
    Quitting from lines 260-269 (A_Brief_Introduction_to_openalexR.Rmd) 
    Error: processing vignette 'A_Brief_Introduction_to_openalexR.Rmd' failed with diagnostics:
    $ operator is invalid for atomic vectors
    --- failed re-building ‘A_Brief_Introduction_to_openalexR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘A_Brief_Introduction_to_openalexR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# portalr

<details>

* Version: 0.3.11
* GitHub: https://github.com/weecology/portalr
* Source code: https://github.com/cran/portalr
* Date/Publication: 2022-12-01 17:40:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "portalr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─portalr::bait_presence_absence(path = portal_data_path, level = "plot") at test-10-summarize_ants.R:49:2
       2. │ ├─compute_presence(bait, level) %>% as.data.frame()
       3. │ └─portalr:::compute_presence(bait, level)
       4. │   └─... %>% ...
       5. ├─base::as.data.frame(.)
       6. ├─tidyr::complete(., !!!grouping, fill = list(presence = 0))
       7. ├─dplyr::mutate(., presence = 1)
       8. ├─dplyr::distinct(.)
       9. └─dplyr::select(., !!!grouping)
      
      [ FAIL 12 | WARN 43 | SKIP 42 | PASS 17 ]
      Error: Test failures
      Execution halted
    ```

# rapbase

<details>

* Version: 1.24.0
* GitHub: https://github.com/Rapporteket/rapbase
* Source code: https://github.com/cran/rapbase
* Date/Publication: 2023-02-27 10:22:31 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "rapbase")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-github.R:6:3'): contributors are provided ────────────────────
      class(getGithub("contributors", "rapbase")) not equal to "character".
      1/1 mismatches
      x[1]: "NULL"
      y[1]: "character"
      ── Failure ('test-github.R:10:3'): key can be provided ─────────────────────────
      grepl("ssh-rsa", getGithub("keys", "areedv")) is not TRUE
      
      `actual`:       
      `expected`: TRUE
      
      [ FAIL 2 | WARN 0 | SKIP 37 | PASS 246 ]
      Error: Test failures
      Execution halted
    ```

