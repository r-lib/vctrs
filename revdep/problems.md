# dplyr

<details>

* Version: 1.1.0
* GitHub: https://github.com/tidyverse/dplyr
* Source code: https://github.com/cran/dplyr
* Date/Publication: 2023-01-29 22:50:02 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "dplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • Can't use 'en_US' locale (2)
      • On CRAN (305)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-filter.R:301'): hybrid function row_number does not trigger warning in filter (#3750) ──
      `out` is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      ── Failure ('test-join-by.R:236'): nicely catches missing arguments when wrapped ──
      `fn(a)` did not throw the expected error.
      
      [ FAIL 2 | WARN 270 | SKIP 311 | PASS 2742 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# GenomeAdmixR

<details>

* Version: 2.1.7
* GitHub: https://github.com/thijsjanzen/GenomeAdmixR
* Source code: https://github.com/cran/GenomeAdmixR
* Date/Publication: 2022-03-01 21:10:15 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "GenomeAdmixR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        |                                                                      |   0%
        |                                                                            
        |===================================                                   |  50%
        |                                                                            
        |======================================================================| 100%[ FAIL 1 | WARN 0 | SKIP 0 | PASS 454 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-simulate_admixture_data.R:202'): simulate_admixture_data_recombination_map ──
      `all_j` not equal to `expected_num_j`.
      1/1 mismatches
      [1] 71 - 100 == -29
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 454 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.9Mb
      sub-directories of 1Mb or more:
        doc    2.0Mb
        libs   9.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# photosynthesis

<details>

* Version: 2.1.1
* GitHub: https://github.com/cdmuir/photosynthesis
* Source code: https://github.com/cran/photosynthesis
* Date/Publication: 2022-11-19 19:40:09 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::cloud_details(, "photosynthesis")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Expected `{ ... }` to run without any conditions.
      ℹ Actually got a <lifecycle_stage>:
        Condition:
        `flatten()` is deprecated as of rlang 1.1.0. ℹ Please use
        `purrr::list_flatten()` or `purrr::list_c()`.
      ── Failure ('test-fit_aq_response2.R:44'): .vars argument renames variables ────
      Expected `{ ... }` to run without any conditions.
      ℹ Actually got a <lifecycle_stage>:
        Condition:
        `flatten()` is deprecated as of rlang 1.1.0. ℹ Please use
        `purrr::list_flatten()` or `purrr::list_c()`.
      
      [ FAIL 6 | WARN 2 | SKIP 0 | PASS 320 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        doc   6.1Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 13 marked UTF-8 strings
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

# rlang

<details>

* Version: 1.0.6
* GitHub: https://github.com/r-lib/rlang
* Source code: https://github.com/cran/rlang
* Date/Publication: 2022-09-24 05:40:02 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "rlang")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘sink.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12.   ├─base::namespaceImportFrom(...)
       13.   │ └─base::asNamespace(ns)
       14.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
       15.     ├─base::namespaceImportFrom(...)
       16.     │ └─base::asNamespace(ns)
       17.     └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
       18.       ├─base::namespaceImportFrom(...)
       19.       │ └─base::asNamespace(ns)
       20.       └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
       21.         ├─base::namespaceImport(...)
       22.         └─base::loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])
      
      [ FAIL 2 | WARN 2 | SKIP 235 | PASS 3661 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘winch’
    ```

