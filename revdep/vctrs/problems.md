# haven

<details>

* Version: 2.3.0
* Source code: https://github.com/cran/haven
* URL: http://haven.tidyverse.org, https://github.com/tidyverse/haven, https://github.com/WizardMac/ReadStat
* BugReports: https://github.com/tidyverse/haven/issues
* Date/Publication: 2020-05-24 15:00:02 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "haven")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Can't convert <integer> to <labelled<double>>.
      Backtrace:
        1. testthat::expect_identical(...)
        9. vctrs:::vec_restore.vctrs_vctr(x = x, to = to, n = n)
       10. vctrs::stop_incompatible_cast(x, to, x_arg = "", to_arg = "")
       11. vctrs::stop_incompatible_type(...)
       12. vctrs:::stop_incompatible(...)
       13. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 300 | SKIPPED: 2 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: combining is symmetrical w.r.t. data types (@test-labelled.R#75) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘dplyr’
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# sf

<details>

* Version: 0.9-3
* Source code: https://github.com/cran/sf
* URL: https://r-spatial.github.io/sf/, https://github.com/r-spatial/sf/
* BugReports: https://github.com/r-spatial/sf/issues/
* Date/Publication: 2020-05-04 17:40:02 UTC
* Number of recursive dependencies: 145

Run `cloud_details(, "sf")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
    ...
    < CRS:            GEOGCS["NAD27",DATUM["North_American_Datum_1927",SPHEROID["Clarke 1866",6378206.4,294.9786982138982,AUTHORITY["EPSG","7008"]],AUTHORITY["EPSG","6267"]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433],AUTHORITY["EPSG","4267"]]
    ---
    > geographic CRS: NAD27
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       5. vctrs::vec_default_ptype2(...)
       6. vctrs::stop_incompatible_type(...)
       7. vctrs:::stop_incompatible(...)
       8. vctrs:::stop_vctrs(...)
      
      Failed to create feature 1 in x
      Failed to create feature 1 in x
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 704 | SKIPPED: 55 | WARNINGS: 5 | FAILED: 3 ]
      1. Failure: `precision` and `crs` attributes of `sfc` vectors are combined (@test_vctrs.R#48) 
      2. Failure: `precision` and `crs` attributes of `sfc` vectors are combined (@test_vctrs.R#51) 
      3. Error: `sfc` vectors have a common type (@test_vctrs.R#59) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 26.7Mb
      sub-directories of 1Mb or more:
        doc     11.9Mb
        libs    10.3Mb
        sqlite   1.5Mb
    ```

