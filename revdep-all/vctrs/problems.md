# dm

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/dm
* Date/Publication: 2020-03-12 17:30:02 UTC
* Number of recursive dependencies: 116

Run `revdep_details(,"dm")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       3. testthat:::compare.default(act$val, exp$val)
       5. dplyr:::all.equal.tbl_df(x, y, ...)
       6. dplyr:::equal_data_frame(...)
      
      <unknown>:1753848: Invalid asm.js: Function definition doesn't match use
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 741 | SKIPPED: 42 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: dm_rm_tbl() works (@test-add-tbl.R#133) 
      2. Error: dm_get_filters() works (@test-dm.R#386) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      In file(filename, "r", encoding = encoding) :
        cannot open file '/Users/tobiasschieferdecker/git/cynkra/dm/.Rprofile': No such file or directory
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘DT’
      All declared Imports should be used.
    ```

# hardhat

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/hardhat
* URL: https://github.com/tidymodels/hardhat
* BugReports: https://github.com/tidymodels/hardhat/issues
* Date/Publication: 2020-02-28 07:20:16 UTC
* Number of recursive dependencies: 100

Run `revdep_details(,"hardhat")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ℹ Writing skeleton files
      ✔ Setting active project to '/private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/Rtmpw7YEm7/model'
      ✔ Writing 'R/random_forest-constructor.R'
      ✔ Writing 'R/random_forest-fit.R'
      ✔ Writing 'R/random_forest-predict.R'
      ● Run `devtools::document()`
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 404 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Failure: asking for the outcome works (@test-forge-xy.R#27) 
      2. Failure: asking for the outcome is special cased for vector `y` values (@test-forge-xy.R#52) 
      3. Failure: new_data can be a matrix (@test-forge-xy.R#84) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# slider

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/slider
* URL: https://github.com/DavisVaughan/slider
* BugReports: https://github.com/DavisVaughan/slider/issues
* Date/Publication: 2020-03-10 15:10:02 UTC
* Number of recursive dependencies: 59

Run `revdep_details(,"slider")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > # `hop()` let's you manually specify locations to apply `.f` at.
    > hop(1:3, .starts = c(1, 3), .stops = 3, ~.x)
    [[1]]
    [1] 1 2 3
    
    [[2]]
    [1] 3
    
    > 
    > # `hop()`'s start/stop locations are allowed to be out of bounds relative
    > # to the size of `.x`.
    > hop(
    +   mtcars,
    +   .starts = c(-1, 3),
    +   .stops  = c(2, 6),
    +   ~.x
    + )
    Error: The size of `names`, 32, must be the same as the size of `x`, 2.
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      slide(x, ~rownames(.x)) not equal to as.list(rownames(x)).
      names for target but not for current
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 608 | SKIPPED: 9 | WARNINGS: 0 | FAILED: 7 ]
      1. Failure: names are not placed on data frames rownames (@test-hop-index-vec.R#122) 
      2. Failure: names are not placed on data frames rownames (@test-hop-vec.R#97) 
      3. Failure: row names are not extracted from data frames (@test-slide-index.R#1069) 
      4. Failure: names are retained on inner sliced object (@test-slide-index.R#1087) 
      5. Failure: names are not placed on data frames rownames (@test-slide-vec.R#106) 
      6. Failure: row names are not extracted from data frames (@test-slide.R#471) 
      7. Failure: names are retained on inner sliced object (@test-slide.R#486) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tibbletime

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/tibbletime
* URL: https://github.com/business-science/tibbletime
* BugReports: https://github.com/business-science/tibbletime/issues
* Date/Publication: 2019-09-20 05:00:02 UTC
* Number of recursive dependencies: 64

Run `revdep_details(,"tibbletime")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. testthat::expect_equal(get_index_col(FANG_g_time), get_index_col(FANG_unnested))
        4. tibbletime::get_index_col(FANG_unnested)
        7. tibbletime::get_index_char(.tbl_time)
       13. tibbletime::get_index_quo(.tbl_time) revdep-all/vctrs/checks.noindex/tibbletime/new/tibbletime.Rcheck/00_pkg_src/tibbletime/R/getters.R:28:2
       14. tibbletime:::glue_stop("Object is not of class `tbl_time`.") revdep-all/vctrs/checks.noindex/tibbletime/new/tibbletime.Rcheck/00_pkg_src/tibbletime/R/getters.R:12:2
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 137 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Failure: nest() with index creates tbl_df (@test_compat-tidyr.R#25) 
      2. Failure: unnest() with index goes back to tbl_time (@test_compat-tidyr.R#49) 
      3. Error: unnest() with index goes back to tbl_time (@test_compat-tidyr.R#50) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

