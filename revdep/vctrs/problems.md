# glue

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/glue
* URL: https://github.com/tidyverse/glue, https://glue.tidyverse.org/
* BugReports: https://github.com/tidyverse/glue/issues
* Date/Publication: 2020-04-03 14:10:30 UTC
* Number of recursive dependencies: 83

Run `revdep_details(,"glue")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        7. vctrs::vec_default_cast(...)
        8. vctrs::stop_incompatible_cast(...)
        9. vctrs::stop_incompatible_type(...)
       10. vctrs:::stop_incompatible(...)
       11. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 210 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 4 ]
      1. Failure: coercion is not inherited (@test-vctrs.R#48) 
      2. Failure: coercion is not inherited (@test-vctrs.R#52) 
      3. Failure: coercion is not inherited (@test-vctrs.R#56) 
      4. Failure: coercion is not inherited (@test-vctrs.R#60) 
      
      Error: testthat unit tests failed
      Execution halted
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

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. testthat::expect_error(...)
        8. vctrs::vec_default_cast(...)
        9. vctrs::stop_incompatible_cast(...)
       10. vctrs::stop_incompatible_type(...)
       11. vctrs:::stop_incompatible(...)
       12. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 402 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: missing ordered factor levels are handled correctly (@test-forge-formula.R#510) 
      2. Failure: ignoring novel levels still passes through incompatible classes (@test-scream.R#50) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ipaddress

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/ipaddress
* URL: https://davidchall.github.io/ipaddress, https://github.com/davidchall/ipaddress
* BugReports: https://github.com/davidchall/ipaddress/issues
* Date/Publication: 2020-03-25 17:30:02 UTC
* Number of recursive dependencies: 41

Run `revdep_details(,"ipaddress")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. testthat::expect_error(...)
        7. vctrs::vec_default_cast(...)
        8. vctrs::stop_incompatible_cast(...)
        9. vctrs::stop_incompatible_type(...)
       10. vctrs:::stop_incompatible(...)
       11. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 508 | SKIPPED: 3 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: casting works (@test-ip_interface.R#35) 
      2. Failure: casting works (@test-ip_interface.R#36) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# probably

<details>

* Version: 0.0.4
* Source code: https://github.com/cran/probably
* URL: https://github.com/tidymodels/probably/
* BugReports: https://github.com/tidymodels/probably/issues
* Date/Publication: 2020-01-13 17:00:05 UTC
* Number of recursive dependencies: 85

Run `revdep_details(,"probably")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       7. vctrs::stop_incompatible_type(...)
       8. vctrs:::stop_incompatible(...)
       9. vctrs:::stop_vctrs(...)
      
      ── 3. Failure: casting character to class_pred (@test-class-pred.R#198)  ───────
      is_ordered_class_pred(vec_cast(chr1, cp3)) isn't true.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 111 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 3 ]
      1. Error: class_pred can be coerced to ordered factor (@test-class-pred.R#74) 
      2. Error: casting class_pred to factor (@test-class-pred.R#124) 
      3. Failure: casting character to class_pred (@test-class-pred.R#198) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# projects

<details>

* Version: 2.1.0
* Source code: https://github.com/cran/projects
* URL: https://cran.r-project.org/package=projects
* Date/Publication: 2020-03-18 16:40:02 UTC
* Number of recursive dependencies: 48

Run `revdep_details(,"projects")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    New author:
    # A tibble: 1 x 7
         id last_name given_names title degree email phone
      <int> <chr>     <chr>       <chr> <chr>  <chr> <chr>
    1   303 <NA>      Plato       <NA>  <NA>   <NA>  <NA> 
    
    New author's affiliations:
    None.> new_project(title = "Test Project 1", authors = c(13, "303", "Stone"),
    +             corresp_auth = "Stone")
    Error: Can't convert <integer> to <character>.
    Backtrace:
        █
     1. ├─projects::new_project(...)
     2. │ └─projects::projects_stage(1L)
     3. │   └─vctrs::vec_cast(x, character())
     4. └─vctrs::vec_default_cast(...)
     5.   └─vctrs::stop_incompatible_cast(...)
     6.     └─vctrs::stop_incompatible_type(...)
     7.       └─vctrs:::stop_incompatible(...)
     8.         └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# slider

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/slider
* URL: https://github.com/DavisVaughan/slider
* BugReports: https://github.com/DavisVaughan/slider/issues
* Date/Publication: 2020-03-10 15:10:02 UTC
* Number of recursive dependencies: 61

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

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 566 | SKIPPED: 9 | WARNINGS: 0 | FAILED: 42 ]
      1. Error: inner type is allowed to be different (@test-hop-index-vec.R#12) 
      2. Failure: inner type can be restricted with list_of (@test-hop-index-vec.R#19) 
      3. Error: .ptype is respected (@test-hop-index-vec.R#31) 
      4. Failure: names are not placed on data frames rownames (@test-hop-index-vec.R#122) 
      5. Error: inner type is allowed to be different (@test-hop-vec.R#12) 
      6. Failure: inner type can be restricted with list_of (@test-hop-vec.R#19) 
      7. Error: .ptype is respected (@test-hop-vec.R#31) 
      8. Failure: names are not placed on data frames rownames (@test-hop-vec.R#97) 
      9. Error: pslide_index_chr() can coerce (@test-pslide-index-vec.R#54) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

