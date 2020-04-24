# dm

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/dm
* Date/Publication: 2020-03-12 17:30:02 UTC
* Number of recursive dependencies: 116

Run `revdep_details(,"dm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   unique_table_names = TRUE
    + ) %>%
    +   dm_get_con()
    Error: Can't convert <character> to <list>.
    Backtrace:
         █
      1. ├─`%>%`(...)
      2. │ └─base::eval(lhs, parent, parent)
      3. │   └─base::eval(lhs, parent, parent)
      4. ├─dm::copy_dm_to(dbplyr::src_memdb(), dm_nycflights13(), unique_table_names = TRUE)
      5. │ └─dm:::dm_set_key_constraints(remote_dm) 00_pkg_src/dm/R/db-interface.R:106:4
      6. │   └─dm::dm_get_all_pks(dm) 00_pkg_src/dm/R/db-interface.R:141:2
      7. │     └─dm_get_all_pks_impl(dm) %>% mutate(pk_col = new_keys(pk_col)) 00_pkg_src/dm/R/primary-keys.R:178:2
      8. │       ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      9. │       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     10. │         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     11. │           └─dm:::`_fseq`(`_lhs`)
     12. │             └─magrittr::freduce(value, `_function_list`)
     13. │               ├─base::withVisible(function_list[[k]](value))
     14. │               └─functi
    Execution halted
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │   └─testthat::test_dir(...)
        4. │     └─testthat::source_test_helpers(path, env)
        5. │       └─testthat::source_dir(path, "^helper.*\\.[rR]$", env = env, wrap = FALSE)
        6. │         └─base::lapply(files, source_file, env = env, chdir = chdir, wrap = wrap)
        7. │           └─testthat:::FUN(X[[i]], ...)
        8. │             └─base::eval(exprs, env)
        9. │               └─base::eval(exprs, env)
       10. │                 ├─dm_for_filter_src %<-% dm_test_load(dm_for_filter) helper-src.R:535:2
       11. │                 └─dm:::dm_test_load(dm_for_filter) helper-src.R:15:4
       12. │                   └─purrr::map(...) /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/checks.noindex/dm/new/dm.Rcheck/00_pkg_src/dm/R/test-dm.R:21:2
       13. │                     └─dm:::.f(.x[
      In addition: Warning message:
      In file(filename, "r", encoding = encoding) :
        cannot open file '/Users/tobiasschieferdecker/git/cynkra/dm/.Rprofile': No such file or directory
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘DT’
      All declared Imports should be used.
    ```

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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        8. vctrs::stop_incompatible_cast(...)
        9. vctrs:::stop_incompatible_type_convert(...)
       10. vctrs:::stop_incompatible_type_impl(...)
       11. vctrs:::stop_incompatible(...)
       12. vctrs:::stop_vctrs(...)
      
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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. testthat::expect_error(...)
        8. vctrs::vec_default_cast(...)
        9. vctrs::stop_incompatible_cast(...)
       10. vctrs:::stop_incompatible_type_convert(...)
       11. vctrs:::stop_incompatible_type_impl(...)
       12. vctrs:::stop_incompatible(...)
       13. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 406 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: ignoring novel levels still passes through incompatible classes (@test-scream.R#50) 
      
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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        8. vctrs:::stop_incompatible_type_impl(...)
        9. vctrs:::stop_incompatible(...)
       10. vctrs:::stop_vctrs(...)
      
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
     2. │ └─projects::projects_stage(1L) 00_pkg_src/projects/R/new.R:275:2
     3. │   └─vctrs::vec_cast(x, character()) 00_pkg_src/projects/R/class-projects_stage.R:73:2
     4. └─vctrs::vec_default_cast(...)
     5.   └─vctrs::stop_incompatible_cast(...)
     6.     └─vctrs:::stop_incompatible_type_convert(...)
     7.       └─vctrs:::stop_incompatible_type_impl(...)
     8.         └─vctrs:::stop_incompatible(...)
     9.           └─vctrs:::stop_vctrs(...)
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

*   checking tests ...
    ```
     ERROR
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

*   checking examples ... ERROR
    ```
    ...
    Error: Can't convert <tibble> to <tbl_time>.
    Backtrace:
         █
      1. ├─`%>%`(...)
      2. │ ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3. │ └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─`_fseq`(`_lhs`)
      6. │       └─magrittr::freduce(value, `_function_list`)
      7. │         ├─base::withVisible(function_list[[k]](value))
      8. │         └─function_list[[k]](value)
      9. │           ├─tidyr::nest(.)
     10. │           └─tibbletime:::nest.tbl_time(.)
     11. │             ├─dplyr::mutate(...) 00_pkg_src/tibbletime/R/compat-tidyr.R:44:6
     12. │             └─dplyr:::mutate.tbl_df(...)
     13. │               └─dplyr:::mutate_impl(.data, dots, caller_env())
     14. ├─vctrs::as_list_of(data, .ptype = data[[1]])
     15. ├─vctrs:::as_list_of.list(data, .ptype = data[[1]])
     16. │ └─vctrs::list_of(!!!x, .ptype = .ptype)
     17. │   └─
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       47. testthat:::failure_summary(result, self$n_fail)
       50. testthat:::format.expectation(x)
       51. testthat:::format_with_trace(x)
       53. rlang:::format.rlang_trace(...)
       54. rlang:::trace_format_branch(x, max_frames, dir, srcrefs)
       55. rlang:::branch_uncollapse_pipe(trace)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 126 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Error: nest() with index creates tbl_df (@test_compat-tidyr.R#22) 
      2. Error: nest() with index creates tbl_df (@test_compat-tidyr.R#20) 
      3. Error: (unknown) (@test_compat-tidyr.R#20) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidyr

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/tidyr
* URL: https://tidyr.tidyverse.org, https://github.com/tidyverse/tidyr
* BugReports: https://github.com/tidyverse/tidyr/issues
* Date/Publication: 2020-01-24 14:30:02 UTC
* Number of recursive dependencies: 61

Run `revdep_details(,"tidyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > df %>% unchop(y, ptype = tibble(y = integer()))
    Error: Can't convert `y` <character> to `y` <integer>.
    Backtrace:
         █
      1. ├─df %>% unchop(y, ptype = tibble(y = integer()))
      2. │ ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3. │ └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─`_fseq`(`_lhs`)
      6. │       └─magrittr::freduce(value, `_function_list`)
      7. │         ├─base::withVisible(function_list[[k]](value))
      8. │         └─function_list[[k]](value)
      9. │           └─tidyr::unchop(., y, ptype = tibble(y = integer()))
     10. │             └─vctrs::vec_rbind(!!!x, .ptype = ptype) 00_pkg_src/tidyr/R/chop.R:101:4
     11. └─vctrs::vec_default_cast(...)
     12.   └─vctrs::stop_incompatible_cast(...)
     13.     └─vctrs:::stop_incompatible_type_convert(...)
     14.       └─vctrs:::stop_incompatible_type_impl(...)
     15.         └─vctrs:::stop_incompatible(...)
     16.           └─vctr
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       55. rlang:::branch_uncollapse_pipe(trace)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 501 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 8 ]
      1. Error: can override default output column type (@test-pivot-long.R#69) 
      2. Error: can cast to custom type (@test-pivot-long.R#204) 
      3. Error: can require specific type with ptype (@test-rectangle.R#18) 
      4. Error: can require specific type with ptype (@test-rectangle.R#12) 
      5. Error: (unknown) (@test-rectangle.R#12) 
      6. Error: drops grouping when needed (@test-separate-rows.R#30) 
      7. Error: drops grouping when needed (@test-separate-rows.R#27) 
      8. Error: (unknown) (@test-separate-rows.R#27) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 24 marked UTF-8 strings
    ```

