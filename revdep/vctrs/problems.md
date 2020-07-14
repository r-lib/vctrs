# projects

<details>

* Version: 2.1.1
* Source code: https://github.com/cran/projects
* URL: https://cran.r-project.org/package=projects
* Date/Publication: 2020-05-29 12:40:02 UTC
* Number of recursive dependencies: 47

Run `cloud_details(, "projects")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    4    42 Art Department          Springfield College  321 University Boulevard, …
    > 
    > # View authors table joined to affiliations table
    > # Notice that multiple rows are created for each affiliation-author combination
    > authors(affiliations = TRUE)
    # A tibble: 6 x 11
         id last_name given_names title degree email phone affiliation_id
      <int> <chr>     <chr>       <chr> <chr>  <chr> <chr>          <int>
    1     1 Rice      Condoleezza <NA>  <NA>   <NA>  <NA>               1
    2     1 Rice      Condoleezza <NA>  <NA>   <NA>  <NA>              42
    3     1 Rice      Condoleezza <NA>  <NA>   <NA>  <NA>               2
    4     1 Rice      Condoleezza <NA>  <NA>   <NA>  <NA>               3
    5    13 Agnew     Spiro       <NA>  LLB    <NA>  <NA>              42
    6   303 <NA>      Plato       <NA>  <NA>   <NA>  <NA>              NA
    # … with 3 more variables: department_name <chr>, institution_name <chr>,
    #   address <chr>
    > 
    > # View only active projects with "Fun" in their title.
    > projects("Fun")
    Error: C stack usage  7973364 is too close to the limit
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      None.# A tibble: 1 x 6
           id title            stage         status  deadline_type deadline           
        <int> <chr>            <prjstg>      <chr>   <chr>         <dttm>             
      1     1 Understanding t… 4: manuscript waitin… submission    2055-02-28 00:00:00
      # A tibble: 2 x 7
        author_id last_name given_names title degree email         phone       
            <int> <chr>     <chr>       <chr> <chr>  <chr>         <chr>       
      1        13 Agnew     Spiro       <NA>  LLB    <NA>          <NA>        
      2      8888 Stone     Rosetta     <NA>  PhD    slab@rock.net 867-555-5309
      # A tibble: 1 x 3
        current_owner corresp_auth creator 
        <prjaut>      <prjaut>     <prjaut>
      1 13: Agnew     8888: Stone  0: root 
      Error: C stack usage  7973204 is too close to the limit
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# tibble

<details>

* Version: 3.0.3
* Source code: https://github.com/cran/tibble
* URL: https://tibble.tidyverse.org/, https://github.com/tidyverse/tibble
* BugReports: https://github.com/tidyverse/tibble/issues
* Date/Publication: 2020-07-10 20:40:03 UTC
* Number of recursive dependencies: 68

Run `cloud_details(, "tibble")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       11. base::tryCatch(.Call(vctrs_try_catch_callback, data, NULL), ...)
       12. base:::tryCatchList(expr, classes, parentenv, handlers)
       13. base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       14. base:::doTryCatch(return(expr), name, parentenv, handler)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1401 | SKIPPED: 108 | WARNINGS: 0 | FAILED: 5 ]
      1. Failure: [.tbl_df rejects unknown column indexes (#83) (@test-subsetting.R#176) 
      2. Failure: [.tbl_df rejects unknown column indexes (#83) (@test-subsetting.R#176) 
      3. Failure: [.tbl_df rejects unknown column indexes (#83) (@test-subsetting.R#176) 
      4. Error: can use classed character indexes (#778) (@test-subsetting.R#398) 
      5. Error: can use classed integer indexes (#778) (@test-subsetting.R#410) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidyselect

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/tidyselect
* URL: https://tidyselect.r-lib.org, https://github.com/r-lib/tidyselect
* BugReports: https://github.com/r-lib/tidyselect/issues
* Date/Publication: 2020-05-11 23:10:07 UTC
* Number of recursive dependencies: 54

Run `cloud_details(, "tidyselect")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. testthat::expect_error(...)
       23. vctrs:::try_catch_impl(...)
       24. base::tryCatch(.Call(vctrs_try_catch_callback, data, NULL), ...)
       25. base:::tryCatchList(expr, classes, parentenv, handlers)
       26. base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       27. base:::doTryCatch(return(expr), name, parentenv, handler)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 451 | SKIPPED: 18 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: vars_select() supports S3 vectors (#109) (@test-lifecycle-deprecated.R#150) 
      2. Failure: vars_select() type-checks inputs (@test-lifecycle-deprecated.R#162) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

