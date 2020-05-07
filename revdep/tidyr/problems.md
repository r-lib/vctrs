# biclustermd

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/biclustermd
* URL: http://github.com/jreisner/biclustermd
* BugReports: http://github.com/jreisner/biclustermd/issues
* Date/Publication: 2020-04-15 05:10:02 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"biclustermd")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    $runtime
       user  system elapsed 
      0.229   0.000   0.229 
    
    > autoplot(repeat_bc$best_bc)
    > plot(repeat_bc$rep_sse, type = 'b', pch = 20)
    > repeat_bc$runtime
       user  system elapsed 
      0.229   0.000   0.229 
    > 
    > # 20 repeats with parallelization over 2 cores
    > repeat_bc <- rep_biclustermd(synthetic, nrep = 20, parallel = TRUE, ncores = 2,
    +                              col_clusters = 3, row_clusters = 2,
    +                              miss_val = mean(synthetic, na.rm = TRUE),
    +                              miss_val_sd = sd(synthetic, na.rm = TRUE),
    +                              col_min_num = 2, row_min_num = 2,
    +                              col_num_to_move = 1, row_num_to_move = 1,
    +                              max.iter = 10)
    Error in z$SSE[z$iteration, 1] : incorrect number of dimensions
    Calls: rep_biclustermd -> sapply -> lapply -> FUN
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘nycflights13’
      All declared Imports should be used.
    ```

# simTool

<details>

* Version: 1.1.5
* Source code: https://github.com/cran/simTool
* URL: https://github.com/MarselScheer/simTool
* BugReports: https://github.com/MarselScheer/simTool/issues
* Date/Publication: 2020-03-15 20:10:02 UTC
* Number of recursive dependencies: 70

Run `revdep_details(,"simTool")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      fs$results not identical to fs$expected.
      names for target but not for current
      
      ── 3. Failure: Create a tibble containing the results sumamrized by one summary 
      fs$results not identical to fs$expected.
      names for target but not for current
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 97 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Failure: Simplify the simulation results (@test_eval_tibbles.R#381) 
      2. Failure: Create a tibble containing the results for every replication (@test_frame_simulation.R#33) 
      3. Failure: Create a tibble containing the results sumamrized by one summary function (@test_frame_simulation.R#73) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidyjson

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/tidyjson
* URL: https://github.com/colearendt/tidyjson
* BugReports: https://github.com/colearendt/tidyjson/issues
* Date/Publication: 2019-12-02 21:39:30
* Number of recursive dependencies: 89

Run `revdep_details(,"tidyjson")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 290 | SKIPPED: 7 | WARNINGS: 0 | FAILED: 10 ]
      1.  Failure: has correct complete structure with simple input (@test-append_values.R#7) 
      2.  Failure: recursive works as expected (@test-append_values.R#191) 
      3.  Failure: recursive works as expected (@test-append_values.R#206) 
      4.  Failure: works in a simple case (@test-gather_object.R#7) 
      5.  Failure: works with compound values (@test-gather_object.R#31) 
      6.  Failure: column.name works and doesn't clobber existing name (@test-gather_object.R#80) 
      7.  Error: preserves a NULL column (@test-gather_object.R#100) 
      8.  Failure: can call repeatedly without having to change column.name (@test-gather_object.R#150) 
      9.  Error: simple object works (@test-json_structure.R#27) 
      10. Error: nested object works (@test-json_structure.R#71) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

