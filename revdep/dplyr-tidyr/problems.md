# gtfs2gps

<details>

* Version: 1.3-2
* Source code: https://github.com/cran/gtfs2gps
* URL: https://github.com/ipeaGIT/gtfs2gps
* BugReports: https://github.com/ipeaGIT/gtfs2gps/issues
* Date/Publication: 2020-11-05 17:30:12 UTC
* Number of recursive dependencies: 78

Run `cloud_details(, "gtfs2gps")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       18.                       ├─future::resolve(...)
       19.                       └─future:::resolve.list(...)
       20.                         ├─future::value(obj, stdout = FALSE, signal = FALSE)
       21.                         └─future:::value.Future(obj, stdout = FALSE, signal = FALSE)
       22.                           ├─future::result(future)
       23.                           └─future:::result.MulticoreFuture(future)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      Warning (test_gtfs2gps.R:71:5): gtfs2gps
      Warning (test_gtfs2gps.R:71:5): gtfs2gps
      ERROR (test_gtfs2gps.R:71:5): gtfs2gps
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 158 ]
      Error: Test failures
      Execution halted
    ```

# survParamSim

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/survParamSim
* URL: https://github.com/yoshidk6/survParamSim
* BugReports: https://github.com/yoshidk6/survParamSim/issues
* Date/Publication: 2020-06-18 06:00:03 UTC
* Number of recursive dependencies: 164

Run `cloud_details(, "survParamSim")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       20.                           └─base::unname(proxy)
       21.                             ├─base::`names<-`(`*tmp*`, value = NULL)
       22.                             └─tibble:::`names<-.tbl_df`(`*tmp*`, value = NULL)
       23.                               └─lifecycle::deprecate_soft(...)
       24.                                 └─lifecycle::deprecate_warn(...)
       25.                                   └─lifecycle::deprecate_stop(when, what, with = with, details = details)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      ERROR (test-calc_hr_pi.R:89:3): check HR calculation
      ERROR (test-calc_hr_pi.R:101:3): check TRT levels assignment
      ERROR (test-calc_km_pi.R:15:1): (code run outside of `test_that()`)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 16 ]
      Error: Test failures
      Execution halted
    ```

