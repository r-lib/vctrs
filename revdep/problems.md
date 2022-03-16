# dibble

<details>

* Version: 0.1.0
* GitHub: https://github.com/UchidaMizuki/dibble
* Source code: https://github.com/cran/dibble
* Date/Publication: 2022-02-14 10:20:05 UTC
* Number of recursive dependencies: 45

Run `cloud_details(, "dibble")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `names(dim(expected))` is absent
      ── Failure (test-broadcast.R:9:3): broadcast ───────────────────────────────────
      as.array(y) (`actual`) not equal to array(1:3, 3) (`expected`).
      
      `names(dim(actual))` is a character vector ('axis2')
      `names(dim(expected))` is absent
      ── Failure (test-matrix.R:12:3): diag ──────────────────────────────────────────
      as.array(ddf_col) (`actual`) not equal to `arr` (`expected`).
      
      `names(dim(actual))` is a character vector ('axis1', 'axis2')
      `names(dim(expected))` is absent
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 28 ]
      Error: Test failures
      Execution halted
    ```

# taxa

<details>

* Version: 0.4.1
* GitHub: https://github.com/ropensci/taxa
* Source code: https://github.com/cran/taxa
* Date/Publication: 2022-03-11 22:30:02 UTC
* Number of recursive dependencies: 57

Run `cloud_details(, "taxa")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test--taxon_authority.R:193:3): taxon_authority objects can be converted to factor ──
      as.factor(x) not equal to as.factor(...).
      Attributes: < Component "levels": 3 string mismatches >
      ── Failure (test--taxonomy.R:364:3): taxonomy objects can be converted to factor ──
      as.factor(x) not equal to as.factor(as.character(x)).
      Attributes: < Component "levels": 5 string mismatches >
      ── Failure (test--taxonomy.R:372:3): named taxonomy objects can be converted to factor ──
      as.factor(x) not equal to as.factor(as.character(x)).
      Attributes: < Component "levels": 5 string mismatches >
      
      [ FAIL 3 | WARN 0 | SKIP 11 | PASS 406 ]
      Error: Test failures
      Execution halted
    ```

# tibblify

<details>

* Version: 0.1.0
* GitHub: https://github.com/mgirlich/tibblify
* Source code: https://github.com/cran/tibblify
* Date/Publication: 2020-09-23 08:40:07 UTC
* Number of recursive dependencies: 62

Run `cloud_details(, "tibblify")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (4)
      • lcol_fct not yet implemented (3)
      • no good way to present error message yet (1)
      • not yet decided what result should be (1)
      • not yet testable (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-guess_col.R:61:3): lists work ─────────────────────────────────
      `guess_col(list(a = list(a = 1), list(a = 2)), "a")` did not throw an error.
      
      [ FAIL 1 | WARN 0 | SKIP 10 | PASS 83 ]
      Error: Test failures
      Execution halted
    ```

# winch

<details>

* Version: 0.0.7
* GitHub: https://github.com/r-prof/winch
* Source code: https://github.com/cran/winch
* Date/Publication: 2021-10-24 09:20:02 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "winch")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘example0.R’
      Running ‘example1.R’
      Running ‘example2.R’
      Running ‘example3.R’
    Running the tests in ‘tests/example3.R’ failed.
    Last 13 lines of output:
      +     rlang_backtrace_on_error = "full",
      +     rlang_trace_use_winch = TRUE
      +   )
      + 
    ...
      + 
      +   tryCatch(vctrs::vec_as_location(quote, 2), error = identity)
      + }
      Loading required package: vctrs
      Error in winch::winch_init_library(vctrs:::vctrs_init$dll[["path"]]) : 
        object 'vctrs_init' not found
      Backtrace:
          ▆
       1. └─winch::winch_init_library(vctrs:::vctrs_init$dll[["path"]])
      Execution halted
    ```

