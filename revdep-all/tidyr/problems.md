# geoviz

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/geoviz
* URL: https://github.com/neilcharles/geoviz/
* Date/Publication: 2020-01-12 10:50:02 UTC
* Number of recursive dependencies: 101

Run `revdep_details(,"geoviz")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(geoviz)
      > 
      > test_check("geoviz")
      ── 1. Failure: slippy_raster() returns data (@test_imagery.R#20)  ──────────────
      `slippy_raster(...)` did not produce any warnings.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 14 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: slippy_raster() returns data (@test_imagery.R#20) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

