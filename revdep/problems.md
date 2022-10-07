# dm

<details>

* Version: 1.0.2
* GitHub: https://github.com/cynkra/dm
* Source code: https://github.com/cran/dm
* Date/Publication: 2022-09-20 07:46:26 UTC
* Number of recursive dependencies: 152

Run `cloud_details(, "dm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • only works on `sqlite` (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-dplyr.R:210:3): basic test: 'slice()'-methods work ────────────
      `expect_equivalent_tbl(...)` produced warnings.
      ── Failure (test-filter-dm.R:200:3): dm_filter() works without primary keys ────
      ``%>%`(...)` produced warnings.
      ── Failure (test-dm.R:49:3): dm() works for adding tables ──────────────────────
      `expect_equivalent_tbl(...)` produced warnings.
      ── Failure (test-validate.R:13:3): validator is silent ─────────────────────────
      `dm(a = tibble(x = 1)) %>% dm_add_pk(a, x) %>% dm_validate()` produced warnings.
      
      [ FAIL 4 | WARN 639 | SKIP 191 | PASS 1333 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘dm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dm_flatten_to_tbl
    > ### Title: Flatten a part of a 'dm' into a wide table
    > ### Aliases: dm_flatten_to_tbl
    > 
    > ### ** Examples
    > 
    > 
    ...
      8. │   └─dm:::check_dm(dm)
      9. │     └─dm::is_dm(dm)
     10. ├─dm::dm_financial()
     11. │ ├─base::withVisible(eval(mc, parent.frame()))
     12. │ └─base::eval(mc, parent.frame())
     13. │   └─base::eval(mc, parent.frame())
     14. └─dm (local) `<fn>`()
     15.   └─dm:::financial_db_con()
     16.     └─rlang::abort(...)
    Execution halted
    ```

# ggh4x

<details>

* Version: 0.2.2
* GitHub: https://github.com/teunbrand/ggh4x
* Source code: https://github.com/cran/ggh4x
* Date/Publication: 2022-08-14 16:50:13 UTC
* Number of recursive dependencies: 78

Run `cloud_details(, "ggh4x")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (6)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-facetted_pos_scales.R:313:3): facetted_pos_scales can handle empty panels ──
      `ggplotGrob(g)` produced warnings.
      ── Failure (test-facetted_pos_scales.R:326:3): facetted_pos_scales can handle discrete scales ──
      `ggplotGrob(g)` produced warnings.
      ── Failure (test-facetted_pos_scales.R:341:3): facetted_pos_scales can handle date scales ──
      `ggplotGrob(g)` produced warnings.
      
      [ FAIL 3 | WARN 24 | SKIP 6 | PASS 769 ]
      Error: Test failures
      Execution halted
    ```

# ggip

<details>

* Version: 0.2.2
* GitHub: https://github.com/davidchall/ggip
* Source code: https://github.com/cran/ggip
* Date/Publication: 2022-09-29 06:00:02 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "ggip")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Loading required package: ipaddress
      > 
      > test_check("ggip")
      [ FAIL 1 | WARN 47 | SKIP 5 | PASS 93 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (5)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-stat-summary-address.R:97:3): addresses outside 2D grid raise warning ──
      `layer_data(p + stat_summary_address(na.rm = TRUE))` produced warnings.
      
      [ FAIL 1 | WARN 47 | SKIP 5 | PASS 93 ]
      Error: Test failures
      Execution halted
    ```

# gratia

<details>

* Version: 0.7.3
* GitHub: https://github.com/gavinsimpson/gratia
* Source code: https://github.com/cran/gratia
* Date/Publication: 2022-05-09 11:20:03 UTC
* Number of recursive dependencies: 83

Run `cloud_details(, "gratia")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      • hgam-paper/hgam-paper-bird-move-model-1.svg
      • hgam-paper/hgam-paper-bird-move-model-2.svg
      • hgam-paper/hgam-paper-bird-move-model-3.svg
      • hgam-paper/hgam-paper-bird-move-model-5.svg
      • hgam-paper/hgam-paper-co2-model-1.svg
      • hgam-paper/hgam-paper-co2-model-2.svg
      • hgam-paper/hgam-paper-co2-model-3.svg
      • hgam-paper/hgam-paper-co2-model-4.svg
      • hgam-paper/hgam-paper-co2-model-5.svg
      • hgam-paper/hgam-paper-zoop-model-4.svg
      • hgam-paper/hgam-paper-zoop-model-5.svg
      • rootograms/draw-gaussian-rootogram.svg
      • rootograms/draw-neg-bin-rootogram.svg
      Error: Test failures
      Execution halted
    ```

# groupr

<details>

* Version: 0.1.0
* GitHub: https://github.com/ngriffiths21/groupr
* Source code: https://github.com/cran/groupr
* Date/Publication: 2020-10-14 12:30:06 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "groupr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        Please report it at <https://github.com/r-lib/vctrs/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.
      Backtrace:
          ▆
       1. ├─... %>% group_by2(is_ok, grp) at test_pivots.R:3:0
       2. ├─groupr::group_by2(., is_ok, grp)
       3. ├─groupr:::group_by2.data.frame(., is_ok, grp)
       4. │ └─groupr:::group_by2_ok(data, dots)
       5. │   └─groupr:::igrouped_df(grouped, groups_out)
       6. │     └─vctrs::vec_rbind(groups, data.frame())
       7. └─rlang:::stop_internal_c_lib(...)
       8.   └─rlang::abort(message, call = call, .internal = TRUE, .frame = frame)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    Quitting from lines 33-35 (introduction.Rmd) 
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    Column `.rows` (size 0) must match the data frame (size 2).
    ℹ In file 'slice.c' at line 188.
    ℹ This is an internal error that was detected in the vctrs package.
      Please report it at <https://github.com/r-lib/vctrs/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# recipes

<details>

* Version: 1.0.1
* GitHub: https://github.com/tidymodels/recipes
* Source code: https://github.com/cran/recipes
* Date/Publication: 2022-07-07 22:30:06 UTC
* Number of recursive dependencies: 128

Run `cloud_details(, "recipes")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (343)
      • dimRed cannot be loaded (10)
      • mixOmics cannot be loaded (14)
      • redundant with check_new_data checks (1)
      • tune_check() is TRUE (6)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test_relu.R:63:3): works with all_predictors() selector ────────────
      `prepped_rec <- prep(rec, iris)` produced warnings.
      
      [ FAIL 1 | WARN 255 | SKIP 374 | PASS 1753 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘fastICA’, ‘dimRed’
    ```

# ricu

<details>

* Version: 0.5.3
* GitHub: https://github.com/eth-mds/ricu
* Source code: https://github.com/cran/ricu
* Date/Publication: 2022-07-12 10:50:14 UTC
* Number of recursive dependencies: 114

Run `cloud_details(, "ricu")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ricu-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: load_dictionary
    > ### Title: Load concept dictionaries
    > ### Aliases: load_dictionary concept_availability explain_dictionary
    > 
    > ### ** Examples
    > 
    > if (require(mimic.demo)) {
    + head(load_dictionary("mimic_demo"))
    + load_dictionary("mimic_demo", c("glu", "lact"))
    + }
    Loading required package: mimic.demo
    Error: C stack usage  9967268 is too close to the limit
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ────────────────────────────────────────────────────────────────────────────────
      
      
      Attaching package: 'ricu'
      
      The following objects are masked from 'package:testthat':
      
          is_false, is_true
      
      > 
      > # for running interactively, do Sys.setenv(TESTTHAT_PKG = "ricu")
      > 
      > test_check("ricu")
      Error: C stack usage  9968580 is too close to the limit
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘jss.Rmd’ using rmarkdown
    Error: C stack usage  9966020 is too close to the limit
    Execution halted
    --- re-building ‘ricu.Rmd’ using rmarkdown
    Error: C stack usage  9968660 is too close to the limit
    Execution halted
    --- re-building ‘uom.Rmd’ using rmarkdown
    Error: C stack usage  9961828 is too close to the limit
    Execution halted
    SUMMARY: processing the following files failed:
      ‘jss.Rmd’ ‘ricu.Rmd’ ‘uom.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# RSDA

<details>

* Version: 3.0.13
* GitHub: NA
* Source code: https://github.com/cran/RSDA
* Date/Publication: 2022-07-16 07:30:37 UTC
* Number of recursive dependencies: 154

Run `cloud_details(, "RSDA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(RSDA)
      
      
      Attaching package: 'RSDA'
      
      The following objects are masked from 'package:stats':
      
          cor, sd, var
      
      > 
      > test_check("RSDA")
      Error: C stack usage  9969876 is too close to the limit
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    Error: C stack usage  9969444 is too close to the limit
    Execution halted
    ```

# tidyr

<details>

* Version: 1.2.1
* GitHub: https://github.com/tidyverse/tidyr
* Source code: https://github.com/cran/tidyr
* Date/Publication: 2022-09-08 07:30:02 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "tidyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::expect_warning(...) at test-rectangle.R:349:2
        2. │ └─testthat:::expect_condition_matching(...)
        3. │   └─testthat:::quasi_capture(...)
        4. │     ├─testthat (local) .capture(...)
        5. │     │ └─base::withCallingHandlers(...)
        6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. └─tidyr::unnest_wider(df, y, names_sep = "_")
        8.   └─tidyr::unchop(data, all_of(cols))
        9.     └─tidyr:::df_unchop(cols, ptype = ptype, keep_empty = keep_empty)
       10.       └─tidyr:::list_init_empty(x = col, null = TRUE, typed = keep_empty)
       11.         └─vctrs::vec_equal_na(x)
      
      [ FAIL 3 | WARN 596 | SKIP 95 | PASS 962 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 24 marked UTF-8 strings
    ```

# workflowsets

<details>

* Version: 1.0.0
* GitHub: https://github.com/tidymodels/workflowsets
* Source code: https://github.com/cran/workflowsets
* Date/Publication: 2022-07-12 23:20:01 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "workflowsets")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        `vec_equal_na()` was deprecated in vctrs 0.5.0.
        Please use `vec_detect_missing()` instead.
      [ FAIL 1 | WARN 523 | SKIP 10 | PASS 376 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (9)
      • rlang::is_installed("rlang") is TRUE (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-workflow_set.R:142:4): workflow_set can handle correctly passed case weights ──
      `{ ... }` produced messages.
      
      [ FAIL 1 | WARN 523 | SKIP 10 | PASS 376 ]
      Error: Test failures
      Execution halted
    ```

