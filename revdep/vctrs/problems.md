# arrow

<details>

* Version: 0.17.1
* Source code: https://github.com/cran/arrow
* URL: https://github.com/apache/arrow/, https://arrow.apache.org/docs/r
* BugReports: https://issues.apache.org/jira/projects/ARROW/issues
* Date/Publication: 2020-05-19 20:30:15 UTC
* Number of recursive dependencies: 56

Run `cloud_details(, "arrow")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       14. base::mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
       16. vctrs::vec_proxy_compare(column, relax = TRUE)
       17. ellipsis::check_dots_empty()
       18. ellipsis:::action_dots(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1847 | SKIPPED: 20 | WARNINGS: 0 | FAILED: 5 ]
      1. Error: Simple interface for datasets (@test-dataset.R#76) 
      2. Error: Hive partitioning (@test-dataset.R#130) 
      3. Error: Partitioning inference (@test-dataset.R#159) 
      4. Error: Creating UnionDataset (@test-dataset.R#214) 
      5. Error: arrange (@test-dplyr.R#284) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 54.9Mb
      sub-directories of 1Mb or more:
        R      3.2Mb
        libs  51.3Mb
    ```

# dials

<details>

* Version: 0.0.8
* Source code: https://github.com/cran/dials
* URL: https://tidymodels.github.io/dials, https://github.com/tidymodels/dials
* BugReports: https://github.com/tidymodels/dials/issues
* Date/Publication: 2020-07-08 17:20:02 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "dials")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. dials:::expect_s3_class_parameters(arrange(x, name))
        6. dplyr:::arrange.data.frame(x, name)
        7. dplyr:::arrange_rows(.data, dots)
        8. dplyr:::map2(...)
        9. base::mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
       11. vctrs::vec_proxy_compare(column, relax = TRUE)
       12. ellipsis::check_dots_empty()
       13. ellipsis:::action_dots(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 430 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: arrange() keeps parameters class when row order is modified (@test-compat-dplyr-old-parameters.R#28) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# dm

<details>

* Version: 0.1.5
* Source code: https://github.com/cran/dm
* URL: https://krlmlr.github.io/dm, https://github.com/krlmlr/dm
* BugReports: https://github.com/krlmlr/dm/issues
* Date/Publication: 2020-07-03 17:10:16 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "dm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   dm_nycflights13(),
    +   set_key_constraints = FALSE
    + )
    + 
    + # Persist, explicitly specify table names:
    + persistent_dm <- copy_dm_to(
    +   con,
    +   dm_nycflights13(),
    +   temporary = FALSE,
    +   table_names = ~ paste0("flights_", .x)
    + )
    + dbplyr::remote_name(persistent_dm$planes)
    + 
    + DBI::dbDisconnect(con)
    + ## Don't show: 
    + }) # examplesIf
    > con <- DBI::dbConnect(RSQLite::SQLite())
    > temp_dm <- copy_dm_to(con, dm_nycflights13(), set_key_constraints = FALSE)
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1. Error: dm_add_tbl() works (@test-add-tbl.R#10) 
      2. Error: dm_rm_tbl() works (@test-add-tbl.R#97) 
      3. Error: check_cardinality_...() functions are checking the cardinality correctly? (@test-check-cardinalities.R#19) 
      4. Error: copy_dm_to() copies data frames to databases (@test-db-interface.R#18) 
      5. Error: copy_dm_to() copies data frames from databases (@test-db-interface.R#27) 
      6. Error: dm_disambiguate_cols() works as intended (@test-disambiguate.R#4) 
      7. Error: can create dm with as_dm() (@test-dm.R#12) 
      8. Error: 'copy_to.dm()' works (@test-dm.R#50) 
      9. Error: 'copy_to.dm()' works (2) (@test-dm.R#71) 
      1. ...
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

# dplyr

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/dplyr
* URL: https://dplyr.tidyverse.org, https://github.com/tidyverse/dplyr
* BugReports: https://github.com/tidyverse/dplyr/issues
* Date/Publication: 2020-05-29 15:00:03 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "dplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dplyr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: arrange
    > ### Title: Arrange rows by column values
    > ### Aliases: arrange arrange.data.frame
    > 
    > ### ** Examples
    > 
    > arrange(mtcars, cyl, disp)
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1731 | SKIPPED: 46 | WARNINGS: 0 | FAILED: 35 ]
      1. Error: can sort empty data frame (@test-arrange.r#17) 
      2. Error: local arrange sorts missing values to end (@test-arrange.r#23) 
      3. Error: arrange handles list columns (#282) (@test-arrange.r#46) 
      4. Error: arrange handles raw columns (#1803) (@test-arrange.r#54) 
      5. Error: arrange handles matrix columns (@test-arrange.r#59) 
      6. Error: arrange handles data.frame columns (#3153) (@test-arrange.r#64) 
      7. Error: arrange handles complex columns (@test-arrange.r#69) 
      8. Error: arrange handles S4 classes (#1105) (@test-arrange.r#78) 
      9. Error: arrange respects locale (#1280) (@test-arrange.r#84) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# evaluator

<details>

* Version: 0.4.2
* Source code: https://github.com/cran/evaluator
* URL: https://evaluator.tidyrisk.org
* BugReports: https://github.com/davidski/evaluator/issues
* Date/Publication: 2020-04-16 09:20:09 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "evaluator")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘evaluator-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: generate_event_outcomes_plot
    > ### Title: Display the distribution of threat events contained vs. realized
    > ###   across all domains
    > ### Aliases: generate_event_outcomes_plot
    > 
    > ### ** Examples
    > 
    > data(mc_domain_summary)
    > generate_event_outcomes_plot(mc_domain_summary)
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      # Scenario model: openfair_tef_tc_diff_lm
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 126 | SKIPPED: 5 | WARNINGS: 0 | FAILED: 9 ]
      1. Error: Domain VaR heatmap (@test-common-graphs.R#24) 
      2. Error: Domain-level outcomes (@test-common-graphs.R#57) 
      3. Error: Loss_exceedance_curve (@test-common-graphs.R#63) 
      4. Error: Default scenarios import (@test-import.R#4) 
      5. Error: Scenarios import succeeds when using defaults (@test-import.R#10) 
      6. Error: Default capabilities import (@test-import.R#18) 
      7. Error: Capabilities import succeeds when using defaults (@test-import.R#28) 
      8. Error: Higher-level import_spreadsheet functions (@test-import.R#33) 
      9. Error: Risk Dashboard renders (@test-reports.R#50) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# exuber

<details>

* Version: 0.4.1
* Source code: https://github.com/cran/exuber
* URL: https://github.com/kvasilopoulos/exuber
* BugReports: https://github.com/kvasilopoulos/exuber/issues
* Date/Publication: 2020-05-12 17:00:07 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "exuber")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘exuber-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: datestamp
    > ### Title: Date-stamping periods of mildly explosive behavior
    > ### Aliases: datestamp datestamp.radf_obj
    > 
    > ### ** Examples
    > 
    > 
    > rsim_data <- radf(sim_data)
    > 
    > ds_data <- datestamp(rsim_data)
    Using `radf_crit` for `cv`.
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 106 | SKIPPED: 4 | WARNINGS: 4 | FAILED: 27 ]
      1. Error: (unknown) (@test-autoplot.R#5) 
      2. Error: datestamp (@test-index.R#61) 
      3. Failure: printing coverage (@test-summary.R#11) 
      4. Failure: printing coverage (@test-summary.R#12) 
      5. Failure: printing coverage (@test-summary.R#13) 
      6. Failure: printing coverage (@test-summary.R#14) 
      7. Error: error diagnostics (@test-summary.R#33) 
      8. Error: Correct output in summary/datestamp (@test-summary.R#55) 
      9. Failure: no problem running rp/ds/dg (mc) (@test-summary.R#64) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# fabletools

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/fabletools
* URL: http://fabletools.tidyverts.org/, https://github.com/tidyverts/fabletools
* BugReports: https://github.com/tidyverts/fabletools/issues
* Date/Publication: 2020-06-15 23:40:08 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "fabletools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Name: autoplot.fbl_ts
    > ### Title: Plot a set of forecasts
    > ### Aliases: autoplot.fbl_ts autolayer.fbl_ts
    > 
    > ### ** Examples
    > 
    > library(tsibbledata)
    > if (requireNamespace("fable", quietly = TRUE)) {
    + library(fable)
    + 
    + fc <- aus_production %>%
    +   model(ets = ETS(log(Beer) ~ error("M") + trend("Ad") + season("A"))) %>% 
    +   forecast(h = "3 years") 
    + 
    + fc %>% 
    +   autoplot(aus_production)
    + }
    Warning: 1 error encountered for ets
    [1] `...` is not empty.
    
    We detected these problematic arguments:
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      * `relax`
      
      These dots only exist to allow future extensions and should be empty.
      Did you misspecify an argument?
       
      3: 2 errors (1 unique) encountered for ets
      [2] `...` is not empty.
      
      We detected these problematic arguments:
      * `relax`
      
      These dots only exist to allow future extensions and should be empty.
      Did you misspecify an argument?
       
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lifecycle’
      All declared Imports should be used.
    ```

# feasts

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/feasts
* URL: http://feasts.tidyverts.org/, https://github.com/tidyverts/feasts/
* BugReports: https://github.com/tidyverts/feasts/issues
* Date/Publication: 2020-06-17 05:40:14 UTC
* Number of recursive dependencies: 88

Run `cloud_details(, "feasts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > library(tsibble)
    > library(tsibbledata)
    > library(dplyr)
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > 
    > vic_elec %>% ACF(Temperature)
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12. dplyr:::transmute.data.frame(...)
       14. dplyr:::mutate.data.frame(.data, ..., .keep = "none")
       15. dplyr:::mutate_cols(.data, ...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 62 | SKIPPED: 0 | WARNINGS: 3 | FAILED: 6 ]
      1. Error: (unknown) (@test-cf.R#3) 
      2. Error: gg_tsdisplay() plots (@test-graphics.R#174) 
      3. Error: X-13ARIMA-SEATS decomposition (@test-seats.R#30) 
      4. Error: Multiple seasonality STL (@test-stl.R#48) 
      5. Error: Additive X11 decomposition (@test-x11.R#24) 
      6. Error: Multiplicative X11 decomposition (@test-x11.R#48) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ggdist

<details>

* Version: 2.2.0
* Source code: https://github.com/cran/ggdist
* URL: http://mjskay.github.io/ggdist, https://github.com/mjskay/ggdist
* BugReports: https://github.com/mjskay/ggdist/issues/new
* Date/Publication: 2020-07-12 05:30:02 UTC
* Number of recursive dependencies: 121

Run `cloud_details(, "ggdist")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > library(ggplot2)
    > 
    > data(RankCorr_u_tau, package = "ggdist")
    > 
    > # orientation is detected automatically based on
    > # which axis is discrete
    > 
    > RankCorr_u_tau %>%
    +   ggplot(aes(x = u_tau)) +
    +   geom_dots()
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       40. vctrs::vec_proxy_compare(column, relax = TRUE)
       41. ellipsis::check_dots_empty()
       42. ellipsis:::action_dots(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 96 | SKIPPED: 133 | WARNINGS: 8 | FAILED: 6 ]
      1. Error: vanilla dots geoms and stats work (@test.geom_dotsinterval.R#24) 
      2. Error: stat_dist_dots[interval] works (@test.geom_dotsinterval.R#70) 
      3. Failure: stat_dist_dots works on NA data (@test.geom_dotsinterval.R#98) 
      4. Error: two-group stat_lineribbons work (@test.geom_lineribbon.R#83) 
      5. Error: group_slab_data_by works (@test.geom_slabinterval.R#33) 
      6. Error: mapping custom aesthetics works (@test.scales.R#173) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# hardhat

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/hardhat
* URL: https://github.com/tidymodels/hardhat
* BugReports: https://github.com/tidymodels/hardhat/issues
* Date/Publication: 2020-07-02 14:40:02 UTC
* Number of recursive dependencies: 100

Run `cloud_details(, "hardhat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
        step
    
    > 
    > # ---------------------------------------------------------------------------
    > # Setup
    > 
    > train <- iris[1:100,]
    > test <- iris[101:150,]
    > 
    > # ---------------------------------------------------------------------------
    > # Recipes example
    > 
    > # Create a recipe that logs a predictor
    > rec <- recipe(Species ~ Sepal.Length + Sepal.Width, train) %>%
    +    step_log(Sepal.Length)
    > 
    > processed <- mold(rec, train)
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 375 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 29 ]
      1. Error: simple forge works (@test-forge-recipe.R#7) 
      2. Error: asking for the outcome works (@test-forge-recipe.R#27) 
      3. Error: asking for the outcome when it isn't there fails (@test-forge-recipe.R#42) 
      4. Error: outcomes steps get processed (@test-forge-recipe.R#59) 
      5. Error: missing predictor columns fail appropriately (@test-forge-recipe.R#76) 
      6. Error: novel predictor levels are caught (@test-forge-recipe.R#105) 
      7. Error: novel predictor levels can be ignored and handled by recipes (@test-forge-recipe.R#135) 
      8. Error: novel predictor levels without any data are silently removed (@test-forge-recipe.R#176) 
      9. Error: novel outcome levels are caught (@test-forge-recipe.R#201) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# pammtools

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/pammtools
* URL: https://github.com/adibender/pammtools
* BugReports: https://github.com/adibender/pammtools/issues
* Date/Publication: 2020-06-06 15:50:02 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "pammtools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       14. base::mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
       16. vctrs::vec_proxy_compare(column, relax = TRUE)
       17. ellipsis::check_dots_empty()
       18. ellipsis:::action_dots(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 264 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 5 ]
      1. Error: Cumulative effects are calculated correctly (@test-cumulative-effect.R#112) 
      2. Error: pec helpers work (@test-model-evaluation.R#10) 
      3. Error: predict functions work correctly (@test-predict-functions.R#13) 
      4. Error: ped class is preserved after dplyr operations (@test-tidyverse-S3methods.R#15) 
      5. Error: nested_fdf class is preserved after tidyr operations (@test-tidyverse-S3methods.R#51) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# parsnip

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/parsnip
* URL: https://parsnip.tidymodels.org, https://github.com/tidymodels/parsnip
* BugReports: https://github.com/tidymodels/parsnip/issues
* Date/Publication: 2020-07-03 16:50:02 UTC
* Number of recursive dependencies: 106

Run `cloud_details(, "parsnip")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > library(modeldata)
    > data("lending_club")
    > 
    > lr_mod <- logistic_reg()
    > 
    > using_formula <-
    +   lr_mod %>%
    +   set_engine("glm") %>%
    +   fit(Class ~ funded_amnt + int_rate, data = lending_club)
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 500 | SKIPPED: 47 | WARNINGS: 36 | FAILED: 131 ]
      1. Error: primary arguments (@test_boost_tree.R#16) 
      2. Error: engine arguments (@test_boost_tree.R#80) 
      3. Error: bad input (@test_boost_tree.R#145) 
      4. Failure: C5.0 execution (@test_boost_tree_C50.R#26) 
      5. Error: C5.0 execution (@test_boost_tree_C50.R#35) 
      6. Error: C5.0 prediction (@test_boost_tree_C50.R#84) 
      7. Error: C5.0 probabilities (@test_boost_tree_C50.R#100) 
      8. Error: submodel prediction (@test_boost_tree_C50.R#124) 
      9. Failure: xgboost execution, classification (@test_boost_tree_xgboost.R#22) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# pmdplyr

<details>

* Version: 0.3.3
* Source code: https://github.com/cran/pmdplyr
* URL: https://nickch-k.github.io/pmdplyr, https://github.com/NickCH-K/pmdplyr
* BugReports: https://github.com/NickCH-K/pmdplyr/issues
* Date/Publication: 2020-05-30 07:30:02 UTC
* Number of recursive dependencies: 105

Run `cloud_details(, "pmdplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘pmdplyr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fixed_check
    > ### Title: Check for inconsistency in variables that should be fixed
    > ### Aliases: fixed_check
    > 
    > ### ** Examples
    > 
    > 
    > # In the Scorecard data, it should be the case that
    > # state_abbr and inst_name never change within university.
    > # Let's see if that's true
    > data(Scorecard)
    > fixed_check(Scorecard, .var = c(state_abbr, inst_name), .within = unitid)
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 210 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 11 ]
      1. Error: inexact_join input failstates (@test-bad_input.R#96) 
      2. Error: panel_convert input failstates (@test-bad_input.R#165) 
      3. Error: id_variable works (@test-id_variable.R#12) 
      4. Error: safe_join works (@test-inexact_join.R#82) 
      5. Error: inexact join methods work (@test-inexact_join.R#96) 
      6. Error: Different inexact joins work (@test-inexact_join.R#143) 
      7. Error: mutate_cascade with tlag works (@test-major_mutate_variations.R#17) 
      8. Error: (unknown) (@test-panel_consistency.R#37) 
      9. Error: (unknown) (@test-panel_convert.R#14) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

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
    # A tibble: 1 x 6
         id title     stage    status       deadline_type deadline           
      <int> <chr>     <prjstg> <chr>        <chr>         <dttm>             
    1     7 Good idea 0: idea  just created <NA>          NA                 
    
    New project's authors:
    # A tibble: 1 x 7
      author_id last_name given_names title degree email phone
          <int> <chr>     <chr>       <chr> <chr>  <chr> <chr>
    1         1 Rice      Condoleezza <NA>  <NA>   <NA>  <NA> 
    # A tibble: 1 x 3
      current_owner corresp_auth creator 
      <prjaut>      <prjaut>     <prjaut>
    1 1: Rice       NA           0: root 
    > #############################################################################
    > 
    > # View entire affiliations table
    > affiliations()
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. projects:::affiliations_internal(p_path)
       11. dplyr::arrange(., .data$department_name, .data$institution_name)
       15. dplyr:::arrange_rows(.data, dots)
       16. dplyr:::map2(...)
       17. base::mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
       19. vctrs::vec_proxy_compare(column, relax = TRUE)
       20. ellipsis::check_dots_empty()
       21. ellipsis:::action_dots(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 15 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: Setup works (@test-setup.R#162) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# rsample

<details>

* Version: 0.0.7
* Source code: https://github.com/cran/rsample
* URL: https://rsample.tidymodels.org, https://github.com/tidymodels/rsample
* BugReports: https://github.com/tidymodels/rsample/issues
* Date/Publication: 2020-06-04 07:40:03 UTC
* Number of recursive dependencies: 95

Run `cloud_details(, "rsample")` for more info

</details>

## Newly broken

*   checking whether package ‘rsample’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rsample/new/rsample.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rsample’ ...
** package ‘rsample’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
Error: `...` is not empty.

We detected these problematic arguments:
* `relax`

These dots only exist to allow future extensions and should be empty.
Did you misspecify an argument?
Backtrace:
     █
  1. ├─base::suppressWarnings(...)
  2. │ └─base::withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
  3. ├─base::serialize(...)
  4. ├─base::as.list(base::getNamespace("rsample"), all.names = TRUE)
  5. ├─base::as.list.environment(base::getNamespace("rsample"), all.names = TRUE)
  6. └─rsample::group_vfold_cv(test_data(), y)
  7.   └─rsample:::group_vfold_splits(data = data, group = group, v = v)
  8.     └─data_ind %>% full_join(keys, by = "..group") %>% arrange(..index)
  9.       ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
 10.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
 11.         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
 12.           └─rsample:::`_fseq`(`_lhs`)
 13.    
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/rsample/new/rsample.Rcheck/rsample’

```
### CRAN

```
* installing *source* package ‘rsample’ ...
** package ‘rsample’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (rsample)

```
# textrecipes

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/textrecipes
* URL: https://github.com/tidymodels/textrecipes, https://textrecipes.tidymodels.org
* BugReports: https://github.com/tidymodels/textrecipes/issues
* Date/Publication: 2020-07-08 21:50:02 UTC
* Number of recursive dependencies: 101

Run `cloud_details(, "textrecipes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: step_ngram
    > ### Title: Generate ngrams from tokenlist
    > ### Aliases: step_ngram tidy.step_ngram
    > 
    > ### ** Examples
    > 
    > library(recipes)
    > library(modeldata)
    > data(okc_text)
    > 
    > okc_rec <- recipe(~ ., data = okc_text) %>%
    +   step_tokenize(essay0) %>%
    +   step_ngram(essay0)
    >   
    > okc_obj <- okc_rec %>%
    +   prep()
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 115 | SKIPPED: 8 | WARNINGS: 0 | FAILED: 53 ]
      1. Error: (unknown) (@test-embeddings.R#18) 
      2. Error: hashing gives double outputs (@test-hashing.R#20) 
      3. Error: hashing output width changes accordingly with num_terms (@test-hashing.R#37) 
      4. Error: hashing output width changes accordingly with num_terms (@test-hashing.R#53) 
      5. Error: printing (@test-hashing.R#79) 
      6. Error: step_lda works as intended (@test-lda.R#18) 
      7. Error: step_lda works with num_topics argument (@test-lda.R#33) 
      8. Error: printing (@test-lda.R#45) 
      9. Error: ngramming is done correctly (@test-ngram.R#135) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
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

# tidyjson

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/tidyjson
* URL: https://github.com/colearendt/tidyjson
* BugReports: https://github.com/colearendt/tidyjson/issues
* Date/Publication: 2020-05-31 21:30:03 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "tidyjson")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > key_employees %>% glimpse
    Rows: 527
    Columns: 8
    $ name              <chr> "OutSmart Power Systems", "Firewall Script", "Firew…
    $ array.index       <int> 1, 1, 2, 3, 1, 2, 1, 2, 3, 4, 5, 1, 1, 2, 3, 1, 1, …
    $ is_past           <lgl> FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FAL…
    $ title             <chr> "Board Observer", "", "", "Founder", "CEO", "CBO", …
    $ person.first_name <chr> "Jeffrey", "Ron", "Brandon", "Daniel", "Miguel", "M…
    $ person.last_name  <chr> "Weiss", "Myers", "Farber", "Blake Saltman", "Olive…
    $ person.permalink  <chr> "jeffrey-weiss", "ron-myers", "brandon-farber", "da…
    $ ..JSON            <list> [[FALSE, "Board Observer", ["Jeffrey", "Weiss", "j…
    > 
    > # Show the top 10 titles
    > key_employees %>%
    +   filter(!is_past) %>%
    +   count(title) %>%
    +   arrange(desc(n)) %>%
    +   top_n(10)
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 338 | SKIPPED: 10 | WARNINGS: 0 | FAILED: 13 ]
      1. Error: /tmp/workdir/tidyjson/new/tidyjson.Rcheck/00_pkg_src/tidyjson/man/companies.Rd 
      2. Error: /tmp/workdir/tidyjson/new/tidyjson.Rcheck/00_pkg_src/tidyjson/man/json_schema.Rd 
      3. Error: json_schema works for simple examples (@test-json_schema.R#9) 
      4. Error: json_schema works for a more complex object (@test-json_schema.R#23) 
      5. Error: json_schema works for a more complex array (@test-json_schema.R#42) 
      6. Error: works for empty arrays (@test-json_schema.R#51) 
      7. Error: works for complex nested types (@test-json_schema.R#59) 
      8. Error: simple mixed type array (@test-json_schema.R#79) 
      9. Error: problem with mixed type arrays (@test-json_schema.R#87) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidyposterior

<details>

* Version: 0.0.3
* Source code: https://github.com/cran/tidyposterior
* URL: https://tidyposterior.tidymodels.org, https://github.com/tidymodels/tidyposterior
* BugReports: https://github.com/tidymodels/tidyposterior/issues
* Date/Publication: 2020-06-11 17:30:02 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "tidyposterior")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Chain 2: Iteration: 40 / 50 [ 80%]  (Sampling)
      Chain 2: Iteration: 45 / 50 [ 90%]  (Sampling)
      Chain 2: Iteration: 50 / 50 [100%]  (Sampling)
      Chain 2: 
      Chain 2:  Elapsed Time: 0.008311 seconds (Warm-up)
      Chain 2:                0.009491 seconds (Sampling)
      Chain 2:                0.017802 seconds (Total)
      Chain 2: 
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 68 | SKIPPED: 0 | WARNINGS: 32 | FAILED: 2 ]
      1. Error: keep posterior class (@test_dplyr_new.R#17) 
      2. Error: keep posterior_diff class (@test_dplyr_new.R#38) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

*   checking Rd \usage sections ... NOTE
    ```
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

# tidyr

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/tidyr
* URL: https://tidyr.tidyverse.org, https://github.com/tidyverse/tidyr
* BugReports: https://github.com/tidyverse/tidyr/issues
* Date/Publication: 2020-05-20 13:10:02 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "tidyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. dplyr::select(., a, b)
        9. dplyr::arrange(., a, b)
       11. dplyr:::arrange_rows(.data, dots)
       12. dplyr:::map2(...)
       13. base::mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
       15. vctrs::vec_proxy_compare(column, relax = TRUE)
       16. ellipsis::check_dots_empty()
       17. ellipsis:::action_dots(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 627 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: order doesn't matter (@test-spread.R#8) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 24 marked UTF-8 strings
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

# tsibble

<details>

* Version: 0.9.1
* Source code: https://github.com/cran/tsibble
* URL: https://tsibble.tidyverts.org
* BugReports: https://github.com/tidyverts/tsibble/issues
* Date/Publication: 2020-06-20 10:00:03 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "tsibble")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tsibble-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as_tsibble
    > ### Title: Coerce to a tsibble object
    > ### Aliases: as_tsibble as_tsibble.ts as_tsibble.mts
    > 
    > ### ** Examples
    > 
    > # coerce tibble to tsibble w/o a key
    > tbl1 <- tibble(
    +   date = as.Date("2017-01-01") + 0:9,
    +   value = rnorm(10)
    + )
    > as_tsibble(tbl1)
    Using `date` as index variable.
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 193 | SKIPPED: 2 | WARNINGS: 1 | FAILED: 29 ]
      1. Error: unknown interval (@test-append.R#14) 
      2. Error: an irregular tbl_ts (@test-append.R#19) 
      3. Error: 4 day interval (@test-append.R#24) 
      4. Error: custom index class (@test-append.R#38) 
      5. Error: ordered? (@test-append.R#44) 
      6. Error: (unknown) (@test-bind.R#8) 
      7. Error: arrange.tbl_ts() (@test-dplyr.R#64) 
      8. Error: (unknown) (@test-dplyr.R#73) 
      9. Error: create an empty tsibble (@test-empty.R#2) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tune

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/tune
* URL: https://github.com/tidymodels/tune, https://tune.tidymodels.org
* Date/Publication: 2020-07-08 18:00:02 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "tune")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > data(two_class_dat, package = "modeldata")
    > 
    > set.seed(2393)
    > res <-
    +   logistic_reg() %>%
    +   set_engine("glm") %>%
    +   fit_resamples(Class ~ ., resamples = vfold_cv(two_class_dat, v = 3),
    +                 control = control_resamples(save_pred = TRUE))
    x Fold1: model: Error: `...` is not empty.
    
    We detected these problematic argumen...
    x Fold2: model: Error: `...` is not empty.
    
    We detected these problematic argumen...
    x Fold3: model: Error: `...` is not empty.
    
    We detected these problematic argumen...
    Error: `...` is not empty.
    
    We detected these problematic arguments:
    ```

