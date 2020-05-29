# amt

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/amt
* URL: https://github.com/jmsigner/amt
* Date/Publication: 2020-04-28 12:10:02 UTC
* Number of recursive dependencies: 167

Run `cloud_details(, "amt")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Last 13 lines of output:
      Running test_track.R..................   56 tests [0;32mOK[0m 
      Running test_track.R..................   57 tests [0;32mOK[0m 
      Running test_track.R..................   58 tests [0;32mOK[0m 
      Running test_track.R..................   59 tests [0;32mOK[0m 
      Running test_track.R..................   60 tests [0;32mOK[0m 
      Running test_track.R..................   61 tests [0;32mOK[0m 
      Running test_track.R..................   62 tests [0;32mOK[0m 
      Running test_track.R..................   63 tests [0;32mOK[0m 
      Error: ----- FAILED[data]: test_random_steps.R<89--89>
       call| expect_equal(rs[1, 1:6], rs[2, 1:6])
       diff| Component "x2_": Names: 1 string mismatch
       diff| Component "y2_": Names: 1 string mismatch
       diff| Component "sl_": Names: 1 string mismatch
       diff| Component "ta_": Names: 1 string mismatch 
      Execution halted
    ```

# anomalize

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/anomalize
* URL: https://github.com/business-science/anomalize
* BugReports: https://github.com/business-science/anomalize/issues
* Date/Publication: 2019-09-21 04:10:03 UTC
* Number of recursive dependencies: 162

Run `cloud_details(, "anomalize")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > 
    > tidyverse_cran_downloads %>%
    +     ungroup() %>%
    +     filter(package == "tidyquant") %>%
    +     decompose_stl(count)
    Error in time_frequency(data, period = frequency, message = message) : 
      Error time_frequency(): Cannot use on a grouped data frame.
    Frequency should be performed on a single time series.
    Calls: %>% ... withVisible -> <Anonymous> -> decompose_stl -> time_frequency
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 53 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 14 ]
      1. Error: returns a ggplot (@test-plot_anomalies.R#8) 
      2. Error: returns a ggplot (@test-plot_anomaly_decomposition.R#10) 
      3. Error: grouped_tbl_time works (@test-time_apply.R#11) 
      4. Error: tbl_time works (@test-time_apply.R#17) 
      5. Failure: single tbl_df (@test-time_decompose.R#20) 
      6. Error: time_frequency works: period = 'auto' (@test-time_frequency.R#26) 
      7. Error: time_frequency works: period = '1 month' (@test-time_frequency.R#35) 
      8. Error: time_frequency works: period = 5 (@test-time_frequency.R#44) 
      9. Error: time_trend works: period = 'auto' (@test-time_frequency.R#55) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# applicable

<details>

* Version: 0.0.1
* Source code: https://github.com/cran/applicable
* URL: https://github.com/tidymodels/applicable
* Date/Publication: 2020-05-25 14:00:02 UTC
* Number of recursive dependencies: 98

Run `cloud_details(, "applicable")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `actual_output` not equivalent to `expected`.
      current is not list-like
      
      ── 3. Failure: `score_apd_pca_bridge` output is correct (@test-pca-score.R#84)  
      `actual_output` not equivalent to `expected`.
      current is not list-like
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 106 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 3 ]
      1. Failure: `score_apd_pca_numeric` pcs output matches `stats::predict` output (@test-pca-score.R#48) 
      2. Failure: `score` pcs output matches `stats::predict` output (@test-pca-score.R#66) 
      3. Failure: `score_apd_pca_bridge` output is correct (@test-pca-score.R#84) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# bdl

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/bdl
* URL: https://github.com/statisticspoland/R_Package_to_API_BDL
* BugReports: https://github.com/statisticspoland/R_Package_to_API_BDL/issues
* Date/Publication: 2020-04-01 13:40:03 UTC
* Number of recursive dependencies: 155

Run `cloud_details(, "bdl")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component 4: Modes: numeric, character
      Component 4: target is numeric, current is character
      Component 5: Mean relative difference: 2.2
      Component 6: Modes: character, numeric
      ...
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 34 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Failure: Proper data (@test-requests.R#72) 
      2. Failure: Proper data (@test-requests.R#127) 
      3. Failure: Proper data (@test-requests.R#179) 
      4. Failure: Proper data (@test-requests.R#244) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# cattonum

<details>

* Version: 0.0.4
* Source code: https://github.com/cran/cattonum
* URL: https://github.com/bfgray3/cattonum
* BugReports: https://github.com/bfgray3/cattonum/issues
* Date/Publication: 2020-02-09 12:30:06 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "cattonum")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 242 | SKIPPED: 4 | WARNINGS: 0 | FAILED: 13 ]
      1. Failure: catto_mean: one tibble training column. (@test-mean.R#63) 
      2. Failure: catto_mean: one tibble training column. (@test-mean.R#63) 
      3. Failure: catto_mean: one tibble training column. (@test-mean.R#63) 
      4. Failure: catto_mean: one tibble training column. (@test-mean.R#67) 
      5. Failure: catto_mean: one tibble training column. (@test-mean.R#67) 
      6. Failure: catto_mean: one tibble training column. (@test-mean.R#67) 
      7. Failure: catto_mean() correctly encodes tibble with logicals. (@test-mean.R#98) 
      8. Failure: catto_median(): one tibble training column. (@test-median.R#60) 
      9. Failure: catto_median(): one tibble training column. (@test-median.R#60) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# codebook

<details>

* Version: 0.8.2
* Source code: https://github.com/cran/codebook
* URL: https://github.com/rubenarslan/codebook
* BugReports: https://github.com/rubenarslan/codebook/issues
* Date/Publication: 2020-01-09 16:20:07 UTC
* Number of recursive dependencies: 179

Run `cloud_details(, "codebook")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > ### ** Examples
    > 
    > # will generate figures in a temporary directory
    > old_base_dir <- knitr::opts_knit$get("base.dir")
    > knitr::opts_knit$set(base.dir = tempdir())
    > on.exit(knitr::opts_knit$set(base.dir = old_base_dir))
    > data("bfi")
    > bfi <- bfi[, c("BFIK_open_1", "BFIK_open_1")]
    > md <- codebook(bfi, survey_repetition = "single", metadata_table = FALSE)
    No missing values.
    Error: Argument 1 must be a data frame or a named atomic vector.
    Backtrace:
        █
     1. └─codebook::codebook(bfi, survey_repetition = "single", metadata_table = FALSE)
     2.   └─codebook::metadata_jsonld(results)
     3.     └─codebook::metadata_list(results)
     4.       └─codebook::codebook_table(results)
     5.         └─codebook:::skim_to_wide_labelled(results)
     6.           └─dplyr::bind_rows(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘jsonlite’ ‘pander’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 65 marked UTF-8 strings
    ```

# CollapseLevels

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/CollapseLevels
* Date/Publication: 2017-12-04 10:30:12 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "CollapseLevels")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ✖ could not find function "n"
    ℹ Input `tot` is `n()`.
    ℹ The error occured in group 1: Account_Balance = "A11".
    Backtrace:
         █
      1. └─CollapseLevels::IVCalc(German_Credit, resp = "Good_Bad", bins = 10)
      2.   └─d %>% dplyr::group_by_(naml) %>% dplyr::summarise(tot = n())
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─CollapseLevels:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             ├─base::withVisible(function_list[[k]](value))
      9.             └─function_list[[k]](value)
     10.               ├─dplyr::summarise(., tot = n())
     11.               └─dplyr:::summarise.grouped_df(., tot = n())
     12.                 └─dplyr:::summarise_cols(.data, ...)
    <parent: error/simpleError>
    Backtrace:
    █
    Execution halted
    ```

# correlationfunnel

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/correlationfunnel
* URL: https://github.com/business-science/correlationfunnel
* BugReports: https://github.com/business-science/correlationfunnel/issues
* Date/Publication: 2019-08-06 09:30:09 UTC
* Number of recursive dependencies: 97

Run `cloud_details(, "correlationfunnel")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ── 4. Failure: Check correlation (@test-correlate.R#61)  ───────────────────────
      nrow(marketing_correlated_tbl) not equal to 74.
      1/1 mismatches
      [1] 65 - 74 == -9
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 12 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: Check binarize - numeric (@test-binarize.R#47) 
      2. Error: Check binarize - numeric (@test-binarize.R#45) 
      3. Error: (unknown) (@test-binarize.R#45) 
      4. Failure: Check correlation (@test-correlate.R#61) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        doc    3.0Mb
        help   1.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# cutpointr

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/cutpointr
* URL: https://github.com/thie1e/cutpointr
* BugReports: https://github.com/thie1e/cutpointr/issues
* Date/Publication: 2020-04-14 08:50:10 UTC
* Number of recursive dependencies: 78

Run `cloud_details(, "cutpointr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      names for target but not for current
      
      ── 4. Failure: boot_test works correctly (@test-cutpointr.R#1428)  ─────────────
      round(bt$p_adj, 3) not equal to c(1, 0.647, 0.731).
      names for target but not for current
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 499 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Failure: boot_ci works correctly (@test-cutpointr.R#1394) 
      2. Failure: boot_test works correctly (@test-cutpointr.R#1425) 
      3. Failure: boot_test works correctly (@test-cutpointr.R#1426) 
      4. Failure: boot_test works correctly (@test-cutpointr.R#1428) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ddpcr

<details>

* Version: 1.14
* Source code: https://github.com/cran/ddpcr
* URL: https://github.com/daattali/ddpcr
* BugReports: https://github.com/daattali/ddpcr/issues
* Date/Publication: 2020-03-23 18:00:06 UTC
* Number of recursive dependencies: 88

Run `cloud_details(, "ddpcr")` for more info

</details>

## Newly broken

*   checking whether package ‘ddpcr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: `mutate_()` is deprecated as of dplyr 0.7.0.
      Warning: `select_()` is deprecated as of dplyr 0.7.0.
    See ‘/tmp/workdir/ddpcr/new/ddpcr.Rcheck/00install.out’ for details.
    ```

# disk.frame

<details>

* Version: 0.3.5
* Source code: https://github.com/cran/disk.frame
* URL: https://diskframe.com
* BugReports: https://github.com/xiaodaigh/disk.frame/issues
* Date/Publication: 2020-05-08 13:10:10 UTC
* Number of recursive dependencies: 105

Run `cloud_details(, "disk.frame")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘disk.frame-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: hard_arrange
    > ### Title: Perform a hard arrange
    > ### Aliases: hard_arrange hard_arrange.data.frame hard_arrange.disk.frame
    > 
    > ### ** Examples
    > 
    > iris.df = as.disk.frame(iris, nchunks = 2)
    > 
    > # arrange iris.df by specifies and ensure rows with the same specifies are in the same chunk
    > iris_hard.df = hard_arrange(iris.df, Species)
    Error in `[.data.table`(split_values, , name) : 
      j (the 2nd argument inside [...]) is a single symbol but column name 'name' is not found. Perhaps you intended DT[, ..name]. This difference to data.frame is deliberate and explained in FAQ 1.1.
    Calls: hard_arrange ... resolve.list -> signalConditionsASAP -> signalConditions
    Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    pull:
      function(.data, var, name, ...)
    pull.disk.frame:
      function(.data, var)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# distrr

<details>

* Version: 0.0.5
* Source code: https://github.com/cran/distrr
* URL: https://gibonet.github.io/distrr, https://github.com/gibonet/distrr
* Date/Publication: 2019-01-03 10:50:06 UTC
* Number of recursive dependencies: 24

Run `cloud_details(, "distrr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > str(invented_wages)
    tibble [1,000 × 5] (S3: tbl_df/data.frame)
     $ gender        : Factor w/ 2 levels "men","women": 1 2 1 2 1 1 1 2 2 2 ...
     $ sector        : Factor w/ 2 levels "secondary","tertiary": 2 1 2 2 1 1 2 1 2 1 ...
     $ education     : Factor w/ 3 levels "I","II","III": 3 2 2 2 2 1 3 1 2 2 ...
     $ wage          : num [1:1000] 8400 4200 5100 7400 4300 4900 5400 2900 4500 3000 ...
     $ sample_weights: num [1:1000] 105 32 36 12 21 46 79 113 34 32 ...
    > tmp <- dcc(.data = invented_wages, 
    +            .variables = c("gender", "sector"), .fun = jointfun_)
    Error: `vec_ptype2.tbl_df.tbl_df()` is implemented at C level.
    This R function is purely indicative and should never be called.
    Backtrace:
        █
     1. ├─distrr::dcc(...)
     2. │ └─distrr::dcc5(...)
     3. │   └─distrr:::prepare_data(...)
     4. │     └─dplyr::bind_cols(data_vars, data_others)
     5. │       └─vctrs::vec_cbind(!!!dots)
     6. └─vctrs:::vec_ptype2.tbl_df.tbl_df(...)
     7.   └─vctrs:::stop_native_implementation("vec_ptype2.tbl_df.tbl_df")
    Execution halted
    ```

# docxtools

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/docxtools
* URL: https://graphdr.github.io/docxtools
* BugReports: https://github.com/graphdr/docxtools/issues
* Date/Publication: 2019-02-09 18:43:13 UTC
* Number of recursive dependencies: 75

Run `cloud_details(, "docxtools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > data("CO2")
    > x <- head(CO2, n = 5L)
    > format_engr(x)
    Error: Problem with `mutate()` input `observ_index`.
    ✖ Input `observ_index` can't be recycled to size 0.
    ℹ Input `observ_index` is `1:dplyr::n()`.
    ℹ Input `observ_index` must be size 0 or 1, not 2.
    Backtrace:
         █
      1. └─docxtools::format_engr(x)
      2.   └─docxtools:::obs_add(numeric_as_is)
      3.     ├─dplyr::mutate(x, observ_index = 1:dplyr::n())
      4.     └─dplyr:::mutate.data.frame(x, observ_index = 1:dplyr::n())
      5.       └─dplyr:::mutate_cols(.data, ...)
      6.         └─base::tryCatch(...)
      7.           └─base:::tryCatchList(expr, classes, parentenv, handlers)
      8.             └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
      9.               └─value[[3L]](cond)
     10.                 └─dplyr:::stop_mutate_recycle_incompatible_size(e, index = i, dots = dots)
     11.                   └─dplyr:::stop_dplyr(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        6. dplyr:::mutate_cols(.data, ...)
        7. base::tryCatch(...)
        8. base:::tryCatchList(expr, classes, parentenv, handlers)
        9. base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       10. value[[3L]](cond)
       11. dplyr:::stop_mutate_recycle_incompatible_size(e, index = i, dots = dots)
       12. dplyr:::stop_dplyr(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 19 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: Factors are returned unaffected (@test_format_engr.R#14) 
      2. Error: (unknown) (@test_format_engr.R#55) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# driftR

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/driftR
* URL: https://github.com/shaughnessyar/driftR
* BugReports: https://github.com/shaughnessyar/driftR/issues
* Date/Publication: 2018-06-13 22:03:03 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "driftR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Component "class": Lengths (3, 1) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      
      ── 2. Failure: importing the data (@test_read.R#40)  ───────────────────────────
      `sondeResult1` not equal to `sondeClean`.
      Attributes: < Component "class": Lengths (3, 1) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 202 | SKIPPED: 0 | WARNINGS: 15 | FAILED: 2 ]
      1. Failure: importing the data (@test_import.R#29) 
      2. Failure: importing the data (@test_read.R#40) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# easyr

<details>

* Version: 0.3-1
* Source code: https://github.com/cran/easyr
* URL: https://github.com/oliver-wyman-actuarial/easyr
* BugReports: https://github.com/oliver-wyman-actuarial/easyr/issues
* Date/Publication: 2020-03-20 18:10:05 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "easyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ** installing vignettes
      ** testing if installed package can be loaded from temporary location
      ** testing if installed package can be loaded from final location
      ** testing if installed package keeps a record of temporary installation path
      * DONE (doParallel)
      
      The downloaded source packages are in
      	'/tmp/Rtmp9m7il2/downloaded_packages'
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 286 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: works as expected (@test_bindf-joinf.R#57) 
      2. Failure: works as expected (@test_bindf-joinf.R#58) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# eda4treeR

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/eda4treeR
* URL: https://github.com/MYaseen208/eda4treeR
* Date/Publication: 2018-02-04 19:06:12 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "eda4treeR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +  dplyr::summarize(Mean=mean(Mean))
    Error: Corrupt `grouped_df` using old (< 0.8.0) format.
    ℹ Strip off old grouping with `ungroup()`.
    Backtrace:
         █
      1. └─DataExam3.1.1 %>% dplyr::group_by(SeedLot) %>% dplyr::summarize(Mean = mean(Mean))
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           └─function_list[[i]](value)
      8.             ├─dplyr::group_by(., SeedLot)
      9.             └─dplyr:::group_by.data.frame(., SeedLot)
     10.               └─dplyr::group_by_prepare(.data, ..., .add = .add)
     11.                 ├─generics::setdiff(group_names, tbl_vars(out))
     12.                 ├─generics:::setdiff.default(group_names, tbl_vars(out))
     13.                 │ └─base::setdiff(x, y, ...)
     14.                 │   └─base::as.vector(y)
     1
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dae’ ‘dplyr’
      All declared Imports should be used.
    ```

# egor

<details>

* Version: 0.20.03
* Source code: https://github.com/cran/egor
* URL: https://github.com/tilltnet/egor, https://tilltnet.github.io/egor/
* BugReports: https://github.com/tilltnet/egor/issues
* Date/Publication: 2020-03-03 00:20:02 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "egor")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    group_modify:
      function(.data, .f, ..., .keep)
    group_modify.egor:
      function(.tbl, .f, ..., keep)
    
    pull:
      function(.data, var, name, ...)
    pull.egor:
      function(.data, var)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# episheet

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/episheet
* URL: https://github.com/epijim/episheet
* BugReports: https://github.com/epijim/episheet/issues
* Date/Publication: 2019-01-23 20:30:03 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "episheet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > risk(data = dat, exposure = exposure_var, outcome = outcome_var)
    Error: Problem with `summarise()` input `n`.
    ✖ could not find function "n"
    ℹ Input `n` is `n()`.
    Backtrace:
         █
      1. └─episheet::risk(data = dat, exposure = exposure_var, outcome = outcome_var)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─episheet:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               ├─dplyr::summarise(., n = n())
     10.               └─dplyr:::summarise.data.frame(., n = n())
     11.                 └─dplyr:::summarise_cols(.data, ...)
    <parent: error/simpleError>
    Backtrace:
    █
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 6 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 9 ]
      1. Error: Test risk_ratio returns exected estimate (@test-risk.R#16) 
      2. Error: Test risk_ratio returns exected lci (@test-risk.R#21) 
      3. Error: Test risk_ratio returns exected uci (@test-risk.R#26) 
      4. Error: Test risk_diff returns exected estimate (@test-risk.R#31) 
      5. Error: Test risk_diff returns exected lci (@test-risk.R#36) 
      6. Error: Test risk_diff returns exected uci (@test-risk.R#41) 
      7. Error: rrmh returns expected value (@test-stratified_risk.R#6) 
      8. Error: lci return expected value (@test-stratified_risk.R#13) 
      9. Error: uci return expected value (@test-stratified_risk.R#28) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# expstudies

<details>

* Version: 0.0.5
* Source code: https://github.com/cran/expstudies
* Date/Publication: 2019-06-14 11:20:03 UTC
* Number of recursive dependencies: 53

Run `cloud_details(, "expstudies")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 9 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 17 ]
      1. Failure: Correct handling of end dates prior to start dates (@test-exposure_functions.R#20) 
      2. Failure: Policy year exposure calculation works (@test-exposure_functions.R#30) 
      3. Failure: Policy month exposure calculation works (@test-exposure_functions.R#33) 
      4. Failure: Policy year with calendar year exposure calculation works, mid and start (@test-exposure_functions.R#36) 
      5. Failure: Policy year with calendar year exposure calculation works, mid and start (@test-exposure_functions.R#37) 
      6. Failure: Policy year with calendar month exposure calculation works, mid and start (@test-exposure_functions.R#40) 
      7. Failure: Policy year with calendar month exposure calculation works, mid and start (@test-exposure_functions.R#41) 
      8. Failure: Policy month with calendar year exposure calculation works, mid and start (@test-exposure_functions.R#44) 
      9. Failure: Policy month with calendar year exposure calculation works, mid and start (@test-exposure_functions.R#45) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# fable

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/fable
* URL: https://fable.tidyverts.org, https://github.com/tidyverts/fable
* BugReports: https://github.com/tidyverts/fable/issues
* Date/Publication: 2020-04-22 13:12:08 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "fable")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ✖ `vars` must be a character vector.
    ℹ Input `cmp` is `map(.fit, components)`.
    Backtrace:
         █
      1. └─`%>%`(...)
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           ├─base::withVisible(function_list[[k]](value))
      8.           └─function_list[[k]](value)
      9.             ├─generics::components(.)
     10.             └─fabletools:::components.mdl_df(.)
     11.               ├─dplyr::transmute(...)
     12.               └─dplyr:::transmute.data.frame(...)
     13.                 ├─dplyr::mutate(.data, ..., .keep = "none")
     14.                 └─dplyr:::mutate.data.frame(.data, ..., .keep = "none")
     15.                   └─dplyr:::mutate_cols(.data, ...)
    <parent: err
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. testthat::expect_warning(...)
        2. tsibbledata::vic_elec
        2. tsibble::index_by(., date = as.Date(Time))
        2. dplyr::summarise(., demand = mean(Demand))
       10. fabletools::model(., SNAIVE(demand ~ lag("year")))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 90 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 3 ]
      1. Error: Manual ETS selection (@test-ets.R#44) 
      2. Error: Automatic NNETAR selection (@test-nnetar.R#13) 
      3. Error: SNAIVE (@test-rw.R#105) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# fabletools

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/fabletools
* URL: http://fabletools.tidyverts.org/, https://github.com/tidyverts/fabletools
* BugReports: https://github.com/tidyverts/fabletools/issues
* Date/Publication: 2020-03-24 07:10:02 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "fabletools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    2 observations are missing between 2010 Q3 and 2010 Q4
    Error: `vars` must be a character vector.
    Backtrace:
         █
      1. └─fc %>% accuracy(aus_production)
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           ├─base::withVisible(function_list[[k]](value))
      8.           └─function_list[[k]](value)
      9.             ├─fabletools::accuracy(., aus_production)
     10.             └─fabletools:::accuracy.fbl_ts(., aus_production)
     11.               ├─dplyr::transmute(object, .fc = !!resp, .dist = !!dist, !!!syms(by))
     12.               └─tsibble:::transmute.tbl_ts(...)
     13.                 ├─dplyr::mutate(.data, !!!lst_quos)
     14.                 ├─fabletools:::mutate.fbl_ts(.data, !!!lst_quos)
     15.                 │ └─fabletools::as_fable(...)
     16.                 
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       21. tsibble:::retain_tsibble(mut_data, key(.data), index(.data))
       22. tsibble:::duplicated_key_index(data, key, index)
       23. dplyr::grouped_df(as_tibble(data), key)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 264 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 6 ]
      1. Error: Out-of-sample accuracy (@test-accuracy.R#52) 
      2. Error: fable dplyr verbs (@test-fable.R#32) 
      3. Failure: features() (@test-features.R#23) 
      4. Error: generate (@test-generate.R#6) 
      5. Error: generate seed setting (@test-generate.R#31) 
      6. Error: reconciliation (@test-reconciliation.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# finalfit

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/finalfit
* URL: https://github.com/ewenharrison/finalfit
* BugReports: https://github.com/ewenharrison/finalfit/issues
* Date/Publication: 2020-04-21 11:50:02 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "finalfit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      mort_5yr mort_5yr.num   n
    1    Alive            1 511
    2     Died            2 404
    3     <NA>           NA  14
    
    $counts[[19]]
      sex.factor2 age.factor2   n
    1           M   <60 years 204
    2           M   60+ years 241
    3           F   <60 years 210
    4           F   60+ years 274
    
    
    > 
    > # Select a tibble and expand
    > out$counts[[9]] %>%
    +   print(n = Inf)
    Error in print.default(m, ..., quote = quote, right = right, max = max) : 
      invalid 'na.print' specification
    Calls: %>% ... print -> print.data.frame -> print -> print.default
    Execution halted
    ```

# foieGras

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/foieGras
* URL: https://cran.r-project.org/package=foieGras
* BugReports: https://github.com/ianjonsen/foieGras/issues
* Date/Publication: 2019-10-07 22:10:03 UTC
* Number of recursive dependencies: 100

Run `cloud_details(, "foieGras")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: fit_ssm
    > ### Title: Fit a continuous-time state-space model to filter Argos
    > ###   satellite geolocation data
    > ### Aliases: fit_ssm
    > 
    > ### ** Examples
    > 
    > ## fit rw model to one seal with Argos KF data
    > data(ellie)
    > fit <- fit_ssm(ellie, model = "rw", time.step = 24)
    
    pre-filtering data...
    
    fitting SSM...
    Warning in sqrt(as.numeric(object$diag.cov.random)) : NaNs produced
    > 
    > ## time series plots of predicted value fits
    > plot(fit, what = "predicted", type = 1)
    New names:
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 2. Error: (unknown) (@test-plot.R#7)  ───────────────────────────────────────
      Can't subset columns that don't exist.
      ✖ Column `shut.up` doesn't exist.
      Backtrace:
        1. graphics::plot(fssm, what = "fitted")
       31. vctrs:::stop_subscript_oob(...)
       32. vctrs:::stop_subscript(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 30 | SKIPPED: 14 | WARNINGS: 1 | FAILED: 2 ]
      1. Error: (unknown) (@test-join.R#7) 
      2. Error: (unknown) (@test-plot.R#7) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 35.2Mb
      sub-directories of 1Mb or more:
        libs  34.0Mb
    ```

# geometr

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/geometr
* URL: https://github.com/EhrmannS/geometr
* BugReports: https://github.com/EhrmannS/geometr/issues
* Date/Publication: 2020-03-30 10:20:02 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "geometr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Simple feature collection with 100 features and 14 fields
      geometry type:  MULTIPOLYGON
      dimension:      XY
      bbox:           xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
      CRS:            4267
      ── 1. Failure: output the history of a plotted object (@test_visualise.R#104)  ─
      Check on output isn't true.
      Must inherit from class 'simpleMessage', but has classes 'dplyr_regroup','condition'
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 731 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: output the history of a plotted object (@test_visualise.R#104) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘readr’
      All declared Imports should be used.
    ```

# getTBinR

<details>

* Version: 0.7.0
* Source code: https://github.com/cran/getTBinR
* URL: https://www.samabbott.co.uk/getTBinR, https://github.com/seabbs/getTBinR
* BugReports: https://github.com/seabbs/getTBinR/issues
* Date/Publication: 2019-09-03 13:50:06 UTC
* Number of recursive dependencies: 149

Run `cloud_details(, "getTBinR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      cannot open URL 'https://extranet.who.int/tme/generateCSV.asp?ds=estimates': HTTP status was '500 Internal Server Error'
    Warning in file(file, "rt") :
      cannot open file '/tmp/RtmpEV62pw/file98ef3ddbb71': No such file or directory
    Downloading data has failed after 2 tries.
    Attempting data download in 1.4 seconds.
    Downloading data from: https://extranet.who.int/tme/generateCSV.asp?ds=estimates
    Warning in file(file, "rt") :
      cannot open URL 'https://extranet.who.int/tme/generateCSV.asp?ds=estimates': HTTP status was '500 Internal Server Error'
    Downloading the data using utils::read.csv has failed trying a 
                      direct download instead.
    trying URL 'https://extranet.who.int/tme/generateCSV.asp?ds=estimates'
    Warning in download.file(url, destfile = tmp_loc) :
      cannot open URL 'https://extranet.who.int/tme/generateCSV.asp?ds=estimates': HTTP status was '500 Internal Server Error'
    Warning in file(file, "rt") :
      cannot open file '/tmp/RtmpEV62pw/file98ef33847fd2': No such file or directory
    Downloading data has failed after 3 tries.
    Attempting data download in 1.9 seconds.
    Error in get_data(url = "https://extranet.who.int/tme/generateCSV.asp?ds=estimates",  : 
      Data downloading has failed, check your internet connection.
               If this issue is not resolved, contact the package author.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [ OK: 3 | SKIPPED: 3 | WARNINGS: 352 | FAILED: 43 ]
      1. Error: get_data retrieves the specified data when download_data is TRUE (@test-get_data.R#12) 
      2. Error: get_data saves a local copy of the data which can then be successfully retrieved (@test-get_data.R#22) 
      3. Error: (unknown) (@test-get_data_dict.R#4) 
      4. Error: (unknown) (@test-get_tb_burden.R#4) 
      5. Error: map_tb_burden produces a plot (@test-map_tb_burden.R#6) 
      6. Error: map_tb_burden produces a plot when a log transform is used (@test-map_tb_burden.R#16) 
      7. Error: map_tb_burden produces a plot with annual change (@test-map_tb_burden.R#26) 
      8. Error: map_tb_burden produces when no year is specified (@test-map_tb_burden.R#36) 
      9. Error: map_tb_burden produces a map when fill type is 
                manually specified and fails when it is misspecifed (@test-map_tb_burden.R#47) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ggedit

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/ggedit
* URL: https://github.com/metrumresearchgroup/ggedit
* BugReports: https://github.com/metrumresearchgroup/ggedit/issues
* Date/Publication: 2018-07-03 21:50:02 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "ggedit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Problem with `filter()` input `..1`.
    ✖ object 'VAR' not found
    ℹ Input `..1` is `!is.null(VAR)`.
    Backtrace:
         █
      1. └─ggedit::layersList(p)
      2.   ├─ggedit:::rmNullObs(lapply(obj, layersListFull))
      3.   └─base::lapply(obj, layersListFull)
      4.     └─ggedit:::FUN(X[[i]], ...)
      5.       └─ggedit:::fetch_aes_ggplotBuild(obj, geom_list = geom_list(obj))
      6.         └─ggedit:::class_layer(p)
      7.           └─base::lapply(...)
      8.             └─ggedit:::FUN(X[[i]], ...)
      9.               └─`%>%`(...)
     10.                 ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
     11.                 └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     12.                   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     13.                     └─ggedit:::`_fseq`(`_lhs`)
     14.                       └─magrittr::freduce(value, `_function_list`)
     15.                         ├─base::withVisible(function_list[[k]](value))
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# gratia

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/gratia
* URL: https://gavinsimpson.github.io/gratia
* BugReports: https://github.com/gavinsimpson/gratia/issues
* Date/Publication: 2020-03-29 18:30:05 UTC
* Number of recursive dependencies: 111

Run `cloud_details(, "gratia")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. testthat::expect_silent(d <- derivatives(m))
       14. vctrs::stop_incompatible_size(...)
       15. vctrs:::stop_incompatible(...)
       16. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 697 | SKIPPED: 74 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: derivatives() returns derivatives for all smooths in a factor by GAM (@test-derivatives.R#183) 
      2. Error: derivatives() returns derivatives for all smooths in a factor by GAM (@test-derivatives.R#225) 
      3. Error: derivatives() works for factor by smooths issue 47 (@test-derivatives.R#339) 
      4. Error: derivatives() works for fs smooths issue 57 (@test-derivatives.R#389) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# groupedstats

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/groupedstats
* URL: https://indrajeetpatil.github.io/groupedstats/, https://github.com/IndrajeetPatil/groupedstats/
* BugReports: https://github.com/IndrajeetPatil/groupedstats/issues/
* Date/Publication: 2020-05-05 16:20:03 UTC
* Number of recursive dependencies: 120

Run `cloud_details(, "groupedstats")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ── 5. Failure: grouped_proptest works - with NAs (@test-grouped_proptest.R#111) 
      df2$statistic not equal to c(...).
      names for target but not for current
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 30 | SKIPPED: 4 | WARNINGS: 0 | FAILED: 5 ]
      1. Failure: grouped_proptest works - without NAs (@test-grouped_proptest.R#37) 
      2. Failure: grouped_proptest works - without NAs (@test-grouped_proptest.R#49) 
      3. Failure: grouped_proptest works - without NAs (@test-grouped_proptest.R#52) 
      4. Failure: grouped_proptest works - with NAs (@test-grouped_proptest.R#87) 
      5. Failure: grouped_proptest works - with NAs (@test-grouped_proptest.R#111) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# HaDeX

<details>

* Version: 1.1
* Source code: https://github.com/cran/HaDeX
* Date/Publication: 2020-02-06 13:50:02 UTC
* Number of recursive dependencies: 127

Run `cloud_details(, "HaDeX")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: quality_control
    > ### Title: Experiment quality control
    > ### Aliases: quality_control
    > 
    > ### ** Examples
    > 
    > # load example data
    > dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
    > 
    > # calculate mean uncertainty 
    > (result <- quality_control(dat = dat,
    +                            state_first = "CD160",
    +                            state_second = "CD160_HVEM", 
    +                            chosen_time = 1, 
    +                            in_time = 0.001))    
    Error in `[.data.table`(dat, "Exposure") : 
      When i is a data.table (or character vector), the columns to join by must be specified using 'on=' argument (see ?data.table), by keying x (i.e. sorted, and, marked as sorted, see ?setkey), or by sharing column names between x and i (i.e., a natural join). Keyed joins might have further speed benefits on very large data due to x being sorted in RAM.
    Calls: quality_control -> unique -> [ -> [.data.table
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ── 2. Error: size is right  ────────────────────────────────────────────────────
      When i is a data.table (or character vector), the columns to join by must be specified using 'on=' argument (see ?data.table), by keying x (i.e. sorted, and, marked as sorted, see ?setkey), or by sharing column names between x and i (i.e., a natural join). Keyed joins might have further speed benefits on very large data due to x being sorted in RAM.
      Backtrace:
       1. testthat::expect_equal(...)
       6. HaDeX::quality_control(...)
       9. data.table:::`[.data.table`(dat, "Exposure")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 16 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: class is right 
      2. Error: size is right 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘gsubfn’ ‘stringr’
      All declared Imports should be used.
    ```

# idmodelr

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/idmodelr
* URL: http://www.samabbott.co.uk/idmodelr, https://github.com/seabbs/idmodelr
* BugReports: https://github.com/seabbs/idmodelr/issues
* Date/Publication: 2019-09-10 22:50:10 UTC
* Number of recursive dependencies: 126

Run `cloud_details(, "idmodelr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 4. Failure: generate_parameter_permutations can use a single parameter sample
      `df_results` not equal to `df_check`.
      Names: 2 string mismatches
      Component 3: Mean absolute difference: 1
      Component 4: Mean relative difference: 1
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 48 | SKIPPED: 41 | WARNINGS: 0 | FAILED: 4 ]
      1. Failure: Holding out of time works as expected (@test-combine_to_age_model.R#25) 
      2. Failure: Specifying compartments, automatically specifies hold out variables (@test-combine_to_age_model.R#31) 
      3. Failure: Specifying hold out variables, automatically specifies compartments (@test-combine_to_age_model.R#37) 
      4. Failure: generate_parameter_permutations can use a single parameter sample (@test-generate_parameter_permutations.R#26) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# immunarch

<details>

* Version: 0.6.4
* Source code: https://github.com/cran/immunarch
* URL: https://immunarch.com/, https://github.com/immunomind/immunarch
* BugReports: https://github.com/immunomind/immunarch/issues
* Date/Publication: 2020-05-13 08:00:02 UTC
* Number of recursive dependencies: 167

Run `cloud_details(, "immunarch")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘immunarch-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pubRep
    > ### Title: Create a repertoire of public clonotypes
    > ### Aliases: pubRep publicRepertoire
    > 
    > ### ** Examples
    > 
    > # Subset the data to make the example faster to run
    > immdata$data <- lapply(immdata$data, head, 2000)
    > pr <- pubRep(immdata$data, .verbose=FALSE)
    > vis(pr, "clonotypes", 1, 2)
    Warning: You are using a dplyr method on a raw data.table, which will call the
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.8Mb
      sub-directories of 1Mb or more:
        data   2.1Mb
        doc    4.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# IncucyteDRC

<details>

* Version: 0.5.4
* Source code: https://github.com/cran/IncucyteDRC
* URL: https://github.com/chapmandu2/IncucyteDRC
* BugReports: https://github.com/chapmandu2/IncucyteDRC/issues
* Date/Publication: 2016-04-23 14:21:03
* Number of recursive dependencies: 119

Run `cloud_details(, "IncucyteDRC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ✖ could not find function "row_number"
    ℹ Input `idx` is `row_number()`.
    ℹ The error occured in group 1: sampleid = "PDD00017273", conc = 0.3703704.
    Backtrace:
         █
      1. └─IncucyteDRC::fitDoseResponseCurve(test_idrc_set)
      2.   └─IncucyteDRC::exportDRCDataToDataFrame(idrc_set, include_control)
      3.     └─`%>%`(...)
      4.       ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      7.           └─IncucyteDRC:::`_fseq`(`_lhs`)
      8.             └─magrittr::freduce(value, `_function_list`)
      9.               └─function_list[[i]](value)
     10.                 ├─dplyr::mutate(., idx = row_number())
     11.                 └─dplyr:::mutate.data.frame(., idx = row_number())
     12.                   └─dplyr:::mutate_cols(.data, ...)
    <parent: error/simpleError>
    Backtrace:
    █
    Execution halted
    ```

# infer

<details>

* Version: 0.5.1
* Source code: https://github.com/cran/infer
* URL: https://github.com/tidymodels/infer, https://infer.netlify.com/
* BugReports: https://github.com/tidymodels/infer/issues
* Date/Publication: 2019-11-19 10:30:02 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "infer")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      gen_iris12 %>% calculate(stat = "count") not equal to `%>%`(...).
      Attributes: < Names: 1 string mismatch >
      Attributes: < Length mismatch: comparison on first 2 components >
      Attributes: < Component 2: Modes: logical, numeric >
      Attributes: < Component 2: Lengths: 1, 10 >
      Attributes: < Component 2: target is logical, current is numeric >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 339 | SKIPPED: 60 | WARNINGS: 21 | FAILED: 3 ]
      1. Failure: chi-square matches chisq.test value (@test-calculate.R#219) 
      2. Failure: chi-square matches chisq.test value (@test-calculate.R#236) 
      3. Failure: calc_impl.count works (@test-calculate.R#473) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lifecycle’
      All declared Imports should be used.
    ```

# iRF

<details>

* Version: 2.0.0
* Source code: https://github.com/cran/iRF
* URL: https://arxiv.org/abs/1706.08457
* Date/Publication: 2017-07-26 04:57:45 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "iRF")` for more info

</details>

## Newly broken

*   checking whether package ‘iRF’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/iRF/new/iRF.Rcheck/00install.out’ for details.
    ```

# jpndistrict

<details>

* Version: 0.3.6
* Source code: https://github.com/cran/jpndistrict
* URL: https://uribo.github.io/jpndistrict
* BugReports: https://github.com/uribo/jpndistrict/issues
* Date/Publication: 2020-04-20 07:50:07 UTC
* Number of recursive dependencies: 101

Run `cloud_details(, "jpndistrict")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      downloaded 9427 bytes
      
      options:        ENCODING=cp932 
      Reading layer `P34-14_47' from data source `/tmp/Rtmp263d1v/P34-14_47_GML/P34-14_47.shp' using driver `ESRI Shapefile'
      Simple feature collection with 65 features and 4 fields
      geometry type:  POINT
      dimension:      XY
      bbox:           xmin: 123.0045 ymin: 24.06092 xmax: 131.2989 ymax: 27.03917
      CRS:            EPSG:4612
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 93 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: (unknown) (@test-export.R#3) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 188 marked UTF-8 strings
    ```

# keyATM

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/keyATM
* URL: https://keyatm.github.io/keyATM/
* BugReports: https://github.com/keyATM/keyATM/issues
* Date/Publication: 2020-05-23 12:30:02 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "keyATM")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      0%   10   20   30   40   50   60   70   80   90   100%
      [----|----|----|----|----|----|----|----|----|----|
      **************************************************|
      0%   10   20   30   40   50   60   70   80   90   100%
      [----|----|----|----|----|----|----|----|----|----|
      **************************************************|
      0%   10   20   30   40   50   60   70   80   90   100%
      [----|----|----|----|----|----|----|----|----|----|
      **************************************************|
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 38 | SKIPPED: 19 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: Visualizing keywords (@test-Initialization.R#17) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.5Mb
      sub-directories of 1Mb or more:
        libs  16.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘quanteda’
      All declared Imports should be used.
    ```

# kiwisR

<details>

* Version: 0.1.8
* Source code: https://github.com/cran/kiwisR
* URL: https://github.com/rywhale/kiwisR
* BugReports: https://github.com/rywhale/kiwisR/issues
* Date/Publication: 2019-12-15 16:10:02 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "kiwisR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(kiwisR)
      > 
      > test_check("kiwisR")
      ── 1. Failure: ki_station_list accepts custom return fields (vector or string) (
      `stn_cust_retr` not equal to `stn_cust_retr2`.
      Component "station_id": 96 string mismatches
      Component "station_no": 96 string mismatches
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 54 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: ki_station_list accepts custom return fields (vector or string) (@test_ki_station_list.R#71) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# lpirfs

<details>

* Version: 0.1.7
* Source code: https://github.com/cran/lpirfs
* BugReports: https://github.com/adaemmerp/lpirfs/issues
* Date/Publication: 2019-11-25 09:20:06 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "lpirfs")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. lpirfs::lp_nl_panel(...)
        6. plm:::model.frame.pdata.frame(...)
        8. Formula:::model.frame.Formula(...)
       10. stats::model.frame.default(...)
       11. [ base::eval(...) ] with 1 more call
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 168 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 3 ]
      1. Failure: Test that data is correctly computed. (@test-lp_lin_panel.R#737) 
      2. Failure: Test that data is correctly computed. (@test-lp_lin_panel.R#783) 
      3. Error: Check output of switching variable I (@test-lp_nl_panel.R#948) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        libs   6.9Mb
    ```

# mason

<details>

* Version: 0.2.6
* Source code: https://github.com/cran/mason
* URL: https://github.com/lwjohnst86/mason
* BugReports: https://github.com/lwjohnst86/mason/issues
* Date/Publication: 2018-07-05 12:20:02 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "mason")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component "estimate": Mean relative difference: 1.999997
      Component "std.error": Mean relative difference: 0.2627753
      Component "statistic": Mean relative difference: 1.789258
      Component "p.value": Mean relative difference: 1.333262
      Component "conf.low": Mean relative difference: 1.751052
      Component "conf.high": Mean relative difference: 1.316709
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 76 | SKIPPED: 1 | WARNINGS: 7 | FAILED: 3 ]
      1. Failure: (for glm bi) results are equal to real results (no covar) (@test-glm-binomial.R#59) 
      2. Failure: (for glm bi) results are equal to real results (with covar) (@test-glm-binomial.R#73) 
      3. Failure: (for glm) results are equal to real results (with covar + int) (@test-glm-binomial.R#88) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘ggplot2’, ‘pander’, ‘pixiedust’
    ```

# mcp

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/mcp
* URL: http://lindeloev.github.io/mcp/, https://github.com/lindeloev/mcp
* BugReports: https://github.com/lindeloev/mcp/issues
* Date/Publication: 2020-01-09 16:30:02 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "mcp")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1719 | SKIPPED: 1 | WARNINGS: 1 | FAILED: 5 ]
      1. Failure: Good variance:
          y ~ 1 + sigma(x + I(x^2)) (@test-runs.R#277) 
      2. Failure: Good variance:
          y ~ 1 + sigma(x + I(x^2)) (@test-runs.R#277) 
      3. Failure: Good variance:
          y ~ 1, 1 + (1 | id) ~ rel(1) + I(x^2) + sigma(rel(1) + x) (@test-runs.R#277) 
      4. Failure: Good Poisson:
          y ~ 1 + x, y ~ 1 ~ rel(1) + rel(x), rel(1) ~ 0 (@test-runs.R#277) 
      5. Failure: Good Poisson:
          y ~ 1, 1 + (1 | id) ~ 1 (@test-runs.R#277) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘bayesplot’ ‘methods’ ‘purrr’
      All declared Imports should be used.
    ```

# metamicrobiomeR

<details>

* Version: 1.1
* Source code: https://github.com/cran/metamicrobiomeR
* URL: https://github.com/nhanhocu/metamicrobiomeR
* BugReports: https://github.com/nhanhocu/metamicrobiomeR/issues
* Date/Publication: 2019-09-03 07:20:02 UTC
* Number of recursive dependencies: 131

Run `cloud_details(, "metamicrobiomeR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > tl<-colnames(taxtab6)[grep("k__bacteria.p__fusobacteria",colnames(taxtab6))]
    > taxacom.ex<-taxa.compare(taxtab=taxtab6[,c("personid","x.sampleid","bf","age.sample",tl)],
    + propmed.rel="gamlss",comvar="bf",adjustvar="age.sample",
    + longitudinal="yes",p.adjust.method="fdr")
    Error: Corrupt `grouped_df` using old (< 0.8.0) format.
    ℹ Strip off old grouping with `ungroup()`.
    Backtrace:
         █
      1. ├─metamicrobiomeR::taxa.compare(...)
      2. │ └─base::as.data.frame(taxtab)
      3. ├─taxtab6[, c("personid", "x.sampleid", "bf", "age.sample", tl)]
      4. └─dplyr:::`[.grouped_df`(...)
      5.   └─dplyr:::group_intersect(x, out)
      6.     ├─generics::intersect(group_vars(x), names(new))
      7.     ├─dplyr::group_vars(x)
      8.     └─dplyr:::group_vars.data.frame(x)
      9.       ├─generics::setdiff(names(group_data(x)), ".rows")
     10.       ├─dplyr::group_data(x)
     11.       └─dplyr:::group_data.grouped_df(x)
     12.         └─dplyr::validate_grouped_df(.data)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RCurl’ ‘foreign’ ‘gplots’ ‘httr’ ‘jsonlite’ ‘knitr’ ‘lmerTest’
      ‘magrittr’ ‘mgcv’ ‘repmis’ ‘reshape2’ ‘rmarkdown’
      All declared Imports should be used.
    ```

# mmetrics

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/mmetrics
* URL: https://github.com/y-bar/mmetrics
* BugReports: https://github.com/y-bar/mmetrics/issues
* Date/Publication: 2019-07-26 08:50:02 UTC
* Number of recursive dependencies: 96

Run `cloud_details(, "mmetrics")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 12 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 9 ]
      1. Error: summarize one key (@test-mmetrics.R#16) 
      2. Error: summarize two keys (@test-mmetrics.R#21) 
      3. Error: summarize all (@test-mmetrics.R#26) 
      4. Error: mutate one key (@test-mmetrics.R#31) 
      5. Error: mutate two keys (@test-mmetrics.R#36) 
      6. Error: mutate all with (@test-mmetrics.R#41) 
      7. Failure: mutate with non summarize mode to evaluate ratio (@test-mmetrics.R#48) 
      8. Failure: not evaluatable metrics must be removed without error (@test-mmetrics.R#55) 
      9. Error: not evaluatable metrics must be removed without error (@test-mmetrics.R#63) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘stringr’
      All declared Imports should be used.
    ```

# neuropsychology

<details>

* Version: 0.5.0
* Source code: https://github.com/cran/neuropsychology
* URL: https://github.com/neuropsychology/neuropsychology.R
* BugReports: https://github.com/neuropsychology/neuropsychology.R/issues
* Date/Publication: 2017-03-22 19:17:18 UTC
* Number of recursive dependencies: 154

Run `cloud_details(, "neuropsychology")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Can't reconstruct data frame.
    ✖ The `[` method for class <psych/describe/data.frame> must return a data frame.
    ℹ It returned a <describe>.
    Backtrace:
         █
      1. └─neuropsychology::describe(df)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─neuropsychology:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             ├─base::withVisible(function_list[[k]](value))
      9.             └─function_list[[k]](value)
     10.               ├─dplyr::transmute_(...)
     11.               └─dplyr:::transmute_.default(...)
     12.                 ├─dplyr::transmute(.data, !!!dots)
     13.                 └─dplyr:::transmute.data.frame(.data, !!!dots)
     14.                   ├─dplyr::mutate(.data, ..., .keep = "none")
     15.                   └
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘htmlTable’ ‘lme4’ ‘stringi’
      All declared Imports should be used.
    ```

# opentripplanner

<details>

* Version: 0.2.1.0
* Source code: https://github.com/cran/opentripplanner
* URL: https://github.com/ropensci/opentripplanner, https://docs.ropensci.org/opentripplanner/
* BugReports: https://github.com/ropensci/opentripplanner/issues
* Date/Publication: 2020-04-14 17:20:03 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "opentripplanner")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Content type 'application/octet-stream' length 126976 bytes (124 KB)
      ==================================================
      downloaded 124 KB
      
      trying URL 'https://github.com/ropensci/opentripplanner/releases/download/0.1/test_data.zip'
      Content type 'application/octet-stream' length 4289597 bytes (4.1 MB)
      ==================================================
      downloaded 4.1 MB
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 74 | SKIPPED: 22 | WARNINGS: 2 | FAILED: 1 ]
      1. Error: test otp_json2sf (@test_01_internal_funcs.R#64) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6 marked UTF-8 strings
    ```

# pammtools

<details>

* Version: 0.2.3
* Source code: https://github.com/cran/pammtools
* URL: https://github.com/adibender/pammtools
* BugReports: https://github.com/adibender/pammtools/issues
* Date/Publication: 2020-05-27 10:00:03 UTC
* Number of recursive dependencies: 101

Run `cloud_details(, "pammtools")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'dplyr_verbs':
    group_by.ped
      Code: function(.data, ..., .add = FALSE)
      Docs: function(.data, ..., add = FALSE)
      Argument names in code not in docs:
        .add
      Argument names in docs not in code:
        add
      Mismatches in argument names:
        Position: 3 Code: .add Docs: add
    group_by.nested_fdf
      Code: function(.data, ..., .add = FALSE)
      Docs: function(.data, ..., add = FALSE)
      Argument names in code not in docs:
        .add
      Argument names in docs not in code:
        add
      Mismatches in argument names:
        Position: 3 Code: .add Docs: add
    ```

# panelr

<details>

* Version: 0.7.2
* Source code: https://github.com/cran/panelr
* URL: https://panelr.jacob-long.com
* BugReports: https://github.com/jacob-long/panelr
* Date/Publication: 2020-03-08 22:10:02 UTC
* Number of recursive dependencies: 169

Run `cloud_details(, "panelr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘panelr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: are_varying
    > ### Title: Check if variables are constant or variable over time.
    > ### Aliases: are_varying
    > 
    > ### ** Examples
    > 
    > 
    > wages <- panel_data(WageData, id = id, wave = t)
    Warning: The `add` argument of `group_by()` is deprecated as of dplyr 1.0.0.
    Please use the `.add` argument instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    > wages %>% are_varying(occ, ind, fem, blk)
    Error in if (get_wave(data) %in% dots) NULL else get_wave(data) : 
      argument is of length zero
    Calls: %>% ... freduce -> withVisible -> <Anonymous> -> are_varying
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 22 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 17 ]
      1. Failure: dplyr functions return panel_data objects (@test-utils.R#29) 
      2. Error: widen_panel works (@test-utils.R#46) 
      3. Error: long_panel works (basic case) (@test-utils.R#72) 
      4. Error: long_panel works (unbalanced data) (@test-utils.R#96) 
      5. Error: long_panel works (unbalanced data, numeric waves not begin w/ 1) (@test-utils.R#120) 
      6. Error: long_panel works (character periods) (@test-utils.R#146) 
      7. Error: long_panel works (beginning label) (@test-utils.R#171) 
      8. Error: long_panel works (beginning label/character periods) (@test-utils.R#198) 
      9. Error: long_panel works (prefix and suffix/character periods) (@test-utils.R#225) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘AER’
    ```

# photosynthesis

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/photosynthesis
* Date/Publication: 2019-05-09 15:10:03 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "photosynthesis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
                          A            g_mc25              g_sc                g_uc
    1 27.48581 [umol/m^2/s] 4 [umol/Pa/m^2/s] 4 [umol/Pa/m^2/s] 0.1 [umol/Pa/m^2/s]
      gamma_star25          J_max25       K_C25        K_O25  k_mc  k_sc  k_uc
    1   3.743 [Pa] 200 [umol/m^2/s] 27.238 [Pa] 16.582 [kPa] 1 [1] 1 [1] 1 [1]
      leafsize     phi_J          R_d25     T_leaf   theta_J         V_cmax25
    1  0.1 [m] 0.331 [1] 2 [umol/m^2/s] 298.15 [K] 0.825 [1] 150 [umol/m^2/s]
               V_tpu25 g_mc gamma_star J_max    K_C    K_O R_d V_cmax V_tpu   C_air
    1 200 [umol/m^2/s]    4      3.743   200 27.238 16.582   2    150   200 41 [Pa]
                   O              P              PPFD      RH    wind
    1 21.27565 [kPa] 101.3246 [kPa] 1500 [umol/m^2/s] 0.5 [1] 2 [m/s]
    > 
    > # Multiple parameter sets with 'photosynthesis'
    > 
    > leaf_par <- make_leafpar(
    +   replace = list(
    +     T_leaf = set_units(c(293.14, 298.15), "K")
    +     ), use_tealeaves = FALSE
    +   )
    > photosynthesis(leaf_par, enviro_par, bake_par, constants,
    +                use_tealeaves = FALSE)
    Solving for photosynthetic rate from 2 parameter sets ...New names:
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# pmdplyr

<details>

* Version: 0.3.1.1
* Source code: https://github.com/cran/pmdplyr
* URL: https://nickch-k.github.io/pmdplyr, https://github.com/NickCH-K/pmdplyr
* BugReports: https://github.com/NickCH-K/pmdplyr/issues
* Date/Publication: 2020-05-15 08:47:17 UTC
* Number of recursive dependencies: 106

Run `cloud_details(, "pmdplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Problem with `mutate()` input `changed`.
    ✖ Input `changed` can't be recycled to size 8.
    ℹ Input `changed` is `<lgl>`.
    ℹ Input `changed` must be size 8 or 1, not 48445.
    ℹ The error occured in group 1: unitid = 100654.
    Backtrace:
         █
      1. └─pmdplyr::fixed_force(...)
      2.   └─.df %>% dplyr::mutate(`:=`(!!.flag, !!newflag))
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─pmdplyr:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             ├─base::withVisible(function_list[[k]](value))
      9.             └─function_list[[k]](value)
     10.               ├─dplyr::mutate(., `:=`(!!.flag, !!newflag))
     11.               └─dplyr:::mutate.data.frame(., `:=`(!!.flag, !!newflag))
     12.                 └─dplyr:::mutate_cols(.data, ...)
     13.                 
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ℹ The error occured in group 1: i = 1.
      Backtrace:
        1. pmdplyr::fixed_force(df, .var = x, .within = i, .flag = "changed")
       10. dplyr::mutate(., `:=`(!!.flag, !!newflag))
       15. dplyr:::mutate.data.frame(.data, ...)
       16. dplyr:::mutate_cols(.data, ...)
       17. base::tryCatch(...)
       18. base:::tryCatchList(expr, classes, parentenv, handlers)
       19. base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       20. value[[3L]](cond)
       21. dplyr:::stop_mutate_recycle_incompatible_size(e, index = i, dots = dots)
       22. dplyr:::stop_dplyr(...)
      
      Error: C stack usage  7969508 is too close to the limit
      Execution halted
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'join.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# ptstem

<details>

* Version: 0.0.7
* Source code: https://github.com/cran/ptstem
* URL: https://github.com/dfalbel/ptstem
* Date/Publication: 2020-05-12 23:40:03 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "ptstem")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      y[4]: "gostou"
      
      x[5]: "gostaram"
      y[5]: "gostou"
      
      x[6]: "gostaram"
      y[6]: "gostou"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 17 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: Stemming Hunspell Works (@test-ptstem.R#15) 
      2. Failure: Stemming Hunspell Works (@test-ptstem.R#19) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        dict   5.1Mb
    ```

# purrrogress

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/purrrogress
* URL: https://github.com/halpo/purrrogress
* BugReports: https://github.com/halpo/purrrogress/issues
* Date/Publication: 2019-07-22 21:10:08 UTC
* Number of recursive dependencies: 48

Run `cloud_details(, "purrrogress")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(purrrogress)
      > 
      > test_check("purrrogress")
      ── 1. Error: with_progress_group_map (@group_map.R#56)  ────────────────────────
      object '.tbl' not found
      Backtrace:
        1. dplyr::group_map(...)
       13. dplyr::count(.tbl)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 224 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: with_progress_group_map (@group_map.R#56) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘hms’
      All declared Imports should be used.
    ```

# rabhit

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/rabhit
* URL: https://yaarilab.bitbucket.io/RAbHIT/
* BugReports: https://bitbucket.org/yaarilab/haplotyper/issues
* Date/Publication: 2020-01-29 20:20:02 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "rabhit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    [46] 0.50 0.50 1.00 1.00 1.00 1.00 1.00 0.50  NaN 0.50 0.25 1.00 1.00 1.00 0.50
    [61] 1.00 0.50 0.50 0.50 1.00 1.00 1.00 0.50  NaN  NaN 0.50 1.50 1.00 1.00
     [1] 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00
    [16] 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00
    [31] 1.00 0.75 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0.50 1.00 1.00 1.00
    [46] 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0.50  NaN 1.00 0.75 1.00 1.00 1.00 1.00
    [61] 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00  NaN  NaN 1.00 1.50 1.00 1.00
    Error: Can't combine `..1$SUBJECT` <logical> and `..2$SUBJECT` <character>.
    Backtrace:
         █
      1. ├─rabhit::hapDendo(samplesHaplotype)
      2. │ └─dplyr::bind_rows(haplo_db_clust_texture, tmp_point)
      3. │   └─vctrs::vec_rbind(!!!dots, .names_to = .id)
      4. ├─vctrs:::vec_ptype2.data.frame.grouped_df(...)
      5. │ └─vctrs:::gdf_ptype2(x, y, ...)
      6. │   └─vctrs::df_ptype2(x, y, ...)
      7. └─vctrs::vec_default_ptype2(...)
      8.   └─vctrs::stop_incompatible_type(...)
      9.     └─vctrs:::stop_incompatible(...)
     10.       └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

# radiant.data

<details>

* Version: 1.3.4
* Source code: https://github.com/cran/radiant.data
* URL: https://github.com/radiant-rstats/radiant.data, https://radiant-rstats.github.io/radiant.data, https://radiant-rstats.github.io/docs
* BugReports: https://github.com/radiant-rstats/radiant.data/issues
* Date/Publication: 2020-03-23 15:50:05 UTC
* Number of recursive dependencies: 144

Run `cloud_details(, "radiant.data")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 1. Failure: transform ts (@test_funs.R#166)  ────────────────────────────────
      dat$mpg not equal to ts(mtcars$mpg, start = c(1971, 1), frequency = 52).
      Classes differ: numeric is not ts
      
      ── 2. Failure: transform ts (@test_funs.R#167)  ────────────────────────────────
      dat$cyl not equal to ts(mtcars$cyl, start = c(1971, 1), frequency = 52).
      Classes differ: numeric is not ts
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 32 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: transform ts (@test_funs.R#166) 
      2. Failure: transform ts (@test_funs.R#167) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rainette

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/rainette
* URL: https://juba.github.io/rainette/
* BugReports: https://github.com/juba/rainette/issues
* Date/Publication: 2020-05-09 12:00:03 UTC
* Number of recursive dependencies: 155

Run `cloud_details(, "rainette")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. rainette:::filter_crosstab(...)
        3. dplyr::filter(., chi2 > min_chi2, n_both > min_members)
        3. dplyr::select(., g1, g2, level1, level2, n_both, chi2)
       10. dplyr::mutate(...)
       13. dplyr:::mutate_cols(.data, ...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 106 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: rainette2_complete_groups (@test_cutree.R#28) 
      2. Error: (unknown) (@test_rainette2.R#12) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      In for (elt in name) { : closing unused connection 4 (doesnt/exist.txtt)
      Execution halted
    ```

# ratPASTA

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/ratPASTA
* URL: https://github.com/ikodvanj/ratPASTA
* BugReports: https://github.com/ikodvanj/ratPASTA/issues
* Date/Publication: 2020-04-28 11:40:02 UTC
* Number of recursive dependencies: 118

Run `cloud_details(, "ratPASTA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Length mismatch: comparison on first 2 components >
      Attributes: < Component 2: Lengths: 1708, 33000 >
      Attributes: < Component 2: names for target but not for current >
      Attributes: < Component 2: Attributes: < Modes: list, NULL > >
      Attributes: < Component 2: Attributes: < Lengths: 1, 0 > >
      Attributes: < Component 2: Attributes: < names for target but not for current > >
      Attributes: < Component 2: Attributes: < current is not list-like > >
      Attributes: < Component 2: target is omit, current is numeric >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 22 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: Testing automated data loading (@test-loadstartledata.R#7) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘hms’
      All declared Imports should be used.
    ```

# RCMIP5

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/RCMIP5
* Date/Publication: 2016-07-30 18:53:27
* Number of recursive dependencies: 61

Run `cloud_details(, "RCMIP5")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component 4: target is logical, current is numeric
      Component 5: Modes: logical, numeric
      Component 5: target is logical, current is numeric
      data.frame
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 799 | SKIPPED: 29 | WARNINGS: 1 | FAILED: 5 ]
      1. Failure: monthly data (@test_chainedOperations.R#42) 
      2. Failure: monthly data (@test_chainedOperations.R#63) 
      3. Failure: annual data (@test_chainedOperations.R#97) 
      4. Failure: four-D data (@test_chainedOperations.R#132) 
      5. Failure: four-D data (@test_chainedOperations.R#159) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rcv

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/rcv
* URL: https://github.com/ds-elections/rcv
* BugReports: https://github.com/ds-elections/rcv/issues
* Date/Publication: 2017-08-11 08:11:33 UTC
* Number of recursive dependencies: 46

Run `cloud_details(, "rcv")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Problem with `summarise()` input `total`.
    ✖ could not find function "n"
    ℹ Input `total` is `n()`.
    ℹ The error occured in group 1: candidate = "BEN MATRANGA".
    Backtrace:
         █
      1. └─rcv::rcv_tally(image = sf_bos_clean, rcvcontest = "Board of Supervisors, District 7")
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─rcv:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               ├─dplyr::summarise(., total = n())
     10.               └─dplyr:::summarise.grouped_df(., total = n())
     11.                 └─dplyr:::summarise_cols(.data, ...)
    <parent: error/simpleError>
    Backtrace:
    █
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6543 marked UTF-8 strings
    ```

# RNeXML

<details>

* Version: 2.4.4
* Source code: https://github.com/cran/RNeXML
* URL: https://docs.ropensci.org/RNeXML, https://github.com/ropensci/RNeXML
* BugReports: https://github.com/ropensci/RNeXML/issues
* Date/Publication: 2020-05-10 07:20:06 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "RNeXML")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      Loading required package: ape
      ── 1. Failure: coalesce_() works correctly (@test_01_utils.R#129)  ─────────────
      `dplyr::coalesce(dta$col1, dta$col3, last)` did not throw an error.
      
      ── 2. Failure: coalesce_() works correctly (@test_01_utils.R#130)  ─────────────
      `dplyr::coalesce(dta$col3, last)` did not throw an error.
      
      Done simulation(s).
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 300 | SKIPPED: 45 | WARNINGS: 2 | FAILED: 2 ]
      1. Failure: coalesce_() works correctly (@test_01_utils.R#129) 
      2. Failure: coalesce_() works correctly (@test_01_utils.R#130) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rsample

<details>

* Version: 0.0.6
* Source code: https://github.com/cran/rsample
* URL: https://tidymodels.github.io/rsample, https://github.com/tidymodels/rsample
* BugReports: https://github.com/tidymodels/rsample/issues
* Date/Publication: 2020-03-31 19:50:02 UTC
* Number of recursive dependencies: 98

Run `cloud_details(, "rsample")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Component 2: Numeric: lengths (40, 1) differ >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 574 | SKIPPED: 2 | WARNINGS: 0 | FAILED: 8 ]
      1. Failure: rsplit labels (@test_boot.R#89) 
      2. Failure: rsplit labels (@test_group.R#104) 
      3. Failure: rsplit labels (@test_mc.R#86) 
      4. Failure: rsplit labels (@test_nesting.R#71) 
      5. Failure: rsplit labels (@test_rolling.R#102) 
      6. Failure: rsplit labels (@test_validation.R#90) 
      7. Failure: rsplit labels (@test_vfold.R#85) 
      8. Failure: rsplit labels (@test_vfold.R#90) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# RTL

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/RTL
* URL: https://github.com/risktoollib/RTL
* Date/Publication: 2020-02-23 18:50:02 UTC
* Number of recursive dependencies: 136

Run `cloud_details(, "RTL")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RTL-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: chart_zscore
    > ### Title: 'chart_zscore'
    > ### Aliases: chart_zscore
    > 
    > ### ** Examples
    > 
    > chart_zscore(df = ng_storage, title = "NG Storage Z Score",
    + per = "yearweek", output = "stl", chart = "seasons")
    Warning: The `add` argument of `group_by()` is deprecated as of dplyr 1.0.0.
    Please use the `.add` argument instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error in UseMethod("model") : 
      no applicable method for 'model' applied to an object of class "c('grouped_df', 'tbl_df', 'tbl', 'data.frame')"
    Calls: chart_zscore ... eval -> _fseq -> freduce -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘quantmod’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 15456 marked UTF-8 strings
    ```

# rules

<details>

* Version: 0.0.1
* Source code: https://github.com/cran/rules
* URL: https://github.com/tidymodels/rules, https://rules.tidymodels.org
* Date/Publication: 2020-05-20 15:00:02 UTC
* Number of recursive dependencies: 96

Run `cloud_details(, "rules")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 7. Error: non-formula method  ───────────────────────────────────────────────
      object 'rf_m_pred' not found
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 55 | SKIPPED: 12 | WARNINGS: 4 | FAILED: 7 ]
      1. Error: mulit-predict 
      2. Failure: formula method (@test-rule-fit-binomial.R#52) 
      3. Failure: formula method (@test-rule-fit-binomial.R#56) 
      4. Error: formula method 
      5. Failure: non-formula method (@test-rule-fit-binomial.R#131) 
      6. Failure: non-formula method (@test-rule-fit-binomial.R#135) 
      7. Error: non-formula method 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# saotd

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/saotd
* BugReports: https://github.com/evan-l-munson/saotd/issues
* Date/Publication: 2019-04-04 16:30:03 UTC
* Number of recursive dependencies: 118

Run `cloud_details(, "saotd")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(saotd)
      > 
      > test_check("saotd")
      ── 1. Failure: bigrams are computed properly (@test_bigram.R#19)  ──────────────
      saotd::bigram(DataFrame = test_bigram_df) not equal to `correct_bigram_df`.
      Attributes: < Component "class": Lengths (1, 3) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      
      Killed
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 826 marked UTF-8 strings
    ```

# sergeant

<details>

* Version: 0.5.2
* Source code: https://github.com/cran/sergeant
* URL: https://github.com/hrbrmstr/sergeant
* BugReports: https://github.com/hrbrmstr/sergeant/issues
* Date/Publication: 2017-07-17 22:36:26 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "sergeant")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    src_tbls:
      function(x, ...)
    src_tbls.src_drill:
      function(x)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# sf

<details>

* Version: 0.9-3
* Source code: https://github.com/cran/sf
* URL: https://r-spatial.github.io/sf/, https://github.com/r-spatial/sf/
* BugReports: https://github.com/r-spatial/sf/issues/
* Date/Publication: 2020-05-04 17:40:02 UTC
* Number of recursive dependencies: 145

Run `cloud_details(, "sf")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    geometry type:  POINT
    dimension:      XY
    bbox:           xmin: 0 ymin: 1 xmax: 0 ymax: 1
    CRS:            EPSG:3857
      a a.1 a.2        geom                geom.1                       geom.2
    1 1   1   4 POINT (0 1) LINESTRING (1 3, 2 4) MULTILINESTRING ((1 3, 2 4))
    > if (require(dplyr))
    +   dplyr::bind_cols(a,b)
    Loading required package: dplyr
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    New names:
    ```

*   checking tests ... ERROR
    ```
    ...
    < CRS:            GEOGCS["NAD27",DATUM["North_American_Datum_1927",SPHEROID["Clarke 1866",6378206.4,294.9786982138982,AUTHORITY["EPSG","7008"]],AUTHORITY["EPSG","6267"]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433],AUTHORITY["EPSG","4267"]]
    ---
    > geographic CRS: NAD27
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       7. vctrs:::stop_incompatible(...)
       8. vctrs:::stop_vctrs(...)
      
      Failed to create feature 1 in x
      Failed to create feature 1 in x
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 702 | SKIPPED: 55 | WARNINGS: 6 | FAILED: 5 ]
      1. Failure: filter to sfc works (@test_tidy.R#17) 
      2. Failure: filter to sfc works (@test_tidy.R#19) 
      3. Failure: `precision` and `crs` attributes of `sfc` vectors are combined (@test_vctrs.R#48) 
      4. Failure: `precision` and `crs` attributes of `sfc` vectors are combined (@test_vctrs.R#51) 
      5. Error: `sfc` vectors have a common type (@test_vctrs.R#59) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 26.7Mb
      sub-directories of 1Mb or more:
        doc     11.9Mb
        libs    10.3Mb
        sqlite   1.5Mb
    ```

# simglm

<details>

* Version: 0.7.4
* Source code: https://github.com/cran/simglm
* Date/Publication: 2019-05-31 17:10:03 UTC
* Number of recursive dependencies: 90

Run `cloud_details(, "simglm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 1. Failure: interupt TS (@test_knots.r#69)  ─────────────────────────────────
      table(temp_long$time_k) not equal to table(temp_long$time >= 3).
      Numeric: lengths (2, 0) differ
      
      ── 2. Failure: interupt TS (@test_knots.r#96)  ─────────────────────────────────
      table(temp_long$time_k) not equal to table(temp_long$time >= 3).
      Numeric: lengths (2, 0) differ
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 129 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 2 ]
      1. Failure: interupt TS (@test_knots.r#69) 
      2. Failure: interupt TS (@test_knots.r#96) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# simplevis

<details>

* Version: 1.1.3
* Source code: https://github.com/cran/simplevis
* URL: https://statisticsnz.github.io/simplevis, https://github.com/statisticsnz/simplevis
* BugReports: https://github.com/statisticsNZ/simplevis/issues
* Date/Publication: 2020-05-14 19:30:02 UTC
* Number of recursive dependencies: 118

Run `cloud_details(, "simplevis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     2 02          Auckland Region   (((175.44 -36.26242, 175.4375 -36.25857, 175.4…
     3 03          Waikato Region    (((174.7847 -38.13349, 174.7865 -38.13396, 174…
     4 04          Bay of Plenty Re… (((176.1715 -37.68738, 176.1707 -37.68717, 176…
     5 05          Gisborne Region   (((177.876 -38.0767, 177.9723 -37.84501, 178.0…
     6 06          Hawke's Bay Regi… (((177.9018 -39.07296, 177.908 -39.07566, 177.…
     7 07          Taranaki Region   (((174.2067 -39.59022, 174.2015 -39.58808, 174…
     8 08          Manawatu-Wanganu… (((175.5606 -38.49023, 175.5655 -38.49453, 175…
     9 09          Wellington Region (((174.8099 -41.34305, 174.8109 -41.34476, 174…
    10 12          West Coast Region (((170.3303 -43.09979, 170.3268 -43.10285, 170…
    11 13          Canterbury Region (((172.6917 -42.10662, 172.7017 -42.1013, 172.…
    12 14          Otago Region      (((169.7051 -46.47425, 169.7035 -46.47368, 169…
    13 15          Southland Region  (((167.7202 -47.04975, 167.7135 -47.04868, 167…
    14 16          Tasman Region     (((172.4941 -41.04557, 172.4961 -41.04071, 172…
    15 17          Nelson Region     (((173.3167 -41.24106, 173.3154 -41.23618, 173…
    16 18          Marlborough Regi… (((173.3044 -41.507, 173.2889 -41.49327, 173.2…
    > 
    > ggplot_sf(nz_region)
    Error in UseMethod("st_agr<-") : 
      no applicable method for 'st_agr<-' applied to an object of class "c('grouped_df', 'tbl_df', 'tbl', 'data.frame')"
    Calls: ggplot_sf ... fortify.grouped_df -> $<- -> $<-.sf -> [[<- -> [[<-.sf -> st_agr<-
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘readr’ ‘rmarkdown’ ‘shinycssloaders’
      All declared Imports should be used.
    ```

# SIRItoGTFS

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/SIRItoGTFS
* URL: https://github.com/bogind/SIRItoGTFS, http://user47094.vs.easily.co.uk/siri/documentation.htm, https://developers.google.com/transit/gtfs/
* BugReports: https://github.com/bogind/SIRItoGTFS/issues
* Date/Publication: 2018-05-21 18:36:10 UTC
* Number of recursive dependencies: 32

Run `cloud_details(, "SIRItoGTFS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > SIRIsample$Longitude = as.numeric(SIRIsample$Longitude)
    > SIRIsample$Latitude = as.numeric(SIRIsample$Latitude)
    > # load your own GTFS data with `readGTFS()`
    > # or use the subset of GTFS data conformable to the SIRI sample, also included in the package
    > data("GTFSstops")
    > data("GTFSstop_times")
    > data("GTFScalendar")
    > data("GTFStrips")
    > data("GTFSagency")
    > data("GTFSroutes")
    > busesDF = STG(SIRIsample,
    +              GTFSstops. = GTFSstops,
    +              GTFSagency. = GTFSagency,
    +              GTFScalendar. = GTFScalendar,
    +              GTFSroutes. = GTFSroutes,
    +              GTFSstop_times. = GTFSstop_times,
    +              GTFStrips. = GTFStrips,
    +              linerefs = unique(SIRIsample$LineRef[1]))
    [1] "Strating"
    Error: $ operator is invalid for atomic vectors
    Execution halted
    ```

# sistec

<details>

* Version: 0.0.2
* Source code: https://github.com/cran/sistec
* URL: https://github.com/r-ifpe/sistec
* BugReports: https://github.com/r-ifpe/sistec/issues
* Date/Publication: 2020-05-11 12:50:03 UTC
* Number of recursive dependencies: 50

Run `cloud_details(, "sistec")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +                                   package = "sistec"))
    >                                   
    > compare_sistec(sistec, qacademico)                                   
    Error: Input must be a vector, not a `data.frame/sistec_data_frame` object.
    Backtrace:
         █
      1. ├─sistec::compare_sistec(sistec, qacademico)
      2. ├─sistec:::compare_sistec.qacademico_data_frame(sistec, qacademico)
      3. │ └─sistec:::compare_sistec_qacademico(sistec, student_registration)
      4. │   └─sistec:::filter_cpf_sistec(sistec)
      5. │     ├─dplyr::filter(x, !!sym("NU_CPF") == "")
      6. │     └─dplyr:::filter.data.frame(x, !!sym("NU_CPF") == "")
      7. │       ├─dplyr::dplyr_row_slice(.data, loc, preserve = .preserve)
      8. │       └─dplyr:::dplyr_row_slice.data.frame(.data, loc, preserve = .preserve)
      9. │         ├─dplyr::dplyr_reconstruct(vec_slice(data, i), data)
     10. │         │ └─dplyr:::dplyr_new_data_frame(data)
     11. │         │   ├─row.names %||% .row_names_info(x, type = 0L)
     12. │         │   └─base::.row_names_info(x, type = 0L)
     13. │         └─vctrs::vec_slice(data, i)
     14. └─vctrs::
    Execution halted
    ```

# skynet

<details>

* Version: 1.3.0
* Source code: https://github.com/cran/skynet
* URL: https://github.com/FilipeamTeixeira/skynet
* BugReports: https://github.com/FilipeamTeixeira/skynet/issues
* Date/Publication: 2018-12-12 10:20:03 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "skynet")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(skynet)
      > 
      > test_check("skynet")
      ── 1. Failure: Find Airport (@test_smallerfunctions.R#7)  ──────────────────────
      `print\(findAirport\("ATL"\)\[2\]\)` does not match "30397".
      Actual value: "   origin city_mkt_id city latitude longitude\\n1:   <NA>          NA <NA>       NA        NA"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 67 | SKIPPED: 0 | WARNINGS: 6 | FAILED: 1 ]
      1. Failure: Find Airport (@test_smallerfunctions.R#7) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maps’
      All declared Imports should be used.
    ```

# strapgod

<details>

* Version: 0.0.4
* Source code: https://github.com/cran/strapgod
* URL: https://github.com/DavisVaughan/strapgod
* BugReports: https://github.com/DavisVaughan/strapgod/issues
* Date/Publication: 2019-09-20 04:50:02 UTC
* Number of recursive dependencies: 64

Run `cloud_details(, "strapgod")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component 4: Mean relative difference: 0.6849706
      Component 5: Attributes: < target is NULL, current is list >
      Component 5: target is numeric, current is factor
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 147 | SKIPPED: 0 | WARNINGS: 3 | FAILED: 6 ]
      1. Failure: bind_rows() fails sadly (@test-dplyr-compat.R#341) 
      2. Failure: bind_cols() works (@test-dplyr-compat.R#354) 
      3. Failure: bind_cols() works (@test-dplyr-compat.R#366) 
      4. Error: group_modify() (@test-dplyr-group-funs.R#43) 
      5. Failure: group_map() (@test-dplyr-group-funs.R#66) 
      6. Failure: group_walk() (@test-dplyr-group-funs.R#106) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# StratigrapheR

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/StratigrapheR
* Date/Publication: 2020-03-20 13:50:06 UTC
* Number of recursive dependencies: 124

Run `cloud_details(, "StratigrapheR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > l  <- matrix(1:30, ncol = 3, byrow = FALSE)
    > r  <- matrix(2:31, ncol = 3, byrow = FALSE)
    > id <- matrix(rep(c("C1", "C2", "C3"),10), ncol = 3, byrow = TRUE)
    > y  <- matrix(rep(1:10,3), ncol = 3, byrow = FALSE)
    > xout <- seq(-2,32,0.5)
    > 
    > res  <- tie.lim(l = l, r = r,  y = y, xout = xout, id = id)
    > 
    > cont <- tie.lim(l = l, r = r,  y = y, id = id)
    Error: Input must be a vector, not NULL.
    Backtrace:
        █
     1. ├─StratigrapheR::tie.lim(l = l, r = r, y = y, id = id)
     2. │ └─dplyr::lag(xout)
     3. │   ├─vctrs::vec_c(...)
     4. │   └─vctrs::vec_slice(inputs$x, seq_len(xlen - n))
     5. └─vctrs:::stop_scalar_type(.Primitive("quote")(NULL), "")
     6.   └─vctrs:::stop_vctrs(msg, "vctrs_error_scalar_type", actual = x)
    Execution halted
    ```

# sugarbag

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/sugarbag
* URL: https://srkobakian.github.io/sugarbag/, https://github.com/srkobakian/sugarbag
* Date/Publication: 2020-01-08 20:40:02 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "sugarbag")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > 
    > data(tas_sa2)
    > data(capital_cities)
    > hexmap <- create_hexmap(
    +   shp = tas_lga,
    +   sf_id = "LGA_CODE16",
    +   focal_points = capital_cities, verbose = TRUE
    + )
    Warning in st_centroid.sf(., of_largest_polygon = largest) :
      st_centroid assumes attributes are constant over geometries of x
    Warning in st_centroid.sfc(st_geometry(x), of_largest_polygon = of_largest_polygon) :
      st_centroid does not give correct centroids for longitude/latitude data
    Warning: st_crs<- : replacing crs does not reproject data; use st_transform for that
    Warning: st_crs<- : replacing crs does not reproject data; use st_transform for that
    Buffer set to 1.224 degrees.
    Converted hexagon size to 0.1205 degrees.
    Filter set to 1.2047 degrees.
    Finding closest point in focal_points data set.
    Closest points found.
    New names:
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lwgeom’
      All declared Imports should be used.
    ```

# tidyjson

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/tidyjson
* URL: https://github.com/colearendt/tidyjson
* BugReports: https://github.com/colearendt/tidyjson/issues
* Date/Publication: 2020-05-28 13:40:02 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "tidyjson")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
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
    Error: json.list is not a list or json.list is not an atomic vector without attributes
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       16. dplyr:::mutate.data.frame(.data, ..., .keep = "none")
       17. dplyr:::dplyr_col_select(out, keep)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 302 | SKIPPED: 18 | WARNINGS: 0 | FAILED: 7 ]
      1. Error: simple array works (@test-json_structure.R#54) 
      2. Error: seq works for a deeply nested sequence (@test-json_structure.R#145) 
      3. Error: imputes document.id when not present (@test-json_structure.R#168) 
      4. Error: imputed document.id works (@test-json_structure.R#181) 
      5. Error: dplyr::transmute works (@test-tbl_json.R#552) 
      6. Error: dplyr::transmute works (@test-tbl_json.R#601) 
      7. Error: dplyr::transmute works with programming (@test-tbl_json.R#720) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidyRSS

<details>

* Version: 2.0.1
* Source code: https://github.com/cran/tidyRSS
* URL: https://github.com/RobertMyles/tidyrss
* BugReports: https://github.com/RobertMyles/tidyrss/issues
* Date/Publication: 2020-03-07 16:00:02 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "tidyRSS")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 2. Error: df is cleaned properly (@test_general.R#84)  ──────────────────────
      Argument 1 must have names.
      Backtrace:
        1. testthat::expect_equal(...)
        4. tidyRSS:::clean_up(df, "rss", clean_tags = TRUE, parse_dates = TRUE)
       10. purrr::map_df(...)
       13. dplyr::bind_rows(res, .id = .id)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 34 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 2 ]
      1. Error: RSS responses are parsed (@test_general.R#35) 
      2. Error: df is cleaned properly (@test_general.R#84) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidystats

<details>

* Version: 0.4
* Source code: https://github.com/cran/tidystats
* Date/Publication: 2019-09-12 07:20:02 UTC
* Number of recursive dependencies: 29

Run `cloud_details(, "tidystats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   group_by(source) %>%
    +   describe_data(response)
    Error: `x` must be a vector, not a `grouped_df/tbl_df/tbl/data.frame/tidystats_descriptives` object.
    Backtrace:
         █
      1. ├─(function (x, ...) ...
      2. └─tibble:::print.tbl(x)
      3.   ├─cli::cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
      4.   │ └─base::paste0(..., collapse = "\n")
      5.   ├─base::format(x, ..., n = n, width = width, n_extra = n_extra)
      6.   └─tibble:::format.tbl(x, ..., n = n, width = width, n_extra = n_extra)
      7.     └─tibble::trunc_mat(x, n = n, width = width, n_extra = n_extra)
      8.       ├─base::as.data.frame(head(x, n))
      9.       ├─utils::head(x, n)
     10.       └─utils:::head.data.frame(x, n)
     11.         ├─x[seq_len(n), , drop = FALSE]
     12.         └─dplyr:::`[.grouped_df`(x, seq_len(n), , drop = FALSE)
     13.           └─dplyr::grouped_df(out, groups, group_by_drop_default(x))
     14.             └─dplyr:::compute_groups(data, vars, drop = drop)
     15.               ├─tibble::as_tib
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 113 marked UTF-8 strings
    ```

# tidystopwords

<details>

* Version: 0.9.0
* Source code: https://github.com/cran/tidystopwords
* Date/Publication: 2019-02-12 17:20:02 UTC
* Number of recursive dependencies: 36

Run `cloud_details(, "tidystopwords")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: generate_stoplist
    > ### Title: Listing of stop words with control over language and part of
    > ###   speech.
    > ### Aliases: generate_stoplist
    > 
    > ### ** Examples
    > 
    >     # standard usage (might return some non-ASCII characters):
    >     generate_stoplist(lang_name = "English")
    Error: Can't combine `..1$language_id` <logical> and `..2$language_id` <character>.
    Backtrace:
        █
     1. ├─tidystopwords::generate_stoplist(lang_name = "English")
     2. │ └─dplyr::bind_rows(stoplist_db, ling_filter_db)
     3. │   └─vctrs::vec_rbind(!!!dots, .names_to = .id)
     4. └─vctrs::vec_default_ptype2(...)
     5.   └─vctrs::stop_incompatible_type(...)
     6.     └─vctrs:::stop_incompatible(...)
     7.       └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 229801 marked UTF-8 strings
    ```

# timetk

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/timetk
* URL: https://github.com/business-science/timetk
* BugReports: https://github.com/business-science/timetk/issues
* Date/Publication: 2020-04-19 17:50:02 UTC
* Number of recursive dependencies: 151

Run `cloud_details(, "timetk")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Problem with `mutate()` input `nested.col`.
    ✖ Can't recycle `..1` (size 169) to match `..2` (size 0).
    ℹ Input `nested.col` is `purrr::map(...)`.
    ℹ The error occured in group 1: id = "H10".
    Backtrace:
         █
      1. └─`%>%`(...)
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           ├─base::withVisible(function_list[[k]](value))
      8.           └─function_list[[k]](value)
      9.             ├─timetk::plot_acf_diagnostics(...)
     10.             └─timetk:::plot_acf_diagnostics.grouped_df(...)
     11.               ├─timetk::tk_acf_diagnostics(...)
     12.               └─timetk:::tk_acf_diagnostics.grouped_df(...)
     13.                 └─`%>%`(...)
     14.                   ├─base::withVisible(eval(quote(`_fseq`(`_
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        doc   5.1Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2750 marked UTF-8 strings
    ```

# tree.bins

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/tree.bins
* Date/Publication: 2018-06-14 05:33:53 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "tree.bins")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Items of 'old' not found in column names: [Categories]. Consider skip_absent=TRUE.
      Backtrace:
       1. test.df.adj.Nei[, !(names(test.df.adj.Nei) %in% "Neighborhood")]
       1. data.table::as.data.table(.)
       9. data.table::setnames(., "Categories", "Neighborhood")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 11 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 2 ]
      1. Error: Test to see if lookup tables and joins are performed correctly (@test-bin.oth.R#63) 
      2. Error: Testing for 2 predictors,
                both will recategorize the variable.
                Recategorized variable will contain multiple leaves. (@test-tree.bins.R#169) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘forcats’
    ```

# treeplyr

<details>

* Version: 0.1.7
* Source code: https://github.com/cran/treeplyr
* URL: https://github.com/uyedaj/treeplyr
* BugReports: https://github.com/uyedaj/treeplyr/issues
* Date/Publication: 2019-07-25 22:50:02 UTC
* Number of recursive dependencies: 52

Run `cloud_details(, "treeplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: group_by_.treedata group_by.treedata ungroup.grouped_treedata
    > 
    > ### ** Examples
    > 
    > data(anolis)
    > td <- make.treedata(anolis$phy, anolis$dat)
    > tdGrouped <- group_by(td, ecomorph)
    Warning: `group_by_()` is deprecated as of dplyr 0.7.0.
    Please use `group_by()` instead.
    See vignette('programming') for more help
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error: `vars` must be a character vector.
    Backtrace:
        █
     1. ├─dplyr::group_by(td, ecomorph)
     2. └─dplyr:::group_by.default(td, ecomorph)
     3.   ├─dplyr::group_by_(.data, .dots = compat_as_lazy_dots(...), add = add)
     4.   └─treeplyr:::group_by_.treedata(...)
     5.     └─dplyr::grouped_df(groups$data, groups$groups)
    Execution halted
    ```

# tsibble

<details>

* Version: 0.8.6
* Source code: https://github.com/cran/tsibble
* URL: https://tsibble.tidyverts.org
* BugReports: https://github.com/tidyverts/tsibble/issues
* Date/Publication: 2020-01-31 06:20:11 UTC
* Number of recursive dependencies: 93

Run `cloud_details(, "tsibble")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   group_by_key() %>%
    +   fill_gaps(kilo = sum(kilo))
    Error: `vars` must be a character vector.
    Backtrace:
         █
      1. └─harvest %>% group_by_key() %>% fill_gaps(kilo = sum(kilo))
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           ├─base::withVisible(function_list[[k]](value))
      8.           └─function_list[[k]](value)
      9.             ├─tsibble::fill_gaps(., kilo = sum(kilo))
     10.             └─tsibble:::fill_gaps.tbl_ts(., kilo = sum(kilo))
     11.               ├─dplyr::left_join(gap_data, replaced_df, by = by_name)
     12.               └─tsibble:::left_join.tbl_ts(gap_data, replaced_df, by = by_name)
     13.                 └─tsibble:::update_meta(...)
     14.                   └─tsibble:::retain_tsibble(new, key = key(old), index = index(old))
     15.               
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 545 | SKIPPED: 2 | WARNINGS: 4 | FAILED: 35 ]
      1. Failure: 4 day interval (@test-append.R#27) 
      2. Error: (unknown) (@test-append.R#31) 
      3. Error: (unknown) (@test-bind.R#11) 
      4. Error: (unknown) (@test-dplyr.R#5) 
      5. Error: (unknown) (@test-empty.R#32) 
      6. Error: (unknown) (@test-gaps.R#93) 
      7. Error: (unknown) (@test-groups.R#3) 
      8. Error: From Date to year-week, year-month, year-quarter and year (@test-indexby.R#84) 
      9. Failure: index_by() with group_by() (@test-indexby.R#106) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘dtplyr’, ‘dbplyr’
    ```

# vcfR

<details>

* Version: 1.10.0
* Source code: https://github.com/cran/vcfR
* URL: https://github.com/knausb/vcfR, https://knausb.github.io/vcfR_documentation/
* Date/Publication: 2020-02-06 09:50:02 UTC
* Number of recursive dependencies: 111

Run `cloud_details(, "vcfR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # to integer or numeric types...
    > Z <- vcfR2tidy(vcf)
    Error: Can't combine `..1$tt` <logical> and `..2$tt` <character>.
    Backtrace:
         █
      1. ├─vcfR::vcfR2tidy(vcf)
      2. │ ├─base::do.call(what = extract_gt_tidy, args = format_dots)
      3. │ └─(function (x, format_fields = NULL, format_types = TRUE, dot_is_NA = TRUE, ...
      4. │   └─vcfR:::guess_types(format_df %>% dplyr::filter(ID %in% format_fields))
      5. │     └─`%>%`(...)
      6. │       ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      7. │       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      8. │         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      9. │           └─vcfR:::`_fseq`(`_lhs`)
     10. │             └─magrittr::freduce(value, `_function_list`)
     11. │               ├─base::withVisible(function_list[[k]](value))
     12. │               └─function_list[[k]](value)
     13. │                 └─dplyr::bind_rows(., tmp)
     14. │                   └─vctrs::vec_rbind(!!!dots, .names_to = .id)
     15.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. vcfR::vcfR2tidy(vcfR_test, info_only = FALSE)
       15. vctrs::vec_default_ptype2(...)
       16. vctrs::stop_incompatible_type(...)
       17. vctrs:::stop_incompatible(...)
       18. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 475 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Error: extract_gt_tidy works for GT element (@test_vcfRtidy.R#55) 
      2. Error: extract_gt_tidy works for all elements (@test_vcfRtidy.R#70) 
      3. Error: vcfR2tidy works (@test_vcfRtidy.R#81) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.3Mb
      sub-directories of 1Mb or more:
        libs   8.4Mb
    ```

# vctrs

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/vctrs
* URL: https://vctrs.r-lib.org/
* BugReports: https://github.com/r-lib/vctrs/issues
* Date/Publication: 2020-05-11 23:20:02 UTC
* Number of recursive dependencies: 68

Run `cloud_details(, "vctrs")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 3578 | SKIPPED: 27 | WARNINGS: 0 | FAILED: 12 ]
      1. Error: grouped-df is proxied and restored (@test-type-dplyr.R#7) 
      2. Error: can take the common type of grouped tibbles and tibbles (@test-type-dplyr.R#22) 
      3. Error: the common type of grouped tibbles includes the union of grouping variables (@test-type-dplyr.R#36) 
      4. Error: can cast to and from `grouped_df` (@test-type-dplyr.R#45) 
      5. Error: casting to `grouped_df` doesn't require grouping variables (@test-type-dplyr.R#75) 
      6. Error: casting to `grouped_df` handles `drop` (@test-type-dplyr.R#82) 
      7. Error: can cbind grouped data frames (@test-type-dplyr.R#87) 
      8. Error: rowwise can be proxied and restored (@test-type-dplyr.R#107) 
      9. Error: can take the common type of rowwise tibbles and tibbles (@test-type-dplyr.R#116) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# vpc

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/vpc
* URL: https://github.com/ronkeizer/vpc
* Date/Publication: 2020-05-07 15:10:02 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "vpc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # simple function to simulate categorical data for single individual
    > sim_id <- function(id = 1) {
    +   n <- 10
    +   logit <- function(x) exp(x) / (1+exp(x))
    +   data.frame(id = id, time = seq(1, n, length.out = n),
    +              dv = round(logit((1:n) - n/2 + rnorm(n, 0, 1.5))) )
    + }
    > ## simple function to simulate categorical data for a trial
    > sim_trial <- function(i = 1, n = 20) { # function to simulate categorical data for a trial
    +   data.frame(sim = i, do.call("rbind", lapply(1:n, sim_id)))
    + }
    > 
    > ## simulate single trial for 20 individuals
    > obs <- sim_trial(n = 20)
    > 
    > ## simulate 200 trials of 20 individuals
    > sim <- do.call("rbind", lapply(1:200, sim_trial, n = 20))
    > 
    > ## Plot categorical VPC
    > vpc_cat(sim = sim, obs = obs)
    New names:
    ```

*   checking tests ... ERROR
    ```
    ...
      Running ‘test-softwaretype.R’
      Running ‘test-strat-col-detection.R’
      Running ‘test-vpc.R’
      Running ‘test-vpc_cat.R’
    Running the tests in ‘tests/test-vpc_cat.R’ failed.
    Last 13 lines of output:
      * `fact_perc(dv, lev[i])` -> `fact_perc(dv, lev[i])...3`
      * sim -> sim...4
      * bin -> bin...5
      * ...
      New names:
      * NA -> ...1
      * NA -> ...2
      * NA -> ...3
      * NA -> ...4
      * NA -> ...5
      * ...
      Error in names(x) <- value : 
        'names' attribute [6] must be the same length as the vector [0]
      Calls: vpc_cat -> colnames<-
      Execution halted
    ```

# yamlet

<details>

* Version: 0.4.7
* Source code: https://github.com/cran/yamlet
* Date/Publication: 2020-05-18 17:50:02 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "yamlet")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Packages unavailable to check Rd xrefs: ‘dtplyr’, ‘dbplyr’
    Missing link or links in documentation object 'anti_join.decorated.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    Missing link or links in documentation object 'full_join.decorated.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    Missing link or links in documentation object 'inner_join.decorated.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    Missing link or links in documentation object 'left_join.decorated.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    Missing link or links in documentation object 'right_join.decorated.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    Missing link or links in documentation object 'semi_join.decorated.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

## Newly fixed

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘dtplyr’, ‘dbplyr’
    ```

