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
      	'/tmp/Rtmp7l5IQU/downloaded_packages'
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

# fingertipsR

<details>

* Version: 1.0.3
* Source code: https://github.com/cran/fingertipsR
* URL: https://fingertips.phe.org.uk, https://github.com/ropensci/fingertipsR, https://docs.ropensci.org/fingertipsR/
* BugReports: https://github.com/ropensci/fingertipsR/issues
* Date/Publication: 2020-04-16 09:00:02 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "fingertipsR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘a-c.R’
    Running the tests in ‘tests/a-c.R’ failed.
    Last 13 lines of output:
      The API is currently unavailable
      Backtrace:
       1. testthat::expect_equal(...)
       4. fingertipsR::nearest_neighbours("E09000001", 102, "CIPFA")
       5. fingertipsR:::fingertips_ensure_api_available(endpoint = path)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 3 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: the area_types function works correctly (@test-area_types.R#14) 
      2. Error: the indicator_areatypes function is working correctly (@test-area_types.R#24) 
      3. Error: category_types returns as expected (@test-area_types.R#38) 
      4. Error: nearest_neighbours returns as expected (@test-area_types.R#46) 
      
      Error: testthat unit tests failed
      Execution halted
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

