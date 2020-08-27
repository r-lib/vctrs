# DiversityOccupancy

<details>

* Version: 1.0.6
* Source code: https://github.com/cran/DiversityOccupancy
* Date/Publication: 2017-03-02 18:32:36
* Number of recursive dependencies: 97

Run `cloud_details(, "DiversityOccupancy")` for more info

</details>

## Newly broken

*   checking whether package ‘DiversityOccupancy’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/DiversityOccupancy/new/DiversityOccupancy.Rcheck/00install.out’ for details.
    ```

# iRF

<details>

* Version: 2.0.0
* Source code: https://github.com/cran/iRF
* URL: https://arxiv.org/abs/1706.08457
* Date/Publication: 2017-07-26 04:57:45 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "iRF")` for more info

</details>

## Newly broken

*   checking whether package ‘iRF’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/iRF/new/iRF.Rcheck/00install.out’ for details.
    ```

# kiwisR

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/kiwisR
* URL: https://github.com/rywhale/kiwisR
* BugReports: https://github.com/rywhale/kiwisR/issues
* Date/Publication: 2020-07-13 14:20:02 UTC
* Number of recursive dependencies: 69

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
      Component "station_id": 95 string mismatches
      Component "station_no": 95 string mismatches
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 54 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: ki_station_list accepts custom return fields (vector or string) (@test_ki_station_list.R#71) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rMorningStar

<details>

* Version: 1.0.6
* Source code: https://github.com/cran/rMorningStar
* Date/Publication: 2020-06-26 09:20:02 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "rMorningStar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Title: ms.Top10Holding
    > ### Aliases: ms.Top10Holding ms.Top10HoldingTotal
    > 
    > ### ** Examples
    > 
    > ms.Top10Holding('FXAIX')
                                 Name Symbol Asset_Percentage
    1                  Microsoft Corp   MSFT           0.0551
    2                       Apple Inc   AAPL           0.0523
    3                  Amazon.com Inc   AMZN           0.0401
    4                  Facebook Inc A     FB           0.0213
    5                  Alphabet Inc A  GOOGL           0.0169
    6            Alphabet Inc Class C   GOOG           0.0168
    7               Johnson & Johnson    JNJ           0.0154
    8  Berkshire Hathaway Inc Class B  BRK.B           0.0143
    9                Visa Inc Class A      V           0.0132
    10            JPMorgan Chase & Co    JPM           0.0120
    > ms.Top10HoldingTotal('FXAIX')
    Error in open.connection(x, "rb") : HTTP error 503.
    Calls: ms.Top10HoldingTotal ... %>% -> eval -> eval -> read_html -> read_html.default
    Execution halted
    ```

