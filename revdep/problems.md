# tibbletime

<details>

* Version: 0.1.6
* GitHub: https://github.com/business-science/tibbletime
* Source code: https://github.com/cran/tibbletime
* Date/Publication: 2020-07-21 13:50:02 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "tibbletime")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. └─tibbletime::get_index_col(FANG_unnested)
        5.   ├─.tbl_time[[get_index_char(.tbl_time)]]
        6.   ├─tibble:::`[[.tbl_df`(.tbl_time, get_index_char(.tbl_time))
        7.   │ └─tibble:::tbl_subset2(x, j = i, j_arg = substitute(i))
        8.   └─tibbletime::get_index_char(.tbl_time)
        9.     ├─rlang::quo_name(get_index_quo(.tbl_time))
       10.     │ ├─rlang::expr_name(quo_squash(quo))
       11.     │ └─rlang::quo_squash(quo)
       12.     │   └─rlang::is_quosure(quo)
       13.     └─tibbletime::get_index_quo(.tbl_time)
       14.       └─tibbletime:::glue_stop("Object is not of class `tbl_time`.")
      
      [ FAIL 3 | WARN 2 | SKIP 0 | PASS 142 ]
      Error: Test failures
      Execution halted
    ```

