# broom

<details>

* Version: 0.5.6
* Source code: https://github.com/cran/broom
* URL: http://github.com/tidyverse/broom
* BugReports: http://github.com/tidyverse/broom/issues
* Date/Publication: 2020-04-20 17:10:02 UTC
* Number of recursive dependencies: 262

Run `cloud_details(, "broom")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +     decomp = c("decompose", "stl"),
    +     model = list(d1, d2)
    + ) %>%
    +     rowwise() %>%
    +     # Pull out the fitted data using broom::augment.
    +     mutate(augment = list(broom::augment(model))) %>%
    +     ungroup() %>%
    +     # Unnest the data frames into a tidy arrangement of
    +     # the series next to its seasonal decomposition, grouped
    +     # by the method (stl or decompose).
    +     group_by(decomp) %>%
    +     unnest(series, augment) %>%
    +     mutate(index = 1:n()) %>%
    +     ungroup() %>%
    +     select(decomp, index, x, adjusted = .seasadj)
    Warning: unnest() has a new interface. See ?unnest for details.
    Try `df %>% unnest(c(series, augment))`, with `mutate()` if needed
    Error in attributes(.Data) <- c(attributes(.Data), attrib) : 
      cannot assign 'tsp' to zero-length vector
    Calls: %>% ... vec_default_ptype2 -> new_common_class_fallback -> structure
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
    ...
    + per = "yearweek", output = "stats", chart = "seasons")
    Error: Can't convert `$freq` <date> to match type of `$freq` <vctrs:::common_class_fallback>.
    Backtrace:
         █
      1. ├─RTL::chart_zscore(...)
      2. │ └─`%>%`(...)
      3. │   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6. │       └─RTL:::`_fseq`(`_lhs`)
      7. │         └─magrittr::freduce(value, `_function_list`)
      8. │           └─function_list[[i]](value)
      9. │             ├─fabletools::components(.)
     10. │             └─fabletools:::components.mdl_df(.)
     11. │               └─fabletools:::unnest_tsbl(object, "cmp", parent_key = kv)
     12. │                 └─fabletools:::unnest_tbl(.data, tsbl_col)
     13. │                   └─fabletools:::map(...)
     14. │                     └─base::lapply(.x, .f, ...)
     15. │                       └─fabletools:::FUN(X[[i]], ...)
     16. │ 
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
    > # Training Data
    > FB_tbl <- FANG %>%
    +     filter(symbol == "FB") %>%
    +     select(symbol, date, adjusted)
    > 
    > # ---- FUNCTION FORMAT ----
    > # - The `.f = mean` function is used. Argument `na.rm = TRUE` is passed as ...
    > FB_tbl %>%
    +     mutate(adjusted_30_ma = roll_apply_vec(
    +         .x      = adjusted,
    +         .period = 30,
    +         .f      = mean,
    +         na.rm   = TRUE,
    +         .align  = "center")) %>%
    +         ggplot(aes(date, adjusted)) +
    +         geom_line() +
    +         geom_line(aes(y = adjusted_30_ma), color = "blue")
    Error: .onLoad failed in loadNamespace() for 'slider', details:
      call: fun(libname, pkgname)
      error: function 'exp_vec_restore' not provided by package 'vctrs'
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

