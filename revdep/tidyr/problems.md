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

