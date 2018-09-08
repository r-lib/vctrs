Row-binding
================

``` r
library(tidyverse)
library(vctrs)

df <- data.frame(x = 1:10, y = letters[1:10], stringsAsFactors = FALSE)
dfs <- map(1:100, ~ df)
```

Currently, `vec_rbind()` is *much* slower than the alternatives:

``` r
bench::mark(
  do.call(rbind, dfs),
  vec_rbind(!!!dfs),
  dplyr::bind_rows(dfs)
)
#>  Warning: Some expressions had a GC in every iteration; so filtering is
#>  disabled.
#>  # A tibble: 3 x 10
#>    expression     min     mean  median     max `itr/sec` mem_alloc  n_gc
#>    <chr>      <bch:t> <bch:tm> <bch:t> <bch:t>     <dbl> <bch:byt> <dbl>
#>  1 do.call(r…  3.01ms   4.19ms   3.4ms 32.52ms     239.    200.7KB    16
#>  2 vec_rbind… 42.18ms  46.06ms  46.2ms 49.17ms      21.7   566.9KB    19
#>  3 dplyr::bi… 97.02µs 130.19µs 109.3µs  4.51ms    7681.     43.1KB    16
#>  # … with 2 more variables: n_itr <int>, total_time <bch:tm>
```

I’ve removed the biggest bottlenecks coercing data frames to lists with
`vec_data()`, operating on them, and then restoring with `vec_recast()`.
This avoids the expensive data frame methods. I think further
improvement (to get on par with base/dplyr) will require a systematic
rewrite in C.

``` r
profvis::profvis(vec_rbind(!!!dfs))
```
