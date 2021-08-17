# base coercions are symmetric and unchanging

    Code
      mat
    Output
                logical   integer   double   character   raw   list  
      logical   "logical" "integer" "double" NA          NA    NA    
      integer   "integer" "integer" "double" NA          NA    NA    
      double    "double"  "double"  "double" NA          NA    NA    
      character NA        NA        NA       "character" NA    NA    
      raw       NA        NA        NA       NA          "raw" NA    
      list      NA        NA        NA       NA          NA    "list"

# vec_ptype2() data frame methods builds argument tags

    Code
      vec_ptype2("foo", 10)
    Error <vctrs_error_incompatible_type>
      Can't combine <character> and <double>.

---

    Code
      df1 <- tibble(x = tibble(y = tibble(z = 1)))
      df2 <- tibble(x = tibble(y = tibble(z = "a")))
      vec_ptype2(df1, df2)
    Error <vctrs_error_incompatible_type>
      Can't combine `x$y$z` <double> and `x$y$z` <character>.

