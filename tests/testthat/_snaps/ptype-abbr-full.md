# data.frames have good default abbr and full methods

    Code
      df <- foobar(data.frame(x = 1, y = "", z = TRUE))
      vec_ptype_abbr(df)
    Output
      [1] "vctrs_fb[,3]"
    Code
      vec_ptype_full(df)
    Output
      [1] "vctrs_foobar<\n  x: double\n  y: character\n  z: logical\n>"

