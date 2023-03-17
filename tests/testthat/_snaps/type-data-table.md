# data table has formatting methods

    Code
      dt <- data.table(x = 1, y = 2, z = 3)
      vec_ptype_abbr(dt)
    Output
      [1] "dt[,3]"
    Code
      vec_ptype_full(dt)
    Output
      [1] "data.table<\n  x: double\n  y: double\n  z: double\n>"

