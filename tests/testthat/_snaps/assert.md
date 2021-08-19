# assertion failures are explained

    Code
      try_cat(vec_assert(lgl(), chr()))
    Output
      > vec_assert(lgl(), chr()):
      
      Error: `lgl()` must be a vector with type <character>.
      Instead, it has type <logical>.
      
      
      
      NULL
    Code
      try_cat(vec_assert(lgl(), factor()))
    Output
      > vec_assert(lgl(), factor()):
      
      Error: `lgl()` must be a vector with type <factor<>>.
      Instead, it has type <logical>.
      
      
      
      NULL
    Code
      try_cat(vec_assert(lgl(), factor(levels = "foo")))
    Output
      > vec_assert(lgl(), factor(levels = "foo")):
      
      Error: `lgl()` must be a vector with type <factor<c1562>>.
      Instead, it has type <logical>.
      
      
      
      NULL
    Code
      try_cat(vec_assert(factor(levels = "bar"), factor(levels = "foo")))
    Output
      > vec_assert(factor(levels = "bar"), factor(levels = "foo")):
      
      Error: `factor(levels = "bar")` must be a vector with type <factor<c1562>>.
      Instead, it has type <factor<9f154>>.
      
      
      
      NULL
    Code
      try_cat(vec_assert(factor(), chr()))
    Output
      > vec_assert(factor(), chr()):
      
      Error: `factor()` must be a vector with type <character>.
      Instead, it has type <factor<>>.
      
      
      
      NULL
    Code
      try_cat(vec_assert(lgl(), data.frame()))
    Output
      > vec_assert(lgl(), data.frame()):
      
      Error: `lgl()` must be a vector with type <data.frame<>>.
      Instead, it has type <logical>.
      
      
      
      NULL
    Code
      try_cat(vec_assert(lgl(), data.frame(x = 1)))
    Output
      > vec_assert(lgl(), data.frame(x = 1)):
      
      Error: `lgl()` must be a vector with type <data.frame<x:double>>.
      Instead, it has type <logical>.
      
      
      
      NULL
    Code
      try_cat(vec_assert(lgl(), data.frame(x = 1, y = 2)))
    Output
      > vec_assert(lgl(), data.frame(x = 1, y = 2)):
      
      Error: `lgl()` must be a vector with type:
      
        <data.frame<
          x: double
          y: double
        >>
      
      Instead, it has type <logical>.
      
      
      
      NULL
    Code
      try_cat(vec_assert(data.frame(), chr()))
    Output
      > vec_assert(data.frame(), chr()):
      
      Error: `data.frame()` must be a vector with type <character>.
      Instead, it has type <data.frame<>>.
      
      
      
      NULL
    Code
      try_cat(vec_assert(data.frame(x = 1), chr()))
    Output
      > vec_assert(data.frame(x = 1), chr()):
      
      Error: `data.frame(x = 1)` must be a vector with type <character>.
      Instead, it has type <data.frame<x:double>>.
      
      
      
      NULL
    Code
      try_cat(vec_assert(data.frame(x = 1), data.frame(x = "foo")))
    Output
      > vec_assert(data.frame(x = 1), data.frame(x = "foo")):
      
      Error: `data.frame(x = 1)` must be a vector with type <data.frame<x:character>>.
      Instead, it has type <data.frame<x:double>>.
      
      
      
      NULL
    Code
      try_cat(vec_assert(data.frame(x = 1), data.frame(x = "foo", y = 2)))
    Output
      > vec_assert(data.frame(x = 1), data.frame(x = "foo", y = 2)):
      
      Error: `data.frame(x = 1)` must be a vector with type:
      
        <data.frame<
          x: character
          y: double
        >>
      
      Instead, it has type <data.frame<x:double>>.
      
      
      
      NULL
    Code
      try_cat(vec_assert(data.frame(x = 1, y = 2), chr()))
    Output
      > vec_assert(data.frame(x = 1, y = 2), chr()):
      
      Error: `data.frame(x = 1, y = 2)` must be a vector with type <character>.
      Instead, it has type:
      
        <data.frame<
          x: double
          y: double
        >>
      
      
      
      NULL
    Code
      try_cat(vec_assert(data.frame(x = 1, y = 2), data.frame(x = "foo")))
    Output
      > vec_assert(data.frame(x = 1, y = 2), data.frame(x = "foo")):
      
      Error: `data.frame(x = 1, y = 2)` must be a vector with type <data.frame<x:character>>.
      Instead, it has type:
      
        <data.frame<
          x: double
          y: double
        >>
      
      
      
      NULL
    Code
      try_cat(vec_assert(data.frame(x = 1, y = 2), data.frame(x = "foo", y = 2)))
    Output
      > vec_assert(data.frame(x = 1, y = 2), data.frame(x = "foo", y = 2)):
      
      Error: `data.frame(x = 1, y = 2)` must be a vector with type:
      
        <data.frame<
          x: character
          y: double
        >>
      
      Instead, it has type:
      
        <data.frame<
          x: double
          y: double
        >>
      
      
      
      NULL

