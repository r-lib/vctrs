# print method gives human friendly output

    Code
      list_of(1, 2:3)
    Output
      <list_of<double>[2]>
      [[1]]
      [1] 1
      
      [[2]]
      [1] 2 3
      

---

    Code
      tibble::tibble(x = list_of(1, 2:3))
    Output
      # A tibble: 2 x 1
                  x
        <list<dbl>>
      1         [1]
      2         [2]

# str method is reasonably correct

    Code
      str(x)
    Output
      list<dbl> [1:2] 
      $ : num 1
      $ : num [1:2] 2 3
      @ ptype: num(0) 

---

    Code
      str(list(list(x, y = 2:1)))
    Output
      List of 1
       $ :List of 2
        ..$  : list<dbl> [1:2] 
        .. ..$ : num 1
        .. ..$ : num [1:2] 2 3
        .. ..@ ptype: num(0) 
        ..$ y: int [1:2] 2 1

---

    Code
      str(x[0])
    Output
      list<dbl> [1:0] 
       list()
      @ ptype: num(0) 

---

    Code
      str(list(list(x[0], y = 2:1)))
    Output
      List of 1
       $ :List of 2
        ..$  : list<dbl> [1:0] 
       list()
        .. ..@ ptype: num(0) 
        ..$ y: int [1:2] 2 1

# list coercions are symmetric and unchanging

    Code
      print(mat)
    Output
                         list   list_of<integer>   list_of<double>   list_of<character>  
      list               "list" "list"             "list"            "list"              
      list_of<integer>   "list" "list_of<integer>" "list_of<double>" "list"              
      list_of<double>    "list" "list_of<double>"  "list_of<double>" "list"              
      list_of<character> "list" "list"             "list"            "list_of<character>"

# error call is passed to inner cast methods

    Code
      (expect_error(fn1()))
    Output
      <error/vctrs_error_cast>
      Error in `fn1()`:
      ! Can't convert `..1` <double> to <character>.
    Code
      (expect_error(fn2()))
    Output
      <error/vctrs_error_cast>
      Error in `fn2()`:
      ! Can't convert `..1` <double> to <character>.

