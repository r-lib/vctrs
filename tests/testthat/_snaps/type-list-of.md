# constructor requires list input

    Code
      new_list_of(1)
    Condition
      Error in `new_list_of()`:
      ! `x` must be a list, not the number 1.

---

    Code
      new_list_of(mtcars)
    Condition
      Error in `new_list_of()`:
      ! `x` must be a list, not a <data.frame> object.

# must lock at least one of ptype or size

    Code
      new_list_of(ptype = NULL, size = NULL)
    Condition
      Error in `new_list_of()`:
      ! Must specify at least one of `ptype` or `size`.

# validates `ptype`

    Code
      new_list_of(ptype = lm(1 ~ 1))
    Condition
      Error in `new_list_of()`:
      ! `ptype` must be a vector, not a <lm> object.
      x Detected incompatible scalar S3 list. To be treated as a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <lm>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?vctrs::howto_faq_fix_scalar_type_error`) to learn more.

# validates `size`

    Code
      new_list_of(size = 1.1)
    Condition
      Error in `new_list_of()`:
      ! `size` must be a whole number, not the number 1.1.

---

    Code
      new_list_of(size = 1:2)
    Condition
      Error in `new_list_of()`:
      ! `size` must be a whole number, not an integer vector.

---

    Code
      new_list_of(size = -5)
    Condition
      Error in `new_list_of()`:
      ! `size` must be a whole number larger than or equal to 0, not the number -5.

# can check for list of

    Code
      check_list_of(1)
    Condition
      Error:
      ! `1` must be a `<list_of>`, not the number 1.

# errors if can't determine type

    Code
      list_of(.ptype = NULL)
    Condition
      Error in `list_of()`:
      ! Can't find common type for elements of `x`.

---

    Code
      list_of(1, "a")
    Condition
      Error in `list_of()`:
      ! Can't combine `..1` <double> and `..2` <character>.

---

    Code
      as_list_of(list())
    Condition
      Error in `as_list_of()`:
      ! Can't find common type for elements of `x`.

# errors if can't determine size

    Code
      list_of(.ptype = zap(), .size = NULL)
    Condition
      Error in `list_of()`:
      ! Can't find common size for elements of `x`.

---

    Code
      list_of(1:2, 3:5, .ptype = zap(), .size = NULL)
    Condition
      Error in `list_of()`:
      ! Can't recycle `..1` (size 2) to match `..2` (size 3).

---

    Code
      as_list_of(list(), .ptype = integer(), .size = NULL)
    Condition
      Error in `as_list_of()`:
      ! Can't find common size for elements of `x`.

# can print empty list-of

    Code
      list_of(.ptype = integer(), .size = 5L)
    Output
      <list_of<integer[5]>[0]>

# print method gives human friendly output

    Code
      cat(vec_ptype_full(x))
    Output
      list_of<double>

---

    Code
      cat(vec_ptype_abbr(x))
    Output
      list<dbl>

---

    Code
      cat(vec_ptype_full(x))
    Output
      list_of<any[2]>

---

    Code
      cat(vec_ptype_abbr(x))
    Output
      list<any[2]>

---

    Code
      cat(vec_ptype_full(x))
    Output
      list_of<double[2]>

---

    Code
      cat(vec_ptype_abbr(x))
    Output
      list<dbl[2]>

# print method gives human friendly output for multi line types

    Code
      cat(vec_ptype_full(x))
    Output
      list_of<
        data.frame<
          x: integer
          y: double
          z: character
        >
      >

---

    Code
      cat(vec_ptype_abbr(x))
    Output
      list<df[,3]>

---

    Code
      cat(vec_ptype_full(x))
    Output
      list_of<any[2]>

---

    Code
      cat(vec_ptype_abbr(x))
    Output
      list<any[2]>

---

    Code
      cat(vec_ptype_full(x))
    Output
      list_of<
        data.frame<
          x: integer
          y: double
          z: character
        >[2]
      >

---

    Code
      cat(vec_ptype_abbr(x))
    Output
      list<df[,3][2]>

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

# [[ works

    Code
      x[[3]]
    Condition
      Error:
      ! Invalid index: out of bounds

---

    Code
      x[["c"]]
    Condition
      Error:
      ! Invalid index: field name 'c' not found

# $ works

    Code
      x$c
    Condition
      Error:
      ! Invalid index: field name 'c' not found

# [<- coerces and recycles

    Code
      x[1] <- list("5")
    Condition
      Error in `lapply()`:
      ! Can't convert `X[[i]]` <character> to <double>.

---

    Code
      x[1] <- list(c(1, 2, 3))
    Condition
      Error in `lapply()`:
      ! Can't recycle input of size 3 to size 2.

# [[<- coerces and recycles

    Code
      x[[1]] <- "5"
    Condition
      Error in `[[<-`:
      ! Can't convert `value` <character> to <double>.

---

    Code
      x[[1]] <- c(1, 2, 3)
    Condition
      Error in `[[<-`:
      ! Can't recycle input of size 3 to size 2.

# $<- coerces and recycles

    Code
      x$a <- "5"
    Condition
      Error in `$<-`:
      ! Can't convert `value` <character> to <double>.

---

    Code
      x$a <- c(1, 2, 3)
    Condition
      Error in `$<-`:
      ! Can't recycle input of size 3 to size 2.

# cast: list to list_of

    Code
      vec_cast(list("x"), to)
    Condition
      Error:
      ! Can't convert `..1` <character> to <double>.

---

    Code
      vec_cast(list(1:3), to)
    Condition
      Error:
      ! Can't recycle `..1` (size 3) to size 2.

---

    Code
      vec_cast(list(1:3), to)
    Condition
      Error:
      ! Can't recycle `..1` (size 3) to size 2.

---

    Code
      vec_cast(list("x"), to)
    Condition
      Error:
      ! Can't convert `..1` <character> to <integer>.

# cast: list_of to list_of

    Code
      vec_cast(x, to)
    Condition
      Error:
      ! Can't convert `..1` <double> to <character>.

---

    Code
      vec_cast(y, to)
    Condition
      Error:
      ! Can't recycle `..1` (size 2) to size 3.

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

