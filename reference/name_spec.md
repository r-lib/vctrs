# Name specifications

A name specification describes how to combine an inner and outer names.
This sort of name combination arises when concatenating vectors or
flattening lists. There are two possible cases:

- Named vector:

      vec_c(outer = c(inner1 = 1, inner2 = 2))

- Unnamed vector:

      vec_c(outer = 1:2)

In r-lib and tidyverse packages, these cases are errors by default,
because there's no behaviour that works well for every case. Instead,
you can provide a name specification that describes how to combine the
inner and outer names of inputs. Name specifications can refer to:

- `outer`: The external name recycled to the size of the input vector.

- `inner`: Either the names of the input vector, or a sequence of
  integer from 1 to the size of the vector if it is unnamed.

## Arguments

- name_spec, .name_spec:

  A name specification for combining inner and outer names. This is
  relevant for inputs passed with a name, when these inputs are
  themselves named, like `outer = c(inner = 1)`, or when they have
  length greater than 1: `outer = 1:2`. By default, these cases trigger
  an error. You can resolve the error by providing a specification that
  describes how to combine the names or the indices of the inner vector
  with the name of the input. This specification can be:

  - A function of two arguments. The outer name is passed as a string to
    the first argument, and the inner names or positions are passed as
    second argument.

  - An anonymous function as a purrr-style formula.

  - A glue specification of the form `"{outer}_{inner}"`.

  - `"inner"`, in which case outer names are ignored, and inner names
    are used if they exist. Note that outer names may still be used to
    provide informative error messages.

  - An [`rlang::zap()`](https://rlang.r-lib.org/reference/zap.html)
    object, in which case both outer and inner names are ignored and the
    result is unnamed.

  See the name specification topic.

## Examples

``` r
# By default, named inputs must be length 1:
vec_c(name = 1)         # ok
#> name 
#>    1 
try(vec_c(name = 1:3))  # bad
#> Error in vec_c(name = 1:3) : 
#>   Can't merge the outer name `name` with a vector of length > 1.
#> Please supply a `.name_spec` specification.

# They also can't have internal names, even if scalar:
try(vec_c(name = c(internal = 1)))  # bad
#> Error in vec_c(name = c(internal = 1)) : 
#>   Can't merge the outer name `name` with a named vector.
#> Please supply a `.name_spec` specification.

# Pass a name specification to work around this. A specification
# can be a glue string referring to `outer` and `inner`:
vec_c(name = 1:3, other = 4:5, .name_spec = "{outer}")
#>  name  name  name other other 
#>     1     2     3     4     5 
vec_c(name = 1:3, other = 4:5, .name_spec = "{outer}_{inner}")
#>  name_1  name_2  name_3 other_1 other_2 
#>       1       2       3       4       5 

# They can also be functions:
my_spec <- function(outer, inner) paste(outer, inner, sep = "_")
vec_c(name = 1:3, other = 4:5, .name_spec = my_spec)
#>  name_1  name_2  name_3 other_1 other_2 
#>       1       2       3       4       5 

# Or purrr-style formulas for anonymous functions:
vec_c(name = 1:3, other = 4:5, .name_spec = ~ paste0(.x, .y))
#>  name1  name2  name3 other1 other2 
#>      1      2      3      4      5 

# Or the string `"inner"` to only use inner names
vec_c(name = 1:3, outer = 4:5, .name_spec = "inner")
#> [1] 1 2 3 4 5
vec_c(name = c(a = 1, b = 2, c = 3), outer = 4:5, .name_spec = "inner")
#> a b c     
#> 1 2 3 4 5 
# This can be useful when you want outer names mentioned in error messages,
# but you don't want them interfering with the result
try(vec_c(x = c(a = 1), y = c(b = "2"), .name_spec = "inner"))
#> Error in vec_c(x = c(a = 1), y = c(b = "2"), .name_spec = "inner") : 
#>   Can't combine `x` <double> and `y` <character>.

# Or `rlang::zap()` to ignore both outer and inner names entirely
vec_c(name = c(a = 1, b = 2), outer = c(c = 3), .name_spec = rlang::zap())
#> [1] 1 2 3
```
