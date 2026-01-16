# Create list_of subclass

Create list_of subclass

## Usage

``` r
new_list_of(
  x = list(),
  ptype = logical(),
  size = NULL,
  ...,
  class = character()
)
```

## Arguments

- x:

  A list

- ptype:

  The prototype which every element of `x` belongs to. If `NULL`, the
  prototype is not specified.

- size:

  The size which every element of `x` has. If `NULL`, the size is not
  specified.

- ...:

  Additional attributes used by subclass

- class:

  Optional subclass name
