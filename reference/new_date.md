# Date, date-time, and duration S3 classes

- A `date` ([Date](https://rdrr.io/r/base/Dates.html)) is a double
  vector. Its value represent the number of days since the Unix "epoch",
  1970-01-01. It has no attributes.

- A `datetime` ([POSIXct](https://rdrr.io/r/base/DateTimeClasses.html)
  is a double vector. Its value represents the number of seconds since
  the Unix "Epoch", 1970-01-01. It has a single attribute: the timezone
  (`tzone`))

- A `duration` ([difftime](https://rdrr.io/r/base/difftime.html))

## Usage

``` r
new_date(x = double())

new_datetime(x = double(), tzone = "")

new_duration(x = double(), units = c("secs", "mins", "hours", "days", "weeks"))

# S3 method for class 'Date'
vec_ptype2(x, y, ...)

# S3 method for class 'POSIXct'
vec_ptype2(x, y, ...)

# S3 method for class 'POSIXlt'
vec_ptype2(x, y, ...)

# S3 method for class 'difftime'
vec_ptype2(x, y, ...)

# S3 method for class 'Date'
vec_cast(x, to, ...)

# S3 method for class 'POSIXct'
vec_cast(x, to, ...)

# S3 method for class 'POSIXlt'
vec_cast(x, to, ...)

# S3 method for class 'difftime'
vec_cast(x, to, ...)

# S3 method for class 'Date'
vec_arith(op, x, y, ...)

# S3 method for class 'POSIXct'
vec_arith(op, x, y, ...)

# S3 method for class 'POSIXlt'
vec_arith(op, x, y, ...)

# S3 method for class 'difftime'
vec_arith(op, x, y, ...)
```

## Arguments

- x:

  A double vector representing the number of days since UNIX epoch for
  `new_date()`, number of seconds since UNIX epoch for `new_datetime()`,
  and number of `units` for `new_duration()`.

- tzone:

  Time zone. A character vector of length 1. Either `""` for the local
  time zone, or a value from
  [`OlsonNames()`](https://rdrr.io/r/base/timezones.html)

- units:

  Units of duration.

## Details

These function help the base `Date`, `POSIXct`, and `difftime` classes
fit into the vctrs type system by providing constructors, coercion
functions, and casting functions.

## Examples

``` r
new_date(0)
#> [1] "1970-01-01"
new_datetime(0, tzone = "UTC")
#> [1] "1970-01-01 UTC"
new_duration(1, "hours")
#> Time difference of 1 hours
```
