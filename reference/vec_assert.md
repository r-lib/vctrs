# Assert an argument has known prototype and/or size

**\[questioning\]**

- `vec_is()` is a predicate that checks if its input is a vector that
  conforms to a prototype and/or a size.

- `vec_assert()` throws an error when the input is not a vector or
  doesn't conform.

## Usage

``` r
vec_assert(
  x,
  ptype = NULL,
  size = NULL,
  arg = caller_arg(x),
  call = caller_env()
)

vec_is(x, ptype = NULL, size = NULL)
```

## Arguments

- x:

  A vector argument to check.

- ptype:

  Prototype to compare against. If the prototype has a class, its
  [`vec_ptype()`](https://vctrs.r-lib.org/reference/vec_ptype.md) is
  compared to that of `x` with
  [`identical()`](https://rdrr.io/r/base/identical.html). Otherwise, its
  [`typeof()`](https://rdrr.io/r/base/typeof.html) is compared to that
  of `x` with `==`.

- size:

  A single integer size against which to compare.

- arg:

  Name of argument being checked. This is used in error messages. The
  label of the expression passed as `x` is taken as default.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

`vec_is()` returns `TRUE` or `FALSE`. `vec_assert()` either throws a
typed error (see section on error types) or returns `x`, invisibly.

## Error types

`vec_is()` never throws. `vec_assert()` throws the following errors:

- If the input is not a vector, an error of class
  `"vctrs_error_scalar_type"` is raised.

- If the prototype doesn't match, an error of class
  `"vctrs_error_assert_ptype"` is raised.

- If the size doesn't match, an error of class
  `"vctrs_error_assert_size"` is raised.

Both errors inherit from `"vctrs_error_assert"`.

## Lifecycle

Both `vec_is()` and `vec_assert()` are questioning because their `ptype`
arguments have semantics that are challenging to define clearly and are
rarely useful.

- Use
  [`obj_is_vector()`](https://vctrs.r-lib.org/reference/vector-checks.md)
  or
  [`obj_check_vector()`](https://vctrs.r-lib.org/reference/vector-checks.md)
  for vector checks

- Use
  [`vec_check_size()`](https://vctrs.r-lib.org/reference/vector-checks.md)
  for size checks

- Use [`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md),
  [`inherits()`](https://rdrr.io/r/base/class.html), or simple type
  predicates like
  [`rlang::is_logical()`](https://rlang.r-lib.org/reference/type-predicates.html)
  for specific type checks

## Vectors and scalars

Informally, a vector is a collection that makes sense to use as column
in a data frame. The following rules define whether or not `x` is
considered a vector.

If no [`vec_proxy()`](https://vctrs.r-lib.org/reference/vec_proxy.md)
method has been registered, `x` is a vector if:

- The [base type](https://rdrr.io/r/base/typeof.html) of the object is
  atomic: `"logical"`, `"integer"`, `"double"`, `"complex"`,
  `"character"`, or `"raw"`.

- `x` is a list, as defined by
  [`obj_is_list()`](https://vctrs.r-lib.org/reference/obj_is_list.md).

- `x` is a [data.frame](https://rdrr.io/r/base/data.frame.html).

If a [`vec_proxy()`](https://vctrs.r-lib.org/reference/vec_proxy.md)
method has been registered, `x` is a vector if:

- The proxy satisfies one of the above conditions.

- The base type of the proxy is `"list"`, regardless of its class. S3
  lists are thus treated as scalars unless they implement a
  [`vec_proxy()`](https://vctrs.r-lib.org/reference/vec_proxy.md)
  method.

Otherwise an object is treated as scalar and cannot be used as a vector.
In particular:

- `NULL` is not a vector.

- S3 lists like `lm` objects are treated as scalars by default.

- Objects of type [expression](https://rdrr.io/r/base/expression.html)
  are not treated as vectors.
