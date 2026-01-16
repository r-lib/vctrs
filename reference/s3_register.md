# Register a method for a suggested dependency

Generally, the recommend way to register an S3 method is to use the
`S3Method()` namespace directive (often generated automatically by the
`@export` roxygen2 tag). However, this technique requires that the
generic be in an imported package, and sometimes you want to suggest a
package, and only provide a method when that package is loaded.
`s3_register()` can be called from your package's `.onLoad()` to
dynamically register a method only if the generic's package is loaded.

## Arguments

- generic:

  Name of the generic in the form `pkg::generic`.

- class:

  Name of the class

- method:

  Optionally, the implementation of the method. By default, this will be
  found by looking for a function called `generic.class` in the package
  environment.

  Note that providing `method` can be dangerous if you use devtools.
  When the namespace of the method is reloaded by
  `devtools::load_all()`, the function will keep inheriting from the old
  namespace. This might cause crashes because of dangling
  [`.Call()`](https://rdrr.io/r/base/CallExternal.html) pointers.

## Details

For R 3.5.0 and later, `s3_register()` is also useful when demonstrating
class creation in a vignette, since method lookup no longer always
involves the lexical scope. For R 3.6.0 and later, you can achieve a
similar effect by using "delayed method registration", i.e. placing the
following in your `NAMESPACE` file:

    if (getRversion() >= "3.6.0") {
      S3method(package::generic, class)
    }

## Usage in other packages

To avoid taking a dependency on vctrs, you copy the source of
[`s3_register()`](https://github.com/r-lib/vctrs/blob/main/R/register-s3.R)
into your own package. It is licensed under the permissive
[unlicense](https://choosealicense.com/licenses/unlicense/) to make it
crystal clear that we're happy for you to do this. There's no need to
include the license or even credit us when using this function.

## Examples

``` r
# A typical use case is to dynamically register tibble/pillar methods
# for your class. That way you avoid creating a hard dependency on packages
# that are not essential, while still providing finer control over
# printing when they are used.

.onLoad <- function(...) {
  s3_register("pillar::pillar_shaft", "vctrs_vctr")
  s3_register("tibble::type_sum", "vctrs_vctr")
}
```
