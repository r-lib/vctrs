# rcrd (record) S3 class

The rcrd class extends
[vctr](https://vctrs.r-lib.org/reference/new_vctr.md). A rcrd is
composed of 1 or more
[field](https://vctrs.r-lib.org/reference/fields.md)s, which must be
vectors of the same length. Is designed specifically for classes that
can naturally be decomposed into multiple vectors of the same length,
like [POSIXlt](https://rdrr.io/r/base/DateTimeClasses.html), but where
the organisation should be considered an implementation detail invisible
to the user (unlike a
[data.frame](https://rdrr.io/r/base/data.frame.html)).

## Usage

``` r
new_rcrd(fields, ..., class = character())
```

## Arguments

- fields:

  A list or a data frame. Lists must be rectangular (same sizes), and
  contain uniquely named vectors (at least one). `fields` is validated
  with [`df_list()`](https://vctrs.r-lib.org/reference/df_list.md) to
  ensure uniquely named vectors.

- ...:

  Additional attributes

- class:

  Name of subclass.

## Details

Record-style objects created with `new_rcrd()` do not do much on their
own. For instance they do not have a default
[`format()`](https://rdrr.io/r/base/format.html) method, which means
printing the object causes an error. See [Record-style
objects](https://vctrs.r-lib.org/articles/s3-vector.html#record-style-objects)
for details on implementing methods for record vectors.
