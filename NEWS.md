# vctrs 0.1.0.9000

* New `vec_expand()` for repeating each element of a vector the same number
  of times.

* `vec_slice()` can correct slice 0-row data frames (#179).

* New `vec_split(x, by)` is a generalisation of `split()` that can divide
  a vector into groups formed by the unique values of another vector. Returns
  a two-column data frame containing unique values of `by` aligned with 
  matching `x` values (#196).

* New `vec_seq_along()` and `vec_na_along()` create useful sequences (#189).

* `vec_equal_na()` now returns `TRUE` for data frames and records when every
  component is mising, not when _any_ component is missing (#201).

* `vec_type2(x, data.frame())` ensures that the returned object has 
  names that are a length-0 character vector.
  
* `vec_cast(x, data.frame())` preserves the number of rows in the `x`.

* Added a `NEWS.md` file to track changes to the package.

* Using classed errors of class `"vctrs_error_assert"` for failed assertions, and of class `"vctrs_error_incompatible"` (with subclasses `_type`, `_cast` and `_op`) for errors on incompatible types (#184).

* Character indexing is now only supported for named objects, an error is raised for unnamed objects (#171).

* `vec_c()` and `vec_rbind()` now handle data.frame columns properly (@yutannihilation, #182).

* `vec_arith()` is consistent with base R when combining `difftime` and `date`, with a warning if casts are lossy (#192).
