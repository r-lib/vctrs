# vctrs 0.1.0.9000

* New `vec_is()` function to check whether a vector conforms to a
  protopty and/or a size. Unlike `vec_assert()`, it doesn't throw
  errors but returns `TRUE` or `FALSE` (#79).

  Called without a specific type or size, it tests whether an object
  is a data vector or a scalar. S3 lists are treated as scalars by
  default. Implement a `vec_is_vector()` for your class to override
  this property (or derive from `vctrs_vctr`).

* New `validate_list_of()` (#193).

* `vec_equal()` now handles missing values symmetrically (#204).

* `vec_proxy_compare()` gains an experimental `relax` argument, which allows 
  data frames to be orderable even if all their columns are not (#210).

* New `vec_order()` and `vec_sort()` for ordering and sorting generalised 
  vectors.

* New `vec_repeat()` for repeating each element of a vector the same number
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
