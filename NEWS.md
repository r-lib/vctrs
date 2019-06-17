# vctrs 0.1.0.9000

* `vec_c()`, `vec_rbind()`, and `vec_cbind()` gain a `.name_repair`
  argument (#227, #229).

* `vec_dims()` has been replaced by `vec_dim_n()`. This name more clearly
  indicates the purpose of the function.

* New `stop_incompatible_size()` to signal a failure due to mismatched sizes.

* `vec_empty()` has been renamed to `vec_is_empty()`.

* `vec_slice<-` now has a `vec_assign()` alias. Use `vec_assign()`
  when you don't want to modify the original input.

* `vec_slice<-` is now type stable (#140). It always returns the same
  type as the LHS. If needed, the RHS is cast to the correct type, but
  only if both inputs are coercible. See examples in `?vec_slice`.

* Lossy casts now throw errors of type `vctrs_error_cast_lossy`.  You
  can suppress these errors selectively with `allow_lossy_cast()` to
  get the partial cast results. To implement your own lossy cast
  operation, call the new exported function `maybe_lossy_cast()`.

* New `vec_proxy()` generic. This is the main customisation point in
  vctrs along with `vec_restore()`. You should only implement it when
  your type is designed around a non-vector class (atomic vectors,
  bare lists, data frames). In this case, `vec_proxy()` should return
  such a vector class. The vctrs operations will be applied on the
  proxy and `vec_restore()` is called to restore the original
  representation of your type.

  The most common case where you need to implement `vec_proxy()` is
  for S3 lists. In vctrs, S3 lists are treated as scalars by
  default. This way we don't treat objects like model fits as
  vectors. To prevent vctrs from treating your S3 list as a scalar,
  unclass it from the `vec_proxy()` method. For instance here is the
  definition for `list_of`:

  ```
  #' @export
  vec_proxy.vctrs_list_of <- function(x) {
    unclass(x)
  }
  ```

  If you inherit from `vctrs_vctr` or `vctrs_rcrd` you don't need to
  implement `vec_proxy()`.

* `vec_slice()` now calls `vec_restore()` automatically. Unlike the
  default `[` method from base R, attributes are preserved by default.

* `vec_slice()` is now a generic. Its default method calls `[`.

* `vec_size()` now works with positive short row names. This fixes
  issues with data frames created with jsonlite (#220).

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
