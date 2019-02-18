# vctrs 0.1.0.9000

* Added a `NEWS.md` file to track changes to the package.

* Using classed errors of class `"vctrs_error_assert"` for failed assertions, and of class `"vctrs_error_incompatible"` (with subclasses `_type`, `_cast` and `_op`) for errors on incompatible types (#184).

* Character indexing is now only supported for named objects, an error is raised for unnamed objects (#171).

* `vec_c()` and `vec_rbind()` now handle data.frame columns properly (@yutannihilation, #182).

* `vec_arith()` is consistent with base R when combining `difftime` and `date`, with a warning if casts are lossy (#192).
