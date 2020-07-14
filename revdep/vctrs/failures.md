# drake

<details>

* Version: 7.12.4
* Source code: https://github.com/cran/drake
* URL: https://github.com/ropensci/drake, https://docs.ropensci.org/drake, https://books.ropensci.org/drake/
* BugReports: https://github.com/ropensci/drake/issues
* Date/Publication: 2020-06-29 17:20:03 UTC
* Number of recursive dependencies: 141

Run `cloud_details(, "drake")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/drake/new/drake.Rcheck’
* using R version 3.6.3 (2020-02-29)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘drake/DESCRIPTION’ ... OK
* this is package ‘drake’ version ‘7.12.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package suggested but not available: ‘qs’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
### CRAN

```
* using log directory ‘/tmp/workdir/drake/old/drake.Rcheck’
* using R version 3.6.3 (2020-02-29)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘drake/DESCRIPTION’ ... OK
* this is package ‘drake’ version ‘7.12.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package suggested but not available: ‘qs’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
# rsample

<details>

* Version: 0.0.7
* Source code: https://github.com/cran/rsample
* URL: https://rsample.tidymodels.org, https://github.com/tidymodels/rsample
* BugReports: https://github.com/tidymodels/rsample/issues
* Date/Publication: 2020-06-04 07:40:03 UTC
* Number of recursive dependencies: 95

Run `cloud_details(, "rsample")` for more info

</details>

## Newly broken

*   checking whether package ‘rsample’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rsample/new/rsample.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rsample’ ...
** package ‘rsample’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
Error: `...` is not empty.

We detected these problematic arguments:
* `relax`

These dots only exist to allow future extensions and should be empty.
Did you misspecify an argument?
Backtrace:
     █
  1. ├─base::suppressWarnings(...)
  2. │ └─base::withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
  3. ├─base::serialize(...)
  4. ├─base::as.list(base::getNamespace("rsample"), all.names = TRUE)
  5. ├─base::as.list.environment(base::getNamespace("rsample"), all.names = TRUE)
  6. └─rsample::group_vfold_cv(test_data(), y)
  7.   └─rsample:::group_vfold_splits(data = data, group = group, v = v)
  8.     └─data_ind %>% full_join(keys, by = "..group") %>% arrange(..index)
  9.       ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
 10.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
 11.         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
 12.           └─rsample:::`_fseq`(`_lhs`)
 13.    
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/rsample/new/rsample.Rcheck/rsample’

```
### CRAN

```
* installing *source* package ‘rsample’ ...
** package ‘rsample’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (rsample)

```
