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
