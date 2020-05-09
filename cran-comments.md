
This is a resubmission to fix the SAN error.


## Test environments

* local macOS: release
* Travis Ubuntu: 3.2, 3.3, 3.4, 3.5, 3.6, 4.0, devel
* win-builder: release, devel
* R-Hub: UBSAN, rchk, and valgrind builds


## R CMD check results

0 errors | 0 warnings | 0 notes


## Revdep checks

We ran revdep checks on 33 revdeps of vctrs: https://github.com/r-lib/vctrs/tree/master/revdep/vctrs. Maintainers of packages with failures were notified on April 26.

There are 6 remaining failures:

- glue
- hardhat
- ipaddress
- probably
- projects
- slider

We will send corrective versions of glue, hardhat, probably, and slider shortly after release.

We also ran revdep checks on 865 revdeps of tidyr: https://github.com/r-lib/vctrs/tree/master/revdep/tidyr. Notifications were sent on April 26.

There are 3 remaining failures:

- biclustermd
- simTool
- tidyjson
