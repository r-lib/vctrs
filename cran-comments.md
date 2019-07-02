
## Test environments

* local macOS: release
* Travis Ubuntu: 3.2, 3.3, oldrel, release, devel
* win-builder: release, devel
* R-Hub: UBSAN, rchk, and valgrind builds

## R CMD check results

0 errors | 0 warnings | 0 notes

There are 3 reverse dependencies, two of which are broken by this
vctrs release (evaluator and probably).

- We will send an update of {probably} shortly.
- We have notified the maintainer of {evaluator} of the failure.
