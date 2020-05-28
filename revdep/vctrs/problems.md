# feasts

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/feasts
* URL: http://feasts.tidyverts.org/, https://github.com/tidyverts/feasts/
* BugReports: https://github.com/tidyverts/feasts/issues
* Date/Publication: 2020-03-18 07:00:11 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "feasts")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 1. Failure: basic features (@test-features.R#30)  ───────────────────────────
      as.list(ft) not equivalent to list(...).
      Length mismatch: comparison on first 3 components
      
      ── 2. Failure: *shift features (@test-features.R#55)  ──────────────────────────
      as.list(ft) not equivalent to list(...).
      Length mismatch: comparison on first 0 components
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 106 | SKIPPED: 0 | WARNINGS: 5 | FAILED: 2 ]
      1. Failure: basic features (@test-features.R#30) 
      2. Failure: *shift features (@test-features.R#55) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# haven

<details>

* Version: 2.3.0
* Source code: https://github.com/cran/haven
* URL: http://haven.tidyverse.org, https://github.com/tidyverse/haven, https://github.com/WizardMac/ReadStat
* BugReports: https://github.com/tidyverse/haven/issues
* Date/Publication: 2020-05-24 15:00:02 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "haven")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Can't convert <integer> to <labelled<double>>.
      Backtrace:
        1. testthat::expect_identical(...)
        9. vctrs:::vec_restore.vctrs_vctr(x = x, to = to, n = n)
       10. vctrs::stop_incompatible_cast(x, to, x_arg = "", to_arg = "")
       11. vctrs::stop_incompatible_type(...)
       12. vctrs:::stop_incompatible(...)
       13. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 300 | SKIPPED: 2 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: combining is symmetrical w.r.t. data types (@test-labelled.R#75) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘dplyr’
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# sf

<details>

* Version: 0.9-3
* Source code: https://github.com/cran/sf
* URL: https://r-spatial.github.io/sf/, https://github.com/r-spatial/sf/
* BugReports: https://github.com/r-spatial/sf/issues/
* Date/Publication: 2020-05-04 17:40:02 UTC
* Number of recursive dependencies: 145

Run `cloud_details(, "sf")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
    ...
    < CRS:            GEOGCS["NAD27",DATUM["North_American_Datum_1927",SPHEROID["Clarke 1866",6378206.4,294.9786982138982,AUTHORITY["EPSG","7008"]],AUTHORITY["EPSG","6267"]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433],AUTHORITY["EPSG","4267"]]
    ---
    > geographic CRS: NAD27
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       5. vctrs::vec_default_ptype2(...)
       6. vctrs::stop_incompatible_type(...)
       7. vctrs:::stop_incompatible(...)
       8. vctrs:::stop_vctrs(...)
      
      Failed to create feature 1 in x
      Failed to create feature 1 in x
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 704 | SKIPPED: 55 | WARNINGS: 5 | FAILED: 3 ]
      1. Failure: `precision` and `crs` attributes of `sfc` vectors are combined (@test_vctrs.R#48) 
      2. Failure: `precision` and `crs` attributes of `sfc` vectors are combined (@test_vctrs.R#51) 
      3. Error: `sfc` vectors have a common type (@test_vctrs.R#59) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 26.7Mb
      sub-directories of 1Mb or more:
        doc     11.9Mb
        libs    10.3Mb
        sqlite   1.5Mb
    ```

# slider

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/slider
* URL: https://github.com/DavisVaughan/slider
* BugReports: https://github.com/DavisVaughan/slider/issues
* Date/Publication: 2020-05-14 14:00:02 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "slider")` for more info

</details>

## Newly broken

*   checking whether package ‘slider’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/slider/new/slider.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘slider’ ...
** package ‘slider’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/new/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c block.c -o block.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/new/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c compare.c -o compare.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/new/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c hop.c -o hop.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/new/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c index.c -o index.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/new/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c init.c -o init.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/new/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c names.c -o names.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/new/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c params.c -o params.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/new/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c slide-period.c -o slide-period.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/new/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c slide.c -o slide.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/new/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c slider-vctrs-private.c -o slider-vctrs-private.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/new/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c slider-vctrs-public.c -o slider-vctrs-public.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/new/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c utils.c -o utils.o
gcc -shared -L/opt/R/3.6.3/lib/R/lib -L/usr/local/lib -o slider.so block.o compare.o hop.o index.o init.o names.o params.o slide-period.o slide.o slider-vctrs-private.o slider-vctrs-public.o utils.o -L/opt/R/3.6.3/lib/R/lib -lR
installing to /tmp/workdir/slider/new/slider.Rcheck/00LOCK-slider/00new/slider/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘slider’:
 .onLoad failed in loadNamespace() for 'slider', details:
  call: fun(libname, pkgname)
  error: function 'exp_vec_restore' not provided by package 'vctrs'
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/slider/new/slider.Rcheck/slider’

```
### CRAN

```
* installing *source* package ‘slider’ ...
** package ‘slider’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/old/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c block.c -o block.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/old/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c compare.c -o compare.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/old/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c hop.c -o hop.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/old/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c index.c -o index.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/old/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c init.c -o init.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/old/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c names.c -o names.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/old/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c params.c -o params.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/old/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c slide-period.c -o slide-period.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/old/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c slide.c -o slide.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/old/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c slider-vctrs-private.c -o slider-vctrs-private.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/old/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c slider-vctrs-public.c -o slider-vctrs-public.o
gcc -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/tmp/workdir/slider/old/vctrs/include" -I/usr/local/include  -fpic  -g -O2  -c utils.c -o utils.o
gcc -shared -L/opt/R/3.6.3/lib/R/lib -L/usr/local/lib -o slider.so block.o compare.o hop.o index.o init.o names.o params.o slide-period.o slide.o slider-vctrs-private.o slider-vctrs-public.o utils.o -L/opt/R/3.6.3/lib/R/lib -lR
installing to /tmp/workdir/slider/old/slider.Rcheck/00LOCK-slider/00new/slider/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (slider)

```
