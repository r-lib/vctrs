# NA

<details>

* Version: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






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
# NA

<details>

* Version: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
