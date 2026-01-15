# bayesdfa (1.3.4)

* GitHub: <https://github.com/fate-ewi/bayesdfa>
* Email: <mailto:eric.ward@noaa.gov>
* GitHub mirror: <https://github.com/cran/bayesdfa>

Run `revdepcheck::cloud_details(, "bayesdfa")` for more info

## In both

*   checking whether package ‘bayesdfa’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/bayesdfa/new/bayesdfa.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘bayesdfa’ ...
** this is package ‘bayesdfa’ version ‘1.3.4’
** package ‘bayesdfa’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/new/bayesdfa.Rcheck/bayesdfa’


```
### CRAN

```
* installing *source* package ‘bayesdfa’ ...
** this is package ‘bayesdfa’ version ‘1.3.4’
** package ‘bayesdfa’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/old/bayesdfa.Rcheck/bayesdfa’


```
# ClustAssess (1.1.0)

* GitHub: <https://github.com/Core-Bioinformatics/ClustAssess>
* Email: <mailto:am3019@cam.ac.uk>
* GitHub mirror: <https://github.com/cran/ClustAssess>

Run `revdepcheck::cloud_details(, "ClustAssess")` for more info

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ClustAssess/new/ClustAssess.Rcheck’
* using R version 4.5.1 (2025-06-13)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ClustAssess/DESCRIPTION’ ... OK
...
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/ClustAssess/old/ClustAssess.Rcheck’
* using R version 4.5.1 (2025-06-13)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ClustAssess/DESCRIPTION’ ... OK
...
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK





```
# CytoML (NA)

* : <UNKNOWN>

Run `revdepcheck::cloud_details(, "CytoML")` for more info

## Error before installation

### Devel

```






```
### CRAN

```






```
# flowWorkspace (NA)

* : <UNKNOWN>

Run `revdepcheck::cloud_details(, "flowWorkspace")` for more info

## Error before installation

### Devel

```






```
### CRAN

```






```
# h3o (0.3.0)

* GitHub: <https://github.com/extendr/h3o>
* Email: <mailto:kenneth.b.vernon@gmail.com>
* GitHub mirror: <https://github.com/cran/h3o>

Run `revdepcheck::cloud_details(, "h3o")` for more info

## In both

*   checking whether package ‘h3o’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/h3o/new/h3o.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘h3o’ ...
** this is package ‘h3o’ version ‘0.3.0’
** package ‘h3o’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
...
export CARGO_HOME=/tmp/workdir/h3o/new/h3o.Rcheck/00_pkg_src/h3o/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target 
error: failed to parse lock file at: /tmp/workdir/h3o/new/h3o.Rcheck/00_pkg_src/h3o/src/rust/Cargo.lock

Caused by:
  lock file version 4 requires `-Znext-lockfile-bump`
make: *** [Makevars:26: rust/target/release/libh3o.a] Error 101
ERROR: compilation failed for package ‘h3o’
* removing ‘/tmp/workdir/h3o/new/h3o.Rcheck/h3o’


```
### CRAN

```
* installing *source* package ‘h3o’ ...
** this is package ‘h3o’ version ‘0.3.0’
** package ‘h3o’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
...
export CARGO_HOME=/tmp/workdir/h3o/old/h3o.Rcheck/00_pkg_src/h3o/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target 
error: failed to parse lock file at: /tmp/workdir/h3o/old/h3o.Rcheck/00_pkg_src/h3o/src/rust/Cargo.lock

Caused by:
  lock file version 4 requires `-Znext-lockfile-bump`
make: *** [Makevars:26: rust/target/release/libh3o.a] Error 101
ERROR: compilation failed for package ‘h3o’
* removing ‘/tmp/workdir/h3o/old/h3o.Rcheck/h3o’


```
# loon.shiny (1.0.4)

* Email: <mailto:z267xu@gmail.com>
* GitHub mirror: <https://github.com/cran/loon.shiny>

Run `revdepcheck::cloud_details(, "loon.shiny")` for more info

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/loon.shiny/new/loon.shiny.Rcheck’
* using R version 4.5.1 (2025-06-13)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘loon.shiny/DESCRIPTION’ ... OK
...
* this is package ‘loon.shiny’ version ‘1.0.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'loon', 'loon.ggplot'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/loon.shiny/old/loon.shiny.Rcheck’
* using R version 4.5.1 (2025-06-13)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘loon.shiny/DESCRIPTION’ ... OK
...
* this is package ‘loon.shiny’ version ‘1.0.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'loon', 'loon.ggplot'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# NanoMethViz (NA)

* : <UNKNOWN>

Run `revdepcheck::cloud_details(, "NanoMethViz")` for more info

## Error before installation

### Devel

```






```
### CRAN

```






```
# rshift (3.1.2)

* GitHub: <https://github.com/alexhroom/rshift>
* Email: <mailto:alexhroom+cran@protonmail.com>
* GitHub mirror: <https://github.com/cran/rshift>

Run `revdepcheck::cloud_details(, "rshift")` for more info

## In both

*   checking whether package ‘rshift’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/rshift/new/rshift.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘rshift’ ...
** this is package ‘rshift’ version ‘3.1.2’
** package ‘rshift’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
...
export CARGO_HOME=/tmp/workdir/rshift/new/rshift.Rcheck/00_pkg_src/rshift/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target
error: failed to parse lock file at: /tmp/workdir/rshift/new/rshift.Rcheck/00_pkg_src/rshift/src/rust/Cargo.lock

Caused by:
  lock file version 4 requires `-Znext-lockfile-bump`
make: *** [Makevars:28: rust/target/release/librshift.a] Error 101
ERROR: compilation failed for package ‘rshift’
* removing ‘/tmp/workdir/rshift/new/rshift.Rcheck/rshift’


```
### CRAN

```
* installing *source* package ‘rshift’ ...
** this is package ‘rshift’ version ‘3.1.2’
** package ‘rshift’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
...
export CARGO_HOME=/tmp/workdir/rshift/old/rshift.Rcheck/00_pkg_src/rshift/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target
error: failed to parse lock file at: /tmp/workdir/rshift/old/rshift.Rcheck/00_pkg_src/rshift/src/rust/Cargo.lock

Caused by:
  lock file version 4 requires `-Znext-lockfile-bump`
make: *** [Makevars:28: rust/target/release/librshift.a] Error 101
ERROR: compilation failed for package ‘rshift’
* removing ‘/tmp/workdir/rshift/old/rshift.Rcheck/rshift’


```
# sift (NA)

* : <UNKNOWN>

Run `revdepcheck::cloud_details(, "sift")` for more info

## Error before installation

### Devel

```






```
### CRAN

```






```
# Signac (1.16.0)

* GitHub: <https://github.com/stuart-lab/signac>
* Email: <mailto:stuartt@a-star.edu.sg>
* GitHub mirror: <https://github.com/cran/Signac>

Run `revdepcheck::cloud_details(, "Signac")` for more info

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/Signac/new/Signac.Rcheck’
* using R version 4.5.1 (2025-06-13)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Signac/DESCRIPTION’ ... OK
...
* checking for GNU extensions in Makefiles ... OK
* checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
* checking use of PKG_*FLAGS in Makefiles ... OK
* checking compiled code ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/Signac/old/Signac.Rcheck’
* using R version 4.5.1 (2025-06-13)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Signac/DESCRIPTION’ ... OK
...
* checking for GNU extensions in Makefiles ... OK
* checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
* checking use of PKG_*FLAGS in Makefiles ... OK
* checking compiled code ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK





```
# tipitaka (NA)

* : <UNKNOWN>

Run `revdepcheck::cloud_details(, "tipitaka")` for more info

## Error before installation

### Devel

```






```
### CRAN

```






```
# triangulr (NA)

* : <UNKNOWN>

Run `revdepcheck::cloud_details(, "triangulr")` for more info

## Error before installation

### Devel

```






```
### CRAN

```






```
# TriDimRegression (1.0.3)

* GitHub: <https://github.com/alexander-pastukhov/tridim-regression>
* Email: <mailto:pastukhov.alexander@gmail.com>
* GitHub mirror: <https://github.com/cran/TriDimRegression>

Run `revdepcheck::cloud_details(, "TriDimRegression")` for more info

## In both

*   checking whether package ‘TriDimRegression’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/TriDimRegression/new/TriDimRegression.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘TriDimRegression’ ...
** this is package ‘TriDimRegression’ version ‘1.0.3’
** package ‘TriDimRegression’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘TriDimRegression’
* removing ‘/tmp/workdir/TriDimRegression/new/TriDimRegression.Rcheck/TriDimRegression’


```
### CRAN

```
* installing *source* package ‘TriDimRegression’ ...
** this is package ‘TriDimRegression’ version ‘1.0.3’
** package ‘TriDimRegression’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘TriDimRegression’
* removing ‘/tmp/workdir/TriDimRegression/old/TriDimRegression.Rcheck/TriDimRegression’


```
