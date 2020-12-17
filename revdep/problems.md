# aws.alexa

<details>

* Version: 0.1.8
* GitHub: NA
* Source code: https://github.com/cran/aws.alexa
* Date/Publication: 2020-11-10 06:10:02 UTC
* Number of recursive dependencies: 64

Run `cloud_details(, "aws.alexa")` for more info

</details>

## Newly broken

*   checking Rd files ... WARNING
    ```
    alexa_GET.Rd: Sections \title, and \name must exist and be unique in Rd files
    problem found in ‘alexa_GET.Rd’
    ```

# bayes4psy

<details>

* Version: 1.2.5
* GitHub: https://github.com/bstatcomp/bayes4psy
* Source code: https://github.com/cran/bayes4psy
* Date/Publication: 2020-12-07 14:00:02 UTC
* Number of recursive dependencies: 95

Run `cloud_details(, "bayes4psy")` for more info

</details>

## Newly broken

*   checking whether package ‘bayes4psy’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/bayes4psy/new/bayes4psy.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 143.5Mb
      sub-directories of 1Mb or more:
        libs  141.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘scales’ ‘utils’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

## Installation

### Devel

```
* installing *source* package ‘bayes4psy’ ...
** package ‘bayes4psy’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/opt/R/4.0.3/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/reaction_time.stan
Wrote C++ file "stan_files/reaction_time.cc"


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c stan_files/reaction_time.cc -o stan_files/reaction_time.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:392,
...
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
stan_files/reaction_time.cc:29:1: fatal error: error writing to /tmp/ccA3nMQq.s: No space left on device
   29 | }
      | ^
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stan_files/reaction_time.o] Error 1
rm stan_files/reaction_time.cc
ERROR: compilation failed for package ‘bayes4psy’
* removing ‘/tmp/workdir/bayes4psy/new/bayes4psy.Rcheck/bayes4psy’


```
### CRAN

```
* installing *source* package ‘bayes4psy’ ...
** package ‘bayes4psy’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/opt/R/4.0.3/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/reaction_time.stan
Wrote C++ file "stan_files/reaction_time.cc"


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c stan_files/reaction_time.cc -o stan_files/reaction_time.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:392,
...
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (bayes4psy)


```
# bigstatsr

<details>

* Version: 1.3.1
* GitHub: https://github.com/privefl/bigstatsr
* Source code: https://github.com/cran/bigstatsr
* Date/Publication: 2020-11-06 14:50:03 UTC
* Number of recursive dependencies: 100

Run `cloud_details(, "bigstatsr")` for more info

</details>

## Newly broken

*   checking whether package ‘bigstatsr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/bigstatsr/new/bigstatsr.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 24.8Mb
      sub-directories of 1Mb or more:
        extdata   2.3Mb
        libs     21.8Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘bigstatsr’ ...
** package ‘bigstatsr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.0.3/lib/R/site-library/rmio/include' -I/usr/local/include   -fpic  -g -O2  -c AUC.cpp -o AUC.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.0.3/lib/R/site-library/rmio/include' -I/usr/local/include   -fpic  -g -O2  -c FBM-extract.cpp -o FBM-extract.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.0.3/lib/R/site-library/rmio/include' -I/usr/local/include   -fpic  -g -O2  -c FBM-file.cpp -o FBM-file.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.0.3/lib/R/site-library/rmio/include' -I/usr/local/include   -fpic  -g -O2  -c FBM-replace.cpp -o FBM-replace.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.0.3/lib/R/site-library/rmio/include' -I/usr/local/include   -fpic  -g -O2  -c FBM-xptr.cpp -o FBM-xptr.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.0.3/lib/R/site-library/rmio/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
...
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.0.3/lib/R/site-library/rmio/include' -I/usr/local/include   -fpic  -g -O2  -c arma-prod-sub.cpp -o arma-prod-sub.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.0.3/lib/R/site-library/rmio/include' -I/usr/local/include   -fpic  -g -O2  -c arma-prod.cpp -o arma-prod.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.0.3/lib/R/site-library/rmio/include' -I/usr/local/include   -fpic  -g -O2  -c biglassoLin.cpp -o biglassoLin.o
biglassoLin.cpp:39:1: fatal error: error writing to /tmp/ccEAMakR.s: No space left on device
   39 | }
      | ^
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:181: biglassoLin.o] Error 1
ERROR: compilation failed for package ‘bigstatsr’
* removing ‘/tmp/workdir/bigstatsr/new/bigstatsr.Rcheck/bigstatsr’


```
### CRAN

```
* installing *source* package ‘bigstatsr’ ...
** package ‘bigstatsr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.0.3/lib/R/site-library/rmio/include' -I/usr/local/include   -fpic  -g -O2  -c AUC.cpp -o AUC.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.0.3/lib/R/site-library/rmio/include' -I/usr/local/include   -fpic  -g -O2  -c FBM-extract.cpp -o FBM-extract.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.0.3/lib/R/site-library/rmio/include' -I/usr/local/include   -fpic  -g -O2  -c FBM-file.cpp -o FBM-file.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.0.3/lib/R/site-library/rmio/include' -I/usr/local/include   -fpic  -g -O2  -c FBM-replace.cpp -o FBM-replace.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.0.3/lib/R/site-library/rmio/include' -I/usr/local/include   -fpic  -g -O2  -c FBM-xptr.cpp -o FBM-xptr.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.0.3/lib/R/site-library/rmio/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
...
Creating a generic function for ‘crossprod’ from package ‘base’ in package ‘bigstatsr’
Creating a generic function for ‘tcrossprod’ from package ‘base’ in package ‘bigstatsr’
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (bigstatsr)


```
# BIRDS

<details>

* Version: 0.1.2
* GitHub: https://github.com/Greensway/BIRDS
* Source code: https://github.com/cran/BIRDS
* Date/Publication: 2020-10-08 16:50:02 UTC
* Number of recursive dependencies: 182

Run `cloud_details(, "BIRDS")` for more info

</details>

## Newly broken

*   checking whether package ‘BIRDS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/BIRDS/new/BIRDS.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘esquisse’ ‘rgdal’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 27976 marked UTF-8 strings
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘dggridR’
    ```

## Installation

### Devel

```
* installing *source* package ‘BIRDS’ ...
** package ‘BIRDS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
Error in lazyLoadDBinsertVariable(vars[i], from, datafile, ascii, compress,  : 
  write failed
ERROR: unable to build sysdata DB for package ‘BIRDS’
* removing ‘/tmp/workdir/BIRDS/new/BIRDS.Rcheck/BIRDS’


```
### CRAN

```
* installing *source* package ‘BIRDS’ ...
** package ‘BIRDS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (BIRDS)


```
# CARBayes

<details>

* Version: 5.2
* GitHub: https://github.com/duncanplee/CARBayes
* Source code: https://github.com/cran/CARBayes
* Date/Publication: 2020-03-13 08:50:06 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "CARBayes")` for more info

</details>

## Newly broken

*   checking R code for possible problems ... ERROR
    ```
    Incorrect (un)loading of package shared object.
    Fatal error: cannot create 'R_TempDir'
    The system-specific extension for shared objects must not be added.
    See ?library.dynam.
     NOTE
    Fatal error: cannot create 'R_TempDir'
    
    Found the following possibly unsafe calls:
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking foreign function calls ... NOTE
    ```
    Fatal error: cannot create 'R_TempDir'
    See chapter ‘System and foreign language interfaces’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... ERROR
    ```
    Incorrect (un)loading of package shared object.
    Fatal error: cannot create 'R_TempDir'
    The system-specific extension for shared objects must not be added.
    See ?library.dynam.
     NOTE
    Fatal error: cannot create 'R_TempDir'
    
    Found the following possibly unsafe calls:
    Fatal error: cannot create 'R_TempDir'
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        libs   4.1Mb
    ```

# chillR

<details>

* Version: 0.72
* GitHub: NA
* Source code: https://github.com/cran/chillR
* Date/Publication: 2020-12-10 19:50:02 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "chillR")` for more info

</details>

## Newly broken

*   checking R code for possible problems ... ERROR
    ```
    Incorrect (un)loading of package shared object.
    Fatal error: cannot create 'R_TempDir'
    The system-specific extension for shared objects must not be added.
    See ?library.dynam.
     NOTE
    Fatal error: cannot create 'R_TempDir'
    
    Fatal error: cannot create 'R_TempDir'
    
    Found the following possibly unsafe calls:
    Fatal error: cannot create 'R_TempDir'
    
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking Rd files ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking Rd metadata ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking Rd cross-references ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking foreign function calls ... NOTE
    ```
    Fatal error: cannot create 'R_TempDir'
    See chapter ‘System and foreign language interfaces’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... ERROR
    ```
    Incorrect (un)loading of package shared object.
    Fatal error: cannot create 'R_TempDir'
    The system-specific extension for shared objects must not be added.
    See ?library.dynam.
     NOTE
    Fatal error: cannot create 'R_TempDir'
    
    Fatal error: cannot create 'R_TempDir'
    
    Found the following possibly unsafe calls:
    Fatal error: cannot create 'R_TempDir'
    
    Fatal error: cannot create 'R_TempDir'
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# Countr

<details>

* Version: 3.5.4
* GitHub: https://github.com/GeoBosh/Countr
* Source code: https://github.com/cran/Countr
* Date/Publication: 2019-08-21 11:30:06 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "Countr")` for more info

</details>

## Newly broken

*   checking whether package ‘Countr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/Countr/new/Countr.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 15.2Mb
      sub-directories of 1Mb or more:
        doc    2.0Mb
        libs  12.3Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘Countr’ ...
** package ‘Countr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c built_in_distributions.cpp -o built_in_distributions.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c conv_all.cpp -o conv_all.o
/tmp/ccs8tIMG.s: Assembler messages:
/tmp/ccs8tIMG.s: Fatal error: can't write 3887 bytes to section .debug_str of conv_all.o: 'No space left on device'
/tmp/ccs8tIMG.s: Fatal error: can't close conv_all.o: No space left on device
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:181: conv_all.o] Error 1
ERROR: compilation failed for package ‘Countr’
* removing ‘/tmp/workdir/Countr/new/Countr.Rcheck/Countr’


```
### CRAN

```
* installing *source* package ‘Countr’ ...
** package ‘Countr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c built_in_distributions.cpp -o built_in_distributions.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c conv_all.cpp -o conv_all.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c conv_dePril.cpp -o conv_dePril.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c conv_naive.cpp -o conv_naive.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c modifiedRenewal.cpp -o modifiedRenewal.o
...
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (Countr)


```
# DeLorean

<details>

* Version: 1.5.0
* GitHub: NA
* Source code: https://github.com/cran/DeLorean
* Date/Publication: 2018-10-17 22:30:16 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "DeLorean")` for more info

</details>

## Newly broken

*   checking whether package ‘DeLorean’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/DeLorean/new/DeLorean.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 152.7Mb
      sub-directories of 1Mb or more:
        libs  149.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lattice’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘knitcitations’
    ```

## Installation

### Devel

```
* installing *source* package ‘DeLorean’ ...
** package ‘DeLorean’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/opt/R/4.0.3/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/lowranksizes.stan
Wrote C++ file "stan_files/lowranksizes.cc"
g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"`"/opt/R/4.0.3/lib/R/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I/usr/local/include   -fpic  -g -O2  -c stan_files/lowranksizes.cc -o stan_files/lowranksizes.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:392,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.0.3/lib/R/site-library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13,
...
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
stan_files/exactsizes.cc:29:1: fatal error: error writing to /tmp/ccC1yu4r.s: No space left on device
   29 | }
      | ^
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stan_files/exactsizes.o] Error 1
rm stan_files/exactsizes.cc stan_files/lowranksizes.cc stan_files/lowrank.cc
ERROR: compilation failed for package ‘DeLorean’
* removing ‘/tmp/workdir/DeLorean/new/DeLorean.Rcheck/DeLorean’


```
### CRAN

```
* installing *source* package ‘DeLorean’ ...
** package ‘DeLorean’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/opt/R/4.0.3/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/lowranksizes.stan
Wrote C++ file "stan_files/lowranksizes.cc"
g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"`"/opt/R/4.0.3/lib/R/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I/usr/local/include   -fpic  -g -O2  -c stan_files/lowranksizes.cc -o stan_files/lowranksizes.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:392,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.0.3/lib/R/site-library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13,
...
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (DeLorean)


```
# DescrTab2

<details>

* Version: 2.0.3
* GitHub: https://github.com/imbi-heidelberg/DescrTab2
* Source code: https://github.com/cran/DescrTab2
* Date/Publication: 2020-12-16 10:00:05 UTC
* Number of recursive dependencies: 131

Run `cloud_details(, "DescrTab2")` for more info

</details>

## Newly broken

*   checking Rd \usage sections ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyselect’
      All declared Imports should be used.
    ```

# electoral

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/electoral
* Date/Publication: 2020-05-20 02:30:02 UTC
* Number of recursive dependencies: 21

Run `cloud_details(, "electoral")` for more info

</details>

## Newly broken

*   checking whether package ‘electoral’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/electoral/new/electoral.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```



```
### CRAN

```
* installing *source* package ‘electoral’ ...
** package ‘electoral’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (electoral)


```
# emuR

<details>

* Version: 2.1.1
* GitHub: https://github.com/IPS-LMU/emuR
* Source code: https://github.com/cran/emuR
* Date/Publication: 2020-09-30 08:40:12 UTC
* Number of recursive dependencies: 96

Run `cloud_details(, "emuR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Warning (test_emuR-get_trackdata.R:267:5): non wrassp functions work
      Warning (test_emuR-get_trackdata.R:267:5): non wrassp functions work
      Warning (test_emuR-get_trackdata.R:267:5): non wrassp functions work
      Warning (test_emuR-get_trackdata.R:280:3): non wrassp functions work
      Warning (test_emuR-get_trackdata.R:280:3): non wrassp functions work
      Warning (test_emuR-get_trackdata.R:280:3): non wrassp functions work
      Warning (test_emuR-get_trackdata.R:280:3): non wrassp functions work
      ERROR (test_emuR-get_trackdata.R:280:3): non wrassp functions work
      ERROR (test_emuR-requery.database.R:222:3): requery_hier inserts NAs
      
      [ FAIL 15 | WARN 42 | SKIP 30 | PASS 536 ]
      Error: Test failures
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

# EpiNow2

<details>

* Version: 1.3.2
* GitHub: https://github.com/epiforecasts/EpiNow2
* Source code: https://github.com/cran/EpiNow2
* Date/Publication: 2020-12-14 09:00:15 UTC
* Number of recursive dependencies: 150

Run `cloud_details(, "EpiNow2")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is 269.1Mb
      sub-directories of 1Mb or more:
        libs  267.4Mb
    ```

## Newly fixed

*   checking whether package ‘EpiNow2’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/EpiNow2/old/EpiNow2.Rcheck/00install.out’ for details.
    ```

# geometr

<details>

* Version: 0.2.5
* GitHub: https://github.com/EhrmannS/geometr
* Source code: https://github.com/cran/geometr
* Date/Publication: 2020-07-14 13:20:05 UTC
* Number of recursive dependencies: 91

Run `cloud_details(, "geometr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ERROR (test_visualise.R:20:3): visualise a matrix
      ERROR (test_visualise.R:28:3): visualise an image
      ERROR (test_visualise.R:38:3): visualise a geom
      ERROR (test_visualise.R:50:3): quick options produce output
      ERROR (test_visualise.R:81:3): visualise a geom on top of an already plotted raster
      FAILURE (test_visualise.R:90:3): output the history of a plotted object
      FAILURE (test_visualise.R:94:3): output the history of a plotted object
      ERROR (test_visualise.R:101:3): output the history of a plotted object
      
      [ FAIL 22 | WARN 7 | SKIP 0 | PASS 696 ]
      Error: Test failures
      Execution halted
      Error: internal read error in PDF_endpage
      Fatal error: error during cleanup
      
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        doc    1.9Mb
        libs   2.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp’ ‘readr’
      All declared Imports should be used.
    ```

# ggstatsplot

<details>

* Version: 0.6.6
* GitHub: https://github.com/IndrajeetPatil/ggstatsplot
* Source code: https://github.com/cran/ggstatsplot
* Date/Publication: 2020-12-03 10:40:02 UTC
* Number of recursive dependencies: 201

Run `cloud_details(, "ggstatsplot")` for more info

</details>

## Newly broken

*   checking whether package ‘ggstatsplot’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggstatsplot/new/ggstatsplot.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggstatsplot’ ...
** package ‘ggstatsplot’ successfully unpacked and MD5 sums checked
** using staged installation
** R
Error in writeLines(tmp, con) : 
  Error writing to connection:  No space left on device
ERROR: unable to collate and parse R files for package ‘ggstatsplot’
* removing ‘/tmp/workdir/ggstatsplot/new/ggstatsplot.Rcheck/ggstatsplot’


```
### CRAN

```
* installing *source* package ‘ggstatsplot’ ...
** package ‘ggstatsplot’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (ggstatsplot)


```
# httk

<details>

* Version: 2.0.3
* GitHub: https://github.com/USEPA/CompTox-ExpoCast-httk
* Source code: https://github.com/cran/httk
* Date/Publication: 2020-09-25 11:00:03 UTC
* Number of recursive dependencies: 114

Run `cloud_details(, "httk")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘httk-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: solve_3comp
    > ### Title: Solve_3comp
    > ### Aliases: solve_3comp
    > ### Keywords: 3compartment Solve
    > 
    > ### ** Examples
    > 
    ...
    [104,]  1.06200  0.000e+00  8.257000 2.444e+00  8.79400    1.484e+02 3.633000
    [105,]  1.07300  0.000e+00  8.201000 2.427e+00  8.85500    1.494e+02 3.658000
    [106,]  1.08300  0.000e+00  8.145000 2.411e+00  8.91600    1.503e+02 3.684000
    [107,]  1.09400  0.000e+00  8.089000 2.394e+00  8.97700    1.513e+02 3.709000
    [108,]  1.10400  0.000e+00  8.034000 2.378e+00  9.03700    1.522e+02 3.734000
    [109,]  1.11500  0.000e+00  7.979000 2.362e+00  9.09700    1.532e+02 3.758000
    [110,]  1.12500  0.000e+00  7.925000 2.346e+00  9.15600    1.541e+02 3.783000
    [111,]  1.13500  0.000e+00  7.871000 2.330e+00  9.21500    1.550e+02 3.807000
    [112,]  1.14600  0.000e+00  7.817000 2.314e+00  9.27300    1.559e+02 3.831000
    [113,]  1.15600  0.000e+00  7.764000 2.298e+00  9.33200    1.568e+0unable to create ‘/tmp/workdir/httk/new/httk.Rcheck/tests’
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.9Mb
      sub-directories of 1Mb or more:
        data   8.3Mb
        doc    2.1Mb
    ```

# iNZightRegression

<details>

* Version: 1.3.0
* GitHub: https://github.com/iNZightVIT/iNZightRegression
* Source code: https://github.com/cran/iNZightRegression
* Date/Publication: 2020-11-27 10:40:02 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "iNZightRegression")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘iNZightRegression-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: inzplot
    > ### Title: inzplot method
    > ### Aliases: inzplot inzplot.lm
    > 
    > ### ** Examples
    > 
    > iris_fit <- lm(Sepal.Width ~ Sepal.Length, data = iris)
    > inzplot(iris_fit)
    > inzplot(iris_fit, which = "residual", show.bootstraps = FALSE)
    Error in grid.newpage() : write failed
    Calls: inzplot -> inzplot.lm
    Execution halted
    Error: internal read error in PDF_endpage
    Fatal error: error during cleanup
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    Fatal error: cannot create 'R_TempDir'
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘iNZightTools’ ‘magrittr’
      All declared Imports should be used.
    ```

# joineRML

<details>

* Version: 0.4.4
* GitHub: https://github.com/graemeleehickey/joineRML
* Source code: https://github.com/cran/joineRML
* Date/Publication: 2020-04-09 09:30:02 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "joineRML")` for more info

</details>

## Newly broken

*   checking whether package ‘joineRML’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/joineRML/new/joineRML.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 11.2Mb
      sub-directories of 1Mb or more:
        data   1.3Mb
        libs   8.6Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘joineRML’ ...
** package ‘joineRML’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c expW.cpp -o expW.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c gammaUpdate.cpp -o gammaUpdate.o
gammaUpdate.cpp:139:1: fatal error: error writing to /tmp/cc2Zy7Ld.s: No space left on device
  139 | }
      | ^
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:181: gammaUpdate.o] Error 1
ERROR: compilation failed for package ‘joineRML’
* removing ‘/tmp/workdir/joineRML/new/joineRML.Rcheck/joineRML’


```
### CRAN

```
* installing *source* package ‘joineRML’ ...
** package ‘joineRML’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c expW.cpp -o expW.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c gammaUpdate.cpp -o gammaUpdate.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c gammaUpdate_approx.cpp -o gammaUpdate_approx.o
gcc -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c init.c -o init.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fpic  -g -O2  -c lambdaUpdate.cpp -o lambdaUpdate.o
...
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (joineRML)


```
# jstable

<details>

* Version: 1.0.1
* GitHub: https://github.com/jinseob2kim/jstable
* Source code: https://github.com/cran/jstable
* Date/Publication: 2020-11-26 14:40:03 UTC
* Number of recursive dependencies: 111

Run `cloud_details(, "jstable")` for more info

</details>

## Newly broken

*   checking Rd files ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking Rd metadata ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    Fatal error: cannot create 'R_TempDir'
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking R code for possible problems ... NOTE
    ```
    Fatal error: cannot create 'R_TempDir'
    
    Fatal error: cannot create 'R_TempDir'
    
    Found the following possibly unsafe calls:
    Fatal error: cannot create 'R_TempDir'
    
    Fatal error: cannot create 'R_TempDir'
    ```

# lipidomeR

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/lipidomeR
* Date/Publication: 2020-03-15 10:20:02 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "lipidomeR")` for more info

</details>

## Newly broken

*   checking R files for syntax errors ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking replacement functions ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    The argument of a replacement function which corresponds to the right
    hand side must be named ‘value’.
    ```

*   checking foreign function calls ... NOTE
    ```
    Fatal error: cannot create 'R_TempDir'
    See chapter ‘System and foreign language interfaces’ in the ‘Writing R
    Extensions’ manual.
    ```

# metawho

<details>

* Version: 0.2.0
* GitHub: https://github.com/ShixiangWang/metawho
* Source code: https://github.com/cran/metawho
* Date/Publication: 2019-12-06 16:00:02 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "metawho")` for more info

</details>

## Newly broken

*   checking DESCRIPTION meta-information ... ERROR
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

# mlr3db

<details>

* Version: 0.3.0
* GitHub: https://github.com/mlr-org/mlr3db
* Source code: https://github.com/cran/mlr3db
* Date/Publication: 2020-12-16 14:00:03 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "mlr3db")` for more info

</details>

## Newly broken

*   checking whether package ‘mlr3db’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/mlr3db/new/mlr3db.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > if (requireNamespace("testthat", quietly = TRUE)) {
      +   library(testthat)
      +   library(mlr3db)
      +   test_check("mlr3db")
      + }
      Loading required package: mlr3
      Loading required namespace: future
      Killed
    ```

## Installation

### Devel

```
* installing *source* package ‘mlr3db’ ...
** package ‘mlr3db’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in lazyLoadDBinsertValue(data, datafile, ascii, compress, envhook) : 
  write failed
Calls: <Anonymous> ... lazyLoadDBinsertVariable -> <Anonymous> -> lazyLoadDBinsertValue
Execution halted
ERROR: lazy loading failed for package ‘mlr3db’
* removing ‘/tmp/workdir/mlr3db/new/mlr3db.Rcheck/mlr3db’


```
### CRAN

```
* installing *source* package ‘mlr3db’ ...
** package ‘mlr3db’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (mlr3db)


```
# mosaicData

<details>

* Version: 0.20.1
* GitHub: https://github.com/ProjectMOSAIC/mosaicData
* Source code: https://github.com/cran/mosaicData
* Date/Publication: 2020-09-13 14:50:02 UTC
* Number of recursive dependencies: 95

Run `cloud_details(, "mosaicData")` for more info

</details>

## Newly broken

*   checking whether package ‘mosaicData’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/mosaicData/new/mosaicData.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7 marked UTF-8 strings
    ```

## Installation

### Devel

```
* installing *source* package ‘mosaicData’ ...
files ‘R/datasetsDoc.R’, ‘man/figures/README-example-1.png’ have the wrong MD5 checksums
** using staged installation
** R
Error in parse(outFile) : 
  /tmp/workdir/mosaicData/new/mosaicData.Rcheck/00_pkg_src/mosaicData/R/datasetsDoc.R:1:12: unexpected symbol
1: king/using drugs
               ^
ERROR: unable to collate and parse R files for package ‘mosaicData’
* removing ‘/tmp/workdir/mosaicData/new/mosaicData.Rcheck/mosaicData’


```
### CRAN

```
* installing *source* package ‘mosaicData’ ...
** package ‘mosaicData’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (mosaicData)


```
# mudata2

<details>

* Version: 1.1.2
* GitHub: https://github.com/paleolimbot/mudata2
* Source code: https://github.com/cran/mudata2
* Date/Publication: 2020-03-20 20:20:03 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "mudata2")` for more info

</details>

## Newly broken

*   checking whether the namespace can be loaded with stated dependencies ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking whether the namespace can be unloaded cleanly ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking replacement functions ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    The argument of a replacement function which corresponds to the right
    hand side must be named ‘value’.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

# mvGPS

<details>

* Version: 1.0.2
* GitHub: https://github.com/williazo/mvGPS
* Source code: https://github.com/cran/mvGPS
* Date/Publication: 2020-09-17 09:10:03 UTC
* Number of recursive dependencies: 112

Run `cloud_details(, "mvGPS")` for more info

</details>

## Newly broken

*   checking whether package ‘mvGPS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/mvGPS/new/mvGPS.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      NA
    ```

*   checking Rd \usage sections ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rdpack’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘mvGPS’ ...
** package ‘mvGPS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
Warning in close.connection(con) :
  Problem closing connection:  No space left on device
** testing if installed package can be loaded from temporary location
Fatal error: cannot create 'R_TempDir'
ERROR: loading failed
* removing ‘/tmp/workdir/mvGPS/new/mvGPS.Rcheck/mvGPS’


```
### CRAN

```
* installing *source* package ‘mvGPS’ ...
** package ‘mvGPS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (mvGPS)


```
# nanostringr

<details>

* Version: 0.1.4
* GitHub: https://github.com/OVCARE/nanostringr
* Source code: https://github.com/cran/nanostringr
* Date/Publication: 2019-05-09 19:20:03 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "nanostringr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
    unable to create ‘/tmp/workdir/nanostringr/new/nanostringr.Rcheck/tests’
    ```

*   checking for unstated dependencies in ‘tests’ ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

# nbTransmission

<details>

* Version: 1.1.1
* GitHub: https://github.com/sv1205/nbTransmission
* Source code: https://github.com/cran/nbTransmission
* Date/Publication: 2020-07-13 09:00:03 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "nbTransmission")` for more info

</details>

## Newly broken

*   checking whether package ‘nbTransmission’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/nbTransmission/new/nbTransmission.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘nbTransmission’ ...
** package ‘nbTransmission’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in lazyLoadDBinsertVariable(vars[i], from, datafile, ascii, compress,  : 
  write failed
Calls: <Anonymous> ... code2LazyLoadDB -> makeLazyLoadDB -> lazyLoadDBinsertVariable
Execution halted
ERROR: lazy loading failed for package ‘nbTransmission’
* removing ‘/tmp/workdir/nbTransmission/new/nbTransmission.Rcheck/nbTransmission’


```
### CRAN

```
* installing *source* package ‘nbTransmission’ ...
** package ‘nbTransmission’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (nbTransmission)


```
# ncappc

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/ncappc
* Date/Publication: 2018-08-24 20:30:03 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "ncappc")` for more info

</details>

## Newly broken

*   checking whether package ‘ncappc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ncappc/new/ncappc.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘bookdown’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘ncappc’ ...
** package ‘ncappc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in lazyLoadDBinsertVariable(vars[i], from, datafile, ascii, compress,  : 
  write failed
Calls: <Anonymous> ... code2LazyLoadDB -> makeLazyLoadDB -> lazyLoadDBinsertVariable
Execution halted
ERROR: lazy loading failed for package ‘ncappc’
* removing ‘/tmp/workdir/ncappc/new/ncappc.Rcheck/ncappc’


```
### CRAN

```
* installing *source* package ‘ncappc’ ...
** package ‘ncappc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ncappc)


```
# nflfastR

<details>

* Version: 3.2.0
* GitHub: https://github.com/mrcaseb/nflfastR
* Source code: https://github.com/cran/nflfastR
* Date/Publication: 2020-11-20 15:10:02 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "nflfastR")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        R   4.1Mb
    ```

# nlshelper

<details>

* Version: 0.2
* GitHub: NA
* Source code: https://github.com/cran/nlshelper
* Date/Publication: 2017-04-03 20:19:13 UTC
* Number of recursive dependencies: 41

Run `cloud_details(, "nlshelper")` for more info

</details>

## Newly broken

*   checking DESCRIPTION meta-information ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
     ERROR
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking DESCRIPTION meta-information ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
     ERROR
    Fatal error: cannot create 'R_TempDir'
    ```

# observer

<details>

* Version: 0.1.2
* GitHub: https://github.com/paulponcet/observer
* Source code: https://github.com/cran/observer
* Date/Publication: 2017-01-29 22:03:43
* Number of recursive dependencies: 70

Run `cloud_details(, "observer")` for more info

</details>

## Newly broken

*   checking for unstated dependencies in examples ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘ensurer’, ‘validate’
    ```

# OncoBayes2

<details>

* Version: 0.6-5
* GitHub: NA
* Source code: https://github.com/cran/OncoBayes2
* Date/Publication: 2020-05-07 14:50:02 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "OncoBayes2")` for more info

</details>

## Newly broken

*   checking whether package ‘OncoBayes2’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/OncoBayes2/new/OncoBayes2.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 59.8Mb
      sub-directories of 1Mb or more:
        libs  58.7Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘bayesplot’ ‘ggplot2’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

## Installation

### Devel

```
* installing *source* package ‘OncoBayes2’ ...
** package ‘OncoBayes2’ successfully unpacked and MD5 sums checked
** using staged installation
DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: current / base
 Positive values rounded down, negative values rounded up or down in platform-dependent way.
Info: integer division implicitly rounds to integer. Found int division: left_ind + right_ind / 2
 Positive values rounded down, negative values rounded up or down in platform-dependent way.

** libs
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_blrm_exnex_namespace::model_blrm_exnex; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_blrm_exnex.o] Error 1
ERROR: compilation failed for package ‘OncoBayes2’
* removing ‘/tmp/workdir/OncoBayes2/new/OncoBayes2.Rcheck/OncoBayes2’


```
### CRAN

```
* installing *source* package ‘OncoBayes2’ ...
** package ‘OncoBayes2’ successfully unpacked and MD5 sums checked
** using staged installation
DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: current / base
 Positive values rounded down, negative values rounded up or down in platform-dependent way.
Info: integer division implicitly rounds to integer. Found int division: left_ind + right_ind / 2
 Positive values rounded down, negative values rounded up or down in platform-dependent way.

** libs
...

** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (OncoBayes2)


```
# openadds

<details>

* Version: 0.2.0
* GitHub: https://github.com/sckott/openadds
* Source code: https://github.com/cran/openadds
* Date/Publication: 2017-01-03 06:26:31
* Number of recursive dependencies: 97

Run `cloud_details(, "openadds")` for more info

</details>

## Newly broken

*   checking Rd files ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking Rd metadata ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking Rd \usage sections ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    Fatal error: cannot create 'R_TempDir'
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking Rd contents ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking foreign function calls ... NOTE
    ```
    Fatal error: cannot create 'R_TempDir'
    See chapter ‘System and foreign language interfaces’ in the ‘Writing R
    Extensions’ manual.
    ```

# oxcgrt

<details>

* Version: 0.1.0
* GitHub: https://github.com/como-ph/oxcgrt
* Source code: https://github.com/cran/oxcgrt
* Date/Publication: 2020-11-27 11:30:02 UTC
* Number of recursive dependencies: 68

Run `cloud_details(, "oxcgrt")` for more info

</details>

## Newly broken

*   checking whether package ‘oxcgrt’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/oxcgrt/new/oxcgrt.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘calculate.Rmd’
      ...
    > proc.time()
       user  system elapsed 
      1.353   2.566   0.367 
    
    ... incomplete output.  Crash?
    
      ‘calculate.Rmd’ using ‘UTF-8’... failed to complete the test
      ‘retrieve.Rmd’ using ‘UTF-8’... OK
    ```

## Installation

### Devel

```
* installing *source* package ‘oxcgrt’ ...
files ‘man/calculate_index.Rd’, ‘man/figures/logo.png’, ‘man/figures/oxcgrt.png’, ‘man/get_data.Rd’, ‘man/get_json.Rd’, ‘man/oxcgrt.Rd’ have the wrong MD5 checksums
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
Error : /tmp/workdir/oxcgrt/new/oxcgrt.Rcheck/00_pkg_src/oxcgrt/man/calculate_index.Rd: Sections \title, and \name must exist and be unique in Rd files
ERROR: installing Rd objects failed for package ‘oxcgrt’
* removing ‘/tmp/workdir/oxcgrt/new/oxcgrt.Rcheck/oxcgrt’


```
### CRAN

```
* installing *source* package ‘oxcgrt’ ...
** package ‘oxcgrt’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (oxcgrt)


```
# PAMmisc

<details>

* Version: 1.6.5
* GitHub: NA
* Source code: https://github.com/cran/PAMmisc
* Date/Publication: 2020-10-28 21:10:17 UTC
* Number of recursive dependencies: 113

Run `cloud_details(, "PAMmisc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running massageExamples to create ‘PAMmisc-Ex.R’ failed
    
     NONE
    ```

*   checking tests ... ERROR
    ```
    unable to create ‘/tmp/workdir/PAMmisc/new/PAMmisc.Rcheck/tests’
    ```

*   checking Rd contents ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking for unstated dependencies in examples ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking data for ASCII and uncompressed saves ... WARNING
    ```
      Fatal error: cannot create 'R_TempDir'
    ```

*   checking for unstated dependencies in ‘tests’ ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Fatal error: cannot create 'R_TempDir'
    ```

# quanteda

<details>

* Version: 2.1.2
* GitHub: https://github.com/quanteda/quanteda
* Source code: https://github.com/cran/quanteda
* Date/Publication: 2020-09-23 04:10:03 UTC
* Number of recursive dependencies: 154

Run `cloud_details(, "quanteda")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is 28.5Mb
      sub-directories of 1Mb or more:
        R      1.1Mb
        libs  25.1Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 3 marked UTF-8 strings
    ```

## Newly fixed

*   checking whether package ‘quanteda’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/quanteda/old/quanteda.Rcheck/00install.out’ for details.
    ```

# randomForestExplainer

<details>

* Version: 0.10.1
* GitHub: https://github.com/ModelOriented/randomForestExplainer
* Source code: https://github.com/cran/randomForestExplainer
* Date/Publication: 2020-07-11 20:30:02 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "randomForestExplainer")` for more info

</details>

## Newly broken

*   checking whether package ‘randomForestExplainer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/randomForestExplainer/new/randomForestExplainer.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘randomForestExplainer’ ...
** package ‘randomForestExplainer’ successfully unpacked and MD5 sums checked
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
Error in as.list.environment(base::getNamespace("randomForestExplainer"),  : 
  lazy-load database '/tmp/workdir/randomForestExplainer/new/randomForestExplainer.Rcheck/randomForestExplainer/R/randomForestExplainer.rdb' is corrupt
Calls: <Anonymous> ... withCallingHandlers -> <Anonymous> -> <Anonymous> -> as.list.environment
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/randomForestExplainer/new/randomForestExplainer.Rcheck/randomForestExplainer’


```
### CRAN

```
* installing *source* package ‘randomForestExplainer’ ...
** package ‘randomForestExplainer’ successfully unpacked and MD5 sums checked
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
* DONE (randomForestExplainer)


```
# RBesT

<details>

* Version: 1.6-1
* GitHub: NA
* Source code: https://github.com/cran/RBesT
* Date/Publication: 2020-05-28 09:40:02 UTC
* Number of recursive dependencies: 127

Run `cloud_details(, "RBesT")` for more info

</details>

## Newly broken

*   checking whether package ‘RBesT’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/RBesT/new/RBesT.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 56.8Mb
      sub-directories of 1Mb or more:
        doc    1.9Mb
        libs  54.0Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

## Installation

### Devel

```
* installing *source* package ‘RBesT’ ...
** package ‘RBesT’ successfully unpacked and MD5 sums checked
** using staged installation
DIAGNOSTIC(S) FROM PARSER:
Info:
Left-hand side of sampling statement (~) may contain a non-linear transform of a parameter or local variable.
If it does, you need to include a target += statement with the log absolute determinant of the Jacobian of the transform.
Left-hand-side of sampling statement:
    beta ~ normal(...)
Info:
...
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_gMAP_namespace::model_gMAP; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
stanExports_gMAP.cc:32:1: fatal error: error writing to /tmp/ccPnTX7t.s: No space left on device
   32 | }
      | ^
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_gMAP.o] Error 1
ERROR: compilation failed for package ‘RBesT’
* removing ‘/tmp/workdir/RBesT/new/RBesT.Rcheck/RBesT’


```
### CRAN

```
* installing *source* package ‘RBesT’ ...
** package ‘RBesT’ successfully unpacked and MD5 sums checked
** using staged installation
DIAGNOSTIC(S) FROM PARSER:
Info:
Left-hand side of sampling statement (~) may contain a non-linear transform of a parameter or local variable.
If it does, you need to include a target += statement with the log absolute determinant of the Jacobian of the transform.
Left-hand-side of sampling statement:
    beta ~ normal(...)
Info:
...

** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (RBesT)


```
# rerddapXtracto

<details>

* Version: 1.0.2
* GitHub: https://github.com/rmendels/rerddapXtracto
* Source code: https://github.com/cran/rerddapXtracto
* Date/Publication: 2020-11-03 22:40:02 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "rerddapXtracto")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rerddapXtracto-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rxtracto
    > ### Title: Extract environmental data along a trajectory from an 'ERDDAP'
    > ###   server using 'rerddap'.
    > ### Aliases: rxtracto
    > 
    > ### ** Examples
    > 
    ...
    > tcoord <- c('2006-01-16')
    > xlen <- 0.01
    > ylen <- 0.01
    > extract <- rxtracto(dataInfo, parameter = parameter, xcoord = xcoord,
    +                     ycoord = ycoord, tcoord= tcoord,
    +                     xlen = xlen, ylen = ylen)
    Error in req_time_index[i] <- which.min(abs(udtTime - temp_time)) : 
      replacement has length zero
    Calls: rxtracto
    Execution halted
    ```

# RNeXML

<details>

* Version: 2.4.5
* GitHub: https://github.com/ropensci/RNeXML
* Source code: https://github.com/cran/RNeXML
* Date/Publication: 2020-06-18 18:40:02 UTC
* Number of recursive dependencies: 138

Run `cloud_details(, "RNeXML")` for more info

</details>

## Newly broken

*   checking whether package ‘RNeXML’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/RNeXML/new/RNeXML.Rcheck/00install.out’ for details.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘knitcitations’
    ```

## Installation

### Devel

```
* installing *source* package ‘RNeXML’ ...
** package ‘RNeXML’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
Error: /tmp/workdir/RNeXML/new/RNeXML.Rcheck/00_pkg_src/RNeXML/man/Annotated-class.Rd: Sections \title, and \name must exist and be unique in Rd files
* removing ‘/tmp/workdir/RNeXML/new/RNeXML.Rcheck/RNeXML’


```
### CRAN

```
* installing *source* package ‘RNeXML’ ...
** package ‘RNeXML’ successfully unpacked and MD5 sums checked
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
* DONE (RNeXML)


```
# roahd

<details>

* Version: 1.4.2
* GitHub: https://github.com/astamm/roahd
* Source code: https://github.com/cran/roahd
* Date/Publication: 2020-08-24 08:30:02 UTC
* Number of recursive dependencies: 59

Run `cloud_details(, "roahd")` for more info

</details>

## Newly broken

*   checking for unstated dependencies in examples ... WARNING
    ```
    Warning in close.connection(con) :
      Problem closing connection:  No space left on device
    Warning in close.connection(con) :
      Problem closing connection:  No space left on device
    Warning in close.connection(con) :
      Problem closing connection:  No space left on device
    Warning in close.connection(con) :
      Problem closing connection:  No space left on device
    Warning in close.connection(con) :
      Problem closing connection:  No space left on device
    ...
    Warning in close.connection(con) :
      Problem closing connection:  No space left on device
    Warning in close.connection(con) :
      Problem closing connection:  No space left on device
    Warning in close.connection(con) :
      Problem closing connection:  No space left on device
    Warning in close.connection(con) :
      Problem closing connection:  No space left on device
    Warning in close.connection(con) :
      Problem closing connection:  No space left on device
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        data   2.9Mb
        doc    1.7Mb
    ```

# RTL

<details>

* Version: 0.1.5
* GitHub: https://github.com/risktoollib/RTL
* Source code: https://github.com/cran/RTL
* Date/Publication: 2020-11-11 06:20:02 UTC
* Number of recursive dependencies: 151

Run `cloud_details(, "RTL")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
    unable to create ‘/tmp/workdir/RTL/new/RTL.Rcheck/tests’
    ```

*   checking for unstated dependencies in ‘tests’ ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        data   5.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘quantmod’ ‘sp’ ‘timetk’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 688 marked UTF-8 strings
    ```

# spiR

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/spiR
* Date/Publication: 2020-01-23 16:20:02 UTC
* Number of recursive dependencies: 43

Run `cloud_details(, "spiR")` for more info

</details>

## Newly broken

*   checking for missing documentation entries ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    All user-level objects in a package should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# TextMiningGUI

<details>

* Version: 0.2
* GitHub: NA
* Source code: https://github.com/cran/TextMiningGUI
* Date/Publication: 2020-12-07 17:40:06 UTC
* Number of recursive dependencies: 158

Run `cloud_details(, "TextMiningGUI")` for more info

</details>

## Newly broken

*   checking whether package ‘TextMiningGUI’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/TextMiningGUI/new/TextMiningGUI.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking examples ... ERROR
    ```
    Running examples in ‘TextMiningGUI-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: TextMiningGUI
    > ### Title: TextMiningGUI
    > ### Aliases: TextMiningGUI
    > 
    > ### ** Examples
    > 
    > library(TextMiningGUI)
    > if(TextMiningGUI()){}
    Error in structure(.External(.C_dotTclObjv, objv), class = "tclObj") : 
      [tcl] invalid command name "toplevel".
    Calls: TextMiningGUI ... tktoplevel -> tkwidget -> tcl -> .Tcl.objv -> structure
    Execution halted
    ```

*   checking whether package ‘TextMiningGUI’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: loading Rplot failed
    See ‘/tmp/workdir/TextMiningGUI/old/TextMiningGUI.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘TextMiningGUI’ ...
** package ‘TextMiningGUI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in lazyLoadDBinsertVariable(vars[i], from, datafile, ascii, compress,  : 
  write failed
Calls: <Anonymous> ... code2LazyLoadDB -> makeLazyLoadDB -> lazyLoadDBinsertVariable
Execution halted
ERROR: lazy loading failed for package ‘TextMiningGUI’
* removing ‘/tmp/workdir/TextMiningGUI/new/TextMiningGUI.Rcheck/TextMiningGUI’


```
### CRAN

```
* installing *source* package ‘TextMiningGUI’ ...
** package ‘TextMiningGUI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) : couldn't connect to display ":109"
** help
...
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
Warning in fun(libname, pkgname) : couldn't connect to display ":109"
Warning: loading Rplot failed
** testing if installed package can be loaded from final location
Warning in fun(libname, pkgname) : couldn't connect to display ":109"
Warning: loading Rplot failed
** testing if installed package keeps a record of temporary installation path
* DONE (TextMiningGUI)


```
