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
# bayesdfa

<details>

* Version: 0.1.6
* GitHub: https://github.com/fate-ewi/bayesdfa
* Source code: https://github.com/cran/bayesdfa
* Date/Publication: 2020-09-20 22:30:02 UTC
* Number of recursive dependencies: 82

Run `cloud_details(, "bayesdfa")` for more info

</details>

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
** package ‘bayesdfa’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/opt/R/4.0.3/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/corr.stan
Wrote C++ file "stan_files/corr.cc"


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c stan_files/corr.cc -o stan_files/corr.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:392,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_dfa_namespace::model_dfa; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stan_files/dfa.o] Error 1
rm stan_files/dfa.cc stan_files/corr.cc stan_files/hmm_gaussian.cc
ERROR: compilation failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/new/bayesdfa.Rcheck/bayesdfa’


```
### CRAN

```
* installing *source* package ‘bayesdfa’ ...
** package ‘bayesdfa’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/opt/R/4.0.3/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/corr.stan
Wrote C++ file "stan_files/corr.cc"


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c stan_files/corr.cc -o stan_files/corr.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:392,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_dfa_namespace::model_dfa; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stan_files/dfa.o] Error 1
rm stan_files/dfa.cc stan_files/corr.cc stan_files/hmm_gaussian.cc
ERROR: compilation failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/old/bayesdfa.Rcheck/bayesdfa’


```
# benthos

<details>

* Version: 1.3-6
* GitHub: NA
* Source code: https://github.com/cran/benthos
* Date/Publication: 2019-03-17 22:43:20 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "benthos")` for more info

</details>

## In both

*   checking whether package ‘benthos’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/benthos/new/benthos.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘benthos’ ...
** package ‘benthos’ successfully unpacked and MD5 sums checked
** using staged installation
Warning in file(file, if (append) "a" else "w") :
  cannot open file '/tmp/workdir/benthos/new/benthos.Rcheck/00LOCK-benthos/00new/benthos/DESCRIPTION': Not a directory
Error in file(file, if (append) "a" else "w") : 
  cannot open the connection
ERROR: installing package DESCRIPTION failed for package ‘benthos’
* removing ‘/tmp/workdir/benthos/new/benthos.Rcheck/benthos’


```
### CRAN

```
* installing *source* package ‘benthos’ ...
** package ‘benthos’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
...
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
Error in as.list.environment(base::getNamespace("benthos"), all.names = TRUE) : 
  lazy-load database '/tmp/workdir/benthos/old/benthos.Rcheck/benthos/R/benthos.rdb' is corrupt
Calls: <Anonymous> ... withCallingHandlers -> <Anonymous> -> <Anonymous> -> as.list.environment
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/benthos/old/benthos.Rcheck/benthos’


```
# bestNormalize

<details>

* Version: 1.6.1
* GitHub: https://github.com/petersonR/bestNormalize
* Source code: https://github.com/cran/bestNormalize
* Date/Publication: 2020-06-08 19:40:02 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "bestNormalize")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/bestNormalize/new/bestNormalize.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘bestNormalize/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘bestNormalize’ version ‘1.6.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
  Running ‘testthat-methods-step.R’
  Running ‘testthat-minor-transforms.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘bestNormalize.Rmd’ using ‘UTF-8’... OK
  ‘customization.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 WARNING, 1 NOTE





```
### CRAN

```






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
# bmlm

<details>

* Version: 1.3.11
* GitHub: https://github.com/mvuorre/bmlm
* Source code: https://github.com/cran/bmlm
* Date/Publication: 2019-02-21 21:30:03 UTC
* Number of recursive dependencies: 124

Run `cloud_details(, "bmlm")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# butcher

<details>

* Version: 0.1.2
* GitHub: https://github.com/tidymodels/butcher
* Source code: https://github.com/cran/butcher
* Date/Publication: 2020-01-23 22:40:02 UTC
* Number of recursive dependencies: 188

Run `cloud_details(, "butcher")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/butcher/new/butcher.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘butcher/DESCRIPTION’ ... OK
* this is package ‘butcher’ version ‘0.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘adding-models-to-butcher.Rmd’ using ‘UTF-8’... OK
  ‘available-axe-methods.Rmd’ using ‘UTF-8’... OK
  ‘butcher.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/butcher/old/butcher.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘butcher/DESCRIPTION’ ... OK
* this is package ‘butcher’ version ‘0.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘adding-models-to-butcher.Rmd’ using ‘UTF-8’... OK
  ‘available-axe-methods.Rmd’ using ‘UTF-8’... OK
  ‘butcher.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 2 NOTEs





```
# cartools

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/cartools
* Date/Publication: 2018-08-20 09:00:03 UTC
* Number of recursive dependencies: 153

Run `cloud_details(, "cartools")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/cartools/new/cartools.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘cartools/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘cartools’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘cartools.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
### CRAN

```






```
# casino

<details>

* Version: 0.1.0
* GitHub: https://github.com/anthonypileggi/casino
* Source code: https://github.com/cran/casino
* Date/Publication: 2019-01-17 17:40:03 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "casino")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/casino/new/casino.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘casino/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘casino’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘casino’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/casino/new/casino.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# catfun

<details>

* Version: 0.1.4
* GitHub: NA
* Source code: https://github.com/cran/catfun
* Date/Publication: 2019-06-14 14:10:03 UTC
* Number of recursive dependencies: 97

Run `cloud_details(, "catfun")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/catfun/new/catfun.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘catfun/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘catfun’ version ‘0.1.4’
* package encoding: UTF-8
* checking package namespace information ... OK
...
   13.                   └─base:::doWithOneRestart(return(expr), restart)
  
  ══ testthat results  ═══════════════════════════════════════════════════════════
  ERROR (test-prop_test.R:6:3): prop_test agrees with prop.test/BinomCI/SAS for single proportion
  
  [ FAIL 1 | WARN 0 | SKIP 0 | PASS 10 ]
  Error: Test failures
  Execution halted
* DONE
Status: 2 ERRORs, 1 NOTE





```
### CRAN

```






```
# CausalImpact

<details>

* Version: 1.2.4
* GitHub: NA
* Source code: https://github.com/cran/CausalImpact
* Date/Publication: 2020-01-08 23:02:47 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "CausalImpact")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/CausalImpact/new/CausalImpact.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘CausalImpact/DESCRIPTION’ ... OK
* this is package ‘CausalImpact’ version ‘1.2.4’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'bsts', 'Boom'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/CausalImpact/old/CausalImpact.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘CausalImpact/DESCRIPTION’ ... OK
* this is package ‘CausalImpact’ version ‘1.2.4’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'bsts', 'Boom'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# CB2

<details>

* Version: 1.3.4
* GitHub: NA
* Source code: https://github.com/cran/CB2
* Date/Publication: 2020-07-24 09:42:24 UTC
* Number of recursive dependencies: 104

Run `cloud_details(, "CB2")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/CB2/new/CB2.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘CB2/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘CB2’ version ‘1.3.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘metap’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/CB2/old/CB2.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘CB2/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘CB2’ version ‘1.3.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘metap’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# cbar

<details>

* Version: 0.1.3
* GitHub: https://github.com/zedoul/cbar
* Source code: https://github.com/cran/cbar
* Date/Publication: 2017-10-24 13:20:22 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "cbar")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/cbar/new/cbar.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘cbar/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘cbar’ version ‘0.1.3’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'Boom', 'bsts'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/cbar/old/cbar.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘cbar/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘cbar’ version ‘0.1.3’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'Boom', 'bsts'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# ccostr

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/ccostr
* Date/Publication: 2019-09-09 10:10:02 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "ccostr")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/ccostr/old/ccostr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ccostr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘ccostr’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘ccostr’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/ccostr/old/ccostr.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR, 1 NOTE





```
# cdom

<details>

* Version: 0.1.0
* GitHub: https://github.com/PMassicotte/cdom
* Source code: https://github.com/cran/cdom
* Date/Publication: 2016-03-04 08:39:29
* Number of recursive dependencies: 72

Run `cloud_details(, "cdom")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/cdom/old/cdom.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘cdom/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘cdom’ version ‘0.1.0’
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘cdom’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/cdom/old/cdom.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
# censusr

<details>

* Version: 0.0.4
* GitHub: https://github.com/transportfoundry/censusr
* Source code: https://github.com/cran/censusr
* Date/Publication: 2018-01-25 16:40:14 UTC
* Number of recursive dependencies: 40

Run `cloud_details(, "censusr")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/censusr/old/censusr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘censusr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘censusr’ version ‘0.0.4’
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘censusr.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK





```
# climate

<details>

* Version: 0.9.8
* GitHub: https://github.com/bczernecki/climate
* Source code: https://github.com/cran/climate
* Date/Publication: 2020-08-03 14:10:06 UTC
* Number of recursive dependencies: 59

Run `cloud_details(, "climate")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/climate/old/climate.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘climate/DESCRIPTION’ ... OK
* this is package ‘climate’ version ‘0.9.8’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
...
except perhaps in comments.
Use \uxxxx escapes for other characters.
* checking R files for syntax errors ... WARNING
Fatal error: cannot create 'R_TempDir'
* checking whether the package can be loaded ... ERROR
Loading this package had a fatal error status code 2
Loading log:
Fatal error: cannot create 'R_TempDir'
* DONE
Status: 1 ERROR, 3 WARNINGs





```
# codified

<details>

* Version: 0.2.0
* GitHub: https://github.com/OuhscBbmc/codified
* Source code: https://github.com/cran/codified
* Date/Publication: 2018-09-30 16:10:02 UTC
* Number of recursive dependencies: 105

Run `cloud_details(, "codified")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/codified/new/codified.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘codified/DESCRIPTION’ ... OK
* this is package ‘codified’ version ‘0.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
...
  [ FAIL 1 | WARN 1 | SKIP 0 | PASS 5 ]
  Error: Test failures
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘nih-enrollment-html.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 2 ERRORs, 1 NOTE





```
### CRAN

```






```
# compareDF

<details>

* Version: 2.3.0
* GitHub: NA
* Source code: https://github.com/cran/compareDF
* Date/Publication: 2020-08-26 12:50:11 UTC
* Number of recursive dependencies: 62

Run `cloud_details(, "compareDF")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# compstatr

<details>

* Version: 0.2.1
* GitHub: https://github.com/slu-openGIS/compstatr
* Source code: https://github.com/cran/compstatr
* Date/Publication: 2020-05-14 17:30:08 UTC
* Number of recursive dependencies: 80

Run `cloud_details(, "compstatr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/compstatr/new/compstatr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘compstatr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘compstatr’ version ‘0.2.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘compstatr.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK





```
### CRAN

```






```
# concurve

<details>

* Version: 2.7.7
* GitHub: https://github.com/zadrafi/concurve
* Source code: https://github.com/cran/concurve
* Date/Publication: 2020-10-12 17:10:06 UTC
* Number of recursive dependencies: 263

Run `cloud_details(, "concurve")` for more info

</details>

## In both

*   checking whether package ‘concurve’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/concurve/new/concurve.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘carData’
    ```

## Installation

### Devel

```
* installing *source* package ‘concurve’ ...
** package ‘concurve’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘generics’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘concurve’
* removing ‘/tmp/workdir/concurve/new/concurve.Rcheck/concurve’


```
### CRAN

```
* installing *source* package ‘concurve’ ...
** package ‘concurve’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘generics’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘concurve’
* removing ‘/tmp/workdir/concurve/old/concurve.Rcheck/concurve’


```
# contrast

<details>

* Version: 0.22
* GitHub: https://github.com/topepo/contrast
* Source code: https://github.com/cran/contrast
* Date/Publication: 2020-03-19 14:10:02 UTC
* Number of recursive dependencies: 114

Run `cloud_details(, "contrast")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# corrcoverage

<details>

* Version: 1.2.1
* GitHub: https://github.com/annahutch/corrcoverage
* Source code: https://github.com/cran/corrcoverage
* Date/Publication: 2019-12-06 23:20:12 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "corrcoverage")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/corrcoverage/new/corrcoverage.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘corrcoverage/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘corrcoverage’ version ‘1.2.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘New-Credible-Set.Rmd’ using ‘UTF-8’... OK
  ‘Useful-Info.Rmd’ using ‘UTF-8’... OK
  ‘a01_gwas_to_cs.Rmd’ using ‘UTF-8’... OK
  ‘corrected-coverage.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 ERROR, 2 NOTEs





```
### CRAN

```






```
# correlation

<details>

* Version: 0.5.0
* GitHub: https://github.com/easystats/correlation
* Source code: https://github.com/cran/correlation
* Date/Publication: 2020-12-02 07:40:19 UTC
* Number of recursive dependencies: 163

Run `cloud_details(, "correlation")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/correlation/new/correlation.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘correlation/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘correlation’ version ‘0.5.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ...





```
### CRAN

```






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
# countytimezones

<details>

* Version: 1.0.0
* GitHub: https://github.com/geanders/countytimezones
* Source code: https://github.com/cran/countytimezones
* Date/Publication: 2016-10-22 12:17:29
* Number of recursive dependencies: 123

Run `cloud_details(, "countytimezones")` for more info

</details>

## In both

*   checking whether package ‘countytimezones’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/countytimezones/new/countytimezones.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘choroplethr’
    ```

## Installation

### Devel

```
* installing *source* package ‘countytimezones’ ...
** package ‘countytimezones’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘lifecycle’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘countytimezones’
* removing ‘/tmp/workdir/countytimezones/new/countytimezones.Rcheck/countytimezones’


```
### CRAN

```
* installing *source* package ‘countytimezones’ ...
** package ‘countytimezones’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘lifecycle’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘countytimezones’
* removing ‘/tmp/workdir/countytimezones/old/countytimezones.Rcheck/countytimezones’


```
# cpss

<details>

* Version: 0.0.2
* GitHub: NA
* Source code: https://github.com/cran/cpss
* Date/Publication: 2020-11-23 09:40:15 UTC
* Number of recursive dependencies: 64

Run `cloud_details(, "cpss")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# cspp

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/cspp
* Date/Publication: 2020-10-27 14:10:11 UTC
* Number of recursive dependencies: 75

Run `cloud_details(, "cspp")` for more info

</details>

## In both

*   checking whether package ‘cspp’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/cspp/new/cspp.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘cspp’ ...
** package ‘cspp’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘R6’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘cspp’
* removing ‘/tmp/workdir/cspp/new/cspp.Rcheck/cspp’


```
### CRAN

```
* installing *source* package ‘cspp’ ...
** package ‘cspp’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘R6’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘cspp’
* removing ‘/tmp/workdir/cspp/old/cspp.Rcheck/cspp’


```
# customsteps

<details>

* Version: 0.7.1.0
* GitHub: https://github.com/smaakage85/customsteps
* Source code: https://github.com/cran/customsteps
* Date/Publication: 2018-12-03 10:12:42 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "customsteps")` for more info

</details>

## In both

*   checking whether package ‘customsteps’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/customsteps/new/customsteps.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘customsteps’ ...
** package ‘customsteps’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading

 *** caught segfault ***
address 0x8, cause 'memory not mapped'

...
14: namespaceImportFrom(ns, loadNamespace(j <- i[[1L]], c(lib.loc,     .libPaths()), versionCheck = vI[[j]]), i[[2L]], from = package)
15: loadNamespace(package = package, lib.loc = lib.loc, keep.source = keep.source,     keep.parse.data = keep.parse.data, partial = TRUE)
16: withCallingHandlers(expr, packageStartupMessage = function(c) tryInvokeRestart("muffleMessage"))
17: suppressPackageStartupMessages(loadNamespace(package = package,     lib.loc = lib.loc, keep.source = keep.source, keep.parse.data = keep.parse.data,     partial = TRUE))
18: code2LazyLoadDB(package, lib.loc = lib.loc, keep.source = keep.source,     keep.parse.data = keep.parse.data, compress = compress, set.install.dir = set.install.dir)
19: tools:::makeLazyLoading("customsteps", "/tmp/workdir/customsteps/new/customsteps.Rcheck/00LOCK-customsteps/00new",     keep.source = FALSE, keep.parse.data = FALSE, set.install.dir = "/tmp/workdir/customsteps/new/customsteps.Rcheck/customsteps")
An irrecoverable exception occurred. R is aborting now ...
Segmentation fault (core dumped)
ERROR: lazy loading failed for package ‘customsteps’
* removing ‘/tmp/workdir/customsteps/new/customsteps.Rcheck/customsteps’


```
### CRAN

```
* installing *source* package ‘customsteps’ ...
** package ‘customsteps’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading

 *** caught segfault ***
address 0x8, cause 'memory not mapped'

...
14: namespaceImportFrom(ns, loadNamespace(j <- i[[1L]], c(lib.loc,     .libPaths()), versionCheck = vI[[j]]), i[[2L]], from = package)
15: loadNamespace(package = package, lib.loc = lib.loc, keep.source = keep.source,     keep.parse.data = keep.parse.data, partial = TRUE)
16: withCallingHandlers(expr, packageStartupMessage = function(c) tryInvokeRestart("muffleMessage"))
17: suppressPackageStartupMessages(loadNamespace(package = package,     lib.loc = lib.loc, keep.source = keep.source, keep.parse.data = keep.parse.data,     partial = TRUE))
18: code2LazyLoadDB(package, lib.loc = lib.loc, keep.source = keep.source,     keep.parse.data = keep.parse.data, compress = compress, set.install.dir = set.install.dir)
19: tools:::makeLazyLoading("customsteps", "/tmp/workdir/customsteps/old/customsteps.Rcheck/00LOCK-customsteps/00new",     keep.source = FALSE, keep.parse.data = FALSE, set.install.dir = "/tmp/workdir/customsteps/old/customsteps.Rcheck/customsteps")
An irrecoverable exception occurred. R is aborting now ...
Segmentation fault (core dumped)
ERROR: lazy loading failed for package ‘customsteps’
* removing ‘/tmp/workdir/customsteps/old/customsteps.Rcheck/customsteps’


```
# cvms

<details>

* Version: 1.2.0
* GitHub: https://github.com/ludvigolsen/cvms
* Source code: https://github.com/cran/cvms
* Date/Publication: 2020-10-18 21:50:05 UTC
* Number of recursive dependencies: 129

Run `cloud_details(, "cvms")` for more info

</details>

## In both

*   checking whether package ‘cvms’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/cvms/new/cvms.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘cvms’ ...
** package ‘cvms’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
Error in lazyLoadDBinsertVariable(vars[i], from, datafile, ascii, compress,  : 
  write failed
ERROR: lazydata failed for package ‘cvms’
* removing ‘/tmp/workdir/cvms/new/cvms.Rcheck/cvms’


```
### CRAN

```
* installing *source* package ‘cvms’ ...
** package ‘cvms’ successfully unpacked and MD5 sums checked
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
ERROR: lazy loading failed for package ‘cvms’
* removing ‘/tmp/workdir/cvms/old/cvms.Rcheck/cvms’


```
# dabestr

<details>

* Version: 0.3.0
* GitHub: https://github.com/ACCLAB/dabestr
* Source code: https://github.com/cran/dabestr
* Date/Publication: 2020-07-13 06:50:03 UTC
* Number of recursive dependencies: 130

Run `cloud_details(, "dabestr")` for more info

</details>

## In both

*   checking whether package ‘dabestr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/dabestr/new/dabestr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dabestr’ ...
files ‘tests/figs/aesthetics/cumming-custom-groupwidth.svg’, ‘tests/figs/aesthetics/cumming-rcolorbrewer-palette.svg’, ‘tests/figs/aesthetics/gardner-altman-unpaired-custom-palette.svg’, ‘tests/figs/aesthetics/gardner-altman-unpaired-custom-rawplot-marker-size.svg’ have the wrong MD5 checksums
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘scales’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘dabestr’
* removing ‘/tmp/workdir/dabestr/new/dabestr.Rcheck/dabestr’


```
### CRAN

```
* installing *source* package ‘dabestr’ ...
files ‘tests/figs/aesthetics/cumming-custom-palette.svg’, ‘tests/figs/aesthetics/cumming-custom-theme.svg’ have the wrong MD5 checksums
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘scales’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘dabestr’
* removing ‘/tmp/workdir/dabestr/old/dabestr.Rcheck/dabestr’


```
# dartR

<details>

* Version: 1.8.3
* GitHub: NA
* Source code: https://github.com/cran/dartR
* Date/Publication: 2020-09-01 11:40:07 UTC
* Number of recursive dependencies: 180

Run `cloud_details(, "dartR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/dartR/new/dartR.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘dartR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘dartR’ version ‘1.8.3’
* package encoding: UTF-8
* checking package namespace information ... OK
...
Packages required but not available:
  'SNPRelate', 'mmod', 'PopGenReport', 'hierfstat'

Packages suggested but not available for checking:
  'rgdal', 'leaflet.minicharts', 'leaflet', 'poppr', 'qvalue'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/dartR/old/dartR.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘dartR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘dartR’ version ‘1.8.3’
* package encoding: UTF-8
* checking package namespace information ... OK
...
Packages required but not available:
  'SNPRelate', 'mmod', 'PopGenReport', 'hierfstat'

Packages suggested but not available for checking:
  'rgdal', 'leaflet.minicharts', 'leaflet', 'poppr', 'qvalue'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# DCPO

<details>

* Version: 0.5.3
* GitHub: NA
* Source code: https://github.com/cran/DCPO
* Date/Publication: 2020-05-29 12:50:02 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "DCPO")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/DCPO/old/DCPO.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘DCPO/DESCRIPTION’ ... OK
* this is package ‘DCPO’ version ‘0.5.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘DCPO’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/DCPO/old/DCPO.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





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
# DepthProc

<details>

* Version: 2.1.3
* GitHub: https://github.com/zzawadz/DepthProc
* Source code: https://github.com/cran/DepthProc
* Date/Publication: 2020-02-19 21:00:03 UTC
* Number of recursive dependencies: 137

Run `cloud_details(, "DepthProc")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/DepthProc/old/DepthProc.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘DepthProc/DESCRIPTION’ ... OK
* this is package ‘DepthProc’ version ‘2.1.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
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
Status: 1 NOTE





```
# dextergui

<details>

* Version: 0.2.2
* GitHub: https://github.com/jessekps/dexter
* Source code: https://github.com/cran/dextergui
* Date/Publication: 2020-02-20 08:30:02 UTC
* Number of recursive dependencies: 126

Run `cloud_details(, "dextergui")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/dextergui/new/dextergui.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘dextergui/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘dextergui’ version ‘0.2.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'dexter', 'ggplot2'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# dfidx

<details>

* Version: 0.0-3
* GitHub: NA
* Source code: https://github.com/cran/dfidx
* Date/Publication: 2020-05-08 15:30:05 UTC
* Number of recursive dependencies: 91

Run `cloud_details(, "dfidx")` for more info

</details>

## In both

*   checking whether package ‘dfidx’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/dfidx/new/dfidx.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dfidx’ ...
** package ‘dfidx’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘R6’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘dfidx’
* removing ‘/tmp/workdir/dfidx/new/dfidx.Rcheck/dfidx’


```
### CRAN

```
* installing *source* package ‘dfidx’ ...
** package ‘dfidx’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘R6’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘dfidx’
* removing ‘/tmp/workdir/dfidx/old/dfidx.Rcheck/dfidx’


```
# diceR

<details>

* Version: 1.0.0
* GitHub: https://github.com/AlineTalhouk/diceR
* Source code: https://github.com/cran/diceR
* Date/Publication: 2020-07-07 22:30:02 UTC
* Number of recursive dependencies: 143

Run `cloud_details(, "diceR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/diceR/new/diceR.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘diceR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘diceR’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘NMF’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/diceR/old/diceR.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘diceR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘diceR’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘NMF’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# dimRed

<details>

* Version: 0.2.3
* GitHub: https://github.com/gdkrmr/dimRed
* Source code: https://github.com/cran/dimRed
* Date/Publication: 2019-05-08 08:10:07 UTC
* Number of recursive dependencies: 140

Run `cloud_details(, "dimRed")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/dimRed/new/dimRed.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘dimRed/DESCRIPTION’ ... OK
* this is package ‘dimRed’ version ‘0.2.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
  
  Error: Test failures
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘dimensionality-reduction.Rnw’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 2 ERRORs, 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/dimRed/old/dimRed.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘dimRed/DESCRIPTION’ ... OK
* this is package ‘dimRed’ version ‘0.2.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
  
  Error: Test failures
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘dimensionality-reduction.Rnw’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 2 ERRORs, 2 NOTEs





```
# easyalluvial

<details>

* Version: 0.2.3
* GitHub: https://github.com/erblast/easyalluvial
* Source code: https://github.com/cran/easyalluvial
* Date/Publication: 2020-05-07 08:40:20 UTC
* Number of recursive dependencies: 148

Run `cloud_details(, "easyalluvial")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/easyalluvial/old/easyalluvial.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘easyalluvial/DESCRIPTION’ ... OK
* this is package ‘easyalluvial’ version ‘0.2.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘easyalluvial’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/easyalluvial/old/easyalluvial.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
# ecochange

<details>

* Version: 1.3
* GitHub: NA
* Source code: https://github.com/cran/ecochange
* Date/Publication: 2020-10-13 15:00:02 UTC
* Number of recursive dependencies: 100

Run `cloud_details(, "ecochange")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/ecochange/old/ecochange.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ecochange/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘ecochange’ version ‘1.3’
* package encoding: latin1
* checking package namespace information ... OK
...
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘ecochange’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... WARNING
Fatal error: cannot create 'R_TempDir'
 ERROR
Fatal error: cannot create 'R_TempDir'
* DONE
Status: 1 ERROR, 1 WARNING





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
# ezec

<details>

* Version: 1.0.1
* GitHub: https://github.com/grunwaldlab/ezec
* Source code: https://github.com/cran/ezec
* Date/Publication: 2016-12-05 08:27:32
* Number of recursive dependencies: 109

Run `cloud_details(, "ezec")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# fishualize

<details>

* Version: 0.2.0
* GitHub: https://github.com/nschiett/fishualize
* Source code: https://github.com/cran/fishualize
* Date/Publication: 2020-04-20 19:10:10 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "fishualize")` for more info

</details>

## In both

*   checking whether package ‘fishualize’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/fishualize/new/fishualize.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘fishualize’ ...
** package ‘fishualize’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘pkgconfig’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘fishualize’
* removing ‘/tmp/workdir/fishualize/new/fishualize.Rcheck/fishualize’


```
### CRAN

```
* installing *source* package ‘fishualize’ ...
** package ‘fishualize’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘pkgconfig’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘fishualize’
* removing ‘/tmp/workdir/fishualize/old/fishualize.Rcheck/fishualize’


```
# gastempt

<details>

* Version: 0.5.1
* GitHub: https://github.com/dmenne/gastempt
* Source code: https://github.com/cran/gastempt
* Date/Publication: 2020-07-29 17:00:03 UTC
* Number of recursive dependencies: 126

Run `cloud_details(, "gastempt")` for more info

</details>

## In both

*   checking whether package ‘gastempt’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/gastempt/new/gastempt.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘gastempt’ ...
** package ‘gastempt’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(name) : there is no package called ‘rstantools’
Calls: :: ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘gastempt’
* removing ‘/tmp/workdir/gastempt/new/gastempt.Rcheck/gastempt’


```
### CRAN

```
* installing *source* package ‘gastempt’ ...
** package ‘gastempt’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(name) : there is no package called ‘rstantools’
Calls: :: ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘gastempt’
* removing ‘/tmp/workdir/gastempt/old/gastempt.Rcheck/gastempt’


```
# ggmsa

<details>

* Version: 0.0.4
* GitHub: NA
* Source code: https://github.com/cran/ggmsa
* Date/Publication: 2020-05-28 10:50:10 UTC
* Number of recursive dependencies: 81

Run `cloud_details(, "ggmsa")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ggmsa/new/ggmsa.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ggmsa/DESCRIPTION’ ... OK
* this is package ‘ggmsa’ version ‘0.0.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘Biostrings’

Packages suggested but not available for checking: 'ggtree', 'seqmagick'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/ggmsa/old/ggmsa.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ggmsa/DESCRIPTION’ ... OK
* this is package ‘ggmsa’ version ‘0.0.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘Biostrings’

Packages suggested but not available for checking: 'ggtree', 'seqmagick'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





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
# glmmfields

<details>

* Version: 0.1.4
* GitHub: https://github.com/seananderson/glmmfields
* Source code: https://github.com/cran/glmmfields
* Date/Publication: 2020-07-09 05:50:03 UTC
* Number of recursive dependencies: 95

Run `cloud_details(, "glmmfields")` for more info

</details>

## In both

*   checking whether package ‘glmmfields’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/glmmfields/new/glmmfields.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘glmmfields’ ...
** package ‘glmmfields’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/opt/R/4.0.3/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/glmmfields.stan
Wrote C++ file "stan_files/glmmfields.cc"


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I/usr/local/include   -fpic  -g -O2  -c stan_files/glmmfields.cc -o stan_files/glmmfields.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:392,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_glmmfields_namespace::model_glmmfields; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stan_files/glmmfields.o] Error 1
rm stan_files/glmmfields.cc
ERROR: compilation failed for package ‘glmmfields’
* removing ‘/tmp/workdir/glmmfields/new/glmmfields.Rcheck/glmmfields’


```
### CRAN

```
* installing *source* package ‘glmmfields’ ...
** package ‘glmmfields’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/opt/R/4.0.3/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/glmmfields.stan
Wrote C++ file "stan_files/glmmfields.cc"


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I/usr/local/include   -fpic  -g -O2  -c stan_files/glmmfields.cc -o stan_files/glmmfields.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:392,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_glmmfields_namespace::model_glmmfields; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stan_files/glmmfields.o] Error 1
rm stan_files/glmmfields.cc
ERROR: compilation failed for package ‘glmmfields’
* removing ‘/tmp/workdir/glmmfields/old/glmmfields.Rcheck/glmmfields’


```
# NA

<details>

* Version: NA
* GitHub: NA
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
# hdpGLM

<details>

* Version: 1.0.0
* GitHub: https://github.com/DiogoFerrari/hdpGLM
* Source code: https://github.com/cran/hdpGLM
* Date/Publication: 2020-11-09 20:30:05 UTC
* Number of recursive dependencies: 174

Run `cloud_details(, "hdpGLM")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/hdpGLM/old/hdpGLM.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘hdpGLM/DESCRIPTION’ ... OK
* this is package ‘hdpGLM’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘hdpGLM.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 2 NOTEs





```
# ICAMS

<details>

* Version: 2.2.4
* GitHub: https://github.com/steverozen/ICAMS
* Source code: https://github.com/cran/ICAMS
* Date/Publication: 2020-09-22 08:40:12 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "ICAMS")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ICAMS/new/ICAMS.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ICAMS/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘ICAMS’ version ‘2.2.4’
* package encoding: UTF-8
* checking package namespace information ... OK
...

Packages suggested but not available for checking:
  'BSgenome.Hsapiens.1000genomes.hs37d5',
  'BSgenome.Hsapiens.UCSC.hg38', 'BSgenome.Mmusculus.UCSC.mm10',
  'testthat'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/ICAMS/old/ICAMS.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ICAMS/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘ICAMS’ version ‘2.2.4’
* package encoding: UTF-8
* checking package namespace information ... OK
...

Packages suggested but not available for checking:
  'BSgenome.Hsapiens.1000genomes.hs37d5',
  'BSgenome.Hsapiens.UCSC.hg38', 'BSgenome.Mmusculus.UCSC.mm10',
  'testthat'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# JFE

<details>

* Version: 2.5.1
* GitHub: NA
* Source code: https://github.com/cran/JFE
* Date/Publication: 2020-10-01 09:50:02 UTC
* Number of recursive dependencies: 226

Run `cloud_details(, "JFE")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






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
# mappoly

<details>

* Version: 0.2.1
* GitHub: https://github.com/mmollina/MAPpoly
* Source code: https://github.com/cran/mappoly
* Date/Publication: 2020-11-23 10:50:11 UTC
* Number of recursive dependencies: 189

Run `cloud_details(, "mappoly")` for more info

</details>

## In both

*   checking whether package ‘mappoly’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/mappoly/new/mappoly.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mappoly’ ...
** package ‘mappoly’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c calc_genoprob.cpp -o calc_genoprob.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c calc_genoprob_based_on_phased_marker_blocks.cpp -o calc_genoprob_based_on_phased_marker_blocks.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c calc_loglike_given_map.cpp -o calc_loglike_given_map.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c combinatorial.cpp -o combinatorial.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c est_hmm_map_based_on_phased_mrk_blocks.cpp -o est_hmm_map_based_on_phased_mrk_blocks.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c est_map_hmm_given_dose.cpp -o est_map_hmm_given_dose.o
/tmp/ccFz3bZH.s: Assembler messages:
/tmp/ccFz3bZH.s: Fatal error: can't close est_map_hmm_given_dose.o: No space left on device
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:181: est_map_hmm_given_dose.o] Error 1
ERROR: compilation failed for package ‘mappoly’
* removing ‘/tmp/workdir/mappoly/new/mappoly.Rcheck/mappoly’


```
### CRAN

```
* installing *source* package ‘mappoly’ ...
** package ‘mappoly’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c calc_genoprob.cpp -o calc_genoprob.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c calc_genoprob_based_on_phased_marker_blocks.cpp -o calc_genoprob_based_on_phased_marker_blocks.o
calc_genoprob_based_on_phased_marker_blocks.cpp:177:1: fatal error: error writing to /tmp/ccNjOgxb.s: No space left on device
  177 | }
      | ^
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:181: calc_genoprob_based_on_phased_marker_blocks.o] Error 1
ERROR: compilation failed for package ‘mappoly’
* removing ‘/tmp/workdir/mappoly/old/mappoly.Rcheck/mappoly’


```
# MarketMatching

<details>

* Version: 1.1.2
* GitHub: NA
* Source code: https://github.com/cran/MarketMatching
* Date/Publication: 2019-07-03 17:10:03 UTC
* Number of recursive dependencies: 81

Run `cloud_details(, "MarketMatching")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/MarketMatching/new/MarketMatching.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘MarketMatching/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘MarketMatching’ version ‘1.1.2’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'CausalImpact', 'bsts'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/MarketMatching/old/MarketMatching.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘MarketMatching/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘MarketMatching’ version ‘1.1.2’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'CausalImpact', 'bsts'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# mcvis

<details>

* Version: 1.0.6
* GitHub: NA
* Source code: https://github.com/cran/mcvis
* Date/Publication: 2020-10-03 05:30:02 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "mcvis")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/mcvis/old/mcvis.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘mcvis/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘mcvis’ version ‘1.0.6’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘mcvis.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK





```
# merTools

<details>

* Version: 0.5.2
* GitHub: NA
* Source code: https://github.com/cran/merTools
* Date/Publication: 2020-06-23 10:30:12 UTC
* Number of recursive dependencies: 152

Run `cloud_details(, "merTools")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/merTools/old/merTools.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘merTools/DESCRIPTION’ ... OK
* this is package ‘merTools’ version ‘0.5.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘merTools’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/merTools/old/merTools.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR, 1 NOTE





```
# metagam

<details>

* Version: 0.2.0
* GitHub: https://github.com/Lifebrain/metagam
* Source code: https://github.com/cran/metagam
* Date/Publication: 2020-11-12 08:10:02 UTC
* Number of recursive dependencies: 151

Run `cloud_details(, "metagam")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/metagam/new/metagam.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘metagam/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘metagam’ version ‘0.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘metap’

Package suggested but not available for checking: ‘multtest’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/metagam/old/metagam.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘metagam/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘metagam’ version ‘0.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘metap’

Package suggested but not available for checking: ‘multtest’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# metaviz

<details>

* Version: 0.3.1
* GitHub: https://github.com/Mkossmeier/metaviz
* Source code: https://github.com/cran/metaviz
* Date/Publication: 2020-04-09 09:10:08 UTC
* Number of recursive dependencies: 144

Run `cloud_details(, "metaviz")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/metaviz/old/metaviz.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘metaviz/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘metaviz’ version ‘0.3.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘funnelinf.Rmd’ using ‘UTF-8’... OK
  ‘metaviz.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK





```
# methcon5

<details>

* Version: 0.1.0
* GitHub: https://github.com/EmilHvitfeldt/methcon5
* Source code: https://github.com/cran/methcon5
* Date/Publication: 2019-12-20 13:50:02 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "methcon5")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/methcon5/new/methcon5.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘methcon5/DESCRIPTION’ ... OK
* this is package ‘methcon5’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK





```
### CRAN

```






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
# modchart

<details>

* Version: 0.4
* GitHub: NA
* Source code: https://github.com/cran/modchart
* Date/Publication: 2020-07-13 10:00:03 UTC
* Number of recursive dependencies: 127

Run `cloud_details(, "modchart")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/modchart/new/modchart.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘modchart/DESCRIPTION’ ... OK
* this is package ‘modchart’ version ‘0.4’
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘modchart’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/modchart/new/modchart.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# modelplotr

<details>

* Version: 1.1.0
* GitHub: https://github.com/jurrr/modelplotr
* Source code: https://github.com/cran/modelplotr
* Date/Publication: 2020-10-13 04:20:05 UTC
* Number of recursive dependencies: 130

Run `cloud_details(, "modelplotr")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/modelplotr/old/modelplotr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘modelplotr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘modelplotr’ version ‘1.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... ERROR
unable to create ‘/tmp/workdir/modelplotr/old/modelplotr.Rcheck/tests’
* DONE
Status: 1 ERROR





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
# mosaicModel

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/mosaicModel
* Date/Publication: 2017-09-22 16:21:41 UTC
* Number of recursive dependencies: 144

Run `cloud_details(, "mosaicModel")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/mosaicModel/old/mosaicModel.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘mosaicModel/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘mosaicModel’ version ‘0.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
Errors in running code in vignettes:
when running code in ‘Basics.Rmd’
  ...

... incomplete output.  Crash?

  ‘Basics.Rmd’ using ‘UTF-8’... failed to complete the test
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 2 ERRORs, 3 WARNINGs, 2 NOTEs





```
# motifr

<details>

* Version: 1.0.0
* GitHub: https://github.com/marioangst/motifr
* Source code: https://github.com/cran/motifr
* Date/Publication: 2020-12-10 15:40:02 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "motifr")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/motifr/old/motifr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘motifr/DESCRIPTION’ ... OK
* this is package ‘motifr’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking examples ... ERROR
Running massageExamples to create ‘motifr-Ex.R’ failed

 NONE
* checking for unstated dependencies in ‘tests’ ... WARNING
Fatal error: cannot create 'R_TempDir'
* checking tests ... ERROR
unable to create ‘/tmp/workdir/motifr/old/motifr.Rcheck/tests’
* DONE
Status: 2 ERRORs, 4 WARNINGs





```
# mrgsolve

<details>

* Version: 0.10.7
* GitHub: https://github.com/metrumresearchgroup/mrgsolve
* Source code: https://github.com/cran/mrgsolve
* Date/Publication: 2020-12-09 14:20:02 UTC
* Number of recursive dependencies: 87

Run `cloud_details(, "mrgsolve")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/mrgsolve/old/mrgsolve.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘mrgsolve/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘mrgsolve’ version ‘0.10.7’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘mrgsolve’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/mrgsolve/old/mrgsolve.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
# MtreeRing

<details>

* Version: 1.4.2
* GitHub: https://github.com/ropensci/MtreeRing
* Source code: https://github.com/cran/MtreeRing
* Date/Publication: 2019-10-03 09:40:02 UTC
* Number of recursive dependencies: 111

Run `cloud_details(, "MtreeRing")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/MtreeRing/new/MtreeRing.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘MtreeRing/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘MtreeRing’ version ‘1.4.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘imager’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# multinma

<details>

* Version: 0.2.0
* GitHub: https://github.com/dmphillippo/multinma
* Source code: https://github.com/cran/multinma
* Date/Publication: 2020-12-04 15:10:02 UTC
* Number of recursive dependencies: 149

Run `cloud_details(, "multinma")` for more info

</details>

## In both

*   checking whether package ‘multinma’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/multinma/new/multinma.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘multinma’ ...
** package ‘multinma’ successfully unpacked and MD5 sums checked
** using staged installation
DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: nint / int_thin
 Positive values rounded down, negative values rounded up or down in platform-dependent way.

DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: nint / int_thin
 Positive values rounded down, negative values rounded up or down in platform-dependent way.
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_binomial_1par_namespace::model_binomial_1par; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_binomial_1par.o] Error 1
ERROR: compilation failed for package ‘multinma’
* removing ‘/tmp/workdir/multinma/new/multinma.Rcheck/multinma’


```
### CRAN

```
* installing *source* package ‘multinma’ ...
** package ‘multinma’ successfully unpacked and MD5 sums checked
** using staged installation
DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: nint / int_thin
 Positive values rounded down, negative values rounded up or down in platform-dependent way.

DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: nint / int_thin
 Positive values rounded down, negative values rounded up or down in platform-dependent way.
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_binomial_1par_namespace::model_binomial_1par; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_binomial_1par.o] Error 1
ERROR: compilation failed for package ‘multinma’
* removing ‘/tmp/workdir/multinma/old/multinma.Rcheck/multinma’


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
# namer

<details>

* Version: 0.1.5
* GitHub: https://github.com/lockedata/namer
* Source code: https://github.com/cran/namer
* Date/Publication: 2019-12-16 12:30:02 UTC
* Number of recursive dependencies: 52

Run `cloud_details(, "namer")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/namer/new/namer.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘namer/DESCRIPTION’ ... OK
* this is package ‘namer’ version ‘0.1.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘namer.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK





```
### CRAN

```






```
# nasadata

<details>

* Version: 0.9.0
* GitHub: NA
* Source code: https://github.com/cran/nasadata
* Date/Publication: 2016-05-07 00:41:17
* Number of recursive dependencies: 24

Run `cloud_details(, "nasadata")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/nasadata/new/nasadata.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘nasadata/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘nasadata’ version ‘0.9.0’
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* DONE
Status: OK





```
### CRAN

```






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
# nc

<details>

* Version: 2020.8.6
* GitHub: https://github.com/tdhock/nc
* Source code: https://github.com/cran/nc
* Date/Publication: 2020-08-10 17:10:17 UTC
* Number of recursive dependencies: 75

Run `cloud_details(, "nc")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/nc/old/nc.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘nc/DESCRIPTION’ ... OK
* this is package ‘nc’ version ‘2020.8.6’
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘nc’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/nc/old/nc.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





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
# nonet

<details>

* Version: 0.4.0
* GitHub: https://github.com/GSLabDev/nonet
* Source code: https://github.com/cran/nonet
* Date/Publication: 2019-01-15 10:50:03 UTC
* Number of recursive dependencies: 129

Run `cloud_details(, "nonet")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/nonet/old/nonet.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘nonet/DESCRIPTION’ ... OK
* this is package ‘nonet’ version ‘0.4.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking examples ... ERROR
Running massageExamples to create ‘nonet-Ex.R’ failed

 NONE
* checking for unstated dependencies in ‘tests’ ... WARNING
Fatal error: cannot create 'R_TempDir'
* checking tests ... ERROR
unable to create ‘/tmp/workdir/nonet/old/nonet.Rcheck/tests’
* DONE
Status: 2 ERRORs, 4 WARNINGs, 3 NOTEs





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
# oncrawlR

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/oncrawlR
* Date/Publication: 2020-01-31 16:40:03 UTC
* Number of recursive dependencies: 145

Run `cloud_details(, "oncrawlR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/oncrawlR/new/oncrawlR.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘oncrawlR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘oncrawlR’ version ‘0.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘oncrawlR’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/oncrawlR/new/oncrawlR.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
### CRAN

```






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
# packDAMipd

<details>

* Version: 0.1.2
* GitHub: https://github.com/sheejamk/packDAMipd
* Source code: https://github.com/cran/packDAMipd
* Date/Publication: 2020-11-20 15:00:02 UTC
* Number of recursive dependencies: 176

Run `cloud_details(, "packDAMipd")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# palaeoSig

<details>

* Version: 2.0-3
* GitHub: https://github.com/richardjtelford/palaeoSig
* Source code: https://github.com/cran/palaeoSig
* Date/Publication: 2019-06-28 08:00:03 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "palaeoSig")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# passport

<details>

* Version: 0.3.0
* GitHub: https://github.com/alistaire47/passport
* Source code: https://github.com/cran/passport
* Date/Publication: 2020-11-07 07:30:03 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "passport")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/passport/new/passport.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘passport/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘passport’ version ‘0.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘passport.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK





```
### CRAN

```






```
# patternplot

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/patternplot
* Date/Publication: 2020-04-21 12:20:09 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "patternplot")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/patternplot/new/patternplot.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘patternplot/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘patternplot’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘patternplot’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/patternplot/new/patternplot.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# pencal

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/pencal
* Date/Publication: 2020-12-04 12:30:02 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "pencal")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/pencal/new/pencal.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘pencal/DESCRIPTION’ ... OK
* this is package ‘pencal’ version ‘0.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'survcomp', 'ptmixed'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/pencal/old/pencal.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘pencal/DESCRIPTION’ ... OK
* this is package ‘pencal’ version ‘0.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'survcomp', 'ptmixed'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# peRspective

<details>

* Version: 0.1.0
* GitHub: https://github.com/favstats/peRspective
* Source code: https://github.com/cran/peRspective
* Date/Publication: 2019-05-20 08:40:02 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "peRspective")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/peRspective/old/peRspective.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘peRspective/DESCRIPTION’ ... OK
* this is package ‘peRspective’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK





```
# photosynthesis

<details>

* Version: 2.0.0
* GitHub: NA
* Source code: https://github.com/cran/photosynthesis
* Date/Publication: 2020-09-12 05:40:03 UTC
* Number of recursive dependencies: 102

Run `cloud_details(, "photosynthesis")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# pingers

<details>

* Version: 0.1.1
* GitHub: https://github.com/JesseVent/pingers
* Source code: https://github.com/cran/pingers
* Date/Publication: 2018-10-26 15:00:03 UTC
* Number of recursive dependencies: 80

Run `cloud_details(, "pingers")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/pingers/old/pingers.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘pingers/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘pingers’ version ‘0.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* DONE
Status: OK





```
# plot3logit

<details>

* Version: 3.0.0
* GitHub: https://github.com/f-santi/plot3logit
* Source code: https://github.com/cran/plot3logit
* Date/Publication: 2020-09-12 08:40:02 UTC
* Number of recursive dependencies: 106

Run `cloud_details(, "plot3logit")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# pocketapi

<details>

* Version: 0.1
* GitHub: https://github.com/CorrelAid/pocketapi
* Source code: https://github.com/cran/pocketapi
* Date/Publication: 2020-11-20 10:20:02 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "pocketapi")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/pocketapi/old/pocketapi.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘pocketapi/DESCRIPTION’ ... OK
* this is package ‘pocketapi’ version ‘0.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘pocketapi’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/pocketapi/old/pocketapi.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
# powdR

<details>

* Version: 1.2.4
* GitHub: https://github.com/benmbutler/powdR
* Source code: https://github.com/cran/powdR
* Date/Publication: 2020-11-16 18:10:02 UTC
* Number of recursive dependencies: 140

Run `cloud_details(, "powdR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/powdR/new/powdR.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘powdR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘powdR’ version ‘1.2.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'shiny', 'DT', 'shinyWidgets', 'tidyr'

Package suggested but not available for checking: ‘rmarkdown’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# pRF

<details>

* Version: 1.2
* GitHub: NA
* Source code: https://github.com/cran/pRF
* Date/Publication: 2016-01-11 17:12:20
* Number of recursive dependencies: 64

Run `cloud_details(, "pRF")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/pRF/old/pRF.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘pRF/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘pRF’ version ‘1.2’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘multtest’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# processR

<details>

* Version: 0.2.3
* GitHub: https://github.com/cardiomoon/processR
* Source code: https://github.com/cran/processR
* Date/Publication: 2020-01-27 14:10:02 UTC
* Number of recursive dependencies: 172

Run `cloud_details(, "processR")` for more info

</details>

## In both

*   checking whether package ‘processR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/processR/new/processR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘processR’ ...
** package ‘processR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘display’ is not exported by 'namespace:flextable'
Execution halted
ERROR: lazy loading failed for package ‘processR’
* removing ‘/tmp/workdir/processR/new/processR.Rcheck/processR’


```
### CRAN

```
* installing *source* package ‘processR’ ...
** package ‘processR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘display’ is not exported by 'namespace:flextable'
Execution halted
ERROR: lazy loading failed for package ‘processR’
* removing ‘/tmp/workdir/processR/old/processR.Rcheck/processR’


```
# qdap

<details>

* Version: 2.4.3
* GitHub: https://github.com/trinker/qdap
* Source code: https://github.com/cran/qdap
* Date/Publication: 2020-09-27 17:10:09 UTC
* Number of recursive dependencies: 101

Run `cloud_details(, "qdap")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# qualtRics

<details>

* Version: 3.1.3
* GitHub: https://github.com/ropensci/qualtRics
* Source code: https://github.com/cran/qualtRics
* Date/Publication: 2020-05-22 20:20:02 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "qualtRics")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# rAltmetric

<details>

* Version: 0.7.0
* GitHub: https://github.com/ropensci/rAltmetric
* Source code: https://github.com/cran/rAltmetric
* Date/Publication: 2017-04-19 19:14:45 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "rAltmetric")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






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
# rcrossref

<details>

* Version: 1.1.0
* GitHub: https://github.com/ropensci/rcrossref
* Source code: https://github.com/cran/rcrossref
* Date/Publication: 2020-10-02 21:50:02 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "rcrossref")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/rcrossref/new/rcrossref.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘rcrossref/DESCRIPTION’ ... OK
* this is package ‘rcrossref’ version ‘1.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'plyr', 'shiny', 'stringr'

Package suggested but not available for checking: ‘rmarkdown’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# readsdr

<details>

* Version: 0.1.0
* GitHub: https://github.com/jandraor/readsdr
* Source code: https://github.com/cran/readsdr
* Date/Publication: 2020-06-12 08:10:03 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "readsdr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/readsdr/new/readsdr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘readsdr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘readsdr’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
  Error: package or namespace load failed for 'testthat' in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
   there is no package called 'R6'
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘Introduction_to_readsdr.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 2 ERRORs, 2 NOTEs





```
### CRAN

```






```
# reclin

<details>

* Version: 0.1.1
* GitHub: https://github.com/djvanderlaan/reclin
* Source code: https://github.com/cran/reclin
* Date/Publication: 2018-08-09 14:30:03 UTC
* Number of recursive dependencies: 56

Run `cloud_details(, "reclin")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/reclin/new/reclin.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘reclin/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘reclin’ version ‘0.1.1’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘Rcpp’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# regions

<details>

* Version: 0.1.5
* GitHub: https://github.com/rOpenGov/regions
* Source code: https://github.com/cran/regions
* Date/Publication: 2020-06-23 13:50:03 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "regions")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/regions/new/regions.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘regions/DESCRIPTION’ ... OK
* this is package ‘regions’ version ‘0.1.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘magrittr’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# reporter

<details>

* Version: 1.0.6
* GitHub: NA
* Source code: https://github.com/cran/reporter
* Date/Publication: 2020-11-20 07:30:06 UTC
* Number of recursive dependencies: 125

Run `cloud_details(, "reporter")` for more info

</details>

## In both

*   checking whether package ‘reporter’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/reporter/new/reporter.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: 'magrittr', 'ggplot2'
    ```

## Installation

### Devel

```
* installing *source* package ‘reporter’ ...
** package ‘reporter’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘magrittr’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘reporter’
* removing ‘/tmp/workdir/reporter/new/reporter.Rcheck/reporter’


```
### CRAN

```
* installing *source* package ‘reporter’ ...
** package ‘reporter’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘magrittr’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘reporter’
* removing ‘/tmp/workdir/reporter/old/reporter.Rcheck/reporter’


```
# rgenius

<details>

* Version: 0.1.0
* GitHub: https://github.com/AlbertoAlmuinha/rgenius
* Source code: https://github.com/cran/rgenius
* Date/Publication: 2020-05-11 15:10:02 UTC
* Number of recursive dependencies: 36

Run `cloud_details(, "rgenius")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/rgenius/new/rgenius.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘rgenius/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘rgenius’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available:
  'purrr', 'stringr', 'httr', 'rvest', 'foreach', 'tibble',
  'doParallel'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# rgho

<details>

* Version: 2.0.1
* GitHub: https://github.com/pierucci/rgho
* Source code: https://github.com/cran/rgho
* Date/Publication: 2020-11-30 11:00:02 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "rgho")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/rgho/new/rgho.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘rgho/DESCRIPTION’ ... OK
* this is package ‘rgho’ version ‘2.0.1’
* checking package namespace information ... OK
* checking package dependencies ... NOTE
Package suggested but not available for checking: ‘rmarkdown’
...
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘a-intro.Rmd’ using ‘UTF-8’... OK
  ‘b-dimensions.Rmd’ using ‘UTF-8’... OK
  ‘c-codes-gho.Rmd’ using ‘UTF-8’... OK
  ‘d-codes-country.Rmd’ using ‘UTF-8’... OK
  ‘e-details.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
### CRAN

```






```
# rIP

<details>

* Version: 1.2.0
* GitHub: https://github.com/MAHDLab/rIP
* Source code: https://github.com/cran/rIP
* Date/Publication: 2019-05-29 17:10:02 UTC
* Number of recursive dependencies: 55

Run `cloud_details(, "rIP")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/rIP/new/rIP.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘rIP/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘rIP’ version ‘1.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'httr', 'jsonlite'

Package suggested but not available for checking: ‘testthat’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# rise

<details>

* Version: 1.0.4
* GitHub: https://github.com/lumenlearning/rise
* Source code: https://github.com/cran/rise
* Date/Publication: 2018-10-04 17:50:03 UTC
* Number of recursive dependencies: 53

Run `cloud_details(, "rise")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/rise/new/rise.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘rise/DESCRIPTION’ ... OK
* this is package ‘rise’ version ‘1.0.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘rise’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/rise/new/rise.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# rmdcev

<details>

* Version: 1.2.4
* GitHub: https://github.com/plloydsmith/rmdcev
* Source code: https://github.com/cran/rmdcev
* Date/Publication: 2020-09-30 18:40:02 UTC
* Number of recursive dependencies: 83

Run `cloud_details(, "rmdcev")` for more info

</details>

## In both

*   checking whether package ‘rmdcev’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rmdcev/new/rmdcev.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rmdcev’ ...
** package ‘rmdcev’ successfully unpacked and MD5 sums checked
** using staged installation
** libs


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I/usr/local/include   -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:392,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.0.3/lib/R/site-library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_mdcev_namespace::model_mdcev; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_mdcev.o] Error 1
ERROR: compilation failed for package ‘rmdcev’
* removing ‘/tmp/workdir/rmdcev/new/rmdcev.Rcheck/rmdcev’


```
### CRAN

```
* installing *source* package ‘rmdcev’ ...
** package ‘rmdcev’ successfully unpacked and MD5 sums checked
** using staged installation
** libs


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I/usr/local/include   -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:392,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.0.3/lib/R/site-library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_mdcev_namespace::model_mdcev; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_mdcev.o] Error 1
ERROR: compilation failed for package ‘rmdcev’
* removing ‘/tmp/workdir/rmdcev/old/rmdcev.Rcheck/rmdcev’


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
# rPraat

<details>

* Version: 1.3.1
* GitHub: https://github.com/bbTomas/rPraat
* Source code: https://github.com/cran/rPraat
* Date/Publication: 2020-04-04 17:10:02 UTC
* Number of recursive dependencies: 56

Run `cloud_details(, "rPraat")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# NA

<details>

* Version: NA
* GitHub: NA
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
# sabre

<details>

* Version: 0.3.2
* GitHub: https://github.com/Nowosad/sabre
* Source code: https://github.com/cran/sabre
* Date/Publication: 2019-10-17 16:20:03 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "sabre")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# NA

<details>

* Version: NA
* GitHub: NA
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
# sarima

<details>

* Version: 0.8.4
* GitHub: https://github.com/GeoBosh/sarima
* Source code: https://github.com/cran/sarima
* Date/Publication: 2020-09-29 10:00:14 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "sarima")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/sarima/new/sarima.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘sarima/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘sarima’ version ‘0.8.4’
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘sarima’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/sarima/new/sarima.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# sars

<details>

* Version: 1.3.0
* GitHub: https://github.com/txm676/sars
* Source code: https://github.com/cran/sars
* Date/Publication: 2020-10-06 15:20:02 UTC
* Number of recursive dependencies: 56

Run `cloud_details(, "sars")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/sars/new/sars.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘sars/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘sars’ version ‘1.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘doParallel’

Package suggested but not available for checking: ‘rmarkdown’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/sars/old/sars.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘sars/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘sars’ version ‘1.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘doParallel’

Package suggested but not available for checking: ‘rmarkdown’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# sazedR

<details>

* Version: 2.0.2
* GitHub: https://github.com/mtoller/autocorr_season_length_detection
* Source code: https://github.com/cran/sazedR
* Date/Publication: 2020-09-29 18:30:02 UTC
* Number of recursive dependencies: 25

Run `cloud_details(, "sazedR")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# scatterpie

<details>

* Version: 0.1.5
* GitHub: NA
* Source code: https://github.com/cran/scatterpie
* Date/Publication: 2020-09-09 05:30:07 UTC
* Number of recursive dependencies: 76

Run `cloud_details(, "scatterpie")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# scgwr

<details>

* Version: 0.1.2-1
* GitHub: NA
* Source code: https://github.com/cran/scgwr
* Date/Publication: 2020-08-07 09:00:02 UTC
* Number of recursive dependencies: 27

Run `cloud_details(, "scgwr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/scgwr/new/scgwr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘scgwr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘scgwr’ version ‘0.1.2-1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* DONE
Status: OK





```
### CRAN

```






```
# NA

<details>

* Version: NA
* GitHub: NA
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
# NA

<details>

* Version: NA
* GitHub: NA
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
* using log directory ‘/tmp/workdir/secrettext/old/secrettext.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* checking package directory ... ERROR
package directory ‘/tmp/workdir/secrettext/old/secrettext.Rcheck/00_pkg_src/secrettext’ does not exist
* DONE
Status: 1 ERROR





```
# NA

<details>

* Version: NA
* GitHub: NA
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
# NA

<details>

* Version: NA
* GitHub: NA
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
# seecolor

<details>

* Version: 0.1.0
* GitHub: https://github.com/lovestat/seecolor
* Source code: https://github.com/cran/seecolor
* Date/Publication: 2020-12-07 17:40:03 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "seecolor")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/seecolor/new/seecolor.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘seecolor/DESCRIPTION’ ... OK
* this is package ‘seecolor’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘Intro-to-seecolor-package.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/seecolor/old/seecolor.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘seecolor/DESCRIPTION’ ... OK
* this is package ‘seecolor’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘Intro-to-seecolor-package.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
# seeds

<details>

* Version: 0.9.1
* GitHub: https://github.com/Newmi1988/seeds
* Source code: https://github.com/cran/seeds
* Date/Publication: 2020-07-14 00:00:02 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "seeds")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/seeds/new/seeds.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘seeds/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘seeds’ version ‘0.9.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘seeds.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/seeds/old/seeds.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘seeds/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘seeds’ version ‘0.9.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘seeds.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
# NA

<details>

* Version: NA
* GitHub: NA
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
# NA

<details>

* Version: NA
* GitHub: NA
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
# seplyr

<details>

* Version: 1.0.1
* GitHub: https://github.com/WinVector/seplyr
* Source code: https://github.com/cran/seplyr
* Date/Publication: 2020-10-18 17:30:02 UTC
* Number of recursive dependencies: 38

Run `cloud_details(, "seplyr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/seplyr/new/seplyr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘seplyr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘seplyr’ version ‘1.0.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...

Packages suggested but not available for checking:
  'knitr', 'rmarkdown', 'tinytest'

VignetteBuilder package required for checking but not installed: ‘knitr’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# NA

<details>

* Version: NA
* GitHub: NA
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
# servosphereR

<details>

* Version: 0.1.1
* GitHub: https://github.com/wittja01/servosphereR
* Source code: https://github.com/cran/servosphereR
* Date/Publication: 2019-05-14 19:10:02 UTC
* Number of recursive dependencies: 51

Run `cloud_details(, "servosphereR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/servosphereR/new/servosphereR.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘servosphereR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘servosphereR’ version ‘0.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘servosphereR’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/servosphereR/new/servosphereR.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR, 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/servosphereR/old/servosphereR.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘servosphereR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘servosphereR’ version ‘0.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘servosphereR’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/servosphereR/old/servosphereR.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR, 1 NOTE





```
# NA

<details>

* Version: NA
* GitHub: NA
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
# shar

<details>

* Version: 1.1
* GitHub: https://github.com/r-spatialecology/shar
* Source code: https://github.com/cran/shar
* Date/Publication: 2019-11-15 15:20:02 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "shar")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/shar/new/shar.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘shar/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘shar’ version ‘1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...

Packages suggested but not available for checking:
  'covr', 'dplyr', 'testthat', 'knitr', 'rmarkdown'

VignetteBuilder package required for checking but not installed: ‘knitr’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# shazam

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/shazam
* Date/Publication: 2020-08-11 16:00:03 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "shazam")` for more info

</details>

## In both

*   checking whether package ‘shazam’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/shazam/new/shazam.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘shazam’ ...
** package ‘shazam’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘ggplot2’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
 there is no package called ‘colorspace’
Error: package ‘ggplot2’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘shazam’
* removing ‘/tmp/workdir/shazam/new/shazam.Rcheck/shazam’


```
### CRAN

```
* installing *source* package ‘shazam’ ...
** package ‘shazam’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘ggplot2’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
 there is no package called ‘colorspace’
Error: package ‘ggplot2’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘shazam’
* removing ‘/tmp/workdir/shazam/old/shazam.Rcheck/shazam’


```
# NA

<details>

* Version: NA
* GitHub: NA
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
# shinyHeatmaply

<details>

* Version: 0.2.0
* GitHub: https://github.com/yonicd/shinyHeatmaply
* Source code: https://github.com/cran/shinyHeatmaply
* Date/Publication: 2020-04-06 16:40:02 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "shinyHeatmaply")` for more info

</details>

## In both

*   checking whether package ‘shinyHeatmaply’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/shinyHeatmaply/new/shinyHeatmaply.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘shinyHeatmaply’ ...
** package ‘shinyHeatmaply’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘plotly’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 there is no package called ‘jsonlite’
Error: package ‘plotly’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘shinyHeatmaply’
* removing ‘/tmp/workdir/shinyHeatmaply/new/shinyHeatmaply.Rcheck/shinyHeatmaply’


```
### CRAN

```
* installing *source* package ‘shinyHeatmaply’ ...
** package ‘shinyHeatmaply’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘plotly’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 there is no package called ‘jsonlite’
Error: package ‘plotly’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘shinyHeatmaply’
* removing ‘/tmp/workdir/shinyHeatmaply/old/shinyHeatmaply.Rcheck/shinyHeatmaply’


```
# NA

<details>

* Version: NA
* GitHub: NA
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
# NA

<details>

* Version: NA
* GitHub: NA
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
# NA

<details>

* Version: NA
* GitHub: NA
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
# sjPlot

<details>

* Version: 2.8.6
* GitHub: https://github.com/strengejacke/sjPlot
* Source code: https://github.com/cran/sjPlot
* Date/Publication: 2020-10-28 21:40:02 UTC
* Number of recursive dependencies: 202

Run `cloud_details(, "sjPlot")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/sjPlot/new/sjPlot.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘sjPlot/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘sjPlot’ version ‘2.8.6’
* package encoding: UTF-8
* checking package namespace information ... OK
...
  ‘plot_model_estimates.Rmd’ using ‘UTF-8’... OK
  ‘sjtitemanalysis.Rmd’ using ‘UTF-8’... OK
  ‘tab_bayes.Rmd’ using ‘UTF-8’... OK
  ‘tab_mixed.Rmd’ using ‘UTF-8’... OK
  ‘tab_model_estimates.Rmd’ using ‘UTF-8’... OK
  ‘tab_model_robust.Rmd’ using ‘UTF-8’... OK
  ‘table_css.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 2 NOTEs





```
### CRAN

```






```
# STRMPS

<details>

* Version: 0.5.8
* GitHub: NA
* Source code: https://github.com/cran/STRMPS
* Date/Publication: 2018-07-02 08:30:06 UTC
* Number of recursive dependencies: 59

Run `cloud_details(, "STRMPS")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/STRMPS/new/STRMPS.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘STRMPS/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘STRMPS’ version ‘0.5.8’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘ShortRead’

Package suggested but not available for checking: ‘STRaitRazoR’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/STRMPS/old/STRMPS.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘STRMPS/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘STRMPS’ version ‘0.5.8’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘ShortRead’

Package suggested but not available for checking: ‘STRaitRazoR’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# styler

<details>

* Version: 1.3.2
* GitHub: https://github.com/r-lib/styler
* Source code: https://github.com/cran/styler
* Date/Publication: 2020-02-23 05:50:02 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "styler")` for more info

</details>

## In both

*   checking whether package ‘styler’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/styler/new/styler.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘styler’ ...
files ‘man/create_node_from_nested.Rd’, ‘man/if_for_while_part_requires_braces.Rd’, ‘man/split_roxygen_segments.Rd’ have the wrong MD5 checksums
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
Error : /tmp/workdir/styler/new/styler.Rcheck/00_pkg_src/styler/man/add_cache_block.Rd: Sections \title, and \name must exist and be unique in Rd files
ERROR: installing Rd objects failed for package ‘styler’
* removing ‘/tmp/workdir/styler/new/styler.Rcheck/styler’


```
### CRAN

```



```
# sugarbag

<details>

* Version: 0.1.3
* GitHub: https://github.com/srkobakian/sugarbag
* Source code: https://github.com/cran/sugarbag
* Date/Publication: 2020-10-26 14:20:03 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "sugarbag")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/sugarbag/new/sugarbag.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘sugarbag/DESCRIPTION’ ... OK
* this is package ‘sugarbag’ version ‘0.1.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
...
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘Tasmania.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
### CRAN

```






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
# trackr

<details>

* Version: 0.10.6
* GitHub: NA
* Source code: https://github.com/cran/trackr
* Date/Publication: 2020-02-29 05:40:07 UTC
* Number of recursive dependencies: 110

Run `cloud_details(, "trackr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/trackr/new/trackr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘trackr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘trackr’ version ‘0.10.6’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available:
  'histry', 'CodeDepends', 'rsolr', 'roprov'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/trackr/old/trackr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘trackr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘trackr’ version ‘0.10.6’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available:
  'histry', 'CodeDepends', 'rsolr', 'roprov'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# usdampr

<details>

* Version: 1.0.0
* GitHub: https://github.com/cbw1243/usdampr
* Source code: https://github.com/cran/usdampr
* Date/Publication: 2020-06-30 09:10:03 UTC
* Number of recursive dependencies: 29

Run `cloud_details(, "usdampr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/usdampr/new/usdampr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘usdampr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘usdampr’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... NOTE
  Note: found 2 marked UTF-8 strings
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```






```
# vkR

<details>

* Version: 0.2
* GitHub: https://github.com/Dementiy/vkR
* Source code: https://github.com/cran/vkR
* Date/Publication: 2020-09-29 05:20:02 UTC
* Number of recursive dependencies: 49

Run `cloud_details(, "vkR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/vkR/new/vkR.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘vkR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘vkR’ version ‘0.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘XML’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```






```
# wrswoR

<details>

* Version: 1.1.1
* GitHub: https://github.com/krlmlr/wrswoR
* Source code: https://github.com/cran/wrswoR
* Date/Publication: 2020-07-26 18:20:02 UTC
* Number of recursive dependencies: 129

Run `cloud_details(, "wrswoR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/wrswoR/new/wrswoR.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘wrswoR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘wrswoR’ version ‘1.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
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
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/wrswoR/old/wrswoR.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘wrswoR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘wrswoR’ version ‘1.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
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
Status: 1 NOTE





```
