# BgeeDB

<details>

* Version: 2.10.0
* Source code: https://github.com/cran/BgeeDB
* URL: https://github.com/BgeeDB/BgeeDB_R
* BugReports: https://github.com/BgeeDB/BgeeDB_R/issues
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 73

Run `revdep_details(,"BgeeDB")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking top-level files ... NOTE
    ```
    File
      LICENSE
    is not mentioned in the DESCRIPTION file.
    ```

# clusterProfiler

<details>

* Version: 
* Source code: ???
* URL: https://github.com/r-lib/vctrs
* BugReports: https://github.com/r-lib/vctrs/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There is a binary version available but the source version is later:
        binary source needs_compilation
stringi  1.4.5  1.4.6              TRUE

  Binaries will be installed


installing the source package ‘reactome.db’



```
### CRAN

```

  There is a binary version available but the source version is later:
        binary source needs_compilation
stringi  1.4.5  1.4.6              TRUE

  Binaries will be installed


installing the source package ‘reactome.db’



```
# CNPBayes

<details>

* Version: 1.13.5
* Source code: https://github.com/cran/CNPBayes
* URL: https://github.com/scristia/CNPBayes
* BugReports: https://github.com/scristia/CNPBayes/issues
* Date/Publication: 2019-01-05
* Number of recursive dependencies: 162

Run `revdep_details(,"CNPBayes")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘CNPBayes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggChains
    > ### Title: Trace plots of MCMC chains and mixture model densities
    > ### Aliases: ggChains ggMixture ggMixture,MultiBatchCopyNumber-method
    > ###   ggMixture,MultiBatchCopyNumberPooled-method
    > ###   ggMixture,MultiBatchModel-method ggMixture,MultiBatch-method
    > ###   ggMixture,MultiBatchPooled-method ggChains,MultiBatchModel-method
    > ###   ggChains,MultiBatchPooled-method
    > 
    > ### ** Examples
    > 
    >   sb <- SingleBatchModelExample
    >   iter(sb) <- 1000
    >   burnin(sb) <- 100
    >   sb <- posteriorSimulation(sb)
    >   fig.chains <- ggChains(sb)
    Error: 1 components of `...` had unexpected names.
    
    We detected these problematic arguments:
    ```

*   R CMD check timed out
    

*   checking for missing documentation entries ... WARNING
    ```
    ...
      generic 'sigma<-' and siglist 'MultiBatchPooled'
      generic 'sigma2' and siglist 'MultiBatch'
      generic 'sigma2.0' and siglist 'MultiBatch'
      generic 'tau2' and siglist 'MultiBatch'
      generic 'theta' and siglist 'MultiBatch'
      generic 'theta<-' and siglist 'McmcChains,ANY'
      generic 'theta<-' and siglist 'MixtureModel,ANY'
      generic 'theta<-' and siglist 'MultiBatch,matrix'
      generic 'theta<-' and siglist 'MultiBatchModel,ANY'
      generic 'thin' and siglist 'MultiBatch'
      generic 'thin' and siglist 'MultiBatchList'
      generic 'thin<-' and siglist 'McmcParams,numeric'
      generic 'thin<-' and siglist 'MultiBatch,numeric'
      generic 'thin<-' and siglist 'MultiBatchList,numeric'
      generic 'triodata_lrr' and siglist 'TrioBatchModel'
      generic 'z' and siglist 'MultiBatch'
      generic 'zFreq' and siglist 'MultiBatch'
    All user-level objects in a package (including S4 classes and methods) should
    have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R Extensions’
    manual.
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    ...
    Slots for class 'MultiBatch'
      Code: chains current_values data down_sample flags parameters specs
            summaries
      Docs: chains current_values data down_sample flags parameters
            summaries
    
    S4 class codoc mismatches from documentation object 'MultiBatchModel-class':
    Slots for class 'MultiBatchModel'
      Code: .internal.constraint .internal.counter batch batchElements data
            data.mean data.prec hyperparams k label_switch loglik logprior
            marginal_lik mcmc.chains mcmc.params modes mu nu.0 pi
            predictive probz sigma2 sigma2.0 tau2 theta u z zfreq zstar
      Inherited: k hyperparams theta sigma2 nu.0 sigma2.0 pi mu tau2
            predictive zstar data data.mean data.prec z zfreq probz u
            logprior loglik mcmc.chains batch batchElements modes
            mcmc.params label_switch marginal_lik .internal.constraint
            .internal.counter
      Docs: .internal.constraint batch batchElements data data.mean
            data.prec hyperparams is_mendelian k label_switch loglik
            logprior mcmc.chains mcmc.params modes mu nu.0 pi probz sigma2
            sigma2.0 tau2 theta z zfreq
    ```

*   checking Rd \usage sections ... WARNING
    ```
    ...
    
    Documented arguments not in \usage in documentation object 'iter<-':
      ‘force’
    
    Documented arguments not in \usage in documentation object 'mcmcParams':
      ‘force’
    
    Undocumented arguments in documentation object 'sigma<-'
      ‘value’
    
    Undocumented arguments in documentation object 'singleBatchGuided,MultiBatchList,MultiBatch-method'
      ‘x’ ‘guide’
    
    Undocumented arguments in documentation object 'theta'
      ‘value’
    
    Functions with \usage entries need to have the appropriate \alias entries, and
    all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R Extensions’
    manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        doc    3.0Mb
        libs   1.1Mb
        R      3.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RcppArmadillo’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    findSurrogates,MultiBatch: no visible binding for global variable ‘id’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00_pkg_src/CNPBayes/R/methods-SummarizedExperiment.R:127-133)
    findSurrogates,MultiBatch: no visible binding for global variable
      ‘provisional_batch’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00_pkg_src/CNPBayes/R/methods-SummarizedExperiment.R:127-133)
    findSurrogates,MultiBatch: no visible binding for global variable
      ‘batch_labels’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00_pkg_src/CNPBayes/R/methods-SummarizedExperiment.R:127-133)
    findSurrogates,MultiBatch: no visible binding for global variable ‘id’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00_pkg_src/CNPBayes/R/methods-SummarizedExperiment.R:138-142)
    sigma,MultiBatchCopyNumberPooled: no visible binding for global
      variable ‘s2’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00_pkg_src/CNPBayes/R/methods-MultiBatchPooled.R:161)
    Undefined global functions or variables:
      := . .gibbs_trios_mcmc2 .gibbs_trios_mcmc3 batch_index batch_labels
      batches bk copy_number father id log_ratio maplabel medians model
      mother mprob nhom parents prec provisional_batch s s2 snpdat spec
      spec<- t.test value
    Consider adding
      importFrom("stats", "t.test")
    to your NAMESPACE file.
    ```

# detrendr

<details>

* Version: 0.6.4
* Source code: https://github.com/cran/detrendr
* URL: https://rorynolan.github.io/detrendr, https://www.github.com/rorynolan/detrendr
* BugReports: https://www.github.com/rorynolan/detrendr/issues
* Date/Publication: 2019-07-08 16:40:03 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"detrendr")` for more info

</details>

## In both

*   checking whether package ‘detrendr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/detrendr/new/detrendr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘detrendr’ ...
** package ‘detrendr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/Rcpp/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c RcppExports.cpp -o RcppExports.o
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/Rcpp/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c anyNA.cpp -o anyNA.o
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/Rcpp/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c col_stats_parallel.cpp -o col_stats_parallel.o
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/wchar.h:90,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/cwchar:44,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/bits/postypes.h:40,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/iosfwd:40,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/ios:38,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/ostream:38,
                 from /Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include/tthread/tinythread.h:88,
                 from /Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include/RcppParallel/TinyThread.h:8,
                 from /Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include/RcppParallel.h:6,
                 from col_stats_parallel.cpp:5:
/usr/local/Cellar/gcc/9.2.0_1/lib/gcc/9/gcc/x86_64-apple-darwin18/9.2.0/include-fixed/stdio.h:222:7: error: conflicting declaration of 'char* ctermid(char*)' with 'C' linkage
  222 | char *ctermid(char *);
      |       ^~~~~~~
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:525,
                 from /Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include/tthread/tinythread.h:83,
                 from /Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include/RcppParallel/TinyThread.h:8,
                 from /Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include/RcppParallel.h:6,
                 from col_stats_parallel.cpp:5:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_ctermid.h:26:10: note: previous declaration with 'C++' linkage
   26 | char    *ctermid(char *);
      |          ^~~~~~~
make: *** [col_stats_parallel.o] Error 1
ERROR: compilation failed for package ‘detrendr’
* removing ‘/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/detrendr/new/detrendr.Rcheck/detrendr’

```
### CRAN

```
* installing *source* package ‘detrendr’ ...
** package ‘detrendr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/Rcpp/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c RcppExports.cpp -o RcppExports.o
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/Rcpp/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c anyNA.cpp -o anyNA.o
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/Rcpp/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c col_stats_parallel.cpp -o col_stats_parallel.o
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/wchar.h:90,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/cwchar:44,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/bits/postypes.h:40,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/iosfwd:40,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/ios:38,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/ostream:38,
                 from /Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include/tthread/tinythread.h:88,
                 from /Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include/RcppParallel/TinyThread.h:8,
                 from /Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include/RcppParallel.h:6,
                 from col_stats_parallel.cpp:5:
/usr/local/Cellar/gcc/9.2.0_1/lib/gcc/9/gcc/x86_64-apple-darwin18/9.2.0/include-fixed/stdio.h:222:7: error: conflicting declaration of 'char* ctermid(char*)' with 'C' linkage
  222 | char *ctermid(char *);
      |       ^~~~~~~
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:525,
                 from /Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include/tthread/tinythread.h:83,
                 from /Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include/RcppParallel/TinyThread.h:8,
                 from /Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/detrendr/RcppParallel/include/RcppParallel.h:6,
                 from col_stats_parallel.cpp:5:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_ctermid.h:26:10: note: previous declaration with 'C++' linkage
   26 | char    *ctermid(char *);
      |          ^~~~~~~
make: *** [col_stats_parallel.o] Error 1
ERROR: compilation failed for package ‘detrendr’
* removing ‘/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/detrendr/old/detrendr.Rcheck/detrendr’

```
# ELMER

<details>

* Version: 2.8.3
* Source code: https://github.com/cran/ELMER
* Date/Publication: 2019-09-06
* Number of recursive dependencies: 210

Run `revdep_details(,"ELMER")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking Rd \usage sections ... WARNING
    ```
    Undocumented arguments in documentation object 'heatmapGene'
      ‘correlation.method’ ‘scatter.plot.width’ ‘scatter.plot.height’
    
    Functions with \usage entries need to have the appropriate \alias entries, and
    all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R Extensions’
    manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 75.8Mb
      sub-directories of 1Mb or more:
        doc  75.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/GetNearbyGenes.R:538-539)
    getRegionNearGenes: no visible binding for global variable 'ID'
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/GetNearbyGenes.R:551)
    getTFtargets: no visible binding for global variable 'TF'
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/Main_function.R:1620)
    getTFtargets: no visible binding for global variable 'TF'
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/Main_function.R:1622)
    heatmapGene: no visible global function definition for 'melt'
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/plots.R:590)
    heatmapGene: no visible binding for global variable 'mae'
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/plots.R:590)
    heatmapGene: no visible binding for global variable 'mae'
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/plots.R:592)
    heatmapGene: no visible global function definition for 'melt'
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/plots.R:593)
    heatmapGene: no visible binding for global variable 'mae'
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/plots.R:593)
    heatmapGene: no visible global function definition for 'stat_cor'
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/plots.R:597-613)
    Undefined global functions or variables:
      DistanceTSS Hugo_Symbol ID mae melt Side stat_cor TF
    ```

# GEOquery

<details>

* Version: 2.52.0
* Source code: https://github.com/cran/GEOquery
* URL: https://github.com/seandavi/GEOquery
* BugReports: https://github.com/seandavi/GEOquery/issues/new
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 65

Run `revdep_details(,"GEOquery")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 13.9Mb
      sub-directories of 1Mb or more:
        extdata  12.8Mb
    ```

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: no function found corresponding to methods exports from ‘GEOquery’ for: ‘show’
    
    A namespace must be able to be loaded with just the base namespace loaded:
    otherwise if the namespace gets loaded by a saved object, the session will be
    unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘httr’
      All declared Imports should be used.
    Package in Depends field not imported from: ‘methods’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:556-564)
    parseGSEMatrix: no visible binding for global variable ‘accession’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:556-564)
    parseGSEMatrix: no visible binding for global variable ‘accession’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:566-567)
    parseGSEMatrix: no visible global function definition for ‘new’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:598)
    parseGSEMatrix: no visible global function definition for ‘new’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:620)
    parseGSEMatrix: no visible global function definition for ‘new’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:636-641)
    parseGSEMatrix: no visible global function definition for ‘as’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:636-641)
    Undefined global functions or variables:
      . accession as characteristics k kvpair MA new read.delim read.table
      v
    Consider adding
      importFrom("methods", "as", "new")
      importFrom("utils", "read.delim", "read.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field contains
    'methods').
    ```

# GillespieSSA2

<details>

* Version: 0.2.5
* Source code: https://github.com/cran/GillespieSSA2
* URL: http://github.com/rcannood/GillespieSSA2
* BugReports: https://github.com/rcannood/GillespieSSA2/issues
* Date/Publication: 2019-08-22 14:40:03 UTC
* Number of recursive dependencies: 77

Run `revdep_details(,"GillespieSSA2")` for more info

</details>

## In both

*   checking whether package ‘GillespieSSA2’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/GillespieSSA2/new/GillespieSSA2.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘GillespieSSA2’ ...
** package ‘GillespieSSA2’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c RcppExports.cpp -o RcppExports.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c ode_em.cpp -o ode_em.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c ssa_btl.cpp -o ssa_btl.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c ssa_etl.cpp -o ssa_etl.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c ssa_exact.cpp -o ssa_exact.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c ssa_method.cpp -o ssa_method.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c ssa_simulation.cpp -o ssa_simulation.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c test_helper_funs.cpp -o test_helper_funs.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c utils.cpp -o utils.o
g++-9 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -Wlto-type-mismatch -o GillespieSSA2.so RcppExports.o ode_em.o ssa_btl.o ssa_etl.o ssa_exact.o ssa_method.o ssa_simulation.o test_helper_funs.o utils.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/GillespieSSA2/new/GillespieSSA2.Rcheck/00LOCK-GillespieSSA2/00new/GillespieSSA2/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
sh: line 1: 79413 Abort trap: 6           '/Library/Frameworks/R.framework/Resources/bin/R' --no-save --slave 2>&1 < '/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//RtmppuRmPx/file132d26c034e40'
ERROR: loading failed
* removing ‘/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/GillespieSSA2/new/GillespieSSA2.Rcheck/GillespieSSA2’

```
### CRAN

```
* installing *source* package ‘GillespieSSA2’ ...
** package ‘GillespieSSA2’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c RcppExports.cpp -o RcppExports.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c ode_em.cpp -o ode_em.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c ssa_btl.cpp -o ssa_btl.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c ssa_etl.cpp -o ssa_etl.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c ssa_exact.cpp -o ssa_exact.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c ssa_method.cpp -o ssa_method.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c ssa_simulation.cpp -o ssa_simulation.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c test_helper_funs.cpp -o test_helper_funs.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/GillespieSSA2/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c utils.cpp -o utils.o
g++-9 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -Wlto-type-mismatch -o GillespieSSA2.so RcppExports.o ode_em.o ssa_btl.o ssa_etl.o ssa_exact.o ssa_method.o ssa_simulation.o test_helper_funs.o utils.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/GillespieSSA2/old/GillespieSSA2.Rcheck/00LOCK-GillespieSSA2/00new/GillespieSSA2/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
sh: line 1: 79083 Segmentation fault: 11  '/Library/Frameworks/R.framework/Resources/bin/R' --no-save --slave 2>&1 < '/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//RtmpLNKtue/file12fe112cdee74'

 *** caught segfault ***
address 0x70, cause 'memory not mapped'

Traceback:
 1: Module(module, mustStart = TRUE, where = env)
 2: doTryCatch(return(expr), name, parentenv, handler)
 3: tryCatchOne(expr, names, parentenv, handlers[[1L]])
 4: tryCatchList(expr, classes, parentenv, handlers)
 5: tryCatch(Module(module, mustStart = TRUE, where = env), error = function(e) e)
 6: loadModule(module = "gillespie", what = TRUE, env = ns, loadNow = TRUE)
 7: (function (ns) loadModule(module = "gillespie", what = TRUE, env = ns, loadNow = TRUE))(<environment>)
 8: doTryCatch(return(expr), name, parentenv, handler)
 9: tryCatchOne(expr, names, parentenv, handlers[[1L]])
10: tryCatchList(expr, classes, parentenv, handlers)
11: tryCatch((function (ns) loadModule(module = "gillespie", what = TRUE, env = ns, loadNow = TRUE))(<environment>),   ...
12: eval(substitute(tryCatch(FUN(WHERE), error = function(e) e),     list(FUN = f, WHERE = where)), where)
13: eval(substitute(tryCatch(FUN(WHERE), error = function(e) e),     list(FUN = f, WHERE = where)), where)
14: .doLoadActions(where, attach)
15: methods::cacheMetaData(ns, TRUE, ns)
16: loadNamespace(package, lib.loc)
17: doTryCatch(return(expr), name, parentenv, handler)
18: tryCatchOne(expr, names, parentenv, handlers[[1L]])
19: tryCatchList(expr, classes, parentenv, handlers)
20: tryCatch({    attr(package, "LibPath") <- which.lib.loc  ...
21: library(pkg_name, lib.loc = lib, character.only = TRUE, logical.return = TRUE)
22: withCallingHandlers(expr, packageStartupMessage = function(c) invokeRestart("muffleMessage"))
23: suppressPackageStartupMessages(library(pkg_name, lib.loc = lib,     character.only = TRUE, logical.return = TRUE))
24: doTryCatch(return(expr), name, parentenv, handler)
25: tryCatchOne(expr, names, parentenv, handlers[[1L]])
26: tryCatchList(expr, classes, parentenv, handlers)
27: tryCatch(expr, error = function(e) {    call <- conditionCall(e)  ...
28: try(suppressPackageStartupMessages(library(pkg_name, lib.loc = lib,     character.only = TRUE, logical.return = TRUE)))
29: tools:::.test_load_package("GillespieSSA2", "/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/GillespieSSA2/old/GillespieSSA2.Rcheck/00LOCK-GillespieSSA2/00new")
An irrecoverable exception occurred. R is aborting now ...
ERROR: loading failed
* removing ‘/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/GillespieSSA2/old/GillespieSSA2.Rcheck/GillespieSSA2’

```
# maEndToEnd

<details>

* Version: 
* Source code: ???
* URL: https://github.com/r-lib/vctrs
* BugReports: https://github.com/r-lib/vctrs/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There is a binary version available but the source version is later:
        binary source needs_compilation
stringi  1.4.5  1.4.6              TRUE

  Binaries will be installed


installing the source packages ‘hugene10sttranscriptcluster.db’, ‘pd.hugene.1.0.st.v1’, ‘reactome.db’



```
### CRAN

```

  There is a binary version available but the source version is later:
        binary source needs_compilation
stringi  1.4.5  1.4.6              TRUE

  Binaries will be installed


installing the source packages ‘hugene10sttranscriptcluster.db’, ‘pd.hugene.1.0.st.v1’, ‘reactome.db’



```
# MSstats

<details>

* Version: 3.16.2
* Source code: https://github.com/cran/MSstats
* URL: http://msstats.org
* BugReports: https://groups.google.com/forum/#!forum/msstats
* Date/Publication: 2019-08-16
* Number of recursive dependencies: 87

Run `revdep_details(,"MSstats")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking whether package ‘MSstats’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘MASS::select’ by ‘dplyr::select’ when loading ‘MSstats’
    See ‘/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/MSstats/new/MSstats.Rcheck/00install.out’ for details.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘fea’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/MSstats/new/MSstats.Rcheck/00_pkg_src/MSstats/R/SpectronauttoMSstatsFormat.R:196)
    SpectronauttoMSstatsFormat: no visible binding for global variable
      ‘Intensity’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/MSstats/new/MSstats.Rcheck/00_pkg_src/MSstats/R/SpectronauttoMSstatsFormat.R:196)
    SpectronauttoMSstatsFormat: no visible binding for global variable
      ‘PeptideSequence’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/MSstats/new/MSstats.Rcheck/00_pkg_src/MSstats/R/SpectronauttoMSstatsFormat.R:222)
    SpectronauttoMSstatsFormat: no visible binding for global variable
      ‘ProteinName’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/MSstats/new/MSstats.Rcheck/00_pkg_src/MSstats/R/SpectronauttoMSstatsFormat.R:222)
    Undefined global functions or variables:
      .resid ABUNDANCE aggr_Fragment_Annotation aggr_Peak_Area analysis
      censored ciw cover_feature datafeature df_resid fea feature FEATURE
      FRACTION Intensity INTENSITY is_censored is_lowcvr is_obs is_olr
      label LABEL log2inty logFC Mean min_obs missing.col Name nb_feature
      nb_full nb_obs nb_run ncount originalRUN ount peptide PEPTIDE
      PeptideSequence pi_obs protein Protein PROTEIN Protein_number
      ProteinName resid_null residual rlm_fit run RUN s_resid s_resid_eb
      Selected_fragments Selected_peptides shape svar_feature svar_ref
      Train_size var_feature var_resid_eb weight x y ymax ymin
    ```

# SEERaBomb

<details>

* Version: 2019.2
* Source code: https://github.com/cran/SEERaBomb
* URL: http://epbi-radivot.cwru.edu/SEERaBomb/SEERaBomb.html
* Date/Publication: 2019-12-12 18:50:03 UTC
* Number of recursive dependencies: 136

Run `revdep_details(,"SEERaBomb")` for more info

</details>

## In both

*   checking whether package ‘SEERaBomb’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/SEERaBomb/new/SEERaBomb.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SEERaBomb’ ...
** package ‘SEERaBomb’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/SEERaBomb/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c RcppExports.cpp -o RcppExports.o
gcc-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/SEERaBomb/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -flto -mtune=core2  -O3 -c SEERaBomb_init.c -o SEERaBomb_init.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/SEERaBomb/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c fillPYM.cpp -o fillPYM.o
g++-9 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -Wlto-type-mismatch -o SEERaBomb.so RcppExports.o SEERaBomb_init.o fillPYM.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/SEERaBomb/new/SEERaBomb.Rcheck/00LOCK-SEERaBomb/00new/SEERaBomb/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
sh: line 1: 71376 Abort trap: 6           '/Library/Frameworks/R.framework/Resources/bin/R' --no-save --slave 2>&1 < '/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//Rtmp09k8RR/file1160e23d89fc5'
ERROR: loading failed
* removing ‘/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/SEERaBomb/new/SEERaBomb.Rcheck/SEERaBomb’

```
### CRAN

```
* installing *source* package ‘SEERaBomb’ ...
** package ‘SEERaBomb’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/SEERaBomb/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c RcppExports.cpp -o RcppExports.o
gcc-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/SEERaBomb/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -flto -mtune=core2  -O3 -c SEERaBomb_init.c -o SEERaBomb_init.o
g++-9 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/SEERaBomb/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -mtune=core2 -O3 -c fillPYM.cpp -o fillPYM.o
g++-9 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -Wlto-type-mismatch -o SEERaBomb.so RcppExports.o SEERaBomb_init.o fillPYM.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/SEERaBomb/old/SEERaBomb.Rcheck/00LOCK-SEERaBomb/00new/SEERaBomb/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
sh: line 1: 71023 Abort trap: 6           '/Library/Frameworks/R.framework/Resources/bin/R' --no-save --slave 2>&1 < '/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//Rtmpx9mcLL/file112bf5d400429'
ERROR: loading failed
* removing ‘/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/SEERaBomb/old/SEERaBomb.Rcheck/SEERaBomb’

```
# sesame

<details>

* Version: 
* Source code: ???
* URL: https://github.com/r-lib/vctrs
* BugReports: https://github.com/r-lib/vctrs/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There is a binary version available but the source version is later:
        binary source needs_compilation
stringi  1.4.5  1.4.6              TRUE

  Binaries will be installed


installing the source packages ‘FlowSorted.Blood.450k’, ‘FlowSorted.CordBloodNorway.450k’



```
### CRAN

```

  There is a binary version available but the source version is later:
        binary source needs_compilation
stringi  1.4.5  1.4.6              TRUE

  Binaries will be installed


installing the source packages ‘FlowSorted.Blood.450k’, ‘FlowSorted.CordBloodNorway.450k’



```
# sismonr

<details>

* Version: 2.1.0
* Source code: https://github.com/cran/sismonr
* URL: https://oliviaab.github.io/sismonr/
* BugReports: https://github.com/oliviaAB/sismonr/issues
* Date/Publication: 2020-02-11 06:50:02 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"sismonr")` for more info

</details>

## In both

*   R CMD check timed out
    

# TCGAbiolinks

<details>

* Version: 2.12.6
* Source code: https://github.com/cran/TCGAbiolinks
* URL: https://github.com/BioinformaticsFMRP/TCGAbiolinks
* BugReports: https://github.com/BioinformaticsFMRP/TCGAbiolinks/issues
* Date/Publication: 2019-09-05
* Number of recursive dependencies: 265

Run `revdep_details(,"TCGAbiolinks")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 97.3Mb
      sub-directories of 1Mb or more:
        data   4.3Mb
        doc   90.1Mb
        R      2.7Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘move’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/clinical.R:712)
    TCGAtumor_purity: no visible binding for global variable ‘Tumor.purity’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/clinical.R:723-724)
    TCGAvisualize_oncoprint: no visible binding for global variable ‘value’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/visualize.R:932)
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetInduce’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/visualize.R:156-157)
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetPipeline’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/visualize.R:161-162)
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dCommSignif’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/visualize.R:174)
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘visNet’
      (/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/visualize.R:184-189)
    Undefined global functions or variables:
      barcode c3net clinical coordinates dCommSignif dNetInduce
      dNetPipeline exon knnmi.cross limmacontrasts.fit limmamakeContrasts
      minet openSesame portions rse_gene Tumor.purity value visNet
    ```

# trialr

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/trialr
* URL: https://github.com/brockk/trialr
* BugReports: https://github.com/brockk/trialr/issues
* Date/Publication: 2020-01-08 22:30:10 UTC
* Number of recursive dependencies: 102

Run `revdep_details(,"trialr")` for more info

</details>

## In both

*   R CMD check timed out
    

# vroom

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/vroom
* URL: https://github.com/r-lib/vroom
* BugReports: https://github.com/r-lib/vroom/issues
* Date/Publication: 2020-01-13 22:40:02 UTC
* Number of recursive dependencies: 89

Run `revdep_details(,"vroom")` for more info

</details>

## In both

*   checking whether package ‘vroom’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/vroom/new/vroom.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘vroom’ ...
** package ‘vroom’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c Iconv.cpp -o Iconv.o
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c LocaleInfo.cpp -o LocaleInfo.o
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c RcppExports.cpp -o RcppExports.o
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c altrep.cc -o altrep.o
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c delimited_index.cc -o delimited_index.o
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/wchar.h:90,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/cwchar:44,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/bits/postypes.h:40,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/iosfwd:40,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/ios:38,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/ostream:38,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/iterator:64,
                 from mio/include/mio/mmap.hpp:26,
                 from mio/include/mio/shared_mmap.hpp:24,
                 from delimited_index.h:12,
                 from delimited_index.cc:1:
/usr/local/Cellar/gcc/9.2.0_1/lib/gcc/9/gcc/x86_64-apple-darwin18/9.2.0/include-fixed/stdio.h:222:7: error: conflicting declaration of 'char* ctermid(char*)' with 'C' linkage
  222 | char *ctermid(char *);
      |       ^~~~~~~
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:525,
                 from mio/include/mio/page.hpp:27,
                 from mio/include/mio/mmap.hpp:24,
                 from mio/include/mio/shared_mmap.hpp:24,
                 from delimited_index.h:12,
                 from delimited_index.cc:1:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_ctermid.h:26:10: note: previous declaration with 'C++' linkage
   26 | char    *ctermid(char *);
      |          ^~~~~~~
make: *** [delimited_index.o] Error 1
ERROR: compilation failed for package ‘vroom’
* removing ‘/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/vroom/new/vroom.Rcheck/vroom’

```
### CRAN

```
* installing *source* package ‘vroom’ ...
** package ‘vroom’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c Iconv.cpp -o Iconv.o
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c LocaleInfo.cpp -o LocaleInfo.o
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c RcppExports.cpp -o RcppExports.o
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c altrep.cc -o altrep.o
g++-9 -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/vctrs/revdep-all/tidyr/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c delimited_index.cc -o delimited_index.o
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/wchar.h:90,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/cwchar:44,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/bits/postypes.h:40,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/iosfwd:40,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/ios:38,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/ostream:38,
                 from /usr/local/Cellar/gcc/9.2.0_1/include/c++/9.2.0/iterator:64,
                 from mio/include/mio/mmap.hpp:26,
                 from mio/include/mio/shared_mmap.hpp:24,
                 from delimited_index.h:12,
                 from delimited_index.cc:1:
/usr/local/Cellar/gcc/9.2.0_1/lib/gcc/9/gcc/x86_64-apple-darwin18/9.2.0/include-fixed/stdio.h:222:7: error: conflicting declaration of 'char* ctermid(char*)' with 'C' linkage
  222 | char *ctermid(char *);
      |       ^~~~~~~
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:525,
                 from mio/include/mio/page.hpp:27,
                 from mio/include/mio/mmap.hpp:24,
                 from mio/include/mio/shared_mmap.hpp:24,
                 from delimited_index.h:12,
                 from delimited_index.cc:1:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_ctermid.h:26:10: note: previous declaration with 'C++' linkage
   26 | char    *ctermid(char *);
      |          ^~~~~~~
make: *** [delimited_index.o] Error 1
ERROR: compilation failed for package ‘vroom’
* removing ‘/Users/lionel/Desktop/vctrs/revdep-all/tidyr/checks.noindex/vroom/old/vroom.Rcheck/vroom’

```
