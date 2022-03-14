# bignum

<details>

* Version: 0.3.0
* GitHub: https://github.com/davidchall/bignum
* Source code: https://github.com/cran/bignum
* Date/Publication: 2021-10-15 04:50:02 UTC
* Number of recursive dependencies: 49

Run `cloud_details(, "bignum")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bignum-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: biginteger
    > ### Title: Arbitrary-Precision Integer Vectors
    > ### Aliases: biginteger as_biginteger is_biginteger
    > 
    > ### ** Examples
    > 
    > # default options limit displayed precision
    ...
    > 
    > # display full precision
    > format(biginteger(2)^50L, notation = "dec")
    [1] "1125899906842624"
    > 
    > # lossy casts raise a warning
    > biginteger(c(2, 2.5, 3))
    Error in if (nzchar(arg)) { : argument is of length zero
    Calls: biginteger ... signal_abort -> signalCondition -> <Anonymous> -> warn
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       26. └─bignum `<fn>`(`<vctrs___>`)
       27.   └─rlang::warn(error = err, class = "bignum_warning_cast_lossy")
       28.     └─base::warning(cnd)
       29.       ├─base::conditionMessage(cond)
       30.       └─bignum:::conditionMessage.bignum_warning_cast_lossy(cond)
       31.         └─rlang::cnd_message(c)
       32.           └─rlang:::cnd_message_format(cnd, ...)
       33.             └─rlang:::cnd_message_lines(cnd, ...)
       34.               ├─rlang::cnd_header(cnd, ...)
       35.               └─bignum:::cnd_header.bignum_warning_cast_lossy(cnd, ...)
       36.                 └─bignum:::format_arg_label(vec_ptype_full(cnd$error$to), cnd$error$to_arg)
      
      [ FAIL 6 | WARN 0 | SKIP 32 | PASS 375 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 14.4Mb
      sub-directories of 1Mb or more:
        libs  14.1Mb
    ```

# cutpointr

<details>

* Version: 1.1.1
* GitHub: https://github.com/thie1e/cutpointr
* Source code: https://github.com/cran/cutpointr
* Date/Publication: 2021-06-29 06:30:02 UTC
* Number of recursive dependencies: 82

Run `cloud_details(, "cutpointr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘cutpointr.Rmd’ using rmarkdown
    Assuming the positive class is yes
    Assuming the positive class has higher x values
    
     *** caught segfault ***
    address (nil), cause 'memory not mapped'
    
    Traceback:
     1: vec_unchop(col, ptype = col_ptype)
    ...
    27: rmarkdown::render(file, encoding = encoding, quiet = quiet, envir = globalenv(),     output_dir = getwd(), ...)
    28: vweave_rmarkdown(...)
    29: engine$weave(file, quiet = quiet, encoding = enc)
    30: doTryCatch(return(expr), name, parentenv, handler)
    31: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    32: tryCatchList(expr, classes, parentenv, handlers)
    33: tryCatch({    engine$weave(file, quiet = quiet, encoding = enc)    setwd(startdir)    output <- find_vignette_product(name, by = "weave", engine = engine)    if (!have.makefile && vignette_is_tex(output)) {        texi2pdf(file = output, clean = FALSE, quiet = quiet)        output <- find_vignette_product(name, by = "texi2pdf",             engine = engine)    }    outputs <- c(outputs, output)}, error = function(e) {    thisOK <<- FALSE    fails <<- c(fails, file)    message(gettextf("Error: processing vignette '%s' failed with diagnostics:\n%s",         file, conditionMessage(e)))})
    34: tools:::buildVignettes(dir = "/tmp/workdir/cutpointr/new/cutpointr.Rcheck/vign_test/cutpointr",     ser_elibs = "/tmp/RtmpiYeVjQ/filefbd4efcc90a.rds")
    An irrecoverable exception occurred. R is aborting now ...
    Segmentation fault (core dumped)
    ```

# dibble

<details>

* Version: 0.1.0
* GitHub: https://github.com/UchidaMizuki/dibble
* Source code: https://github.com/cran/dibble
* Date/Publication: 2022-02-14 10:20:05 UTC
* Number of recursive dependencies: 45

Run `cloud_details(, "dibble")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `names(dim(expected))` is absent
      ── Failure (test-broadcast.R:9:3): broadcast ───────────────────────────────────
      as.array(y) (`actual`) not equal to array(1:3, 3) (`expected`).
      
      `names(dim(actual))` is a character vector ('axis2')
      `names(dim(expected))` is absent
      ── Failure (test-matrix.R:12:3): diag ──────────────────────────────────────────
      as.array(ddf_col) (`actual`) not equal to `arr` (`expected`).
      
      `names(dim(actual))` is a character vector ('axis1', 'axis2')
      `names(dim(expected))` is absent
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 28 ]
      Error: Test failures
      Execution halted
    ```

# redist

<details>

* Version: 3.1.5
* GitHub: https://github.com/alarm-redist/redist
* Source code: https://github.com/cran/redist
* Date/Publication: 2021-10-05 23:20:19 UTC
* Number of recursive dependencies: 147

Run `cloud_details(, "redist")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      31: test_code(NULL, exprs, env)
      32: source_file(path, child_env(env), wrap = wrap)
      33: FUN(X[[i]], ...)
      34: lapply(test_paths, test_one_file, env = env, wrap = wrap)
      35: doTryCatch(return(expr), name, parentenv, handler)
      36: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      37: tryCatchList(expr, classes, parentenv, handlers)
      38: tryCatch(code, testthat_abort_reporter = function(cnd) {    cat(conditionMessage(cnd), "\n")    NULL})
      39: with_reporter(reporters$multi, lapply(test_paths, test_one_file,     env = env, wrap = wrap))
      40: test_files(test_dir = test_dir, test_package = test_package,     test_paths = test_paths, load_helpers = load_helpers, reporter = reporter,     env = env, stop_on_failure = stop_on_failure, stop_on_warning = stop_on_warning,     wrap = wrap, load_package = load_package)
      41: test_files(test_dir = path, test_paths = test_paths, test_package = package,     reporter = reporter, load_helpers = load_helpers, env = env,     stop_on_failure = stop_on_failure, stop_on_warning = stop_on_warning,     wrap = wrap, load_package = load_package, parallel = parallel)
      42: test_dir("testthat", package = package, reporter = reporter,     ..., load_package = "installed")
      43: test_check("redist")
      An irrecoverable exception occurred. R is aborting now ...
      Segmentation fault (core dumped)
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘glossary.Rmd’ using rmarkdown
    --- finished re-building ‘glossary.Rmd’
    
    --- re-building ‘map-preproc.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
    ...
    35: tryCatchList(expr, classes, parentenv, handlers)
    36: tryCatch({    engine$weave(file, quiet = quiet, encoding = enc)    setwd(startdir)    output <- find_vignette_product(name, by = "weave", engine = engine)    if (!have.makefile && vignette_is_tex(output)) {        texi2pdf(file = output, clean = FALSE, quiet = quiet)        output <- find_vignette_product(name, by = "texi2pdf",             engine = engine)    }}, error = function(e) {    OK <<- FALSE    message(gettextf("Error: processing vignette '%s' failed with diagnostics:\n%s",         file, conditionMessage(e)))})
    37: tools:::.buildOneVignette("redist.Rmd", "/tmp/workdir/redist/new/redist.Rcheck/vign_test/redist",     TRUE, FALSE, "redist", "UTF-8", "/tmp/RtmpLzMp38/file21021196066.rds")
    An irrecoverable exception occurred. R is aborting now ...
    Segmentation fault (core dumped)
    SUMMARY: processing the following files failed:
      ‘map-preproc.Rmd’ ‘redist.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 36.0Mb
      sub-directories of 1Mb or more:
        libs  32.3Mb
    ```

# taxa

<details>

* Version: 0.4.1
* GitHub: https://github.com/ropensci/taxa
* Source code: https://github.com/cran/taxa
* Date/Publication: 2022-03-11 22:30:02 UTC
* Number of recursive dependencies: 57

Run `cloud_details(, "taxa")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test--taxon_authority.R:193:3): taxon_authority objects can be converted to factor ──
      as.factor(x) not equal to as.factor(...).
      Attributes: < Component "levels": 3 string mismatches >
      ── Failure (test--taxonomy.R:364:3): taxonomy objects can be converted to factor ──
      as.factor(x) not equal to as.factor(as.character(x)).
      Attributes: < Component "levels": 5 string mismatches >
      ── Failure (test--taxonomy.R:372:3): named taxonomy objects can be converted to factor ──
      as.factor(x) not equal to as.factor(as.character(x)).
      Attributes: < Component "levels": 5 string mismatches >
      
      [ FAIL 3 | WARN 0 | SKIP 11 | PASS 406 ]
      Error: Test failures
      Execution halted
    ```

# tibblify

<details>

* Version: 0.1.0
* GitHub: https://github.com/mgirlich/tibblify
* Source code: https://github.com/cran/tibblify
* Date/Publication: 2020-09-23 08:40:07 UTC
* Number of recursive dependencies: 62

Run `cloud_details(, "tibblify")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (4)
      • lcol_fct not yet implemented (3)
      • no good way to present error message yet (1)
      • not yet decided what result should be (1)
      • not yet testable (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-guess_col.R:61:3): lists work ─────────────────────────────────
      `guess_col(list(a = list(a = 1), list(a = 2)), "a")` did not throw an error.
      
      [ FAIL 1 | WARN 0 | SKIP 10 | PASS 83 ]
      Error: Test failures
      Execution halted
    ```

# winch

<details>

* Version: 0.0.7
* GitHub: https://github.com/r-prof/winch
* Source code: https://github.com/cran/winch
* Date/Publication: 2021-10-24 09:20:02 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "winch")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘example0.R’
      Running ‘example1.R’
      Running ‘example2.R’
      Running ‘example3.R’
    Running the tests in ‘tests/example3.R’ failed.
    Last 13 lines of output:
      +     rlang_backtrace_on_error = "full",
      +     rlang_trace_use_winch = TRUE
      +   )
      + 
    ...
      + 
      +   tryCatch(vctrs::vec_as_location(quote, 2), error = identity)
      + }
      Loading required package: vctrs
      Error in winch::winch_init_library(vctrs:::vctrs_init$dll[["path"]]) : 
        object 'vctrs_init' not found
      Backtrace:
          ▆
       1. └─winch::winch_init_library(vctrs:::vctrs_init$dll[["path"]])
      Execution halted
    ```

