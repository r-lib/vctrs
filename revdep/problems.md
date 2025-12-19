# blob (1.2.4)

* GitHub: <https://github.com/tidyverse/blob>
* Email: <mailto:kirill@cynkra.com>
* GitHub mirror: <https://github.com/cran/blob>

Run `revdepcheck::cloud_details(, "blob")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
       Running ‘testthat.R’
     Running the tests in ‘tests/testthat.R’ failed.
     Complete output:
       > library(testthat)
       > library(blob)
       > 
       > test_check("blob")
       Saving _problems/test-accessors-39.R
       [ FAIL 1 | WARN 0 | SKIP 3 | PASS 35 ]
       
       ══ Skipped tests (3) ═══════════════════════════════════════════════════════════
       • On CRAN (3): 'test-format.R:11:3', 'test-format.R:35:3', 'test-format.R:59:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-accessors.R:39:3'): can insert raw or NULL ───────────────────
       Expected `x` to equal `blob(as.raw(0), as.raw(0), NULL, NULL)`.
       Differences:
       Component 3: Modes: raw, NULL
       Component 3: Lengths: 1, 0
       Component 3: target is raw, current is NULL
       
       [ FAIL 1 | WARN 0 | SKIP 3 | PASS 35 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# crmPack (2.0.1)

* GitHub: <https://github.com/openpharma/crmPack>
* Email: <mailto:daniel.sabanes_bove@rconis.com>
* GitHub mirror: <https://github.com/cran/crmPack>

Run `revdepcheck::cloud_details(, "crmPack")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > 
     > # Run the simulation on the desired design
     > # We only generate 1 trial outcome here for illustration, for the actual study
     > # this should be increased of course, similarly for the McmcOptions -
     > # they also need to be increased.
     > mySims <- simulate(
     +   design,
     +   trueTox = trueTox,
     +   trueBiomarker = trueBiomarker,
     +   sigma2W = 0.01,
     +   rho = 0,
     +   nsim = 1,
     +   parallel = FALSE,
     +   seed = 3,
     +   startingDose = 6,
     +   mcmcOptions = McmcOptions(
     +     burnin = 100,
     +     step = 1,
     +     samples = 300
     +   )
     + )
     Error in matrix(rnorm(n * ncol(sigma)), nrow = n, byrow = !pre0.9_9994) %*%  : 
       non-conformable arguments
     Calls: simulate ... .local -> get_result_list -> lapply -> FUN -> <Anonymous>
     Execution halted
     ```

# diceR (3.1.0)

* GitHub: <https://github.com/AlineTalhouk/diceR>
* Email: <mailto:dchiu@bccrc.ca>
* GitHub mirror: <https://github.com/cran/diceR>

Run `revdepcheck::cloud_details(, "diceR")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       > # It is recommended that you do not modify it.
       > #
       > # Where should you do additional test configuration?
       > # Learn more about the roles of various files in:
       > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
       > # * https://testthat.r-lib.org/articles/special-files.html
       > 
       > library(testthat)
       > library(diceR)
       > 
       > test_check("diceR")
       Saving _problems/test-consensus_combine-52.R
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 113 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-consensus_combine.R:51:3'): reweighing (potentially) replicates each slice of algorithm ──
       <purrr_error_indexed/rlang_error/error/condition>
       Error in `purrr::map(k, consensus_trim, E = E, ii = ii, k.method = k.method, reweigh = reweigh, n = n)`: i In index: 1.
       Caused by error in `list_flatten()`:
       ! `x` must be a node.
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 113 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# feisr (1.3.0)

* GitHub: <https://github.com/ruettenauer/feisr>
* Email: <mailto:ruettenauer@sowi.uni-kl.de>
* GitHub mirror: <https://github.com/cran/feisr>

Run `revdepcheck::cloud_details(, "feisr")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘feisr-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: detrend
     > ### Title: Detrend data by individual slopes
     > ### Aliases: detrend
     > 
     > ### ** Examples
     > 
     > data("mwp", package = "feisr")
     > 
     > # Detrend entire data.frame
     > mwp_det <- detrend(data = mwp, slopes = c("exp", "expq"), id = "id")
     Error in `list_flatten()`:
     ! `x` must be a list, not a list matrix.
     Backtrace:
         ▆
      1. ├─feisr::detrend(data = mwp, slopes = c("exp", "expq"), id = "id")
      2. │ └─dplyr::bind_rows(rbind(dhat), .id = NULL)
      3. │   └─dplyr:::list_flatten(dots, fn = is_flattenable)
      4. │     └─vctrs::obj_check_list(x)
      5. └─vctrs:::stop_non_list_type(x, y, z)
      6.   └─cli::cli_abort(...)
      7.     └─rlang::abort(...)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       Backtrace:
           ▆
        1. ├─feisr::feis(...) at test_slopes.R:14:1
        2. │ └─dplyr::bind_rows(rbind(dhat), .id = NULL)
        3. │   └─dplyr:::list_flatten(dots, fn = is_flattenable)
        4. │     └─vctrs::obj_check_list(x)
        5. └─vctrs:::stop_non_list_type(x, y, z)
        6.   └─cli::cli_abort(...)
        7.     └─rlang::abort(...)
       ── Error ('test_weights.R:19:1'): (code run outside of `test_that()`) ──────────
       Error in `list_flatten(dots, fn = is_flattenable)`: `x` must be a list, not a list matrix.
       Backtrace:
           ▆
        1. ├─feisr::feis(...) at test_weights.R:19:1
        2. │ └─dplyr::bind_rows(rbind(dhat), .id = NULL)
        3. │   └─dplyr:::list_flatten(dots, fn = is_flattenable)
        4. │     └─vctrs::obj_check_list(x)
        5. └─vctrs:::stop_non_list_type(x, y, z)
        6.   └─cli::cli_abort(...)
        7.     └─rlang::abort(...)
       
       [ FAIL 9 | WARN 0 | SKIP 0 | PASS 0 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘feisr-vignette.Rmd’ using rmarkdown
     
     Quitting from feisr-vignette.Rmd:81-84 [feis0]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'feisr-vignette.Rmd' failed with diagnostics:
     `x` must be a list, not a list matrix.
     --- failed re-building ‘feisr-vignette.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘feisr-vignette.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# ggstats (0.11.0)

* GitHub: <https://github.com/larmarange/ggstats>
* Email: <mailto:joseph@larmarange.net>
* GitHub mirror: <https://github.com/cran/ggstats>

Run `revdepcheck::cloud_details(, "ggstats")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
       4. │ └─ggplot2 (local) `ggplot_gtable.ggplot2::ggplot_built`(data)
       5. │   └─ggplot2:::by_layer(...)
       6. │     ├─rlang::try_fetch(...)
       7. │     │ ├─base::tryCatch(...)
       8. │     │ │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
       9. │     │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
      10. │     │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
      11. │     │ └─base::withCallingHandlers(...)
      12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
      13. │       └─l$draw_geom(d, layout)
      14. │         └─ggplot2 (local) draw_geom(..., self = self)
      15. │           └─self$geom$draw_layer(...)
      16. │             └─ggplot2 (local) draw_layer(..., self = self)
      17. │               └─base::lapply(...)
      18. │                 └─ggplot2 (local) FUN(X[[i]], ...)
      19. │                   ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
      20. │                   └─self$draw_panel(...)
      21. │                     └─ggstats (local) draw_panel(...)
      22. │                       └─dplyr::bind_rows(...)
      23. │                         └─dplyr:::list_flatten(dots, fn = is_flattenable)
      24. │                           └─vctrs::obj_check_list(x)
      25. └─vctrs:::stop_non_list_type(x, y, z)
      26.   └─cli::cli_abort(...)
      27.     └─rlang::abort(...)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       [ FAIL 1 | WARN 0 | SKIP 21 | PASS 17 ]
       
       ══ Skipped tests (21) ══════════════════════════════════════════════════════════
       • On CRAN (21): 'test-geom_connector.R:2:3', 'test-geom_stripped.R:2:3',
         'test-ggcascade.R:2:3', 'test-ggcoef_model.R:2:3',
         'test-ggcoef_model.R:163:3', 'test-ggcoef_model.R:229:3',
         'test-ggcoef_model.R:272:3', 'test-ggcoef_model.R:308:3',
         'test-ggcoef_model.R:363:3', 'test-ggcoef_model.R:454:3',
         'test-gglikert.R:2:3', 'test-position_likert.R:2:3',
         'test-position_likert.R:92:1', 'test-stat_cross.R:2:3',
         'test-stat_prop.R:2:3', 'test-stat_prop.R:66:3', 'test-stat_prop.R:79:3',
         'test-stat_prop.R:96:3', 'test-stat_prop.R:122:3',
         'test-stat_weighted_mean.R:2:3', 'test_ggsurvey.R:2:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-stat_prop.R:170:3'): geom_prop_bar() & geom_prop_text() & geom_prop_connector() ──
       Error in `geom_bar_connector(mapping = mapping, data = data, position = position, complete = complete, default_by = default_by, stat = StatPropProp, width = width, ...)`: Problem while converting geom to grob.
       i Error occurred in the 3rd layer.
       Caused by error in `list_flatten()`:
       ! `x` must be a list, not a list 1D array.
       
       [ FAIL 1 | WARN 0 | SKIP 21 | PASS 17 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# klassR (1.0.4)

* GitHub: <https://github.com/statisticsnorway/ssb-klassr>
* Email: <mailto:susie.jentoft@ssb.no>
* GitHub mirror: <https://github.com/cran/klassR>

Run `revdepcheck::cloud_details(, "klassR")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
       Running ‘testthat.R’
     Running the tests in ‘tests/testthat.R’ failed.
     Complete output:
       > library(testthat)
       > library(klassR)
       > 
       > test_check("klassR")
       Connection failed with error code 404
       Stream error in the HTTP/2 framing layer [data.ssb.no]:
       HTTP/2 stream 223 was not closed cleanly: INTERNAL_ERROR (err 2)
       Saving _problems/test_ListKlass-17.R
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 94 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test_ListKlass.R:17:3'): list_klass returns a list ──────────────────
       Error in `data.frame(klass_name = dt$name, klass_nr = nums, klass_family = i, klass_type = dt$classificationType)`: arguments imply differing number of rows: 0, 1
       Backtrace:
           ▆
        1. └─klassR::list_klass(codelists = TRUE) at test_ListKlass.R:17:3
        2.   └─base::data.frame(...)
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 94 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# merTools (0.6.3)

* GitHub: <https://github.com/jknowles/merTools>
* Email: <mailto:jared@civilytics.com>
* GitHub mirror: <https://github.com/cran/merTools>

Run `revdepcheck::cloud_details(, "merTools")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘merTools-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: REsim
     > ### Title: Simulate random effects from merMod 'REsim' simulates random
     > ###   effects from merMod object posterior distributions
     > ### Aliases: REsim
     > 
     > ### ** Examples
     > 
     > require(lme4)
     > m2 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
     > re2 <- REsim(m2, 25)
     Error in `list_flatten()`:
     ! `x` must be a list, not a list matrix.
     Backtrace:
         ▆
      1. ├─merTools::REsim(m2, 25)
      2. │ └─dplyr::bind_rows(zed)
      3. │   └─dplyr:::list_flatten(dots, fn = is_flattenable)
      4. │     └─vctrs::obj_check_list(x)
      5. └─vctrs:::stop_non_list_type(x, y, z)
      6.   └─cli::cli_abort(...)
      7.     └─rlang::abort(...)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
         'test-helpers.R:37:3', 'test-helpers.R:72:3', 'test-helpers.R:90:3',
         'test-helpers.R:124:3', 'test-expectedRank.R:12:3',
         'test-expectedRank.R:83:3', 'test-merModList.R:10:3',
         'test-merModList.R:46:3', 'test-merModList.R:72:3', 'test-merModList.R:128:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-merExtract.R:88:3'): REsim produces data.frames ────────────────
       Error in `list_flatten(dots, fn = is_flattenable)`: `x` must be a list, not a list matrix.
       Backtrace:
            ▆
         1. ├─testthat::expect_s3_class(REsim(lmerSlope1, n.sims = 100), "data.frame") at test-merExtract.R:88:3
         2. │ └─testthat::quasi_label(enquo(object))
         3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
         4. ├─merTools::REsim(lmerSlope1, n.sims = 100)
         5. │ └─dplyr::bind_rows(zed)
         6. │   └─dplyr:::list_flatten(dots, fn = is_flattenable)
         7. │     └─vctrs::obj_check_list(x)
         8. └─vctrs:::stop_non_list_type(x, y, z)
         9.   └─cli::cli_abort(...)
        10.     └─rlang::abort(...)
       
       [ FAIL 1 | WARN 0 | SKIP 12 | PASS 234 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# myTAI (2.3.4)

* GitHub: <https://github.com/drostlab/myTAI>
* Email: <mailto:hajk-georg.drost@tuebingen.mpg.de>
* GitHub mirror: <https://github.com/cran/myTAI>

Run `revdepcheck::cloud_details(, "myTAI")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘myTAI.Rmd’ using rmarkdown
     ```

# orderly (2.0.0)

* GitHub: <https://github.com/mrc-ide/orderly>
* Email: <mailto:rich.fitzjohn@gmail.com>
* GitHub mirror: <https://github.com/cran/orderly>

Run `revdepcheck::cloud_details(, "orderly")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-util.R:676:3'): replace_ragged with an empty result preserves the input's type ──
       <vctrs_error_cast/vctrs_error_incompatible_type/vctrs_error_incompatible/vctrs_error/rlang_error/error/condition>
       Error in `vctrs::list_unchop(ret, ptype = vctrs::vec_ptype(x))`: Can't convert `x[[1]]` <double> to <character>.
       Backtrace:
            ▆
         1. ├─orderly:::replace_ragged("foo", 1, list(numeric(0))) at test-util.R:676:3
         2. │ └─vctrs::list_unchop(ret, ptype = vctrs::vec_ptype(x))
         3. └─vctrs (local) `<fn>`()
         4.   └─vctrs::vec_default_cast(...)
         5.     ├─base::withRestarts(...)
         6.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
         7.     │   └─base (local) doWithOneRestart(return(expr), restart)
         8.     └─vctrs::stop_incompatible_cast(...)
         9.       └─vctrs::stop_incompatible_type(...)
        10.         └─vctrs:::stop_incompatible(...)
        11.           └─vctrs:::stop_vctrs(...)
        12.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 1 | WARN 0 | SKIP 17 | PASS 2204 ]
       Error:
       ! Test failures.
       Execution halted
       Ran 16/16 deferred expressions
     ```

# purrr (1.2.0)

* GitHub: <https://github.com/tidyverse/purrr>
* Email: <mailto:hadley@posit.co>
* GitHub mirror: <https://github.com/cran/purrr>

Run `revdepcheck::cloud_details(, "purrr")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > ### Name: array-coercion
     > ### Title: Coerce array to list
     > ### Aliases: array-coercion array_branch array_tree
     > 
     > ### ** Examples
     > 
     > # We create an array with 3 dimensions
     > x <- array(1:12, c(2, 2, 3))
     > 
     > # A full margin for such an array would be the vector 1:3. This is
     > # the default if you don't specify a margin
     > 
     > # Creating a branch along the full margin is equivalent to
     > # as.list(array) and produces a list of size length(x):
     > array_branch(x) |> str()
     Error in `list_flatten()`:
     ! `x` must be a node.
     Backtrace:
         ▆
      1. ├─utils::str(array_branch(x))
      2. └─purrr::array_branch(x)
      3.   └─purrr::list_flatten(apply(array, margin, list))
      4.     └─cli::cli_abort("{.arg x} must be a node.")
      5.       └─rlang::abort(...)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       Backtrace:
           ▆
        1. ├─testthat::expect_length(array_branch(x, m1), prod(dim(x)[m1])) at test-arrays.R:23:3
        2. │ └─testthat::quasi_label(enquo(object))
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. └─purrr::array_branch(x, m1)
        5.   └─purrr::list_flatten(apply(array, margin, list))
        6.     └─cli::cli_abort("{.arg x} must be a node.")
        7.       └─rlang::abort(...)
       ── Error ('test-arrays.R:30:3'): array_branch retains dimnames when going over one dimension ──
       Error in `list_flatten(apply(array, margin, list))`: `x` must be a node.
       Backtrace:
           ▆
        1. ├─testthat::expect_identical(...) at test-arrays.R:30:3
        2. │ └─testthat::quasi_label(enquo(object), label)
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. └─purrr::array_branch(x, 2:3)
        5.   └─purrr::list_flatten(apply(array, margin, list))
        6.     └─cli::cli_abort("{.arg x} must be a node.")
        7.       └─rlang::abort(...)
       
       [ FAIL 3 | WARN 0 | SKIP 135 | PASS 824 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# rainette (0.3.1.1)

* GitHub: <https://github.com/juba/rainette>
* Email: <mailto:julien.barnier@cnrs.fr>
* GitHub mirror: <https://github.com/cran/rainette>

Run `revdepcheck::cloud_details(, "rainette")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       ℹ In argument: `chi2 = sum(cross_groups$chi2[.data$clusters])`.
       ℹ In row 1.
       Caused by error in `cross_groups$chi2[.data$clusters]`:
       ! invalid subscript type 'list'
       
       [ FAIL 2 | WARN 0 | SKIP 1 | PASS 126 ]
       Deleting unused snapshots: 'plots/base-rainette-plot-measure-docprop.svg',
       'plots/base-rainette-plot-measure-frequency.svg',
       'plots/base-rainette-plot-measure-lr.svg',
       'plots/base-rainette-plot-with-free-scales.svg',
       'plots/base-rainette-plot-with-k-and-without-negative.svg',
       'plots/base-rainette-plot-with-k-n-terms-and-font-size.svg',
       'plots/base-rainette-plot.svg',
       'plots/base-rainette2-plot-measure-docprop.svg',
       'plots/base-rainette2-plot-measure-frequency.svg',
       'plots/base-rainette2-plot-measure-lr.svg',
       'plots/base-rainette2-plot-with-complete-groups.svg',
       'plots/base-rainette2-plot-with-free-scales.svg',
       'plots/base-rainette2-plot-with-k-5.svg',
       'plots/base-rainette2-plot-with-k-and-without-negative.svg',
       'plots/base-rainette2-plot-with-k-n-terms-and-font-size.svg', and
       'plots/base-rainette2-plot.svg'
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     Caused by error in `cross_groups$chi2[.data$clusters]`:
     ! invalid subscript type 'list'
     --- failed re-building ‘introduction_en.Rmd’
     
     --- re-building ‘introduction_usage.Rmd’ using rmarkdown
     
     Quitting from introduction_usage.Rmd:186-188 [unnamed-chunk-19]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'introduction_usage.Rmd' failed with diagnostics:
     ℹ In index: 1.
     Caused by error in `dplyr::mutate()`:
     ℹ In argument: `chi2 = sum(cross_groups$chi2[.data$clusters])`.
     ℹ In row 1.
     Caused by error in `cross_groups$chi2[.data$clusters]`:
     ! invalid subscript type 'list'
     --- failed re-building ‘introduction_usage.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘introduction_en.Rmd’ ‘introduction_usage.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# riskmetric (0.2.5)

* GitHub: <https://github.com/pharmaR/riskmetric>
* Email: <mailto:eli.miller@atorusresearch.com>
* GitHub mirror: <https://github.com/cran/riskmetric>

Run `revdepcheck::cloud_details(, "riskmetric")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         2. │ └─testthat::test_dir(...)
         3. │   └─testthat:::test_files(...)
         4. │     └─testthat:::test_files_serial(...)
         5. │       └─testthat:::test_files_setup_state(...)
         6. │         └─testthat::source_test_setup(".", env)
         7. │           └─testthat::source_dir(path, "^setup.*\\.[rR]$", env = env, wrap = FALSE)
         8. │             └─base::lapply(...)
         9. │               └─testthat (local) FUN(X[[i]], ...)
        10. │                 └─testthat::source_file(...)
        11. │                   ├─base::withCallingHandlers(...)
        12. │                   └─base::eval(exprs, env)
        13. │                     └─base::eval(exprs, env)
        14. │                       └─riskmetric::pkg_ref(c("utils", "tools"), source = "pkg_install") at ./setup_test_packages.R:58:1
        15. │                         └─riskmetric::as_pkg_ref(x, ...)
        16. │                           └─vctrs::new_list_of(pkg_ref_list, ptype = pkg_ref(), class = "list_of_pkg_ref")
        17. │                             └─vctrs::vec_ptype(ptype, x_arg = "ptype")
        18. │                               └─vctrs (local) `<fn>`()
        19. │                                 ├─base::names(x = x)
        20. │                                 └─riskmetric:::names.pkg_ref(x = x)
        21. │                                   └─riskmetric:::bare_env(x, names(x))
        22. └─base::.handleSimpleError(...)
        23.   └─testthat (local) h(simpleError(msg, call))
        24.     └─cli::cli_abort(...)
        25.       └─rlang::abort(...)
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     ! "environment" can only be set as the class if the object has this type; found "logical"
     ---
     Backtrace:
          ▆
       1. ├─riskmetric::pkg_assess(as_tibble(pkg_ref("riskmetric")))
       2. ├─tibble::as_tibble(pkg_ref("riskmetric"))
       3. └─riskmetric:::as_tibble.pkg_ref(pkg_ref("riskmetric"))
       4.   ├─tibble::as_tibble(...)
       5.   └─vctrs::new_list_of(list(x), ptype = pkg_ref(), class = "list_of_pkg_ref")
       6.     └─vctrs::vec_ptype(ptype, x_arg = "ptype")
       7.       └─vctrs (local) `<fn>`()
       8.         ├─base::names(x = x)
       9.         └─riskmetric:::names.pkg_ref(x = x)
      10.           └─riskmetric:::bare_env(x, names(x))
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'riskmetric.Rmd' failed with diagnostics:
     "environment" can only be set as the class if the object has this type; found "logical"
     --- failed re-building ‘riskmetric.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘extending-riskmetric.Rmd’ ‘riskmetric.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# rlang (1.1.6)

* GitHub: <https://github.com/r-lib/rlang>
* Email: <mailto:lionel@posit.co>
* GitHub mirror: <https://github.com/cran/rlang>

Run `revdepcheck::cloud_details(, "rlang")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       `expected`: "<int[,2]>"
       
       ── Failure ('test-deparse.R:877:3'): matrices and arrays are formatted (#383) ──
       Expected `expr_deparse(mat2)` to equal "<int[,2]: 1L, 2L, 3L, 4L>".
       Differences:
       `actual`:   "<int: 1L, 2L, 3L, 4L>"    
       `expected`: "<int[,2]: 1L, 2L, 3L, 4L>"
       
       ── Failure ('test-deparse.R:880:3'): matrices and arrays are formatted (#383) ──
       Expected `as_label(arr)` to equal "<int[,1,3]>".
       Differences:
       `actual`:   "<int>"      
       `expected`: "<int[,1,3]>"
       
       ── Failure ('test-deparse.R:881:3'): matrices and arrays are formatted (#383) ──
       Expected `expr_deparse(arr)` to equal "<int[,1,3]: 1L, 2L, 3L>".
       Differences:
       `actual`:   "<int: 1L, 2L, 3L>"      
       `expected`: "<int[,1,3]: 1L, 2L, 3L>"
       
       
       [ FAIL 7 | WARN 1 | SKIP 256 | PASS 3806 ]
       Error:
       ! Test failures.
       Execution halted
     ```

## In both

*   checking compiled code ... NOTE
     ```
     File ‘rlang/libs/rlang.so’:
       Found non-API calls to R: ‘ENCLOS’, ‘OBJECT’, ‘PRENV’, ‘PRVALUE’,
         ‘R_PromiseExpr’, ‘Rf_allocSExp’, ‘Rf_findVarInFrame3’, ‘SETLENGTH’,
         ‘SET_BODY’, ‘SET_CLOENV’, ‘SET_ENCLOS’, ‘SET_FORMALS’,
         ‘SET_GROWABLE_BIT’, ‘SET_TRUELENGTH’, ‘SET_TYPEOF’, ‘XTRUELENGTH’
     
     Compiled code should not call non-API entry points in R.
     
     See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual,
     and section ‘Moving into C API compliance’ for issues with the use of
     non-API entry points.
     ```

# svytest (1.1.0)

* Email: <mailto:cnlubianski@yahoo.com>
* GitHub mirror: <https://github.com/cran/svytest>

Run `revdepcheck::cloud_details(, "svytest")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘svytest.Rmd’ using rmarkdown
     Killed
     ```

# testthat (3.3.1)

* GitHub: <https://github.com/r-lib/testthat>
* Email: <mailto:hadley@posit.co>
* GitHub mirror: <https://github.com/cran/testthat>

Run `revdepcheck::cloud_details(, "testthat")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       • file.exists(rd_path) is not TRUE (2): 'test-test-example.R:3:3',
         'test-test-example.R:12:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-expect-vector.R:10:3'): basic properties upheld ──────────────
       Snapshot of code has changed:
       old[3:5] vs new[3:6]
           expect_vector(y)
         Condition
           Error:
           ! `y` must be a vector, not `NULL`.
       +   i Read our FAQ about scalar types (`?vctrs::faq_error_scalar_type`) to learn more.
       * Run `testthat::snapshot_accept("expect-vector", "testthat")` to accept the change.
       * Run `testthat::snapshot_review("expect-vector", "testthat")` to review the change.
       
       ── Snapshots ───────────────────────────────────────────────────────────────────
       To review and process snapshots locally:
       * Locate check directory.
       * Copy 'tests/testthat/_snaps' to local package.
       * Run `testthat::snapshot_accept()` to accept all changes.
       * Run `testthat::snapshot_review()` to review all changes.
       [ FAIL 1 | WARN 0 | SKIP 177 | PASS 1065 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# tibble (3.3.0)

* GitHub: <https://github.com/tidyverse/tibble>
* Email: <mailto:kirill@cynkra.com>
* GitHub mirror: <https://github.com/cran/tibble>

Run `revdepcheck::cloud_details(, "tibble")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘digits.Rmd’ using rmarkdown
     --- finished re-building ‘digits.Rmd’
     
     --- re-building ‘extending.Rmd’ using rmarkdown
     --- finished re-building ‘extending.Rmd’
     
     --- re-building ‘formats.Rmd’ using rmarkdown
     ```

# wk (0.9.4)

* GitHub: <https://github.com/paleolimbot/wk>
* Email: <mailto:dewey@fishandwhistle.net>
* GitHub mirror: <https://github.com/cran/wk>

Run `revdepcheck::cloud_details(, "wk")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        13.           └─vctrs:::stop_vctrs(...)
        14.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       ── Error ('test-pkg-vctrs.R:240:7'): vec_c() propagates the geodesic attribute through points ──
       Error: objects have differing values for geodesic
       Backtrace:
            ▆
         1. ├─testthat::expect_identical(...) at test-pkg-vctrs.R:240:7
         2. │ └─testthat::quasi_label(enquo(object), label)
         3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
         4. └─vctrs::vec_c(`<wk_wkb>`, `<wk_xy>`)
         5.   └─vctrs (local) `<fn>`()
         6.     └─wk:::vec_cast.wk_wkb.wk_xy(...)
         7.       ├─wk::wk_translate(x, to)
         8.       └─wk:::wk_translate.default(x, to)
         9.         ├─wk::wk_set_geodesic(...)
        10.         ├─wk:::wk_set_geodesic.wk_wkb(...)
        11.         │ └─wk:::geodesic_attr(geodesic)
        12.         └─wk::wk_is_geodesic_output(handleable, to)
        13.           └─base::Reduce(wk_is_geodesic2, geodesic)
        14.             └─wk (local) f(init, x[[i]])
       
       [ FAIL 2 | WARN 0 | SKIP 0 | PASS 1660 ]
       Error:
       ! Test failures.
       Execution halted
     ```

