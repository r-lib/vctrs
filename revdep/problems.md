# APCI (1.0.8)

* Email: <mailto:jpx5053@psu.edu>
* GitHub mirror: <https://github.com/cran/APCI>

Run `revdepcheck::cloud_details(, "APCI")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         (Intercept)          acc1          acc2          acc3          acc4
        4.078669e+01 -1.915412e+01 -1.752095e+01 -1.490608e+01 -1.131444e+01
                acc5          acc6          acc7          acc8          acc9
       -6.726356e+00 -1.354069e+00  5.177093e+00  1.289094e+01  2.169297e+01
                pcc1          pcc2          pcc3          pcc4          pcc5
       -7.284883e+00 -5.717909e+00 -3.018352e+00  5.009928e-01  4.998061e+00
           acc1:pcc1     acc2:pcc1     acc3:pcc1     acc4:pcc1     acc5:pcc1
        2.273902e-01 -1.816606e-01 -1.372888e-03  7.044980e-02  3.234843e-02
           acc6:pcc1     acc7:pcc1     acc8:pcc1     acc9:pcc1     acc1:pcc2
       -4.279481e-02 -2.855554e-01  2.478864e-01 -4.020285e-02  3.537320e-01
           acc2:pcc2     acc3:pcc2     acc4:pcc2     acc5:pcc2     acc6:pcc2
       -2.285807e-01 -1.410753e-01 -2.132745e-01  1.199713e-02  4.303632e-01
           acc7:pcc2     acc8:pcc2     acc9:pcc2     acc1:pcc3     acc2:pcc3
        2.255996e-01 -3.718861e-01  2.911546e-01 -2.651324e-03 -1.717160e-03
           acc3:pcc3     acc4:pcc3     acc5:pcc3     acc6:pcc3     acc7:pcc3
       -3.083693e-01  3.764244e-02  1.283516e-01 -2.848821e-01  1.605444e-01
           acc8:pcc3     acc9:pcc3     acc1:pcc4     acc2:pcc4     acc3:pcc4
       -3.873582e-02  1.550242e-01 -4.106514e-01  1.050597e-03  2.919885e-01
           acc4:pcc4     acc5:pcc4     acc6:pcc4     acc7:pcc4     acc8:pcc4
        1.917067e-01  2.746679e-01 -1.137581e-01 -1.022215e-01 -2.771887e-01
           acc9:pcc4     acc1:pcc5     acc2:pcc5     acc3:pcc5     acc4:pcc5
        8.528408e-02 -7.084064e-03  3.049374e-01  5.407306e-04 -1.256263e-01
           acc5:pcc5     acc6:pcc5     acc7:pcc5     acc8:pcc5     acc9:pcc5
       -5.646555e-01 -3.242138e-01  3.134797e-01  6.593600e-01 -3.001129e-01
       Killed
     ```

# crane (0.3.0)

* GitHub: <https://github.com/insightsengineering/crane>
* Email: <mailto:danield.sjoberg@gmail.com>
* GitHub mirror: <https://github.com/cran/crane>

Run `revdepcheck::cloud_details(, "crane")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         'test-tbl_listing.R:98:1', 'test-tbl_roche_summary.R:14:1',
         'test-tbl_roche_summary.R:85:1', 'test-tbl_shift.R:23:1',
         'test-tbl_shift.R:53:1', 'test-tbl_shift.R:72:1', 'test-tbl_shift.R:89:1',
         'test-tbl_shift.R:124:1', 'test-tbl_shift.R:159:1',
         'test-tbl_survfit_quantiles.R:3:1', 'test-tbl_survfit_quantiles.R:53:1',
         'test-tbl_survfit_quantiles.R:73:1', 'test-tbl_survfit_quantiles.R:109:1',
         'test-tbl_baseline_chg.R:12:1', 'test-tbl_baseline_chg.R:42:1',
         'test-tbl_baseline_chg.R:56:1', 'test-tbl_baseline_chg.R:73:1',
         'test-tbl_baseline_chg.R:104:1', 'test-tbl_baseline_chg.R:155:1',
         'test-tbl_baseline_chg.R:170:1', 'test-tbl_survfit_times.R:3:1',
         'test-tbl_survfit_times.R:57:1', 'test-tbl_survfit_times.R:79:1',
         'test-theme_gtsummary_roche.R:1:1', 'test-theme_gtsummary_roche.R:8:1'

       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-add_blank_rows.R:93:3'): add_blank_rows(row_numbers) error ───
       Expected `dplyr::pull(out$table_body[, 1], trt)` to equal `c("Drug A", NA, "Drug B", NA, "Drug A")`.
       Differences:
       `attr(actual, 'label')` is a character vector ('Chemotherapy Treatment')
       `attr(expected, 'label')` is absent


       [ FAIL 1 | WARN 0 | SKIP 41 | PASS 76 ]
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
     ...
       ...
     --- re-building ‘feisr-vignette.Rmd’ using rmarkdown

     Quitting from feisr-vignette.Rmd:81-84 [feis0]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     <error/rlang_error>
     Error in `list_flatten()`:
     ! `x` must be a list, not a list matrix.
     ---
     Backtrace:
         ▆
      1. └─feisr::feis(lnw ~ marry | year, data = mwp, id = "id")
      2.   └─dplyr::bind_rows(rbind(dhat), .id = NULL)
      3.     └─dplyr:::list_flatten(dots, fn = is_flattenable)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

     Error: processing vignette 'feisr-vignette.Rmd' failed with diagnostics:
     `x` must be a list, not a list matrix.
     --- failed re-building ‘feisr-vignette.Rmd’

     SUMMARY: processing the following file failed:
       ‘feisr-vignette.Rmd’

     Error: Vignette re-building failed.
     Execution halted
     ```

# ISCA (0.1.0)

* Email: <mailto:l.g.m.drouhot@uu.nl>
* GitHub mirror: <https://github.com/cran/ISCA>

Run `revdepcheck::cloud_details(, "ISCA")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       Iteration:   6, Error: 97.7753119520
       Iteration:   7, Error: 96.9538966405
       Iteration:   8, Error: 96.5524607129
       Iteration:   9, Error: 96.4156756053
       Iteration:  10, Error: 96.3602517476
       Iteration:  11, Error: 96.3572637421
       Iteration:  12, Error: 96.3566978165
       Iteration:  13 converged, Error: 96.3566978165
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 14 ]

       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-ISCA_random_assignments.R:11:3'): number of clusters are correct ──
       Error in `Hmisc::rMultinom(probs.cluster, draws)`: error in multinom: probabilities do not sum to 1
       Backtrace:
           ▆
        1. └─ISCA::ISCA_random_assignments(...) at test-ISCA_random_assignments.R:11:3
        2.   ├─base::cbind(data_ref, Hmisc::rMultinom(probs.cluster, draws))
        3.   │ └─base::cbind(deparse.level, ...)
        4.   │   └─base::data.frame(..., check.names = FALSE)
        5.   └─Hmisc::rMultinom(probs.cluster, draws)

       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 14 ]
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
         'test-helpers.R:6:3', 'test-helpers.R:23:3', 'test-helpers.R:37:3',
         'test-helpers.R:72:3', 'test-helpers.R:90:3', 'test-helpers.R:124:3',
         'test-merModList.R:10:3', 'test-merModList.R:46:3', 'test-merModList.R:72:3',
         'test-merModList.R:128:3'

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

# nswgeo (0.5.0)

* GitHub: <https://github.com/cidm-ph/nswgeo>
* Email: <mailto:Carl.Suster@health.nsw.gov.au>
* GitHub mirror: <https://github.com/cran/nswgeo>

Run `revdepcheck::cloud_details(, "nswgeo")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
       5. │   └─ggplot2:::by_layer(...)
       6. │     ├─rlang::try_fetch(...)
       7. │     │ ├─base::tryCatch(...)
       8. │     │ │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
       9. │     │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
      10. │     │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
      11. │     │ └─base::withCallingHandlers(...)
      12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
      13. │       └─l$compute_geom_2(d, theme = plot@theme)
      14. │         └─ggplot2 (local) compute_geom_2(..., self = self)
      15. │           └─ggproto_parent(Layer, self)$compute_geom_2(data, params, ...)
      16. │             └─ggplot2 (local) compute_geom_2(..., self = self)
      17. │               └─self$geom$use_defaults(...)
      18. │                 └─ggplot2 (local) use_defaults(..., self = self)
      19. │                   └─vctrs::vec_c(points, lines, others, collections)
      20. │                     └─vctrs (local) `<fn>`()
      21. │                       ├─vctrs:::vec_restore_dispatch(x = x, to = to)
      22. │                       └─sf:::vec_restore.sfc(x = x, to = to)
      23. │                         └─sf::st_sfc(x, crs = st_crs(to), precision = st_precision(to))
      24. └─base::.handleSimpleError(...)
      25.   └─rlang (local) h(simpleError(msg, call))
      26.     └─handlers[[1L]](cnd)
      27.       └─cli::cli_abort(...)
      28.         └─rlang::abort(...)
     Execution halted
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

# simstudy (0.9.1)

* GitHub: <https://github.com/kgoldfeld/simstudy>
* Email: <mailto:keith.goldfeld@nyulangone.org>
* GitHub mirror: <https://github.com/cran/simstudy>

Run `revdepcheck::cloud_details(, "simstudy")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         'test-survival.R:72:3', 'test-survival.R:91:3', 'test-survival.R:102:3',
         'test-utility.R:11:3', 'test-utility.R:21:3', 'test-utility.R:46:3',
         'test-utility.R:62:3', 'test-utility.R:72:3', 'test-utility.R:78:3',
         'test-utility.R:84:3', 'test-utility.R:94:3', 'test-utility.R:106:3',
         'test-utility.R:120:3', 'test-utility.R:130:3', 'test-utility.R:137:3',
         'test-utility.R:155:3', 'test-utility.R:229:3', 'test-utility.R:256:3',
         'test-utility.R:263:3', 'test-utility.R:269:3', 'test-utility.R:292:3',
         'test-utility.R:298:3', 'test-utility.R:307:3', 'test-utility.R:366:3',
         'test-utility.R:375:3', 'test-utility.R:383:3', 'test-utility.R:392:3',
         'test-utility.R:401:3', 'test-utility.R:410:3', 'test-utility.R:420:3',
         'test-utility.R:426:3', 'test-utility.R:432:3', 'test-utility.R:438:3',
         'test-utility.R:444:3', 'test-utility.R:450:1'

       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-generate_correlated_data.R:621:3'): Correlation Structure Test ──
       Expected `cor_matrix` to equal `matrix(c(1, 0.3, 0.3, 1), 2, 2)`.
       Differences:
       2/4 mismatches (average diff: 0.166)
       [2] 0.134 - 0.3 == -0.166
       [3] 0.134 - 0.3 == -0.166

       [ FAIL 1 | WARN 0 | SKIP 307 | PASS 214 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# volker (3.3.0)

* GitHub: <https://github.com/strohne/volker>
* Email: <mailto:jakob.juenger@uni-muenster.de>
* GitHub mirror: <https://github.com/cran/volker>

Run `revdepcheck::cloud_details(, "volker")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       -   |advantage_02 | 2.7 (1.2)| 3.0 (NA)| 2.6 (1.3)| 2.7 (1.2)|
       +   |Using ChatGPT brings financial benefits. | 2.7 (1.2)| 3.0 (NA)| 2.6 (1.2)| 2.7 (1.2)|
       -   |advantage_03 | 3.6 (1.1)| 4.0 (NA)| 3.7 (1.0)| 3.5 (1.1)|
       +   |Using ChatGPT is advantageous in many... | 3.6 (1.1)| 4.0 (NA)| 3.7 (1.0)| 3.5 (1.1)|
       -   |advantage_04 | 3.5 (1.0)| 3.0 (NA)| 3.6 (1.0)| 3.5 (1.0)|
       +   |Compared to other systems, using Chat... | 3.5 (1.0)| 3.0 (NA)| 3.6 (1.0)| 3.5 (1.0)|
       -   |fearofuse_01 | 3.1 (1.1)| 3.0 (NA)| 3.2 (1.0)| 3.1 (1.2)|
       +   |Much can go wrong when using ChatGPT.    | 3.1 (1.1)| 3.0 (NA)| 3.1 (1.0)| 3.1 (1.2)|
       and 11 more ...

       Backtrace:
           ▆
        1. ├─... %>% expect_snapshot(cran = TRUE) at test-tables.R:353:3
        2. └─testthat::expect_snapshot(., cran = TRUE)

       ── Snapshots ───────────────────────────────────────────────────────────────────
       To review and process snapshots locally:
       * Locate check directory.
       * Copy 'tests/testthat/_snaps' to local package.
       * Run `testthat::snapshot_accept()` to accept all changes.
       * Run `testthat::snapshot_review()` to review all changes.
       [ FAIL 1 | WARN 0 | SKIP 11 | PASS 180 ]
       Error:
       ! Test failures.
       Execution halted
     ```
