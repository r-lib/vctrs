# slider

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/slider
* URL: https://github.com/DavisVaughan/slider
* BugReports: https://github.com/DavisVaughan/slider/issues
* Date/Publication: 2020-02-23 17:10:02 UTC
* Number of recursive dependencies: 59

Run `revdep_details(,"slider")` for more info

</details>

## Newly broken

*   checking whether the package can be loaded ... ERROR
    ```
    Loading this package had a fatal error status code 1
    Loading log:
    Error: package or namespace load failed for ‘slider’ in dyn.load(file, DLLpath = DLLpath, ...):
     unable to load shared object '/Users/lionel/Desktop/vctrs/revdep-all/vctrs/checks.noindex/slider/new/slider.Rcheck/slider/libs/slider.so':
      dlopen(/Users/lionel/Desktop/vctrs/revdep-all/vctrs/checks.noindex/slider/new/slider.Rcheck/slider/libs/slider.so, 6): Symbol not found: _vec_assign_impl
      Referenced from: /Users/lionel/Desktop/vctrs/revdep-all/vctrs/checks.noindex/slider/new/slider.Rcheck/slider/libs/slider.so
      Expected in: flat namespace
     in /Users/lionel/Desktop/vctrs/revdep-all/vctrs/checks.noindex/slider/new/slider.Rcheck/slider/libs/slider.so
    Execution halted
    ```

*   checking whether package ‘slider’ can be installed ... WARNING
    ```
    ...
      hop.c:95:11: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
      hop.c:110:17: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
      index.c:74:19: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
      index.c:98:21: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
      index.c:125:19: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
      index.c:172:19: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
      index.c:202:21: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
      index.c:221:19: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
      names.c:5:10: warning: incompatible integer to pointer conversion returning 'int' from a function with result type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
      names.c:10:10: warning: incompatible integer to pointer conversion returning 'int' from a function with result type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
      slide.c:97:7: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
      slide.c:144:11: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
      slide.c:159:7: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
      utils.c:106:21: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
      utils.c:110:21: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
      utils.c:114:10: warning: incompatible integer to pointer conversion returning 'int' from a function with result type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
      utils.c:153:15: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
      utils.c:160:15: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
      utils.c:162:15: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
      utils.c:171:11: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
    See ‘/Users/lionel/Desktop/vctrs/revdep-all/vctrs/checks.noindex/slider/new/slider.Rcheck/00install.out’ for details.
    ```

