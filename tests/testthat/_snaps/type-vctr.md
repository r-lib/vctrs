# na.fail() works

    Code
      na.fail(x)
    Condition
      Error in `na.fail()`:
      ! missing values in object

# default print and str methods are useful

    Code
      h
    Output
      <hidden[4]>
      [1] xxx xxx xxx xxx

---

    Code
      h[0]
    Output
      <hidden[0]>

---

    Code
      str(h)
    Output
       hidden [1:4] xxx, xxx, xxx, xxx

# default print method shows names

    Code
      h
    Output
      <hidden[3]>
        A   B   C 
      xxx xxx xxx 

# `min` and `max` either combine `x` and `...` or abort (#1372)

    Code
      min(rep(x, 5), y)
    Condition <vctrs_error_min_intent_uncertain>
      Error:
      ! Can't use `...` unless `x` and each of `...` are of size 1.
      i Size of `x` was 5
      i Size of `...` was 1
      > If you wanted a size-1 result with the overall `min()`, use `min(c(<args>))`
      > If you wanted a vectorized/parallel `pmin()`, use `pmin(<args>)`

---

    Code
      min(x, rep(y, 5), z)
    Condition <vctrs_error_min_intent_uncertain>
      Error:
      ! Can't use `...` unless `x` and each of `...` are of size 1.
      i Size of `x` was 1
      i Sizes of `...` were 5 and 1
      > If you wanted a size-1 result with the overall `min()`, use `min(c(<args>))`
      > If you wanted a vectorized/parallel `pmin()`, use `pmin(<args>)`

---

    Code
      max(rep(x, 5), y)
    Condition <vctrs_error_max_intent_uncertain>
      Error:
      ! Can't use `...` unless `x` and each of `...` are of size 1.
      i Size of `x` was 5
      i Size of `...` was 1
      > If you wanted a size-1 result with the overall `max()`, use `max(c(<args>))`
      > If you wanted a vectorized/parallel `pmax()`, use `pmax(<args>)`

