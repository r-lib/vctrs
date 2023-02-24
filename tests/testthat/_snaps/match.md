# must have at least 1 column to match

    Code
      vec_locate_matches(data_frame(), data_frame())
    Condition
      Error in `vec_locate_matches()`:
      ! Must have at least 1 column to match on.

---

    Code
      vec_locate_matches(data_frame(), data_frame(), error_call = call("foo"))
    Condition
      Error in `foo()`:
      ! Must have at least 1 column to match on.

# common type of `needles` and `haystack` is taken

    Code
      vec_locate_matches(x, y)
    Condition
      Error in `vec_locate_matches()`:
      ! Can't combine `needles` <double> and `haystack` <character>.

---

    Code
      vec_locate_matches(x, y, needles_arg = "x", error_call = call("foo"))
    Condition
      Error in `foo()`:
      ! Can't combine `x` <double> and `haystack` <character>.

# `incomplete` can error informatively

    Code
      (expect_error(vec_locate_matches(NA, 1, incomplete = "error")))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `vec_locate_matches()`:
      ! `needles` can't contain missing values.
      x Location 1 contains missing values.
    Code
      (expect_error(vec_locate_matches(NA, 1, incomplete = "error", needles_arg = "foo"))
      )
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `vec_locate_matches()`:
      ! `foo` can't contain missing values.
      x Location 1 contains missing values.
    Code
      (expect_error(vec_locate_matches(NA, 1, incomplete = "error", needles_arg = "foo",
        error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `fn()`:
      ! `foo` can't contain missing values.
      x Location 1 contains missing values.

# `incomplete` is validated

    Code
      (expect_error(vec_locate_matches(1, 2, incomplete = 1.5)))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `vec_locate_matches()`:
      ! Can't convert from `incomplete` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      (expect_error(vec_locate_matches(1, 2, incomplete = c("match", "drop"))))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `incomplete` must be length 1, not length 2.
    Code
      (expect_error(vec_locate_matches(1, 2, incomplete = "x")))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `incomplete` must be one of: "compare", "match", "drop", or "error".
    Code
      (expect_error(vec_locate_matches(1, 2, incomplete = "x", error_call = call("fn")))
      )
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `incomplete` must be one of: "compare", "match", "drop", or "error".

# `multiple` is validated

    Code
      (expect_error(vec_locate_matches(1, 2, multiple = 1.5)))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `multiple` must be a string.
    Code
      (expect_error(vec_locate_matches(1, 2, multiple = c("first", "last"))))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `multiple` must be a string.
    Code
      (expect_error(vec_locate_matches(1, 2, multiple = "x")))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `multiple` must be one of "all", "any", "first", or "last".
    Code
      (expect_error(vec_locate_matches(1, 2, multiple = "x", error_call = call("fn")))
      )
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `multiple` must be one of "all", "any", "first", or "last".

# `multiple` can error informatively

    Code
      (expect_error(vec_locate_matches(1L, c(1L, 1L), multiple = "error")))
    Output
      <error/vctrs_error_matches_multiple>
      Error in `vec_locate_matches()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 1 of `needles` matches multiple values.
    Code
      (expect_error(vec_locate_matches(1L, c(1L, 1L), multiple = "error",
      needles_arg = "foo")))
    Output
      <error/vctrs_error_matches_multiple>
      Error in `vec_locate_matches()`:
      ! Each value of `foo` can match at most 1 value from `haystack`.
      x Location 1 of `foo` matches multiple values.
    Code
      (expect_error(vec_locate_matches(1L, c(1L, 1L), multiple = "error",
      needles_arg = "foo", error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_multiple>
      Error in `fn()`:
      ! Each value of `foo` can match at most 1 value from `haystack`.
      x Location 1 of `foo` matches multiple values.
    Code
      (expect_error(vec_locate_matches(1L, c(1L, 1L), multiple = "error",
      needles_arg = "foo", haystack_arg = "bar")))
    Output
      <error/vctrs_error_matches_multiple>
      Error in `vec_locate_matches()`:
      ! Each value of `foo` can match at most 1 value from `bar`.
      x Location 1 of `foo` matches multiple values.

# `multiple` can warn informatively

    Code
      (expect_warning(vec_locate_matches(1L, c(1L, 1L), multiple = "warning")))
    Output
      <warning/vctrs_warning_matches_multiple>
      Warning in `vec_locate_matches()`:
      Each value of `needles` can match at most 1 value from `haystack`.
      x Location 1 of `needles` matches multiple values.
    Code
      (expect_warning(vec_locate_matches(1L, c(1L, 1L), multiple = "warning",
      needles_arg = "foo")))
    Output
      <warning/vctrs_warning_matches_multiple>
      Warning in `vec_locate_matches()`:
      Each value of `foo` can match at most 1 value from `haystack`.
      x Location 1 of `foo` matches multiple values.
    Code
      (expect_warning(vec_locate_matches(1L, c(1L, 1L), multiple = "warning",
      needles_arg = "foo", error_call = call("fn"))))
    Output
      <warning/vctrs_warning_matches_multiple>
      Warning in `fn()`:
      Each value of `foo` can match at most 1 value from `haystack`.
      x Location 1 of `foo` matches multiple values.
    Code
      (expect_warning(vec_locate_matches(1L, c(1L, 1L), multiple = "warning",
      needles_arg = "foo", haystack_arg = "bar")))
    Output
      <warning/vctrs_warning_matches_multiple>
      Warning in `vec_locate_matches()`:
      Each value of `foo` can match at most 1 value from `bar`.
      x Location 1 of `foo` matches multiple values.

# errors on multiple matches that come from different nesting containers

    Code
      vec_locate_matches(df, df2, condition = c("<=", "<="), multiple = "error")
    Condition
      Error in `vec_locate_matches()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 1 of `needles` matches multiple values.

# errors when a match from a different nesting container is processed early on

    Code
      vec_locate_matches(needles, haystack, condition = "<", multiple = "error")
    Condition
      Error in `vec_locate_matches()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 1 of `needles` matches multiple values.

# `multiple = 'error' / 'warning'` throw correctly when combined with `relationship`

    Code
      (expect_error(vec_locate_matches(x, y, relationship = "one-to-one", multiple = "error"))
      )
    Output
      <error/vctrs_error_matches_multiple>
      Error in `vec_locate_matches()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 2 of `needles` matches multiple values.

---

    Code
      (expect_error(vec_locate_matches(x, y, relationship = "warn-many-to-many",
        multiple = "error")))
    Output
      <error/vctrs_error_matches_multiple>
      Error in `vec_locate_matches()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 2 of `needles` matches multiple values.

---

    Code
      vec_locate_matches(x, y, relationship = "warn-many-to-many", multiple = "warning")
    Condition
      Warning in `vec_locate_matches()`:
      Each value of `needles` can match at most 1 value from `haystack`.
      x Location 2 of `needles` matches multiple values.
      Warning in `vec_locate_matches()`:
      Detected an unexpected many-to-many relationship between `needles` and `haystack`.
      x Location 2 of `needles` matches multiple values.
      x Location 1 of `haystack` matches multiple values.
    Output
        needles haystack
      1       1        2
      2       2        1
      3       2        3
      4       3        1
      5       3        3

---

    Code
      vec_locate_matches(x, y, relationship = "one-to-one", multiple = "warning")
    Condition
      Warning in `vec_locate_matches()`:
      Each value of `needles` can match at most 1 value from `haystack`.
      x Location 2 of `needles` matches multiple values.
      Error in `vec_locate_matches()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 2 of `needles` matches multiple values.

---

    Code
      (expect_error(vec_locate_matches(x, y, relationship = "warn-many-to-many",
        multiple = "error")))
    Output
      <error/vctrs_error_matches_multiple>
      Error in `vec_locate_matches()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 2 of `needles` matches multiple values.

---

    Code
      vec_locate_matches(x, y, relationship = "warn-many-to-many", multiple = "warning")
    Condition
      Warning in `vec_locate_matches()`:
      Each value of `needles` can match at most 1 value from `haystack`.
      x Location 2 of `needles` matches multiple values.
    Output
        needles haystack
      1       1        2
      2       2        1
      3       2        3

# `relationship` handles one-to-one case

    Code
      (expect_error(vec_locate_matches(c(2, 1), c(1, 1), relationship = "one-to-one"))
      )
    Output
      <error/vctrs_error_matches_relationship_one_to_one>
      Error in `vec_locate_matches()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 2 of `needles` matches multiple values.
    Code
      (expect_error(vec_locate_matches(c(1, 1), c(1, 2), relationship = "one-to-one"))
      )
    Output
      <error/vctrs_error_matches_relationship_one_to_one>
      Error in `vec_locate_matches()`:
      ! Each value of `haystack` can match at most 1 value from `needles`.
      x Location 1 of `haystack` matches multiple values.

# `relationship` handles one-to-many case

    Code
      (expect_error(vec_locate_matches(c(1, 2, 2), c(2, 1), relationship = "one-to-many"))
      )
    Output
      <error/vctrs_error_matches_relationship_one_to_many>
      Error in `vec_locate_matches()`:
      ! Each value of `haystack` can match at most 1 value from `needles`.
      x Location 1 of `haystack` matches multiple values.

# `relationship` handles many-to-one case

    Code
      (expect_error(vec_locate_matches(c(1, 2), c(1, 2, 2), relationship = "many-to-one"))
      )
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `vec_locate_matches()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 2 of `needles` matches multiple values.

# `relationship` handles warn-many-to-many case

    Code
      (expect_warning(vec_locate_matches(c(1, 2, 1), c(1, 2, 2), relationship = "warn-many-to-many"))
      )
    Output
      <warning/vctrs_warning_matches_relationship_many_to_many>
      Warning in `vec_locate_matches()`:
      Detected an unexpected many-to-many relationship between `needles` and `haystack`.
      x Location 2 of `needles` matches multiple values.
      x Location 1 of `haystack` matches multiple values.
    Code
      (expect_warning(vec_locate_matches(c(1, 1, 2), c(2, 2, 1), relationship = "warn-many-to-many"))
      )
    Output
      <warning/vctrs_warning_matches_relationship_many_to_many>
      Warning in `vec_locate_matches()`:
      Detected an unexpected many-to-many relationship between `needles` and `haystack`.
      x Location 3 of `needles` matches multiple values.
      x Location 3 of `haystack` matches multiple values.

# `relationship` considers `incomplete` matches as possible multiple matches

    Code
      (expect_error(vec_locate_matches(x, y, relationship = "one-to-many")))
    Output
      <error/vctrs_error_matches_relationship_one_to_many>
      Error in `vec_locate_matches()`:
      ! Each value of `haystack` can match at most 1 value from `needles`.
      x Location 1 of `haystack` matches multiple values.

# `relationship` errors on multiple matches that come from different nesting containers

    Code
      (expect_error(vec_locate_matches(df, df2, condition = c("<=", "<="),
      relationship = "many-to-one")))
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `vec_locate_matches()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 1 of `needles` matches multiple values.

# `relationship` errors when a match from a different nesting container is processed early on

    Code
      (expect_error(vec_locate_matches(needles, haystack, condition = "<",
        relationship = "many-to-one")))
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `vec_locate_matches()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 1 of `needles` matches multiple values.

# `relationship` can still detect problematic `haystack` relationships when `multiple = first/last` are used

    Code
      (expect_error(vec_locate_matches(c(3, 1, 1), c(2, 1, 3, 3), multiple = "first",
      relationship = "one-to-one")))
    Output
      <error/vctrs_error_matches_relationship_one_to_one>
      Error in `vec_locate_matches()`:
      ! Each value of `haystack` can match at most 1 value from `needles`.
      x Location 2 of `haystack` matches multiple values.
    Code
      (expect_error(vec_locate_matches(c(3, 1, 1), c(2, 1, 3, 3), multiple = "first",
      relationship = "one-to-many")))
    Output
      <error/vctrs_error_matches_relationship_one_to_many>
      Error in `vec_locate_matches()`:
      ! Each value of `haystack` can match at most 1 value from `needles`.
      x Location 2 of `haystack` matches multiple values.

# `relationship` and `remaining` work properly together

    Code
      out <- vec_locate_matches(c(1, 2, 2), c(2, 3, 1, 1, 4), relationship = "warn-many-to-many",
      remaining = NA_integer_)
    Condition
      Warning in `vec_locate_matches()`:
      Detected an unexpected many-to-many relationship between `needles` and `haystack`.
      x Location 1 of `needles` matches multiple values.
      x Location 1 of `haystack` matches multiple values.

# `relationship` errors if `condition` creates multiple matches

    Code
      (expect_error(vec_locate_matches(1, c(1, 2), condition = "<=", relationship = "many-to-one"))
      )
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `vec_locate_matches()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 1 of `needles` matches multiple values.

# `relationship` still errors if `filter` hasn't removed all multiple matches

    Code
      (expect_error(vec_locate_matches(1, c(1, 2, 1), condition = "<=", filter = "min",
      relationship = "many-to-one")))
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `vec_locate_matches()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 1 of `needles` matches multiple values.

# `relationship` errors respect argument tags and error call

    Code
      (expect_error(vec_locate_matches(1L, c(1L, 1L), relationship = "one-to-one",
      needles_arg = "foo", haystack_arg = "bar", error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_relationship_one_to_one>
      Error in `fn()`:
      ! Each value of `foo` can match at most 1 value from `bar`.
      x Location 1 of `foo` matches multiple values.
    Code
      (expect_error(vec_locate_matches(c(1L, 1L), 1L, relationship = "one-to-one",
      needles_arg = "foo", haystack_arg = "bar", error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_relationship_one_to_one>
      Error in `fn()`:
      ! Each value of `bar` can match at most 1 value from `foo`.
      x Location 1 of `bar` matches multiple values.
    Code
      (expect_error(vec_locate_matches(c(1L, 1L), 1L, relationship = "one-to-many",
      needles_arg = "foo", haystack_arg = "bar", error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_relationship_one_to_many>
      Error in `fn()`:
      ! Each value of `bar` can match at most 1 value from `foo`.
      x Location 1 of `bar` matches multiple values.
    Code
      (expect_error(vec_locate_matches(1L, c(1L, 1L), relationship = "many-to-one",
      needles_arg = "foo", haystack_arg = "bar", error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `fn()`:
      ! Each value of `foo` can match at most 1 value from `bar`.
      x Location 1 of `foo` matches multiple values.

# `relationship` warnings respect argument tags and error call

    Code
      (expect_warning(vec_locate_matches(c(1L, 1L), c(1L, 1L), relationship = "warn-many-to-many",
      needles_arg = "foo", haystack_arg = "bar", error_call = call("fn"))))
    Output
      <warning/vctrs_warning_matches_relationship_many_to_many>
      Warning in `fn()`:
      Detected an unexpected many-to-many relationship between `foo` and `bar`.
      x Location 1 of `foo` matches multiple values.
      x Location 1 of `bar` matches multiple values.
    Code
      (expect_warning(vec_locate_matches(c(1L, 1L), c(1L, 1L), relationship = "warn-many-to-many",
      needles_arg = "foo", error_call = call("fn"))))
    Output
      <warning/vctrs_warning_matches_relationship_many_to_many>
      Warning in `fn()`:
      Detected an unexpected many-to-many relationship between `foo` and `haystack`.
      x Location 1 of `foo` matches multiple values.
      x Location 1 of `haystack` matches multiple values.
    Code
      (expect_warning(vec_locate_matches(c(1L, 1L), c(1L, 1L), relationship = "warn-many-to-many",
      haystack_arg = "bar", error_call = call("fn"))))
    Output
      <warning/vctrs_warning_matches_relationship_many_to_many>
      Warning in `fn()`:
      Detected an unexpected many-to-many relationship between `needles` and `bar`.
      x Location 1 of `needles` matches multiple values.
      x Location 1 of `bar` matches multiple values.

# `relationship` is validated

    Code
      (expect_error(vec_locate_matches(1, 2, relationship = 1.5)))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `relationship` must be a string.
    Code
      (expect_error(vec_locate_matches(1, 2, relationship = c("one-to-one",
        "one-to-many"))))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `relationship` must be a string.
    Code
      (expect_error(vec_locate_matches(1, 2, relationship = "x")))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `relationship` must be one of "none", "one-to-one", "one-to-many", "many-to-one", "many-to-many", or "warn-many-to-many".
    Code
      (expect_error(vec_locate_matches(1, 2, relationship = "x", error_call = call(
        "fn"))))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `relationship` must be one of "none", "one-to-one", "one-to-many", "many-to-one", "many-to-many", or "warn-many-to-many".

# `no_match` can error informatively

    Code
      (expect_error(vec_locate_matches(1, 2, no_match = "error")))
    Output
      <error/vctrs_error_matches_nothing>
      Error in `vec_locate_matches()`:
      ! Each value of `needles` must have a match in `haystack`.
      x Location 1 of `needles` does not have a match.
    Code
      (expect_error(vec_locate_matches(1, 2, no_match = "error", needles_arg = "foo"))
      )
    Output
      <error/vctrs_error_matches_nothing>
      Error in `vec_locate_matches()`:
      ! Each value of `foo` must have a match in `haystack`.
      x Location 1 of `foo` does not have a match.
    Code
      (expect_error(vec_locate_matches(1, 2, no_match = "error", needles_arg = "foo",
        error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_nothing>
      Error in `fn()`:
      ! Each value of `foo` must have a match in `haystack`.
      x Location 1 of `foo` does not have a match.
    Code
      (expect_error(vec_locate_matches(1, 2, no_match = "error", needles_arg = "foo",
        haystack_arg = "bar")))
    Output
      <error/vctrs_error_matches_nothing>
      Error in `vec_locate_matches()`:
      ! Each value of `foo` must have a match in `bar`.
      x Location 1 of `foo` does not have a match.

# errors with the right location on unmatched needles when different nesting containers are present

    Code
      (expect_error(vec_locate_matches(df, df2, condition = ">=", no_match = "error"))
      )
    Output
      <error/vctrs_error_matches_nothing>
      Error in `vec_locate_matches()`:
      ! Each value of `needles` must have a match in `haystack`.
      x Location 2 of `needles` does not have a match.

# `no_match` is validated

    Code
      (expect_error(vec_locate_matches(1, 2, no_match = 1.5)))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `vec_locate_matches()`:
      ! Can't convert from `no_match` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      (expect_error(vec_locate_matches(1, 2, no_match = c(1L, 2L))))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `no_match` must be length 1, not length 2.
    Code
      (expect_error(vec_locate_matches(1, 2, no_match = "x")))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `no_match` must be either "drop" or "error".
    Code
      (expect_error(vec_locate_matches(1, 2, no_match = "x", error_call = call("fn")))
      )
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `no_match` must be either "drop" or "error".

# `remaining` can error informatively

    Code
      (expect_error(vec_locate_matches(1, 2, remaining = "error")))
    Output
      <error/vctrs_error_matches_remaining>
      Error in `vec_locate_matches()`:
      ! Each value of `haystack` must be matched by `needles`.
      x Location 1 of `haystack` was not matched.
    Code
      (expect_error(vec_locate_matches(1, 2, remaining = "error", needles_arg = "foo"))
      )
    Output
      <error/vctrs_error_matches_remaining>
      Error in `vec_locate_matches()`:
      ! Each value of `haystack` must be matched by `foo`.
      x Location 1 of `haystack` was not matched.
    Code
      (expect_error(vec_locate_matches(1, 2, remaining = "error", needles_arg = "foo",
        error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_remaining>
      Error in `fn()`:
      ! Each value of `haystack` must be matched by `foo`.
      x Location 1 of `haystack` was not matched.
    Code
      (expect_error(vec_locate_matches(1, 2, remaining = "error", needles_arg = "foo",
        haystack_arg = "bar")))
    Output
      <error/vctrs_error_matches_remaining>
      Error in `vec_locate_matches()`:
      ! Each value of `bar` must be matched by `foo`.
      x Location 1 of `bar` was not matched.

# `remaining` is validated

    Code
      (expect_error(vec_locate_matches(1, 2, remaining = 1.5)))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `vec_locate_matches()`:
      ! Can't convert from `remaining` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      (expect_error(vec_locate_matches(1, 2, remaining = c(1L, 2L))))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `remaining` must be length 1, not length 2.
    Code
      (expect_error(vec_locate_matches(1, 2, remaining = "x")))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `remaining` must be either "drop" or "error".
    Code
      (expect_error(vec_locate_matches(1, 2, remaining = "x", error_call = call("fn")))
      )
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `remaining` must be either "drop" or "error".

# potential overflow on large output size is caught informatively

    Code
      (expect_error(vec_locate_matches(1:1e+07, 1:1e+07, condition = ">=")))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! Match procedure results in an allocation larger than 2^31-1 elements. Attempted allocation size was 50000005000000.
      i In file 'match.c' at line <scrubbed>.
      i This is an internal error that was detected in the vctrs package.
        Please report it at <https://github.com/r-lib/vctrs/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.

