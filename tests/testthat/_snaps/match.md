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
      ! Can't combine <double> and <character>.

---

    Code
      vec_locate_matches(x, y, needles_arg = "x", error_call = call("foo"))
    Condition
      Error in `foo()`:
      ! Can't combine `x` <double> and <character>.

# `incomplete` can error informatively

    Code
      (expect_error(vec_locate_matches(NA, 1, incomplete = "error")))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `vec_locate_matches()`:
      ! No element can contain missing values.
      x The element at location 1 contains missing values.
    Code
      (expect_error(vec_locate_matches(NA, 1, incomplete = "error", needles_arg = "foo"))
      )
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `vec_locate_matches()`:
      ! No element of `foo` can contain missing values.
      x The element at location 1 contains missing values.
    Code
      (expect_error(vec_locate_matches(NA, 1, incomplete = "error", needles_arg = "foo",
        error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `fn()`:
      ! No element of `foo` can contain missing values.
      x The element at location 1 contains missing values.

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
      ! Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.
    Code
      (expect_error(vec_locate_matches(1L, c(1L, 1L), multiple = "error",
      needles_arg = "foo")))
    Output
      <error/vctrs_error_matches_multiple>
      Error in `vec_locate_matches()`:
      ! Each element of `foo` can match at most 1 observation.
      x The element at location 1 has multiple matches.
    Code
      (expect_error(vec_locate_matches(1L, c(1L, 1L), multiple = "error",
      needles_arg = "foo", error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_multiple>
      Error in `fn()`:
      ! Each element of `foo` can match at most 1 observation.
      x The element at location 1 has multiple matches.
    Code
      (expect_error(vec_locate_matches(1L, c(1L, 1L), multiple = "error",
      needles_arg = "foo", haystack_arg = "bar")))
    Output
      <error/vctrs_error_matches_multiple>
      Error in `vec_locate_matches()`:
      ! Each element of `foo` can match at most 1 observation from `bar`.
      x The element at location 1 has multiple matches.

# `multiple` can warn informatively

    Code
      (expect_warning(vec_locate_matches(1L, c(1L, 1L), multiple = "warning")))
    Output
      <warning/vctrs_warning_matches_multiple>
      Warning in `vec_locate_matches()`:
      Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.
    Code
      (expect_warning(vec_locate_matches(1L, c(1L, 1L), multiple = "warning",
      needles_arg = "foo")))
    Output
      <warning/vctrs_warning_matches_multiple>
      Warning in `vec_locate_matches()`:
      Each element of `foo` can match at most 1 observation.
      x The element at location 1 has multiple matches.
    Code
      (expect_warning(vec_locate_matches(1L, c(1L, 1L), multiple = "warning",
      needles_arg = "foo", error_call = call("fn"))))
    Output
      <warning/vctrs_warning_matches_multiple>
      Warning in `fn()`:
      Each element of `foo` can match at most 1 observation.
      x The element at location 1 has multiple matches.
    Code
      (expect_warning(vec_locate_matches(1L, c(1L, 1L), multiple = "warning",
      needles_arg = "foo", haystack_arg = "bar")))
    Output
      <warning/vctrs_warning_matches_multiple>
      Warning in `vec_locate_matches()`:
      Each element of `foo` can match at most 1 observation from `bar`.
      x The element at location 1 has multiple matches.

# `relationship` handles one-to-one case

    Code
      (expect_error(vec_locate_matches(c(2, 1), c(1, 1), relationship = "one_to_one"))
      )
    Output
      <error/vctrs_error_matches_relationship_one_to_one>
      Error in `vec_locate_matches()`:
      ! Each element can match at most 1 observation.
      x The element at location 2 has multiple matches.
    Code
      (expect_error(vec_locate_matches(c(1, 1), c(1, 2), relationship = "one_to_one"))
      )
    Output
      <error/vctrs_error_matches_relationship_one_to_one>
      Error in `vec_locate_matches()`:
      ! Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.

# `relationship` handles one-to-many case

    Code
      (expect_error(vec_locate_matches(c(1, 2, 2), c(2, 1), relationship = "one_to_many"))
      )
    Output
      <error/vctrs_error_matches_relationship_one_to_many>
      Error in `vec_locate_matches()`:
      ! Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.

# `relationship` handles many-to-one case

    Code
      (expect_error(vec_locate_matches(c(1, 2), c(1, 2, 2), relationship = "many_to_one"))
      )
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `vec_locate_matches()`:
      ! Each element can match at most 1 observation.
      x The element at location 2 has multiple matches.

# `relationship` handles warn-many-to-many case

    Code
      (expect_warning(vec_locate_matches(c(1, 2, 1), c(1, 2, 2), relationship = "warn_many_to_many"))
      )
    Output
      <warning/vctrs_warning_matches_relationship_many_to_many>
      Warning in `vec_locate_matches()`:
      Detected an unexpected many-to-many relationship.
      x The element at location 2 has multiple matches.
      x The element at location 1 has multiple matches.
    Code
      (expect_warning(vec_locate_matches(c(1, 1, 2), c(2, 2, 1), relationship = "warn_many_to_many"))
      )
    Output
      <warning/vctrs_warning_matches_relationship_many_to_many>
      Warning in `vec_locate_matches()`:
      Detected an unexpected many-to-many relationship.
      x The element at location 3 has multiple matches.
      x The element at location 3 has multiple matches.

# `relationship` considers `incomplete` matches as possible multiple matches

    Code
      (expect_error(vec_locate_matches(x, y, relationship = "one_to_many")))
    Output
      <error/vctrs_error_matches_relationship_one_to_many>
      Error in `vec_locate_matches()`:
      ! Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.

# `relationship` errors on multiple matches that come from different nesting containers

    Code
      (expect_error(vec_locate_matches(df, df2, condition = c("<=", "<="),
      relationship = "many_to_one")))
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `vec_locate_matches()`:
      ! Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.

# `relationship` errors when a match from a different nesting container is processed early on

    Code
      (expect_error(vec_locate_matches(needles, haystack, condition = "<",
        relationship = "many_to_one")))
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `vec_locate_matches()`:
      ! Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.

# `relationship` can still detect problematic `haystack` relationships when `multiple = first/last` are used

    Code
      (expect_error(vec_locate_matches(c(3, 1, 1), c(2, 1, 3, 3), multiple = "first",
      relationship = "one_to_one")))
    Output
      <error/vctrs_error_matches_relationship_one_to_one>
      Error in `vec_locate_matches()`:
      ! Each element can match at most 1 observation.
      x The element at location 2 has multiple matches.
    Code
      (expect_error(vec_locate_matches(c(3, 1, 1), c(2, 1, 3, 3), multiple = "first",
      relationship = "one_to_many")))
    Output
      <error/vctrs_error_matches_relationship_one_to_many>
      Error in `vec_locate_matches()`:
      ! Each element can match at most 1 observation.
      x The element at location 2 has multiple matches.

# `relationship` and `remaining` work properly together

    Code
      out <- vec_locate_matches(c(1, 2, 2), c(2, 3, 1, 1, 4), relationship = "warn_many_to_many",
      remaining = NA_integer_)
    Condition
      Warning in `vec_locate_matches()`:
      Detected an unexpected many-to-many relationship.
      x The element at location 1 has multiple matches.
      x The element at location 1 has multiple matches.

# `relationship` errors if `condition` creates multiple matches

    Code
      (expect_error(vec_locate_matches(1, c(1, 2), condition = "<=", relationship = "many_to_one"))
      )
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `vec_locate_matches()`:
      ! Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.

# `relationship` still errors if `filter` hasn't removed all multiple matches

    Code
      (expect_error(vec_locate_matches(1, c(1, 2, 1), condition = "<=", filter = "min",
      relationship = "many_to_one")))
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `vec_locate_matches()`:
      ! Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.

# `relationship` errors respect argument tags and error call

    Code
      (expect_error(vec_locate_matches(1L, c(1L, 1L), relationship = "one_to_one",
      needles_arg = "foo", haystack_arg = "bar", error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_relationship_one_to_one>
      Error in `fn()`:
      ! Each element of `foo` can match at most 1 observation from `bar`.
      x The element of `foo` at location 1 has multiple matches.
    Code
      (expect_error(vec_locate_matches(c(1L, 1L), 1L, relationship = "one_to_one",
      needles_arg = "foo", haystack_arg = "bar", error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_relationship_one_to_one>
      Error in `fn()`:
      ! Each element of `bar` can match at most 1 observation from `foo`.
      x The element of `bar` at location 1 has multiple matches.
    Code
      (expect_error(vec_locate_matches(c(1L, 1L), 1L, relationship = "one_to_many",
      needles_arg = "foo", haystack_arg = "bar", error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_relationship_one_to_many>
      Error in `fn()`:
      ! Each element of `bar` can match at most 1 observation from `foo`.
      x The element of `bar` at location 1 has multiple matches.
    Code
      (expect_error(vec_locate_matches(1L, c(1L, 1L), relationship = "many_to_one",
      needles_arg = "foo", haystack_arg = "bar", error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `fn()`:
      ! Each element of `foo` can match at most 1 observation from `bar`.
      x The element of `foo` at location 1 has multiple matches.

# `relationship` warnings respect argument tags and error call

    Code
      (expect_warning(vec_locate_matches(c(1L, 1L), c(1L, 1L), relationship = "warn_many_to_many",
      needles_arg = "foo", haystack_arg = "bar", error_call = call("fn"))))
    Output
      <warning/vctrs_warning_matches_relationship_many_to_many>
      Warning in `fn()`:
      Detected an unexpected many-to-many relationship between `foo` and `bar`.
      x The element of `foo` at location 1 has multiple matches.
      x The element of `bar` at location 1 has multiple matches.
    Code
      (expect_warning(vec_locate_matches(c(1L, 1L), c(1L, 1L), relationship = "warn_many_to_many",
      needles_arg = "foo", error_call = call("fn"))))
    Output
      <warning/vctrs_warning_matches_relationship_many_to_many>
      Warning in `fn()`:
      Detected an unexpected many-to-many relationship.
      x The element of `foo` at location 1 has multiple matches.
      x The element at location 1 has multiple matches.
    Code
      (expect_warning(vec_locate_matches(c(1L, 1L), c(1L, 1L), relationship = "warn_many_to_many",
      haystack_arg = "bar", error_call = call("fn"))))
    Output
      <warning/vctrs_warning_matches_relationship_many_to_many>
      Warning in `fn()`:
      Detected an unexpected many-to-many relationship.
      x The element at location 1 has multiple matches.
      x The element of `bar` at location 1 has multiple matches.

# `relationship` is validated

    Code
      (expect_error(vec_locate_matches(1, 2, relationship = 1.5)))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `relationship` must be a string.
    Code
      (expect_error(vec_locate_matches(1, 2, relationship = c("one_to_one",
        "one_to_many"))))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `relationship` must be a string.
    Code
      (expect_error(vec_locate_matches(1, 2, relationship = "x")))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `relationship` must be one of "none", "one_to_one", "one_to_many", "many_to_one", "many_to_many", or "warn_many_to_many".
    Code
      (expect_error(vec_locate_matches(1, 2, relationship = "x", error_call = call(
        "fn"))))
    Output
      <error/rlang_error>
      Error in `vec_locate_matches()`:
      ! `relationship` must be one of "none", "one_to_one", "one_to_many", "many_to_one", "many_to_many", or "warn_many_to_many".

# `no_match` can error informatively

    Code
      (expect_error(vec_locate_matches(1, 2, no_match = "error")))
    Output
      <error/vctrs_error_matches_nothing>
      Error in `vec_locate_matches()`:
      ! Each element must have a match.
      x The element at location 1 does not have a match.
    Code
      (expect_error(vec_locate_matches(1, 2, no_match = "error", needles_arg = "foo"))
      )
    Output
      <error/vctrs_error_matches_nothing>
      Error in `vec_locate_matches()`:
      ! Each element of `foo` must have a match.
      x The element at location 1 does not have a match.
    Code
      (expect_error(vec_locate_matches(1, 2, no_match = "error", needles_arg = "foo",
        error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_nothing>
      Error in `fn()`:
      ! Each element of `foo` must have a match.
      x The element at location 1 does not have a match.
    Code
      (expect_error(vec_locate_matches(1, 2, no_match = "error", needles_arg = "foo",
        haystack_arg = "bar")))
    Output
      <error/vctrs_error_matches_nothing>
      Error in `vec_locate_matches()`:
      ! Each element of `foo` must have a match in `bar`.
      x The element at location 1 does not have a match.

# errors with the right location on unmatched needles when different nesting containers are present

    Code
      (expect_error(vec_locate_matches(df, df2, condition = ">=", no_match = "error"))
      )
    Output
      <error/vctrs_error_matches_nothing>
      Error in `vec_locate_matches()`:
      ! Each element must have a match.
      x The element at location 2 does not have a match.

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
      ! Each haystack value must be matched.
      x The value at location 1 was not matched.
    Code
      (expect_error(vec_locate_matches(1, 2, remaining = "error", needles_arg = "foo"))
      )
    Output
      <error/vctrs_error_matches_remaining>
      Error in `vec_locate_matches()`:
      ! Each haystack value must be matched by `foo`.
      x The value at location 1 was not matched.
    Code
      (expect_error(vec_locate_matches(1, 2, remaining = "error", needles_arg = "foo",
        error_call = call("fn"))))
    Output
      <error/vctrs_error_matches_remaining>
      Error in `fn()`:
      ! Each haystack value must be matched by `foo`.
      x The value at location 1 was not matched.
    Code
      (expect_error(vec_locate_matches(1, 2, remaining = "error", needles_arg = "foo",
        haystack_arg = "bar")))
    Output
      <error/vctrs_error_matches_remaining>
      Error in `vec_locate_matches()`:
      ! Each haystack value of `bar` must be matched by `foo`.
      x The value at location 1 was not matched.

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

