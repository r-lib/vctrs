# `incomplete` can error informatively

    Code
      (expect_error(vec_locate_matches(NA, 1, incomplete = "error")))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `stop_vctrs()`: No element can contain missing values.
      x The element at location 1 contains missing values.
    Code
      (expect_error(vec_locate_matches(NA, 1, incomplete = "error", needles_arg = "foo"))
      )
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `stop_vctrs()`: No element of `foo` can contain missing values.
      x The element at location 1 contains missing values.

# `incomplete` is validated

    Code
      (expect_error(vec_locate_matches(1, 2, incomplete = 1.5)))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `stop_vctrs()`: Can't convert from `incomplete` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      (expect_error(vec_locate_matches(1, 2, incomplete = c("match", "drop"))))
    Output
      <error/rlang_error>
      Error: `incomplete` must be length 1, not length 2.
    Code
      (expect_error(vec_locate_matches(1, 2, incomplete = "x")))
    Output
      <error/rlang_error>
      Error: `incomplete` must be one of: "match", "drop", or "error".

# `multiple` can error informatively

    Code
      (expect_error(vec_locate_matches(1L, c(1L, 1L), multiple = "error")))
    Output
      <error/vctrs_error_matches_multiple>
      Error in `stop_vctrs()`: Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.
    Code
      (expect_error(vec_locate_matches(1L, c(1L, 1L), multiple = "error",
      needles_arg = "foo")))
    Output
      <error/vctrs_error_matches_multiple>
      Error in `stop_vctrs()`: Each element of `foo` can match at most 1 observation.
      x The element at location 1 has multiple matches.
    Code
      (expect_error(vec_locate_matches(1L, c(1L, 1L), multiple = "error",
      needles_arg = "foo", haystack_arg = "bar")))
    Output
      <error/vctrs_error_matches_multiple>
      Error in `stop_vctrs()`: Each element of `foo` can match at most 1 observation from `bar`.
      x The element at location 1 has multiple matches.
    Code
      (expect_error(vec_locate_matches(1, 1:2, multiple = "error", condition = NULL)))
    Output
      <error/vctrs_error_matches_multiple>
      Error in `stop_vctrs()`: Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.

# `multiple` can warn informatively

    Code
      (expect_warning(vec_locate_matches(1L, c(1L, 1L), multiple = "warning")))
    Output
      <warning/vctrs_warning_matches_multiple>
      Warning: Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.
    Code
      (expect_warning(vec_locate_matches(1L, c(1L, 1L), multiple = "warning",
      needles_arg = "foo")))
    Output
      <warning/vctrs_warning_matches_multiple>
      Warning: Each element of `foo` can match at most 1 observation.
      x The element at location 1 has multiple matches.
    Code
      (expect_warning(vec_locate_matches(1L, c(1L, 1L), multiple = "warning",
      needles_arg = "foo", haystack_arg = "bar")))
    Output
      <warning/vctrs_warning_matches_multiple>
      Warning: Each element of `foo` can match at most 1 observation from `bar`.
      x The element at location 1 has multiple matches.
    Code
      (expect_warning(vec_locate_matches(1, 1:2, multiple = "warning", condition = NULL))
      )
    Output
      <warning/vctrs_warning_matches_multiple>
      Warning: Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.

# `no_match` can error informatively

    Code
      (expect_error(vec_locate_matches(1, 2, no_match = "error")))
    Output
      <error/vctrs_error_matches_nothing>
      Error in `stop_vctrs()`: Each element must have a match.
      x The element at location 1 does not have a match.
    Code
      (expect_error(vec_locate_matches(1, 2, no_match = "error", needles_arg = "foo"))
      )
    Output
      <error/vctrs_error_matches_nothing>
      Error in `stop_vctrs()`: Each element of `foo` must have a match.
      x The element at location 1 does not have a match.
    Code
      (expect_error(vec_locate_matches(1, 2, no_match = "error", needles_arg = "foo",
        haystack_arg = "bar")))
    Output
      <error/vctrs_error_matches_nothing>
      Error in `stop_vctrs()`: Each element of `foo` must have a match in `bar`.
      x The element at location 1 does not have a match.
    Code
      (expect_error(vec_locate_matches(1, double(), no_match = "error", condition = NULL))
      )
    Output
      <error/vctrs_error_matches_nothing>
      Error in `stop_vctrs()`: Each element must have a match.
      x The element at location 1 does not have a match.

# `no_match` is validated

    Code
      (expect_error(vec_locate_matches(1, 2, no_match = 1.5)))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `stop_vctrs()`: Can't convert from `no_match` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      (expect_error(vec_locate_matches(1, 2, no_match = c(1L, 2L))))
    Output
      <error/rlang_error>
      Error: `no_match` must be length 1, not length 2.
    Code
      (expect_error(vec_locate_matches(1, 2, no_match = "x")))
    Output
      <error/rlang_error>
      Error: `no_match` must be either "drop" or "error".

# `remaining` can error informatively

    Code
      (expect_error(vec_locate_matches(1, 2, remaining = "error")))
    Output
      <error/vctrs_error_matches_remaining>
      Error in `stop_vctrs()`: Each haystack value must be matched.
      x The value at location 1 was not matched.
    Code
      (expect_error(vec_locate_matches(1, 2, remaining = "error", needles_arg = "foo"))
      )
    Output
      <error/vctrs_error_matches_remaining>
      Error in `stop_vctrs()`: Each haystack value must be matched by `foo`.
      x The value at location 1 was not matched.
    Code
      (expect_error(vec_locate_matches(1, 2, remaining = "error", needles_arg = "foo",
        haystack_arg = "bar")))
    Output
      <error/vctrs_error_matches_remaining>
      Error in `stop_vctrs()`: Each haystack value of `bar` must be matched by `foo`.
      x The value at location 1 was not matched.
    Code
      (expect_error(vec_locate_matches(double(), c(1, 2), remaining = "error",
      condition = NULL)))
    Output
      <error/vctrs_error_matches_remaining>
      Error in `stop_vctrs()`: Each haystack value must be matched.
      x The value at location 1 was not matched.

# `remaining` is validated

    Code
      (expect_error(vec_locate_matches(1, 2, remaining = 1.5)))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `stop_vctrs()`: Can't convert from `remaining` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      (expect_error(vec_locate_matches(1, 2, remaining = c(1L, 2L))))
    Output
      <error/rlang_error>
      Error: `remaining` must be length 1, not length 2.
    Code
      (expect_error(vec_locate_matches(1, 2, remaining = "x")))
    Output
      <error/rlang_error>
      Error: `remaining` must be either "drop" or "error".

# potential overflow on large output size is caught informatively

    Code
      (expect_error(vec_locate_matches(1:1e+07, 1:1e+07, condition = ">=")))
    Output
      <error/rlang_error>
      Error: Match procedure results in an allocation larger than 2^31-1 elements. Attempted allocation size was 50000005000000. Please report this to the vctrs maintainers at <https://github.com/r-lib/vctrs/issues>.
    Code
      (expect_error(vec_locate_matches(1:1e+07, 1:1e+07, condition = NULL)))
    Output
      <error/rlang_error>
      Error: Match procedure results in an allocation larger than 2^31-1 elements. Attempted allocation size was 100000000000000. Please report this to the vctrs maintainers at <https://github.com/r-lib/vctrs/issues>.
