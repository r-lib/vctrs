# `incomplete` can error informatively

    Code
      vec_matches(NA, 1, incomplete = "error")
    Error <vctrs_error_matches_incomplete>
      No element can contain missing values.
      x The element at location 1 contains missing values.

---

    Code
      vec_matches(NA, 1, incomplete = "error", needles_arg = "foo")
    Error <vctrs_error_matches_incomplete>
      No element of `foo` can contain missing values.
      x The element at location 1 contains missing values.

# `incomplete` is validated

    Code
      vec_matches(1, 2, incomplete = 1.5)
    Error <vctrs_error_cast_lossy>
      Can't convert from `incomplete` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      vec_matches(1, 2, incomplete = c("match", "drop"))
    Error <rlang_error>
      `incomplete` must be length 1, not length 2.

---

    Code
      vec_matches(1, 2, incomplete = "x")
    Error <rlang_error>
      `incomplete` must be one of: "match", "drop", or "error".

# `multiple` can error informatively

    Code
      vec_matches(1L, c(1L, 1L), multiple = "error")
    Error <vctrs_error_matches_multiple>
      Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.

---

    Code
      vec_matches(1L, c(1L, 1L), multiple = "error", needles_arg = "foo")
    Error <vctrs_error_matches_multiple>
      Each element of `foo` can match at most 1 observation.
      x The element at location 1 has multiple matches.

---

    Code
      vec_matches(1L, c(1L, 1L), multiple = "error", needles_arg = "foo",
      haystack_arg = "bar")
    Error <vctrs_error_matches_multiple>
      Each element of `foo` can match at most 1 observation from `bar`.
      x The element at location 1 has multiple matches.

---

    Code
      vec_matches(1, 1:2, multiple = "error", condition = NULL)
    Error <vctrs_error_matches_multiple>
      Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.

# `multiple` can warn informatively

    Code
      vec_matches(1L, c(1L, 1L), multiple = "warning")
    Warning <vctrs_warning_matches_multiple>
      Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.
    Output
        needles haystack
      1       1        1
      2       1        2

---

    Code
      vec_matches(1L, c(1L, 1L), multiple = "warning", needles_arg = "foo")
    Warning <vctrs_warning_matches_multiple>
      Each element of `foo` can match at most 1 observation.
      x The element at location 1 has multiple matches.
    Output
        needles haystack
      1       1        1
      2       1        2

---

    Code
      vec_matches(1L, c(1L, 1L), multiple = "warning", needles_arg = "foo",
      haystack_arg = "bar")
    Warning <vctrs_warning_matches_multiple>
      Each element of `foo` can match at most 1 observation from `bar`.
      x The element at location 1 has multiple matches.
    Output
        needles haystack
      1       1        1
      2       1        2

---

    Code
      vec_matches(1, 1:2, multiple = "warning", condition = NULL)
    Warning <vctrs_warning_matches_multiple>
      Each element can match at most 1 observation.
      x The element at location 1 has multiple matches.
    Output
        needles haystack
      1       1        1
      2       1        2

# `no_match` can error informatively

    Code
      vec_matches(1, 2, no_match = "error")
    Error <vctrs_error_matches_nothing>
      Each element must have a match.
      x The element at location 1 does not have a match.

---

    Code
      vec_matches(1, 2, no_match = "error", needles_arg = "foo")
    Error <vctrs_error_matches_nothing>
      Each element of `foo` must have a match.
      x The element at location 1 does not have a match.

---

    Code
      vec_matches(1, 2, no_match = "error", needles_arg = "foo", haystack_arg = "bar")
    Error <vctrs_error_matches_nothing>
      Each element of `foo` must have a match in `bar`.
      x The element at location 1 does not have a match.

---

    Code
      vec_matches(1, double(), no_match = "error", condition = NULL)
    Error <vctrs_error_matches_nothing>
      Each element must have a match.
      x The element at location 1 does not have a match.

# `no_match` is validated

    Code
      vec_matches(1, 2, no_match = 1.5)
    Error <vctrs_error_cast_lossy>
      Can't convert from `no_match` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      vec_matches(1, 2, no_match = c(1L, 2L))
    Error <rlang_error>
      `no_match` must be length 1, not length 2.

---

    Code
      vec_matches(1, 2, no_match = "x")
    Error <rlang_error>
      `no_match` must be either "drop" or "error".

# `remaining` can error informatively

    Code
      vec_matches(1, 2, remaining = "error")
    Error <vctrs_error_matches_remaining>
      Each haystack value must be matched.
      x The value at location 1 was not matched.

---

    Code
      vec_matches(1, 2, remaining = "error", needles_arg = "foo")
    Error <vctrs_error_matches_remaining>
      Each haystack value must be matched by `foo`.
      x The value at location 1 was not matched.

---

    Code
      vec_matches(1, 2, remaining = "error", needles_arg = "foo", haystack_arg = "bar")
    Error <vctrs_error_matches_remaining>
      Each haystack value of `bar` must be matched by `foo`.
      x The value at location 1 was not matched.

---

    Code
      vec_matches(double(), c(1, 2), remaining = "error", condition = NULL)
    Error <vctrs_error_matches_remaining>
      Each haystack value must be matched.
      x The value at location 1 was not matched.

# `remaining` is validated

    Code
      vec_matches(1, 2, remaining = 1.5)
    Error <vctrs_error_cast_lossy>
      Can't convert from `remaining` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      vec_matches(1, 2, remaining = c(1L, 2L))
    Error <rlang_error>
      `remaining` must be length 1, not length 2.

---

    Code
      vec_matches(1, 2, remaining = "x")
    Error <rlang_error>
      `remaining` must be either "drop" or "error".

# potential overflow on large output size is caught informatively

    Code
      vec_matches(1:1e+07, 1:1e+07, condition = ">=")
    Error <rlang_error>
      Match procedure results in an allocation larger than 2^31-1 elements. Attempted allocation size was 50000005000000. Please report this to the vctrs maintainers at <https://github.com/r-lib/vctrs/issues>.

---

    Code
      vec_matches(1:1e+07, 1:1e+07, condition = NULL)
    Error <rlang_error>
      Match procedure results in an allocation larger than 2^31-1 elements. Attempted allocation size was 100000000000000. Please report this to the vctrs maintainers at <https://github.com/r-lib/vctrs/issues>.

