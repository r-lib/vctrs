# `missing` can error informatively

    Code
      vec_matches(NA, 1, missing = "error")
    Error <vctrs_error_matches_missing>
      No element can be missing.
      x The element at location 1 is missing.

---

    Code
      vec_matches(NA, 1, missing = "error", needles_arg = "foo")
    Error <vctrs_error_matches_missing>
      No element of `foo` can be missing.
      x The element at location 1 is missing.

# `missing` is validated

    Code
      vec_matches(1, 2, missing = 1.5)
    Error <vctrs_error_cast_lossy>
      Can't convert from `missing` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      vec_matches(1, 2, missing = c("match", "drop"))
    Error <rlang_error>
      `missing` must be length 1, not length 2.

---

    Code
      vec_matches(1, 2, missing = "x")
    Error <rlang_error>
      `missing` must be one of: "match", "drop", or "error".

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

