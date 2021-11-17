# `class` must be a character vector

    Code
      (expect_error(new_vctr(1, class = 1)))
    Output
      <error/rlang_error>
      Error: `class` must be a character vector.

# `inherit_base_type` is validated

    Code
      (expect_error(new_vctr(1, inherit_base_type = 1)))
    Output
      <error/rlang_error>
      Error: `inherit_base_type` must be `NULL` or a single `TRUE` or `FALSE`.
    Code
      (expect_error(new_vctr(1, inherit_base_type = NA)))
    Output
      <error/rlang_error>
      Error: `inherit_base_type` must be `NULL` or a single `TRUE` or `FALSE`.
    Code
      (expect_error(new_vctr(1, inherit_base_type = c(TRUE, FALSE))))
    Output
      <error/rlang_error>
      Error: `inherit_base_type` must be `NULL` or a single `TRUE` or `FALSE`.

# na.fail() works

    Code
      na.fail(x)
    Error <simpleError>
      missing values in object

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

