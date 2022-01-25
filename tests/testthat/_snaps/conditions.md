# incompatible type error validates `action`

    Code
      (expect_error(stop_incompatible_type(1, 1, x_arg = "", y_arg = "", action = "c"))
      )
    Output
      <error/rlang_error>
      Error in `stop_incompatible_type()`:
      ! `action` must be one of "combine" or "convert", not "c".
    Code
      (expect_error(stop_incompatible_type(1, 1, x_arg = "", y_arg = "", action = 1)))
    Output
      <error/rlang_error>
      Error in `stop_incompatible_type()`:
      ! `action` must be a character vector, not a number.

# can override arg in OOB conditions

    Code
      (expect_error(with_subscript_data(vec_slice(set_names(letters), "foo"), NULL),
      class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements that don't exist.
      x Element `foo` doesn't exist.
    Code
      (expect_error(with_subscript_data(vec_slice(set_names(letters), "foo"), quote(
        foo)), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements that don't exist.
      x Element `foo` doesn't exist.
    Code
      (expect_error(with_subscript_data(vec_slice(set_names(letters), "foo"), quote(
        foo(bar))), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements that don't exist.
      x Element `foo` doesn't exist.

# scalar type errors are informative

    Code
      (expect_error(vec_slice(foobar(list(1)), 1), class = "vctrs_error_scalar_type"))
    Output
      <error/vctrs_error_scalar_type>
      Error:
      ! Input must be a vector, not a <vctrs_foobar> object.
    Code
      (expect_error(stop_scalar_type(foobar(list(1)), arg = "foo"), class = "vctrs_error_scalar_type")
      )
    Output
      <error/vctrs_error_scalar_type>
      Error:
      ! `foo` must be a vector, not a <vctrs_foobar> object.

# empty names errors are informative

    Code
      (expect_error(vec_as_names(c("x", "", "y"), repair = "check_unique"), class = "vctrs_error_names_cannot_be_empty")
      )
    Output
      <error/vctrs_error_names_cannot_be_empty>
      Error:
      ! Names can't be empty.
      x Empty name found at location 2.
    Code
      (expect_error(vec_as_names(c("x", "", "y", ""), repair = "check_unique"),
      class = "vctrs_error_names_cannot_be_empty"))
    Output
      <error/vctrs_error_names_cannot_be_empty>
      Error:
      ! Names can't be empty.
      x Empty names found at locations 2 and 4.
    Code
      (expect_error(vec_as_names(rep("", 10), repair = "check_unique"), class = "vctrs_error_names_cannot_be_empty")
      )
    Output
      <error/vctrs_error_names_cannot_be_empty>
      Error:
      ! Names can't be empty.
      x Empty names found at locations 1, 2, 3, 4, 5, etc.

# dot dot names errors are informative

    Code
      (expect_error(vec_as_names(c("..1", "..1", "..1", "...", "z"), repair = "check_unique"),
      class = "vctrs_error_names_cannot_be_dot_dot"))
    Output
      <error/vctrs_error_names_cannot_be_dot_dot>
      Error:
      ! Names can't be of the form `...` or `..j`.
      x These names are invalid:
        * "..1" at locations 1, 2, and 3.
        * "..." at location 4.
    Code
      (expect_error(vec_as_names(c(rep("..1", 20), rep(c("..2", "..3", "..4", "...",
        "..5"), 2)), repair = "check_unique"), class = "vctrs_error_names_cannot_be_dot_dot")
      )
    Output
      <error/vctrs_error_names_cannot_be_dot_dot>
      Error:
      ! Names can't be of the form `...` or `..j`.
      x These names are invalid:
        * "..1" at locations 1, 2, 3, 4, 5, etc.
        * "..2" at locations 21 and 26.
        * "..3" at locations 22 and 27.
        * "..4" at locations 23 and 28.
        * "..." at locations 24 and 29.
        * ...

# unique names errors are informative

    Code
      (expect_error(vec_as_names(c("x", "x", "x", "y", "y", "z"), repair = "check_unique"),
      class = "vctrs_error_names_must_be_unique"))
    Output
      <error/vctrs_error_names_must_be_unique>
      Error:
      ! Names must be unique.
      x These names are duplicated:
        * "x" at locations 1, 2, and 3.
        * "y" at locations 4 and 5.
    Code
      (expect_error(vec_as_names(c(rep("x", 20), rep(c("a", "b", "c", "d", "e"), 2)),
      repair = "check_unique"), class = "vctrs_error_names_must_be_unique"))
    Output
      <error/vctrs_error_names_must_be_unique>
      Error:
      ! Names must be unique.
      x These names are duplicated:
        * "x" at locations 1, 2, 3, 4, 5, etc.
        * "a" at locations 21 and 26.
        * "b" at locations 22 and 27.
        * "c" at locations 23 and 28.
        * "d" at locations 24 and 29.
        * ...

# lossy cast from character to factor mentions loss of generality

    Code
      (expect_error(vec_cast("a", factor("b")), class = "vctrs_error_cast_lossy"))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `fct_cast_impl()`:
      ! Can't convert from <character> to <factor<9b7e3>> due to loss of generality.
      * Locations: 1

# ordered cast failures mention conversion

    Code
      (expect_error(vec_cast(ordered("x"), ordered("y")), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Error:
      ! Can't convert <ordered<bf275>> to <ordered<fd1ad>>.

# incompatible size errors

    Code
      (expect_error(stop_incompatible_size(1:2, 3:5, 2L, 3L, x_arg = "", y_arg = "")))
    Output
      <error/vctrs_error_incompatible_size>
      Error:
      ! Can't recycle input of size 2 to size 3.
    Code
      (expect_error(stop_incompatible_size(1:2, 3:5, 2L, 3L, x_arg = quote(foo),
      y_arg = "")))
    Output
      <error/vctrs_error_incompatible_size>
      Error:
      ! Can't recycle `foo` (size 2) to size 3.
    Code
      (expect_error(stop_incompatible_size(1:2, 3:5, 2L, 3L, x_arg = "", y_arg = "bar"))
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error:
      ! Can't recycle input of size 2 to match `bar` (size 3).
    Code
      (expect_error(stop_incompatible_size(1:2, 3:5, 2L, 3L, x_arg = quote(foo),
      y_arg = quote(bar))))
    Output
      <error/vctrs_error_incompatible_size>
      Error:
      ! Can't recycle `foo` (size 2) to match `bar` (size 3).

