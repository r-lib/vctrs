# incompatible type error validates `action`

    Code
      (expect_error(stop_incompatible_type(1, 1, x_arg = "", y_arg = "", action = "c"))
      )
    Output
      <error/rlang_error>
      `action` must be one of "combine" or "convert", not "c".
      Call: `stop_incompatible_type()`
    Code
      (expect_error(stop_incompatible_type(1, 1, x_arg = "", y_arg = "", action = 1)))
    Output
      <error/rlang_error>
      `action` must be a character vector.
      Call: `stop_incompatible_type()`

# can override arg in OOB conditions

    Code
      (expect_error(with_subscript_data(vec_slice(set_names(letters), "foo"), NULL),
      class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Can't subset elements that don't exist.
      x Element `foo` doesn't exist.
      Call: `stop_subscript()`
    Code
      (expect_error(with_subscript_data(vec_slice(set_names(letters), "foo"), quote(
        foo)), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Can't subset elements that don't exist.
      x Element `foo` doesn't exist.
      Call: `stop_subscript()`
    Code
      (expect_error(with_subscript_data(vec_slice(set_names(letters), "foo"), quote(
        foo(bar))), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Can't subset elements that don't exist.
      x Element `foo` doesn't exist.
      Call: `stop_subscript()`

# scalar type errors are informative

    Code
      (expect_error(vec_slice(foobar(list(1)), 1), class = "vctrs_error_scalar_type"))
    Output
      <error/vctrs_error_scalar_type>
      Input must be a vector, not a <vctrs_foobar> object.
      Call: `stop_vctrs()`
    Code
      (expect_error(stop_scalar_type(foobar(list(1)), arg = "foo"), class = "vctrs_error_scalar_type")
      )
    Output
      <error/vctrs_error_scalar_type>
      `foo` must be a vector, not a <vctrs_foobar> object.
      Call: `stop_vctrs()`

# empty names errors are informative

    Code
      (expect_error(vec_as_names(c("x", "", "y"), repair = "check_unique"), class = "vctrs_error_names_cannot_be_empty")
      )
    Output
      <error/vctrs_error_names_cannot_be_empty>
      Names can't be empty.
      x Empty name found at location 2.
      Call: `stop_vctrs()`
    Code
      (expect_error(vec_as_names(c("x", "", "y", ""), repair = "check_unique"),
      class = "vctrs_error_names_cannot_be_empty"))
    Output
      <error/vctrs_error_names_cannot_be_empty>
      Names can't be empty.
      x Empty names found at locations 2 and 4.
      Call: `stop_vctrs()`
    Code
      (expect_error(vec_as_names(rep("", 10), repair = "check_unique"), class = "vctrs_error_names_cannot_be_empty")
      )
    Output
      <error/vctrs_error_names_cannot_be_empty>
      Names can't be empty.
      x Empty names found at locations 1, 2, 3, 4, 5, etc.
      Call: `stop_vctrs()`

# dot dot names errors are informative

    Code
      (expect_error(vec_as_names(c("..1", "..1", "..1", "...", "z"), repair = "check_unique"),
      class = "vctrs_error_names_cannot_be_dot_dot"))
    Output
      <error/vctrs_error_names_cannot_be_dot_dot>
      Names can't be of the form `...` or `..j`.
      x These names are invalid:
        * "..1" at locations 1, 2, and 3.
        * "..." at location 4.
      Call: `stop_vctrs()`
    Code
      (expect_error(vec_as_names(c(rep("..1", 20), rep(c("..2", "..3", "..4", "...",
        "..5"), 2)), repair = "check_unique"), class = "vctrs_error_names_cannot_be_dot_dot")
      )
    Output
      <error/vctrs_error_names_cannot_be_dot_dot>
      Names can't be of the form `...` or `..j`.
      x These names are invalid:
        * "..1" at locations 1, 2, 3, 4, 5, etc.
        * "..2" at locations 21 and 26.
        * "..3" at locations 22 and 27.
        * "..4" at locations 23 and 28.
        * "..." at locations 24 and 29.
        * ...
      Call: `stop_vctrs()`

# unique names errors are informative

    Code
      (expect_error(vec_as_names(c("x", "x", "x", "y", "y", "z"), repair = "check_unique"),
      class = "vctrs_error_names_must_be_unique"))
    Output
      <error/vctrs_error_names_must_be_unique>
      Names must be unique.
      x These names are duplicated:
        * "x" at locations 1, 2, and 3.
        * "y" at locations 4 and 5.
      Call: `stop_vctrs()`
    Code
      (expect_error(vec_as_names(c(rep("x", 20), rep(c("a", "b", "c", "d", "e"), 2)),
      repair = "check_unique"), class = "vctrs_error_names_must_be_unique"))
    Output
      <error/vctrs_error_names_must_be_unique>
      Names must be unique.
      x These names are duplicated:
        * "x" at locations 1, 2, 3, 4, 5, etc.
        * "a" at locations 21 and 26.
        * "b" at locations 22 and 27.
        * "c" at locations 23 and 28.
        * "d" at locations 24 and 29.
        * ...
      Call: `stop_vctrs()`

# lossy cast from character to factor mentions loss of generality

    Code
      (expect_error(vec_cast("a", factor("b")), class = "vctrs_error_cast_lossy"))
    Output
      <error/vctrs_error_cast_lossy>
      Can't convert from <character> to <factor<9b7e3>> due to loss of generality.
      * Locations: 1
      Call: `stop_vctrs()`

# ordered cast failures mention conversion

    Code
      (expect_error(vec_cast(ordered("x"), ordered("y")), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Can't convert <ordered<bf275>> to <ordered<fd1ad>>.
      Call: `stop_vctrs()`

# incompatible size errors

    Code
      (expect_error(stop_incompatible_size(1:2, 3:5, 2L, 3L, x_arg = "", y_arg = "")))
    Output
      <error/vctrs_error_incompatible_size>
      Can't recycle input of size 2 to size 3.
      Call: `stop_vctrs()`
    Code
      (expect_error(stop_incompatible_size(1:2, 3:5, 2L, 3L, x_arg = quote(foo),
      y_arg = "")))
    Output
      <error/vctrs_error_incompatible_size>
      Can't recycle `foo` (size 2) to size 3.
      Call: `stop_vctrs()`
    Code
      (expect_error(stop_incompatible_size(1:2, 3:5, 2L, 3L, x_arg = "", y_arg = "bar"))
      )
    Output
      <error/vctrs_error_incompatible_size>
      Can't recycle input of size 2 to match `bar` (size 3).
      Call: `stop_vctrs()`
    Code
      (expect_error(stop_incompatible_size(1:2, 3:5, 2L, 3L, x_arg = quote(foo),
      y_arg = quote(bar))))
    Output
      <error/vctrs_error_incompatible_size>
      Can't recycle `foo` (size 2) to match `bar` (size 3).
      Call: `stop_vctrs()`

