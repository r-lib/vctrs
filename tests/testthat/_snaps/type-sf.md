# sf has a cast method

    Code
      vctrs::vec_cast(sf1, sf2)
    Condition
      Error:
      ! Can't convert from `sf1` <sf<
        x   : logical
        geo1: sfc_POINT
      >> to <sf<
        y   : character
        x   : double
        geo2: sfc_LINESTRING
      >> due to loss of precision.

---

    Code
      vctrs::vec_cast(sf2, sf1)
    Condition
      Error:
      ! Can't convert from `sf2` <sf<
        y   : character
        x   : double
        geo2: sfc_LINESTRING
      >> to <sf<
        x   : logical
        geo1: sfc_POINT
      >> due to loss of precision.

---

    Code
      vctrs::vec_cast(new_data_frame(sf1), common)
    Condition
      Error in `vec_cast.sf.data.frame()`:
      ! Can't convert `new_data_frame(sf1)` <data.frame> to <sf>.

---

    Code
      vctrs::vec_cast(tibble::new_tibble(sf1), common)
    Condition
      Error in `vec_cast.sf.tbl_df()`:
      ! Can't convert `tibble::new_tibble(sf1)` <tibble> to <sf>.

# `precision` attributes of `sfc` vectors must be the same

    Code
      vctrs::vec_c(x, y)
    Condition
      Error in `vctrs::vec_c()`:
      ! Can't combine `..1` <sfc_GEOMETRY> and `..2` <sfc_GEOMETRY>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# `crs` attributes of `sfc` vectors must be the same

    Code
      vctrs::vec_c(x, y)
    Condition
      Error in `vctrs::vec_c()`:
      ! Can't combine `..1` <sfc_GEOMETRY> and `..2` <sfc_GEOMETRY>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

