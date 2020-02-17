#include "vctrs.h"
#include "utils.h"

static SEXP vec_cast_dispatch2(SEXP x,
                               SEXP to,
                               enum vctrs_type x_type,
                               bool* lossy,
                               struct vctrs_arg* x_arg,
                               struct vctrs_arg* to_arg);

// [[ include("vctrs.h") ]]
SEXP vec_cast_dispatch(SEXP x,
                       SEXP to,
                       enum vctrs_type x_type,
                       enum vctrs_type to_type,
                       bool* lossy,
                       struct vctrs_arg* x_arg,
                       struct vctrs_arg* to_arg) {
  switch (to_type) {
  case vctrs_type_character:
    switch (class_type(x)) {
    case vctrs_class_bare_factor:
      return fct_as_character(x, x_arg);

    case vctrs_class_bare_ordered:
      return ord_as_character(x, x_arg);

    default:
      break;
    }
    break;

  case vctrs_type_dataframe:
    switch(class_type(x)) {
    case vctrs_class_bare_data_frame:
      Rf_errorcall(R_NilValue, "Internal error: `x` should have been classified as a `vctrs_type_dataframe`");

    case vctrs_class_bare_tibble:
      return df_as_dataframe(x, to, x_arg, to_arg);

    default:
      break;
    }
    break;

  case vctrs_type_s3:
    return vec_cast_dispatch2(x, to, x_type, lossy, x_arg, to_arg);

  default:
    break;
  }

  return R_NilValue;
}


static SEXP vec_cast_dispatch2(SEXP x, SEXP to,
                               enum vctrs_type x_type,
                               bool* lossy,
                               struct vctrs_arg* x_arg,
                               struct vctrs_arg* to_arg) {
  switch (class_type(to)) {
  case vctrs_class_bare_factor:
    switch (x_type) {
    case vctrs_type_character:
      return chr_as_factor(x, to, lossy, to_arg);

    case vctrs_type_s3:
      switch (class_type(x)) {
      case vctrs_class_bare_factor:
        return fct_as_factor(x, to, lossy, x_arg, to_arg);

      default:
        break;
      }

    default:
      break;
    }
    break;

  case vctrs_class_bare_ordered:
    switch (x_type) {
    case vctrs_type_character:
      return chr_as_ordered(x, to, lossy, to_arg);

    case vctrs_type_s3:
      switch (class_type(x)) {
      case vctrs_class_bare_ordered:
        return ord_as_ordered(x, to, lossy, x_arg, to_arg);

      default:
        break;
      }

    default:
      break;
    }
    break;

  case vctrs_class_bare_tibble:
    switch (x_type) {
    case vctrs_type_dataframe:
      return df_as_dataframe(x, to, x_arg, to_arg);

    case vctrs_type_s3:
      switch (class_type(x)) {
      case vctrs_class_bare_data_frame:
        Rf_errorcall(R_NilValue, "Internal error: `x` should have been classified as a `vctrs_type_dataframe`");

      case vctrs_class_bare_tibble:
        return df_as_dataframe(x, to, x_arg, to_arg);

      default:
        break;
      }

    default:
      break;
    }
    break;

  case vctrs_class_bare_data_frame:
    Rf_errorcall(R_NilValue, "Internal error: `to` should have been classified as a `vctrs_type_dataframe`");

  default:
    break;
  }

  return R_NilValue;
}
