#include "vctrs.h"
#include "utils.h"

static SEXP vec_cast_dispatch2(SEXP x,
                               SEXP to,
                               enum vctrs_type x_type,
                               bool* lossy,
                               struct vctrs_arg* x_arg,
                               struct vctrs_arg* to_arg);

static SEXP vec_cast_dispatch_unspecified_s3(SEXP x, SEXP to);

// [[ include("vctrs.h") ]]
SEXP vec_cast_dispatch(SEXP x,
                       SEXP to,
                       enum vctrs_type x_type,
                       enum vctrs_type to_type,
                       bool* lossy,
                       struct vctrs_arg* x_arg,
                       struct vctrs_arg* to_arg) {
  if (x_type == vctrs_type_unspecified) {
    return vec_cast_dispatch_unspecified_s3(x, to);
  }

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

  default:
    break;
  }

  return R_NilValue;
}

// For known bare classes, we can immediately call `vec_init()`.
// Otherwise, we have to allow R level dispatch.
static SEXP vec_cast_dispatch_unspecified_s3(SEXP x, SEXP to) {
  switch(class_type(to)) {
  case vctrs_class_bare_data_frame:
  case vctrs_class_bare_tibble:
  case vctrs_class_bare_factor:
  case vctrs_class_bare_ordered:
  case vctrs_class_bare_date:
  case vctrs_class_bare_posixct:
  case vctrs_class_bare_posixlt:
    return vec_init(to, vec_size(x));

  case vctrs_class_rcrd:
  case vctrs_class_data_frame:
  case vctrs_class_posixlt:
  case vctrs_class_unknown:
    return R_NilValue;

  case vctrs_class_none:
    Rf_errorcall(R_NilValue, "Internal error: The non-unspecified object should be S3");
  }

  never_reached("vec_cast_dispatch_unspecified_s3");
}
