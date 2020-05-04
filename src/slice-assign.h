#ifndef VCTRS_SLICE_ASSIGN_H
#define VCTRS_SLICE_ASSIGN_H

#include "utils.h"

enum vctrs_ownership {
  vctrs_ownership_owned,
  vctrs_ownership_shared,
  vctrs_ownership_unknown
};

static inline enum vctrs_ownership parse_ownership(SEXP ownership) {
  if (!r_is_string(ownership)) {
    Rf_errorcall(R_NilValue, "Internal error: `ownership` must be a string.");
  }

  const char* str = CHAR(STRING_ELT(ownership, 0));

  if (!strcmp(str, "owned")) return vctrs_ownership_owned;
  if (!strcmp(str, "shared")) return vctrs_ownership_shared;
  if (!strcmp(str, "unknown")) return vctrs_ownership_unknown;

  Rf_errorcall(R_NilValue, "Internal error: `ownership` must be one of 'owned', 'shared', or 'unknown'.");
}

struct vec_assign_opts {
  bool assign_names;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* value_arg;
};

SEXP vec_assign_opts(SEXP x, SEXP index, SEXP value,
                     enum vctrs_ownership ownership,
                     const struct vec_assign_opts* opts);

SEXP vec_proxy_assign_opts(SEXP proxy, SEXP index, SEXP value,
                           enum vctrs_ownership ownership,
                           const struct vec_assign_opts* opts);

SEXP chr_assign(SEXP out, SEXP index, SEXP value, enum vctrs_ownership ownership);
SEXP list_assign(SEXP out, SEXP index, SEXP value, enum vctrs_ownership ownership);
SEXP df_assign(SEXP x, SEXP index, SEXP value,
               enum vctrs_ownership ownership,
               const struct vec_assign_opts* opts);

SEXP vec_assign_shaped(SEXP proxy, SEXP index, SEXP value,
                       enum vctrs_ownership ownership,
                       const struct vec_assign_opts* opts);

#endif
