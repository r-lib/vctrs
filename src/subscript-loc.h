#ifndef VCTRS_SUBSCRIPT_LOC_H
#define VCTRS_SUBSCRIPT_LOC_H

#include "utils.h"
#include "subscript.h"


enum subscript_missing {
  SUBSCRIPT_MISSING_PROPAGATE,
  SUBSCRIPT_MISSING_ERROR
};
enum num_as_location_loc_negative {
  LOC_NEGATIVE_INVERT,
  LOC_NEGATIVE_ERROR,
  LOC_NEGATIVE_IGNORE
};
enum num_as_location_loc_oob {
  LOC_OOB_EXTEND,
  LOC_OOB_ERROR
};
enum num_as_location_loc_zero {
  LOC_ZERO_REMOVE,
  LOC_ZERO_ERROR,
  LOC_ZERO_IGNORE
};

struct vec_as_location_opts {
  const struct vec_as_subscript_opts* subscript_opts;
  enum num_as_location_loc_negative loc_negative;
  enum num_as_location_loc_oob loc_oob;
  enum num_as_location_loc_zero loc_zero;
  enum subscript_missing missing;
};

extern struct vec_as_location_opts vec_as_location_default_opts_obj;
extern struct vec_as_location_opts vec_as_location_default_assign_opts_obj;

static const struct vec_as_location_opts* const vec_as_location_default_opts = &vec_as_location_default_opts_obj;
static const struct vec_as_location_opts* const vec_as_location_default_assign_opts = &vec_as_location_default_assign_opts_obj;


SEXP vec_as_location(SEXP i, R_len_t n, SEXP names);
SEXP vec_as_location_opts(SEXP subscript, R_len_t n, SEXP names,
                          const struct vec_as_location_opts* location_opts);


#endif
