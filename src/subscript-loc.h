#ifndef VCTRS_SUBSCRIPT_LOC_H
#define VCTRS_SUBSCRIPT_LOC_H

#include "vctrs-core.h"
#include "utils.h"
#include "subscript.h"


enum subscript_missing {
  SUBSCRIPT_MISSING_PROPAGATE = 0,
  SUBSCRIPT_MISSING_REMOVE,
  SUBSCRIPT_MISSING_ERROR
};
enum num_loc_negative {
  LOC_NEGATIVE_INVERT = 0,
  LOC_NEGATIVE_ERROR,
  LOC_NEGATIVE_IGNORE
};
enum num_loc_oob {
  LOC_OOB_ERROR = 0,
  LOC_OOB_REMOVE,
  LOC_OOB_EXTEND
};
enum num_loc_zero {
  LOC_ZERO_REMOVE = 0,
  LOC_ZERO_ERROR,
  LOC_ZERO_IGNORE
};

struct location_opts {
  struct subscript_opts subscript_opts;
  enum num_loc_negative loc_negative;
  enum num_loc_oob loc_oob;
  enum num_loc_zero loc_zero;
  enum subscript_missing missing;
};

static inline
struct location_opts new_location_opts_assign() {
  return (struct location_opts) {
    .subscript_opts = new_subscript_opts_assign()
  };
}


r_obj* vec_as_location(r_obj* i,
                       r_ssize n,
                       r_obj* names);

r_obj* vec_as_location_ctxt(r_obj* subscript,
                            r_ssize n,
                            r_obj* names,
                            struct vctrs_arg* arg,
                            struct r_lazy call);

r_obj* vec_as_location_opts(r_obj* subscript,
                            r_ssize n,
                            r_obj* names,
                            const struct location_opts* location_opts);


#endif
